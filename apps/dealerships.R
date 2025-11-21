# Dealership Inventory Analysis - Standalone
# Shows percentage of dealerships with at least one vehicle (faceted time series)

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

# Load dealership data from jsDelivr CDN (CORS-enabled for Shinylive)
base_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/"

# Preload all possible data combinations
dealership_data <- list(
  "powertrain_vehicle_type" = read.csv(paste0(base_url, "p_one_powertrain_vehicle_type.csv")),
  "powertrain_price_bin" = read.csv(paste0(base_url, "p_one_powertrain_price_bin.csv")),
  "vehicle_type_price_bin" = read.csv(paste0(base_url, "p_one_vehicle_type_price_bin.csv"))
)

# Define UI
ui <- page_sidebar(
  title = "Dealership Inventory Analysis",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#ffffff",
    fg = "#2c3e50"
  ),
  fillable = TRUE,

  sidebar = sidebar(
    title = "Dealerships Controls",
    width = 250,

    radioButtons(
      "dealer_var1",
      "First Variable:",
      choices = list(
        "Powertrain" = "powertrain",
        "Vehicle Type" = "vehicle_type",
        "Price Bin" = "price_bin"
      ),
      selected = "powertrain"
    ),

    radioButtons(
      "dealer_var2",
      "Second Variable:",
      choices = list(
        "Powertrain" = "powertrain",
        "Vehicle Type" = "vehicle_type",
        "Price Bin" = "price_bin"
      ),
      selected = "vehicle_type"
    )
  ),

  # Main content
  card(
    full_screen = TRUE,
    card_header(
      "Percentage of Dealerships with at Least One Vehicle",
      class = "text-center"
    ),
    plotlyOutput("dealer_plot", height = "700px")
  )
)

# Server
server <- function(input, output, session) {

  # Observer to update var2 choices based on var1 selection
  observeEvent(input$dealer_var1, {
    all_choices <- list(
      "Powertrain" = "powertrain",
      "Vehicle Type" = "vehicle_type",
      "Price Bin" = "price_bin"
    )

    # Remove the selected var1 from var2 choices
    available_choices <- all_choices[all_choices != input$dealer_var1]

    # Get current var2 selection
    current_var2 <- input$dealer_var2

    # If current var2 is same as var1, select the first available option
    new_selection <- if (current_var2 == input$dealer_var1) {
      available_choices[[1]]
    } else {
      current_var2
    }

    updateRadioButtons(
      session,
      "dealer_var2",
      choices = available_choices,
      selected = new_selection
    )
  })

  # Load appropriate data based on selected variables
  dealerships_data_reactive <- reactive({
    var1 <- input$dealer_var1
    var2 <- input$dealer_var2

    # Construct key for data lookup
    data_key <- paste0(var1, "_", var2)

    if (data_key %in% names(dealership_data)) {
      return(dealership_data[[data_key]])
    } else {
      return(data.frame()) # Return empty if not found
    }
  })

  # Dealerships Plot (from app.R lines 3463-3635)
  output$dealer_plot <- renderPlotly({
    data <- dealerships_data_reactive()

    if (nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available",
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE
        ))
    }

    # Label mapping function (same as in Listings)
    get_label <- function(var_name, value) {
      if (var_name == "powertrain") {
        labels <- c(
          "bev" = "BEV",
          "cv" = "Conventional",
          "hev" = "Hybrid",
          "phev" = "PHEV"
        )
        return(labels[value])
      } else if (var_name == "vehicle_type") {
        labels <- c("car" = "Car", "cuv" = "CUV", "suv" = "SUV",
                   "pickup" = "Pickup", "minivan" = "Minivan")
        return(labels[value])
      } else {
        return(value)
      }
    }

    # Get selected variables
    var1 <- input$dealer_var1
    var2 <- input$dealer_var2

    # Get unique values for each variable with proper ordering
    if (var1 == "powertrain") {
      var1_order <- c("cv", "hev", "phev", "bev")
      var1_values <- intersect(var1_order, unique(data[[var1]]))
    } else if (var1 == "vehicle_type") {
      var1_order <- c("car", "cuv", "suv", "pickup", "minivan")
      var1_values <- intersect(var1_order, unique(data[[var1]]))
    } else if (var1 == "price_bin") {
      var1_order <- c("$0-$30k", "$30k-$40k", "$40k-$50k", "$50k-$60k", "$60k+")
      var1_values <- intersect(var1_order, unique(data[[var1]]))
    } else {
      var1_values <- sort(unique(data[[var1]]))
    }

    if (var2 == "powertrain") {
      var2_order <- c("cv", "hev", "phev", "bev")
      var2_values <- intersect(var2_order, unique(data[[var2]]))
    } else if (var2 == "vehicle_type") {
      var2_order <- c("car", "cuv", "suv", "pickup", "minivan")
      var2_values <- intersect(var2_order, unique(data[[var2]]))
    } else if (var2 == "price_bin") {
      var2_order <- c("$0-$30k", "$30k-$40k", "$40k-$50k", "$50k-$60k", "$60k+")
      var2_values <- intersect(var2_order, unique(data[[var2]]))
    } else {
      var2_values <- sort(unique(data[[var2]]))
    }

    # Create grid dimensions
    n_rows <- length(var2_values)
    n_cols <- length(var1_values)

    # Create list to store individual plots
    plot_list <- list()

    for (i in seq_along(var2_values)) {
      for (j in seq_along(var1_values)) {
        val1 <- var1_values[j]
        val2 <- var2_values[i]

        # Filter data for this combination
        plot_data <- data %>%
          filter(.data[[var1]] == val1, .data[[var2]] == val2) %>%
          arrange(listing_year)

        # Create a plot for this facet
        p <- plot_ly()

        if (nrow(plot_data) > 0) {
          p <- p %>%
            add_trace(
              type = "scatter",
              mode = "lines",
              x = plot_data$listing_year,
              y = plot_data$p,
              line = list(color = "rgba(50, 50, 50, 0.8)", width = 2),
              showlegend = FALSE,
              hovertemplate = paste0(
                "Year: %{x}<br>",
                "Percentage: ",
                sprintf("%.1f", plot_data$p * 100),
                "%",
                "<extra></extra>"
              )
            )
        }

        plot_list[[(i - 1) * n_cols + j]] <- p
      }
    }

    # Create subplot grid
    fig <- subplot(
      plot_list,
      nrows = n_rows,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    )

    # Add annotations for facet labels
    annotations <- list()

    # Column headers (var1 values)
    for (j in seq_along(var1_values)) {
      var1_label <- get_label(var1, var1_values[j])
      annotations[[length(annotations) + 1]] <- list(
        x = (j - 0.5) / n_cols,
        y = 1.02,
        text = paste0("<b>", var1_label, "</b>"),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 11)
      )
    }

    # Row headers (var2 values)
    for (i in seq_along(var2_values)) {
      var2_label <- get_label(var2, var2_values[i])
      annotations[[length(annotations) + 1]] <- list(
        x = -0.02,
        y = 1 - (i - 0.5) / n_rows,
        text = paste0("<b>", var2_label, "</b>"),
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "middle",
        showarrow = FALSE,
        font = list(size = 11),
        textangle = -90
      )
    }

    fig <- fig %>%
      layout(
        title = list(
          text = paste0(
            "Percentage of Dealerships with At Least One Vehicle<br>",
            "<sub>by ",
            tools::toTitleCase(var1),
            " and ",
            tools::toTitleCase(var2),
            "</sub>"
          ),
          x = 0.5,
          xanchor = "center"
        ),
        annotations = annotations,
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Percentage (%)",
          tickformat = ".0%"
        ),
        hovermode = "closest",
        showlegend = FALSE,
        margin = list(t = 100, b = 60, l = 100, r = 30)
      )

    return(fig)
  })
}

# Run app
shinyApp(ui = ui, server = server)
