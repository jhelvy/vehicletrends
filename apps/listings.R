# Share of Listings Dashboard - Standalone
# Displays 2018 vs 2024 market composition changes with dumbbell plots

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

# Load market share data from jsDelivr CDN (CORS-enabled for Shinylive)
base_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/"

# Preload all possible data combinations
market_data <- list(
  "powertrain_vehicle_type" = read.csv(paste0(base_url, "p_market_powertrain_vehicle_type.csv")),
  "powertrain_price_bin" = read.csv(paste0(base_url, "p_market_powertrain_price_bin.csv")),
  "vehicle_type_powertrain" = read.csv(paste0(base_url, "p_market_vehicle_type_powertrain.csv")),
  "vehicle_type_price_bin" = read.csv(paste0(base_url, "p_market_vehicle_type_price_bin.csv")),
  "price_bin_powertrain" = read.csv(paste0(base_url, "p_market_price_bin_powertrain.csv")),
  "price_bin_vehicle_type" = read.csv(paste0(base_url, "p_market_price_bin_vehicle_type.csv"))
)

# Define UI
ui <- page_sidebar(
  title = "Share of Listings by Variable (2018 vs 2024)",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#ffffff",
    fg = "#2c3e50"
  ),
  fillable = TRUE,

  sidebar = sidebar(
    title = "Listings Controls",
    width = 250,

    radioButtons(
      "share_xaxis",
      "X-Axis Variable:",
      choices = list(
        "Powertrain" = "powertrain",
        "Vehicle Type" = "vehicle_type",
        "Price Bin" = "price_bin"
      ),
      selected = "price_bin"
    ),

    radioButtons(
      "share_facet",
      "Facet Variable:",
      choices = list(
        "Powertrain" = "powertrain",
        "Vehicle Type" = "vehicle_type",
        "Price Bin" = "price_bin"
      ),
      selected = "powertrain"
    )
  ),

  # Main content
  card(
    full_screen = TRUE,
    card_header(
      "Share of Listings by Variable (2018 vs 2024)",
      class = "text-center"
    ),
    plotlyOutput("share_plot", height = "700px")
  )
)

# Server
server <- function(input, output, session) {

  # Observer to update facet choices based on X-axis selection
  observeEvent(input$share_xaxis, {
    all_choices <- list(
      "Powertrain" = "powertrain",
      "Vehicle Type" = "vehicle_type",
      "Price Bin" = "price_bin"
    )

    # Remove the selected X-axis variable from facet choices
    available_choices <- all_choices[all_choices != input$share_xaxis]

    # Get current facet selection
    current_facet <- input$share_facet

    # If current facet is same as X-axis, select the first available option
    new_selection <- if (current_facet == input$share_xaxis) {
      available_choices[[1]]
    } else {
      current_facet
    }

    updateRadioButtons(
      session,
      "share_facet",
      choices = available_choices,
      selected = new_selection
    )
  })

  # Load appropriate data based on selected variables
  share_listings_data <- reactive({
    xaxis_var <- input$share_xaxis
    facet_var <- input$share_facet

    # Construct key for data lookup
    data_key <- paste0(xaxis_var, "_", facet_var)

    if (data_key %in% names(market_data)) {
      return(market_data[[data_key]])
    } else {
      return(data.frame()) # Return empty if not found
    }
  })

  # Share of Listings Plot (from app.R lines 3214-3460)
  output$share_plot <- renderPlotly({
    data <- share_listings_data()

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

    # Label mapping function
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
        labels <- c(
          "car" = "Car",
          "suv" = "SUV",
          "pickup" = "Pickup",
          "minivan" = "Minivan"
        )
        return(labels[value])
      } else {
        return(value)
      }
    }

    # Get selected variables
    xaxis_var <- input$share_xaxis
    facet_var <- input$share_facet

    # Get unique facet values for subplots with proper ordering
    if (facet_var == "powertrain") {
      facet_order <- c("bev", "cv", "hev", "phev")
      facet_values <- intersect(facet_order, unique(data[[facet_var]]))
    } else if (facet_var == "vehicle_type") {
      facet_order <- c("car", "suv", "pickup", "minivan")
      facet_values <- intersect(facet_order, unique(data[[facet_var]]))
    } else if (facet_var == "price_bin") {
      facet_order <- c(
        "$0-$30k",
        "$30k-$40k",
        "$40k-$50k",
        "$50k-$60k",
        "$60k+"
      )
      facet_values <- intersect(facet_order, unique(data[[facet_var]]))
    } else {
      facet_values <- sort(unique(data[[facet_var]]))
    }
    n_facets <- length(facet_values)

    # Create list to store individual plots
    plot_list <- list()

    for (i in seq_along(facet_values)) {
      facet_val <- facet_values[i]

      # Filter data for this facet
      facet_data <- data %>%
        filter(.data[[facet_var]] == facet_val)

      # Get unique x-axis values with proper ordering
      if (xaxis_var == "price_bin") {
        xaxis_order <- c(
          "$0-$30k",
          "$30k-$40k",
          "$40k-$50k",
          "$50k-$60k",
          "$60k+"
        )
        xaxis_values <- intersect(xaxis_order, unique(facet_data[[xaxis_var]]))
      } else if (xaxis_var == "powertrain") {
        xaxis_order <- c("bev", "cv", "hev", "phev")
        xaxis_values <- intersect(xaxis_order, unique(facet_data[[xaxis_var]]))
      } else if (xaxis_var == "vehicle_type") {
        xaxis_order <- c("car", "suv", "pickup", "minivan")
        xaxis_values <- intersect(xaxis_order, unique(facet_data[[xaxis_var]]))
      } else {
        xaxis_values <- sort(unique(facet_data[[xaxis_var]]))
      }

      # Create a plot for this facet
      p <- plot_ly()

      for (j in seq_along(xaxis_values)) {
        x_val <- xaxis_values[j]
        x_label <- get_label(xaxis_var, x_val)
        x_data <- facet_data %>%
          filter(.data[[xaxis_var]] == x_val)

        if (nrow(x_data) > 0) {
          val_2018 <- x_data$year_2018[1]
          val_2024 <- x_data$year_2024[1]

          # Add line connecting 2018 to 2024 (vertical dumbbell)
          p <- p %>%
            add_trace(
              type = "scatter",
              mode = "lines",
              x = c(j - 1, j - 1), # Use numeric position
              y = c(val_2018, val_2024),
              line = list(color = "rgba(80, 80, 80, 0.7)", width = 2.5),
              showlegend = FALSE,
              hoverinfo = "none"
            ) %>%
            # Add circle for 2018
            add_trace(
              type = "scatter",
              mode = "markers",
              x = c(j - 1), # Use numeric position
              y = c(val_2018),
              marker = list(
                size = 12,
                color = "rgba(60, 60, 60, 0.9)",
                symbol = "circle",
                line = list(width = 1, color = "white")
              ),
              name = if (i == 1 && j == 1) "2018" else NULL,
              showlegend = if (i == 1 && j == 1) TRUE else FALSE,
              hovertemplate = paste0(
                x_label,
                "<br>",
                "2018: ",
                round(val_2018 * 100, 1),
                "%",
                "<extra></extra>"
              ),
              legendgroup = "2018"
            ) %>%
            # Add arrow/triangle for 2024
            add_trace(
              type = "scatter",
              mode = "markers",
              x = c(j - 1), # Use numeric position
              y = c(val_2024),
              marker = list(
                size = 13,
                color = "rgba(60, 60, 60, 0.9)",
                symbol = if (val_2024 > val_2018) {
                  "triangle-up"
                } else {
                  "triangle-down"
                },
                line = list(width = 1, color = "white")
              ),
              name = if (i == 1 && j == 1) "2024" else NULL,
              showlegend = if (i == 1 && j == 1) TRUE else FALSE,
              hovertemplate = paste0(
                x_label,
                "<br>",
                "2024: ",
                round(val_2024 * 100, 1),
                "%",
                "<extra></extra>"
              ),
              legendgroup = "2024"
            )
        }
      }

      # Configure X-axis for this subplot with categorical labels
      xaxis_labels <- sapply(xaxis_values, function(v) get_label(xaxis_var, v))
      p <- p %>%
        layout(
          xaxis = list(
            tickmode = "array",
            tickvals = seq(0, length(xaxis_values) - 1),
            ticktext = xaxis_labels,
            tickangle = -45
          )
        )

      plot_list[[i]] <- p
    }

    # Create subplot with facets
    fig <- subplot(
      plot_list,
      nrows = 1,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.04
    )

    # Add titles and layout
    annotations <- list()
    for (i in seq_along(facet_values)) {
      facet_label <- get_label(facet_var, facet_values[i])
      annotations[[i]] <- list(
        x = (i - 0.5) / n_facets,
        y = 1.02,
        text = paste0("<b>", facet_label, "</b>"),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 12)
      )
    }

    fig <- fig %>%
      layout(
        title = list(
          text = paste0(
            "Change in Share of Listings by ",
            tools::toTitleCase(facet_var),
            " and ",
            tools::toTitleCase(xaxis_var),
            " (2018-2024)"
          ),
          x = 0.5,
          xanchor = "center",
          font = list(size = 14)
        ),
        annotations = annotations,
        yaxis = list(
          title = "Share of Listings (%)",
          tickformat = ".0%",
          gridcolor = "rgba(200, 200, 200, 0.3)",
          gridwidth = 1,
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = "rgba(150, 150, 150, 0.5)",
          zerolinewidth = 1
        ),
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          yanchor = "top",
          orientation = "h"
        ),
        plot_bgcolor = "rgba(250, 250, 250, 0.5)",
        paper_bgcolor = "white",
        margin = list(t = 80, b = 100, l = 80, r = 30)
      )

    return(fig)
  })
}

# Run app
shinyApp(ui = ui, server = server)
