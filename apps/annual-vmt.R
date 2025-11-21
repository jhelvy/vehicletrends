# Annual VMT by Vehicle Age - Standalone Dashboard with Faceted Subplots
# Exact replication of annual-vmt functionality from main app.R

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)
library(tibble)

# Load data from jsDelivr CDN (CORS-enabled for Shinylive)
mileage_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/mileage_powertrain_type.csv"
mileage_data_raw <- read.csv(mileage_url)

# Define UI
ui <- page_sidebar(
  title = "Annual VMT by Vehicle Age",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#ffffff",
    fg = "#2c3e50"
  ),
  fillable = TRUE,

  sidebar = sidebar(
    title = "Annual VMT Controls",
    width = 250,

    checkboxGroupInput(
      "annual_vmt_fuel_types",
      "Powertrain Type:",
      choices = list(
        "BEV" = "bev",
        "Conventional" = "cv",
        "Diesel" = "diesel",
        "Flex Fuel" = "flex",
        "Hybrid" = "hev",
        "PHEV" = "phev"
      ),
      selected = c("bev", "cv", "hev")
    ),

    checkboxGroupInput(
      "annual_vmt_vehicle_types",
      "Vehicle Types:",
      choices = list(
        "Car" = "car",
        "CUV" = "cuv",
        "Minivan" = "minivan",
        "Pickup" = "pickup",
        "SUV" = "suv"
      ),
      selected = c("car", "cuv", "minivan", "pickup", "suv")
    ),

    hr(),

    radioButtons(
      "comparison_category",
      "Compare by Category:",
      choices = list(
        "Powertrain" = "powertrain",
        "Vehicle Type" = "vehicle_type"
      ),
      selected = "powertrain"
    ),

    hr(style = "border-color: #ddd; margin: 10px 0;"),

    sliderInput(
      "age_range",
      "Vehicle Age Range (Years):",
      min = 1,
      max = 10,
      value = c(2, 8),
      step = 0.5
    ),

    checkboxInput(
      "annual_vmt_show_confidence_bands",
      "Show Confidence Regions (25th-75th percentile)",
      value = FALSE
    )
  ),

  # Main content
  card(
    full_screen = TRUE,
    card_header(
      "Vehicle Mileage Trends by Age",
      class = "text-center"
    ),
    plotlyOutput("mileage_plot", height = "800px")
  )
)

# Server
server <- function(input, output, session) {
  # Load and filter mileage data
  filtered_mileage_data <- reactive({
    data <- mileage_data_raw

    # Filter by powertrain
    fuel_types <- if (is.null(input$annual_vmt_fuel_types) || length(input$annual_vmt_fuel_types) == 0) {
      unique(data$powertrain)
    } else {
      input$annual_vmt_fuel_types
    }

    # Filter by vehicle type
    vehicle_types <- if (is.null(input$annual_vmt_vehicle_types) || length(input$annual_vmt_vehicle_types) == 0) {
      unique(data$vehicle_type)
    } else {
      input$annual_vmt_vehicle_types
    }

    data %>%
      filter(
        powertrain %in% fuel_types,
        vehicle_type %in% vehicle_types
      )
  })

  # Create context for mileage plot (similar to mileage_plot_context in app.R)
  mileage_plot_context <- reactive({
    data <- filtered_mileage_data()

    if (is.null(data) || nrow(data) == 0) {
      return(list(
        status = "empty",
        message = "No data matches current filters"
      ))
    }

    required_cols <- c("age_years", "mileage_predicted", "coef")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      return(list(
        status = "error",
        message = "Mileage data missing required columns"
      ))
    }

    # Filter for age > 2 years
    data <- data %>%
      filter(age_years > 2)

    age_min <- if (!is.null(input$age_range)) max(2, input$age_range[1]) else 2
    age_max <- if (!is.null(input$age_range)) input$age_range[2] else 8
    selected_category <- if (!is.null(input$comparison_category)) {
      input$comparison_category
    } else {
      "powertrain"
    }

    data <- data %>%
      filter(
        age_years >= age_min,
        age_years <= age_max,
        !is.na(mileage_predicted)
      )

    if (nrow(data) == 0) {
      return(list(
        status = "empty",
        message = "No data matches current filters"
      ))
    }

    if (selected_category == "powertrain") {
      aggregated <- data %>%
        group_by(powertrain, vehicle_type, age_years) %>%
        summarise(
          mileage_predicted = mean(mileage_predicted, na.rm = TRUE),
          coef = mean(coef, na.rm = TRUE),
          .groups = "drop"
        )
      categories <- sort(unique(aggregated$powertrain))
      category_var <- "powertrain"
      category_label <- "Powertrain"
      secondary <- sort(unique(aggregated$vehicle_type))
    } else {
      aggregated <- data %>%
        group_by(vehicle_type, powertrain, age_years) %>%
        summarise(
          mileage_predicted = mean(mileage_predicted, na.rm = TRUE),
          coef = mean(coef, na.rm = TRUE),
          .groups = "drop"
        )
      categories <- sort(unique(aggregated$vehicle_type))
      category_var <- "vehicle_type"
      category_label <- "Vehicle Type"
      secondary <- sort(unique(aggregated$powertrain))
    }

    if (
      nrow(aggregated) == 0 || length(categories) == 0 || length(secondary) == 0
    ) {
      return(list(
        status = "empty",
        message = "No data matches current filters"
      ))
    }

    list(
      status = "ok",
      aggregated = aggregated,
      category_var = category_var,
      category_label = category_label,
      categories = categories,
      secondary_cats = secondary,
      age_min = age_min,
      age_max = age_max,
      selected_category = selected_category
    )
  })

  # Smooth series function (from app.R)
  smooth_series <- function(df, y_col, age_seq, span = 0.65) {
    df <- df %>% arrange(age_years)
    if (length(unique(df$age_years)) < 3 || nrow(df) < 3) {
      approx_res <- approx(
        x = df$age_years,
        y = df[[y_col]],
        xout = age_seq,
        rule = 2,
        ties = mean
      )
      return(tibble(age_years = approx_res$x, value = approx_res$y))
    }
    fit <- tryCatch(
      loess(
        stats::as.formula(paste(y_col, "~ age_years")),
        data = df,
        span = span,
        degree = 2
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      approx_res <- approx(
        x = df$age_years,
        y = df[[y_col]],
        xout = age_seq,
        rule = 2,
        ties = mean
      )
      return(tibble(age_years = approx_res$x, value = approx_res$y))
    }
    preds <- predict(fit, newdata = data.frame(age_years = age_seq))
    if (all(is.na(preds))) {
      approx_res <- approx(
        x = df$age_years,
        y = df[[y_col]],
        xout = age_seq,
        rule = 2,
        ties = mean
      )
      preds <- approx_res$y
    }
    tibble(age_years = age_seq, value = preds)
  }

  # Render mileage plot with faceted subplots (from app.R lines 2185-2519)
  output$mileage_plot <- renderPlotly({
    context <- mileage_plot_context()

    if (is.null(context) || context$status != "ok") {
      message <- if (!is.null(context$message)) {
        context$message
      } else {
        "No mileage data available"
      }
      return(
        plot_ly() %>%
          add_annotations(
            text = message,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
      )
    }

    aggregated <- context$aggregated
    categories <- context$categories
    secondary_cats <- context$secondary_cats
    category_var <- context$category_var
    category_label <- context$category_label
    age_min <- context$age_min
    age_max <- context$age_max

    if (length(categories) == 0 || length(secondary_cats) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data matches current filters",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
      )
    }

    colors <- viridis(length(secondary_cats))
    color_map <- setNames(colors, secondary_cats)
    secondary_label <- if (category_var == "powertrain") {
      "Vehicle Type"
    } else {
      "Powertrain"
    }

    fig <- plot_ly()

    n_cols <- min(3, length(categories))
    n_rows <- ceiling(length(categories) / n_cols)
    h_gap <- 0.05
    v_gap <- 0.08
    cell_width <- (1 - (n_cols - 1) * h_gap) / n_cols
    cell_height <- (1 - (n_rows - 1) * v_gap) / n_rows

    xaxes <- list()
    yaxes <- list()
    annotations <- list()

    for (idx in seq_along(categories)) {
      cat_value <- categories[idx]

      cat_data <- if (category_var == "powertrain") {
        aggregated %>% filter(powertrain == cat_value)
      } else {
        aggregated %>% filter(vehicle_type == cat_value)
      }

      row <- ceiling(idx / n_cols)
      col <- ((idx - 1) %% n_cols) + 1

      x_start <- (col - 1) * (cell_width + h_gap)
      x_end <- x_start + cell_width
      y_start <- 1 - row * (cell_height + v_gap)
      y_end <- y_start + cell_height

      xaxis_name <- if (idx == 1) "x" else paste0("x", idx)
      yaxis_name <- if (idx == 1) "y" else paste0("y", idx)

      # Calculate adaptive X-axis range for this facet
      facet_age_min <- age_max
      facet_age_max <- age_min

      if (nrow(cat_data) > 0) {
        available_secondary <- if (category_var == "powertrain") {
          sort(unique(cat_data$vehicle_type))
        } else {
          sort(unique(cat_data$powertrain))
        }

        for (sec_cat in available_secondary) {
          sec_data <- if (category_var == "powertrain") {
            cat_data %>% filter(vehicle_type == sec_cat) %>% arrange(age_years)
          } else {
            cat_data %>% filter(powertrain == sec_cat) %>% arrange(age_years)
          }

          if (nrow(sec_data) == 0) {
            next
          }

          min_age <- max(age_min, min(sec_data$age_years))
          max_age <- min(age_max, max(sec_data$age_years))

          # Update facet-specific age range
          facet_age_min <- min(facet_age_min, min_age)
          facet_age_max <- max(facet_age_max, max_age)

          if (min_age >= max_age) {
            next
          }

          age_seq <- seq(
            min_age,
            max_age,
            length.out = max(80, length(sec_data$age_years) * 5)
          )
          age_seq <- unique(age_seq)

          # Use prediction data for main line
          mileage_smooth <- smooth_series(
            sec_data,
            "mileage_predicted",
            age_seq
          ) %>%
            rename(mileage_predicted = value)

          if (nrow(mileage_smooth) == 0) {
            next
          }

          # Add main prediction line
          fig <- fig %>%
            add_trace(
              data = mileage_smooth,
              x = ~age_years,
              y = ~mileage_predicted,
              type = "scatter",
              mode = "lines",
              line = list(
                color = color_map[sec_cat],
                width = 2.5,
                shape = "spline"
              ),
              name = toupper(sec_cat),
              showlegend = (idx == 1),
              legendgroup = sec_cat,
              xaxis = xaxis_name,
              yaxis = yaxis_name,
              hovertemplate = paste0(
                "<b>",
                toupper(cat_value),
                " - ",
                toupper(sec_cat),
                "</b><br>",
                "Age: %{x:.1f} years<br>",
                "Mileage: %{y:,.0f} miles<br>",
                "<extra></extra>"
              )
            )
        }
      }

      # Use adaptive range if data exists, otherwise use global range
      x_range <- if (facet_age_min < facet_age_max) {
        c(facet_age_min, facet_age_max)
      } else {
        c(age_min, age_max)
      }

      xaxis_config <- list(
        domain = c(x_start, x_end),
        range = x_range,
        showticklabels = TRUE,
        title = if (row == n_rows) {
          list(text = "Vehicle Age (years)", font = list(size = 10))
        } else {
          ""
        }
      )

      yaxis_config <- list(
        domain = c(y_start, y_end),
        anchor = xaxis_name,
        showticklabels = TRUE,
        title = if (col == 1) {
          list(text = "Cumulative Miles", font = list(size = 10))
        } else {
          ""
        }
      )

      if (idx == 1) {
        xaxes[["xaxis"]] <- xaxis_config
        yaxes[["yaxis"]] <- yaxis_config
      } else {
        xaxes[[paste0("xaxis", idx)]] <- xaxis_config
        yaxes[[paste0("yaxis", idx)]] <- yaxis_config
      }

      annotations[[length(annotations) + 1]] <- list(
        x = x_start + cell_width / 2,
        y = y_end + 0.02,
        text = paste0("<b>", toupper(cat_value), "</b>"),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 11)
      )
    }

    layout_args <- c(
      list(
        title = list(
          text = paste0("Vehicle Mileage Trends by ", category_label),
          font = list(size = 16)
        ),
        annotations = annotations,
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          title = list(text = paste0("<b>", secondary_label, "</b>"))
        ),
        hovermode = "closest",
        margin = list(t = 80, b = 100, l = 70, r = 30)
      ),
      xaxes,
      yaxes
    )

    fig <- do.call(layout, c(list(fig), layout_args))

    fig %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d"),
        toImageButtonOptions = list(format = "png", filename = "mileage_plot")
      )
  })

}

# Run app
shinyApp(ui = ui, server = server)
