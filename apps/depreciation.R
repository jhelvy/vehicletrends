# Depreciation - Standalone Dashboard with Faceted Subplots
# Exact replication of depreciation functionality from main app.R

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)
library(tibble)

# Load data from jsDelivr CDN (CORS-enabled for Shinylive)
depreciation_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/depreciation_powertrain_type.csv"
depreciation_data_raw <- read.csv(depreciation_url)

# Define UI
ui <- page_sidebar(
  title = "Depreciation by Vehicle Age",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#ffffff",
    fg = "#2c3e50"
  ),
  fillable = TRUE,

  sidebar = sidebar(
    title = "Depreciation Controls",
    width = 250,

    checkboxGroupInput(
      "depreciation_fuel_types",
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
      "depreciation_vehicle_types",
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
      "depreciation_category",
      "Compare by Category:",
      choices = list(
        "Powertrain" = "powertrain",
        "Vehicle Type" = "vehicle_type"
      ),
      selected = "powertrain"
    ),

    hr(style = "border-color: #ddd; margin: 10px 0;"),

    sliderInput(
      "depreciation_age_range",
      "Age Range for Analysis (Years):",
      min = 1,
      max = 8,
      value = c(1, 8),
      step = 0.5
    ),

    checkboxInput(
      "depreciation_show_confidence_bands",
      "Show Confidence Regions (25th-75th percentile)",
      value = FALSE
    )
  ),

  # Main content
  card(
    full_screen = TRUE,
    card_header(
      "Vehicle Retention Rates by Age",
      class = "text-center"
    ),
    plotlyOutput("retention_plot", height = "850px")
  )
)

# Server
server <- function(input, output, session) {
  # Load and filter depreciation data
  filtered_retention_data <- reactive({
    data <- depreciation_data_raw

    # Filter by powertrain
    fuel_types <- if (is.null(input$depreciation_fuel_types) || length(input$depreciation_fuel_types) == 0) {
      unique(data$powertrain)
    } else {
      input$depreciation_fuel_types
    }

    # Filter by vehicle type
    vehicle_types <- if (is.null(input$depreciation_vehicle_types) || length(input$depreciation_vehicle_types) == 0) {
      unique(data$vehicle_type)
    } else {
      input$depreciation_vehicle_types
    }

    data %>%
      filter(
        powertrain %in% fuel_types,
        vehicle_type %in% vehicle_types
      )
  })

  # Create context for retention plot (similar to retention_plot_context in app.R)
  retention_plot_context <- reactive({
    data <- filtered_retention_data()

    if (is.null(data) || nrow(data) == 0) {
      return(list(
        status = "empty",
        message = "No data matches current filters"
      ))
    }

    required_cols <- c("age_years", "rr_predicted")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      return(list(
        status = "error",
        message = "Retention data missing required columns"
      ))
    }

    age_min <- if (!is.null(input$depreciation_age_range)) {
      input$depreciation_age_range[1]
    } else {
      1
    }
    age_max <- if (!is.null(input$depreciation_age_range)) {
      input$depreciation_age_range[2]
    } else {
      8
    }
    selected_category <- if (!is.null(input$depreciation_category)) {
      input$depreciation_category
    } else {
      "powertrain"
    }

    data <- data %>%
      filter(
        age_years >= age_min,
        age_years <= age_max,
        !is.na(rr_predicted)
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
          rr_predicted = mean(rr_predicted, na.rm = TRUE),
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
          rr_predicted = mean(rr_predicted, na.rm = TRUE),
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
  smooth_series <- function(df, y_col, age_seq, span = 0.6) {
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

  # Render retention plot with faceted subplots (from app.R lines 2522-2864)
  output$retention_plot <- renderPlotly({
    context <- retention_plot_context()

    if (is.null(context) || context$status != "ok") {
      message <- if (!is.null(context$message)) {
        context$message
      } else {
        "No retention data available"
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
    h_gap <- 0.03
    v_gap <- 0.10
    panel_width <- (1 - (n_cols - 1) * h_gap) / n_cols
    panel_height <- (1 - (n_rows - 1) * v_gap) / n_rows

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

      x_start <- (col - 1) * (panel_width + h_gap)
      x_end <- x_start + panel_width
      y_start <- 1 - row * (panel_height + v_gap)
      y_end <- y_start + panel_height

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
          rr_smooth <- smooth_series(
            sec_data,
            "rr_predicted",
            age_seq,
            span = 0.6
          ) %>%
            rename(rr_predicted = value)

          if (nrow(rr_smooth) == 0) {
            next
          }

          # Add main prediction line
          fig <- fig %>%
            add_trace(
              data = rr_smooth,
              x = ~age_years,
              y = ~rr_predicted,
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
                "Retention Rate: %{y:.1%}<br>",
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
        title = if (row == n_rows) {
          list(text = "Vehicle age (years)", font = list(size = 10))
        } else {
          ""
        },
        tickmode = "linear",
        tick0 = 1,
        dtick = 1,
        showticklabels = TRUE
      )

      yaxis_config <- list(
        domain = c(y_start, y_end),
        anchor = xaxis_name,
        range = c(0, 1.2), # Set to 120% (1.2)
        tickformat = ".0%",
        title = if (col == 1) {
          list(text = "Retention rate", font = list(size = 10))
        } else {
          ""
        },
        showticklabels = TRUE,
        fixedrange = TRUE # Fix Y-axis to prevent zooming beyond 120%
      )

      if (idx == 1) {
        xaxes[["xaxis"]] <- xaxis_config
        yaxes[["yaxis"]] <- yaxis_config
      } else {
        xaxes[[paste0("xaxis", idx)]] <- xaxis_config
        yaxes[[paste0("yaxis", idx)]] <- yaxis_config
      }

      annotations[[length(annotations) + 1]] <- list(
        x = x_start + panel_width / 2,
        y = y_end + 0.03,
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
          text = paste0("Vehicle Retention Rates by ", category_label),
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
        toImageButtonOptions = list(format = "png", filename = "retention_plot")
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)
