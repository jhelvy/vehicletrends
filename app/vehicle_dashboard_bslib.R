# Vehicle Trends Dashboard - Modern bslib Version
# Multi-page app with horizontal navigation bar

# Load required libraries
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(scales)
library(colourpicker)
library(arrow)
library(here)
library(stringr)
library(zoo)

# Define UI using bslib page_navbar with light theme
ui <- page_navbar(
  title = "Vehicle Trends Analytics Dashboard",
  id = "tabs",
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",  # Light, clean theme
    primary = "#2c3e50",   # Dark blue-gray primary
    secondary = "#95a5a6", # Light gray secondary  
    success = "#18bc9c",   # Teal success
    info = "#3498db",      # Blue info
    warning = "#f39c12",   # Orange warning
    danger = "#e74c3c",    # Red danger
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    bg = "#ffffff",        # White background
    fg = "#2c3e50"         # Dark text
  ),
  fillable = TRUE,
  
  # Custom CSS for better navbar spacing and centering
  header = tags$head(
    tags$style(HTML("
      .navbar-nav { 
        margin: 0 auto; 
        display: flex;
        justify-content: center;
        width: 100%;
      }
      .navbar-nav .nav-item {
        margin: 0 15px;
      }
      .navbar-nav .nav-link {
        font-weight: 500;
        font-size: 16px;
        padding: 12px 20px;
      }
      .navbar-brand {
        font-weight: 600;
        font-size: 20px;
      }
    "))
  ),
  
  # Global sidebar for filters (shared across all pages)
  sidebar = sidebar(
    title = "Global Filters",
    width = 280,

    h5("Dataset Filters", style = "font-weight: bold; color: #2c3e50;"),

    checkboxGroupInput("fuel_types",
                      "Powertrain Type:",
                      choices = list("BEV" = "bev",
                                   "Conventional" = "cv",
                                   "Diesel" = "diesel",
                                   "Flex Fuel" = "flex",
                                   "Hybrid" = "hev",
                                   "PHEV" = "phev"),
                      selected = c("bev", "cv", "hev")),

    checkboxGroupInput("vehicle_types",
                      "Vehicle Types:",
                      choices = list("Car" = "car",
                                   "CUV" = "cuv",
                                   "Minivan" = "minivan",
                                   "Pickup" = "pickup",
                                   "SUV" = "suv"),
                      selected = c("car", "cuv", "minivan", "pickup", "suv")),

    hr(),

    h5("Metric-Specific Controls", style = "font-weight: bold; color: #2c3e50;"),

    # Conditional UI for metric-specific filters
    conditionalPanel(
      condition = "input.tabs == 'Daily VMT'",
      h6("Daily VMT Controls", style = "font-weight: 600; color: #34495e;"),

      sliderInput("dvmt_range",
                 "DVMT Range:",
                 min = 0, max = 100, value = c(0, 100)),

      radioButtons("plot_type", "Plot Type:",
                  choices = list("CDF" = "cdf",
                               "PDF" = "pdf",
                               "Histogram" = "hist"),
                  selected = "cdf"),

      checkboxInput("show_aggregated", "Show Aggregated Line", TRUE)
    ),

    conditionalPanel(
      condition = "input.tabs == 'Cumulative VMT'",
      h6("Cumulative VMT Controls", style = "font-weight: 600; color: #34495e;"),

      sliderInput("age_range",
                 "Vehicle Age Range (Years):",
                 min = 1, max = 10, value = c(2, 8), step = 0.5),

      checkboxInput("show_confidence_bands", "Show Confidence Bands", TRUE),

      radioButtons("mileage_display", "Display Mode:",
                  choices = list("Research Style" = "research",
                               "Interactive" = "interactive"),
                  selected = "research")
    ),

    conditionalPanel(
      condition = "input.tabs == 'Depreciation'",
      h6("Depreciation Controls", style = "font-weight: 600; color: #34495e;"),

      sliderInput("depreciation_age_range",
                 "Age Range for Analysis (Years):",
                 min = 1, max = 8, value = c(1, 8), step = 0.5),

      radioButtons("depreciation_type", "Analysis Type:",
                  choices = list("Retention Rate" = "retention",
                               "Depreciation Rate" = "depreciation",
                               "Both" = "both"),
                  selected = "both")
    )
  ),
  
  # Daily VMT Page
  nav_panel(
    title = "Daily VMT",
    value = "Daily VMT",
    icon = icon("chart-line"),
    card(
      full_screen = TRUE,
      card_header(
        "Daily Vehicle Miles Traveled Distribution",
        class = "text-center"
      ),
      plotlyOutput("cdf_plot", height = "650px")
    )
  ),
  
  # Cumulative VMT Page
  nav_panel(
    title = "Cumulative VMT",
    value = "Cumulative VMT",
    icon = icon("road"),
    layout_columns(
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header(
          "Vehicle Mileage Trends by Age",
          class = "text-center"
        ),
        plotlyOutput("mileage_plot", height = "700px")
      )
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Mileage Summary Statistics"),
        DT::dataTableOutput("mileage_summary")
      )
    )
  ),
  
  # Depreciation Page
  nav_panel(
    title = "Depreciation",
    value = "Depreciation",
    icon = icon("chart-line"),

    # First Section: Reference Plot (3-panel layout like your image)
    card(
      full_screen = TRUE,
      card_header(
        "Vehicle Retention Rates by Age - Standard Comparison",
        class = "text-center",
        span("Three-panel comparison showing Hybrid, Plug-in Hybrid, and Battery Electric vs Conventional",
             style = "font-size: 14px; font-weight: normal; color: #666;")
      ),
      plotlyOutput("retention_plot", height = "600px")
    ),

    br(),

    # Second Section: Custom Graph Builder (Default Open)
    card(
      full_screen = TRUE,
      card_header(
        "Build Your Custom Vehicle Comparison",
        class = "text-center",
        span("Select which vehicle types to compare and customize your analysis",
             style = "font-size: 14px; font-weight: normal; color: #666;")
      ),
      layout_columns(
        col_widths = c(3, 9),
        # Left sidebar for custom controls
        card(
          card_header(
            "Graph Builder Controls",
            style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;"
          ),

          h6("Vehicle Types to Compare:", style = "font-weight: 600; margin-top: 15px; color: #2c3e50;"),
          checkboxGroupInput("custom_vehicle_types",
                            "",
                            choices = list(
                              "Conventional" = "conventional",
                              "Hybrid" = "hybrid",
                              "Plug-in Hybrid (PHEV)" = "phev",
                              "Battery Electric (BEV)" = "bev",
                              "Tesla Vehicles" = "tesla"
                            ),
                            selected = c("conventional", "hybrid", "bev")),

          hr(style = "border-color: #ddd; margin: 20px 0;"),

          h6("Visualization Options:", style = "font-weight: 600; color: #2c3e50;"),

          div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
            checkboxInput("show_confidence_ribbons",
                         "Show Confidence Bands", TRUE),
            checkboxInput("show_labels_custom",
                         "Show Vehicle Type Labels", TRUE),

            radioButtons("plot_style", "Plot Style:",
                        choices = list(
                          "Research Style" = "research",
                          "Clean Minimal" = "minimal"
                        ),
                        selected = "research")
          ),

          actionButton("update_custom_plot", "Update Graph",
                      class = "btn-primary btn-lg",
                      style = "width: 100%; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; font-weight: 600; padding: 12px;"),

          br(), br(),

          div(style = "background: #e3f2fd; padding: 10px; border-radius: 6px; font-size: 12px; color: #1565c0;",
            "Tip: Select different combinations to see how vehicle types ",
            "compare directly"
          )
        ),

        # Right side for custom plot
        card(
          card_header(
            "Your Custom Vehicle Retention Comparison",
            style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); color: white;"
          ),
          plotlyOutput("custom_retention_plot", height = "600px")
        )
      )
    )
  ),
  
  # Market Concentration Page
  nav_panel(
    title = "Market Concentration",
    value = "Market Concentration",
    icon = icon("chart-bar"),
    layout_columns(
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header(
          "Market Share by Vehicle Type and Fuel",
          class = "text-center"
        ),
        plotlyOutput("comparison_plot", height = "500px")
      )
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Market Concentration Metrics"),
        DT::dataTableOutput("metrics_table")
      )
    )
  ),
  
  # Nearest Vehicle Analysis Page
  nav_panel(
    title = "Nearest Vehicle",
    value = "Nearest Vehicle",
    icon = icon("location-dot"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Vehicle Similarity Analysis"),
        p("This analysis shows the nearest vehicle matches based on characteristics like DVMT, fuel type, and vehicle type."),
        plotlyOutput("similarity_plot", height = "400px")
      ),
      card(
        card_header("Vehicle Clustering"),
        plotlyOutput("cluster_plot", height = "400px")
      )
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Data Explorer - Find Similar Vehicles"),
        DT::dataTableOutput("raw_data_table")
      )
    )
  )
)

# Define Server (keeping the existing server logic)
server <- function(input, output, session) {
  
  # Load VMT data
  sample_data <- reactive({
    # Try multiple potential data paths
    data_paths <- c(
      "data/quantiles_dvmt.parquet",
      "../data/quantiles_dvmt.parquet",
      file.path(dirname(getwd()), "data", "quantiles_dvmt.parquet")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find quantiles_dvmt.parquet file")
    }

    quantiles_dvmt <- read_parquet(data_path)

    data <- quantiles_dvmt %>%
      filter(powertrain != "all", vehicle_type != "all")

    return(data)
  })

  # Update filter choices based on loaded data
  observe({
    # Update powertrain choices based on available data
    sample_data_available <- tryCatch({
      sample_data()
    }, error = function(e) NULL)

    if (!is.null(sample_data_available)) {
      available_powertrains <- sort(unique(sample_data_available$powertrain))
      available_vehicle_types <- sort(unique(sample_data_available$vehicle_type))

      # Update powertrain choices
      powertrain_choices <- setNames(available_powertrains,
                                   case_when(
                                     available_powertrains == "bev" ~ "BEV",
                                     available_powertrains == "cv" ~ "Conventional",
                                     available_powertrains == "diesel" ~ "Diesel",
                                     available_powertrains == "flex" ~ "Flex Fuel",
                                     available_powertrains == "hev" ~ "Hybrid",
                                     available_powertrains == "phev" ~ "PHEV",
                                     TRUE ~ str_to_title(available_powertrains)
                                   ))

      # Update vehicle type choices
      vehicle_type_choices <- setNames(available_vehicle_types,
                                     case_when(
                                       available_vehicle_types == "car" ~ "Car",
                                       available_vehicle_types == "cuv" ~ "CUV",
                                       available_vehicle_types == "minivan" ~ "Minivan",
                                       available_vehicle_types == "pickup" ~ "Pickup",
                                       available_vehicle_types == "suv" ~ "SUV",
                                       TRUE ~ str_to_title(available_vehicle_types)
                                     ))

      updateCheckboxGroupInput(session, "fuel_types",
                              choices = powertrain_choices,
                              selected = intersect(c("bev", "cv", "hev"), available_powertrains))

      updateCheckboxGroupInput(session, "vehicle_types",
                              choices = vehicle_type_choices,
                              selected = available_vehicle_types)

      # Update DVMT range based on actual data
      if("dvmt" %in% colnames(sample_data_available)) {
        dvmt_min <- floor(min(sample_data_available$dvmt, na.rm = TRUE))
        dvmt_max <- ceiling(max(sample_data_available$dvmt, na.rm = TRUE))

        updateSliderInput(session, "dvmt_range",
                         min = dvmt_min,
                         max = dvmt_max,
                         value = c(dvmt_min, dvmt_max))
      }
    }
  })

  # Load mileage data
  mileage_data <- reactive({
    # Try multiple potential data paths
    data_paths <- c(
      "data/quantiles_miles.parquet",
      "../data/quantiles_miles.parquet",
      file.path(dirname(getwd()), "data", "quantiles_miles.parquet")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find quantiles_miles.parquet file")
    }

    quantiles_miles <- read_parquet(data_path)

    data <- quantiles_miles %>%
      filter(powertrain != "all", vehicle_type != "all")

    return(data)
  })
  
  # Load retention rate data
  retention_data <- reactive({
    # Try multiple potential data paths
    data_paths <- c(
      "data/quantiles_rr.parquet",
      "../data/quantiles_rr.parquet",
      file.path(dirname(getwd()), "data", "quantiles_rr.parquet")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find quantiles_rr.parquet file")
    }

    quantiles_rr <- read_parquet(data_path)

    data <- quantiles_rr %>%
      filter(powertrain != "all", vehicle_type != "all")

    return(data)
  })
  
  # Filtered VMT data based on user inputs
  filtered_data <- reactive({
    data <- sample_data()

    # Ensure input values exist and are valid
    fuel_types <- if(is.null(input$fuel_types) || length(input$fuel_types) == 0) {
      unique(data$powertrain)
    } else {
      input$fuel_types
    }

    vehicle_types <- if(is.null(input$vehicle_types) || length(input$vehicle_types) == 0) {
      unique(data$vehicle_type)
    } else {
      input$vehicle_types
    }

    dvmt_range <- if(is.null(input$dvmt_range)) {
      c(0, max(data$dvmt, na.rm = TRUE))
    } else {
      input$dvmt_range
    }

    data <- data %>%
      filter(powertrain %in% fuel_types,
             vehicle_type %in% vehicle_types,
             dvmt >= dvmt_range[1],
             dvmt <= dvmt_range[2])

    return(data)
  })
  
  # Filtered mileage data based on user inputs
  filtered_mileage_data <- reactive({
    data <- mileage_data()

    # Ensure input values exist and are valid
    fuel_types <- if(is.null(input$fuel_types) || length(input$fuel_types) == 0) {
      unique(data$powertrain)
    } else {
      input$fuel_types
    }

    vehicle_types <- if(is.null(input$vehicle_types) || length(input$vehicle_types) == 0) {
      unique(data$vehicle_type)
    } else {
      input$vehicle_types
    }

    data %>%
      filter(powertrain %in% fuel_types,
             vehicle_type %in% vehicle_types)
  })
  
  # Filtered retention data based on user inputs
  filtered_retention_data <- reactive({
    data <- retention_data()

    # Ensure input values exist and are valid
    fuel_types <- if(is.null(input$fuel_types) || length(input$fuel_types) == 0) {
      unique(data$powertrain)
    } else {
      input$fuel_types
    }

    vehicle_types <- if(is.null(input$vehicle_types) || length(input$vehicle_types) == 0) {
      unique(data$vehicle_type)
    } else {
      input$vehicle_types
    }

    data %>%
      filter(powertrain %in% fuel_types,
             vehicle_type %in% vehicle_types)
  })
  
  # Main Plot (CDF/PDF/Histogram) - Pure Plotly with Subplots
# Daily VMT Plot - Fully Interactive

output$cdf_plot <- renderPlotly({
  tryCatch({
    data <- filtered_data()

    if(nrow(data) == 0) {
      return(plot_ly() %>%
               add_annotations(text = "No data available for selected filters",
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5, showarrow = FALSE))
    }

    # Ensure we have valid data columns
    if(!"dvmt" %in% colnames(data) || !"quantile" %in% colnames(data)) {
      return(plot_ly() %>%
               add_annotations(text = "Missing required columns in data",
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5, showarrow = FALSE))
    }

    # Remove any NA values
    data <- data %>%
      filter(!is.na(dvmt), !is.na(quantile), !is.na(powertrain), !is.na(vehicle_type))

    if(nrow(data) == 0) {
      return(plot_ly() %>%
               add_annotations(text = "No valid data after cleaning",
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5, showarrow = FALSE))
    }

    # Get plot type from input (default to CDF)
    plot_type <- if(is.null(input$plot_type)) "cdf" else input$plot_type

    # Get unique powertrains for faceting
    powertrains <- unique(data$powertrain)
    vehicle_types <- unique(data$vehicle_type)

    # Create color palette
    colors <- viridisLite::viridis(length(vehicle_types))
    color_map <- setNames(colors, vehicle_types)

    if(plot_type == "cdf") {
      # Create subplots for each powertrain
      plots <- lapply(powertrains, function(pt) {
        pt_data <- data %>% filter(powertrain == pt)

        p <- plot_ly()

        for(vt in vehicle_types) {
          vt_data <- pt_data %>% filter(vehicle_type == vt)
          if(nrow(vt_data) > 0) {
            p <- p %>%
              add_trace(
                data = vt_data,
                x = ~dvmt,
                y = ~quantile,
                type = 'scatter',
                mode = 'lines',
                name = vt,
                line = list(color = color_map[vt], width = 2),
                hovertemplate = paste0(
                  "<b>", vt, "</b><br>",
                  "DVMT: %{x:.1f} miles<br>",
                  "Quantile: %{y:.1%}<br>",
                  "<extra></extra>"
                ),
                showlegend = (pt == powertrains[1])
              )
          }
        }

        p <- p %>%
          layout(
            xaxis = list(title = "DVMT"),
            yaxis = list(title = "Cumulative Probability"),
            annotations = list(
              x = 0.5, y = 1.05,
              text = paste("<b>", toupper(pt), "</b>"),
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 12)
            )
          )

        return(p)
      })

      # Combine into subplot
      fig <- subplot(plots, nrows = ceiling(length(powertrains)/2),
                    shareX = FALSE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(
            text = "<b>Cumulative Distribution Function (CDF) of Daily VMT</b><br><sub>Distribution of daily vehicle miles traveled by powertrain and vehicle type</sub>",
            font = list(size = 16)
          ),
          hovermode = 'closest',
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          )
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c('lasso2d', 'select2d'),
          toImageButtonOptions = list(format = 'png', filename = 'dvmt_cdf_plot')
        )

      return(fig)

    } else {
      # For PDF and Histogram, return a simple message for now
      return(plot_ly() %>%
               add_annotations(text = "PDF and Histogram modes coming soon - using CDF for now",
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5, showarrow = FALSE))
    }

  }, error = function(e) {
    return(plot_ly() %>%
             add_annotations(text = paste("Error:", e$message),
                           xref = "paper", yref = "paper",
                           x = 0.5, y = 0.5, showarrow = FALSE))
  })
})

  # Average DVMT Plot
  output$dvmt_summary <- renderPlotly({
    data <- filtered_data()

    summary_data <- data %>%
      group_by(powertrain, vehicle_type) %>%
      summarise(avg_dvmt = mean(dvmt, na.rm = TRUE), .groups = "drop")

    p <- ggplot(summary_data, aes(x = powertrain, y = avg_dvmt, fill = vehicle_type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      labs(
        title = "Average DVMT by Fuel and Vehicle Type",
        x = "Fuel Type",
        y = "Average DVMT",
        fill = "Vehicle Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })
  
  # Cumulative VMT by Age - Grid layout (rows=vehicle types, cols=powertrains)
  output$mileage_plot <- renderPlotly({
    tryCatch({
      # Load and process mileage data
      data <- mileage_data() %>%
        mutate(age_years = age_months / 12) %>%
        filter(age_years > 2)

      if(nrow(data) == 0) {
        return(plot_ly() %>%
                 add_annotations(text = "No mileage data available",
                               xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE))
      }

      # Get age range from input
      age_min <- if(!is.null(input$age_range)) max(2, input$age_range[1]) else 2
      age_max <- if(!is.null(input$age_range)) input$age_range[2] else 8

      # Apply filters
      plot_data <- data %>%
        filter(
          age_years >= age_min,
          age_years <= age_max,
          !is.na(miles50), !is.na(miles25), !is.na(miles75)
        )

      # Apply user filters
      if(!is.null(input$fuel_types) && length(input$fuel_types) > 0) {
        plot_data <- plot_data %>% filter(powertrain %in% input$fuel_types)
      }
      if(!is.null(input$vehicle_types) && length(input$vehicle_types) > 0) {
        plot_data <- plot_data %>% filter(vehicle_type %in% input$vehicle_types)
      }

      if(nrow(plot_data) == 0) {
        return(plot_ly() %>%
                 add_annotations(text = "No data matches current filters",
                               xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE))
      }

      # Aggregate by powertrain AND vehicle type (don't combine them)
      plot_data_agg <- plot_data %>%
        group_by(powertrain, vehicle_type, age_years) %>%
        summarise(
          miles25 = mean(miles25, na.rm = TRUE),
          miles50 = mean(miles50, na.rm = TRUE),
          miles75 = mean(miles75, na.rm = TRUE),
          .groups = "drop"
        )

      # Get unique powertrains and vehicle types
      powertrains <- sort(unique(plot_data_agg$powertrain))
      vehicle_types <- sort(unique(plot_data_agg$vehicle_type))
      show_bands <- if(!is.null(input$show_confidence_bands)) input$show_confidence_bands else TRUE

      # Create single figure with manual domain positioning
      fig <- plot_ly()

      # Calculate grid dimensions
      n_rows <- length(vehicle_types)
      n_cols <- length(powertrains)

      # Margins between subplots
      h_gap <- 0.05
      v_gap <- 0.05

      # Calculate cell dimensions
      cell_width <- (1 - (n_cols - 1) * h_gap) / n_cols
      cell_height <- (1 - (n_rows - 1) * v_gap) / n_rows

      # Build axis layout lists
      xaxes <- list()
      yaxes <- list()
      annotations <- list()

      trace_counter <- 0

      for(i in seq_along(vehicle_types)) {
        vt <- vehicle_types[i]

        for(j in seq_along(powertrains)) {
          pt <- powertrains[j]

          # Get data for this cell
          cell_data <- plot_data_agg %>%
            filter(vehicle_type == vt, powertrain == pt)

          # Calculate domain positions
          x_start <- (j - 1) * (cell_width + h_gap)
          x_end <- x_start + cell_width
          y_start <- 1 - i * (cell_height + v_gap)
          y_end <- y_start + cell_height

          # Create unique axis names
          xaxis_name <- if(i == 1 && j == 1) "x" else paste0("x", trace_counter + 1)
          yaxis_name <- if(i == 1 && j == 1) "y" else paste0("y", trace_counter + 1)

          if(nrow(cell_data) > 0) {
            # Add ribbon trace
            if(show_bands) {
              fig <- fig %>%
                add_trace(
                  data = cell_data,
                  x = ~age_years,
                  y = ~miles25,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "transparent"),
                  showlegend = FALSE,
                  xaxis = xaxis_name,
                  yaxis = yaxis_name,
                  hoverinfo = "none"
                ) %>%
                add_trace(
                  data = cell_data,
                  x = ~age_years,
                  y = ~miles75,
                  type = "scatter",
                  mode = "lines",
                  fill = "tonexty",
                  fillcolor = "rgba(128, 128, 128, 0.25)",
                  line = list(color = "transparent"),
                  showlegend = FALSE,
                  xaxis = xaxis_name,
                  yaxis = yaxis_name,
                  hoverinfo = "none"
                )
            }

            # Add median line
            fig <- fig %>%
              add_trace(
                data = cell_data,
                x = ~age_years,
                y = ~miles50,
                type = "scatter",
                mode = "lines",
                line = list(color = "black", width = 2),
                showlegend = FALSE,
                xaxis = xaxis_name,
                yaxis = yaxis_name,
                hoverinfo = "none"
              )
          }

          # Configure axis
          xaxis_config <- list(
            domain = c(x_start, x_end),
            range = c(age_min, age_max),
            showticklabels = TRUE,
            title = if(i == n_rows) list(text = "", font = list(size = 9)) else ""
          )

          yaxis_config <- list(
            domain = c(y_start, y_end),
            anchor = xaxis_name,
            showticklabels = TRUE,
            title = if(j == 1) list(text = "", font = list(size = 9)) else ""
          )

          if(trace_counter == 0) {
            xaxes[["xaxis"]] <- xaxis_config
            yaxes[["yaxis"]] <- yaxis_config
          } else {
            xaxes[[paste0("xaxis", trace_counter + 1)]] <- xaxis_config
            yaxes[[paste0("yaxis", trace_counter + 1)]] <- yaxis_config
          }

          trace_counter <- trace_counter + 1
        }
      }

      # Add column headers (powertrains)
      for(j in seq_along(powertrains)) {
        x_pos <- (j - 1) * (cell_width + h_gap) + cell_width / 2
        annotations[[length(annotations) + 1]] <- list(
          x = x_pos,
          y = 1.02,
          text = paste0("<b>", toupper(powertrains[j]), "</b>"),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12)
        )
      }

      # Add row labels (vehicle types)
      for(i in seq_along(vehicle_types)) {
        y_pos <- 1 - (i - 1) * (cell_height + v_gap) - cell_height / 2
        annotations[[length(annotations) + 1]] <- list(
          x = 1.01,
          y = y_pos,
          text = paste0("<b>", toupper(vehicle_types[i]), "</b>"),
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "middle",
          showarrow = FALSE,
          font = list(size = 12),
          textangle = -90
        )
      }

      # Apply layout
      layout_args <- c(
        list(
          title = list(text = "Vehicle Mileage Trends by Age", font = list(size = 16)),
          annotations = annotations,
          showlegend = FALSE,
          hovermode = "closest",
          margin = list(t = 60, b = 50, l = 60, r = 80)
        ),
        xaxes,
        yaxes
      )

      fig <- do.call(layout, c(list(fig), layout_args))

      fig <- fig %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d"),
          toImageButtonOptions = list(format = "png", filename = "mileage_plot")
        )

      return(fig)

    }, error = function(e) {
      return(plot_ly() %>%
               add_annotations(text = paste("Error:", e$message),
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5, showarrow = FALSE))
    })
  })

  # Cumulative VMT Summary Table (Research-focused)
  output$mileage_summary <- DT::renderDataTable({
    data <- filtered_mileage_data()

    # Create age-filtered summary following research approach
    summary_stats <- data %>%
      mutate(age_years = age_months / 12) %>%
      filter(age_years >= input$age_range[1], age_years <= input$age_range[2]) %>%  # Use dynamic age filter
      group_by(powertrain, vehicle_type) %>%
      summarise(
        `Median Miles` = scales::comma(round(mean(miles50, na.rm = TRUE), 0)),
        `Q1 (25%)` = scales::comma(round(mean(miles25, na.rm = TRUE), 0)),
        `Q3 (75%)` = scales::comma(round(mean(miles75, na.rm = TRUE), 0)),
        `Age Range` = paste0(round(min(age_years), 1), "-", round(max(age_years), 1), " years"),
        `Annual Miles/Year` = scales::comma(round(mean(miles50, na.rm = TRUE) / mean(age_years), 0)),
        `Observations` = n(),
        .groups = "drop"
      ) %>%
      arrange(powertrain, vehicle_type) %>%
      rename(
        `Powertrain` = powertrain,
        `Vehicle Type` = vehicle_type
      )

    DT::datatable(
      summary_stats,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = 2:7)),
        order = list(list(0, 'asc'), list(1, 'asc'))
      ),
      caption = paste("Cumulative VMT Analysis Summary (Age", input$age_range[1], "-", input$age_range[2], "years) - Research-style Statistics"),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        columns = 1:ncol(summary_stats),
        fontSize = '13px'
      ) %>%
      DT::formatStyle(
        'Powertrain',
        backgroundColor = DT::styleEqual(
          c('bev', 'cv', 'hev', 'phev'),
          c('#e8f5e8', '#f0f0f0', '#e8f5e8', '#e8f5e8')
        )
      )
  })
  
  # Vehicle Retention Rates by Age - 3-panel layout matching target design
  output$retention_plot <- renderPlotly({
    tryCatch({
      # Load retention data
      data_paths <- c(
        "data/quantiles_rr.parquet",
        "../data/quantiles_rr.parquet",
        file.path(dirname(getwd()), "data", "quantiles_rr.parquet")
      )

      data_path_rr <- NULL
      for (path in data_paths) {
        if (file.exists(path)) {
          data_path_rr <- path
          break
        }
      }

      if (is.null(data_path_rr)) {
        stop("Cannot find quantiles_rr.parquet file")
      }

      # Load main retention data
      quantiles <- read_parquet(data_path_rr) %>%
        mutate(age_years = age_months / 12) %>%
        filter(between(age_years, 1, 8))

      # Apply vehicle type filter
      if(!is.null(input$vehicle_types) && length(input$vehicle_types) > 0) {
        quantiles <- quantiles %>% filter(vehicle_type %in% input$vehicle_types)
      }

      # Aggregate by powertrain and age
      quantiles_agg <- quantiles %>%
        group_by(powertrain, age_years) %>%
        summarise(
          rr25 = mean(rr25, na.rm = TRUE),
          rr50 = mean(rr50, na.rm = TRUE),
          rr75 = mean(rr75, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(rr25), !is.na(rr50), !is.na(rr75))

      if(nrow(quantiles_agg) == 0) {
        return(plot_ly() %>%
                 add_annotations(text = "No retention data available",
                               xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE))
      }

      # Load BEV data
      data_paths_bev <- c(
        "data/quantiles_rr_bev.parquet",
        "../data/quantiles_rr_bev.parquet",
        file.path(dirname(getwd()), "data", "quantiles_rr_bev.parquet")
      )

      data_path_bev <- NULL
      for (path in data_paths_bev) {
        if (file.exists(path)) {
          data_path_bev <- path
          break
        }
      }

      quantiles_bev <- NULL
      if (!is.null(data_path_bev)) {
        quantiles_bev <- read_parquet(data_path_bev) %>%
          mutate(age_years = age_months / 12) %>%
          filter(between(age_years, 1, 8)) %>%
          group_by(tesla, age_years) %>%
          summarise(
            rr25 = mean(rr25, na.rm = TRUE),
            rr50 = mean(rr50, na.rm = TRUE),
            rr75 = mean(rr75, na.rm = TRUE),
            .groups = "drop"
          )
      }

      # Prepare data for plotting
      # Get conventional baseline
      conv_data <- quantiles_agg %>% filter(powertrain == "cv")

      # Get other powertrains
      hev_data <- quantiles_agg %>% filter(powertrain == "hev")
      phev_data <- quantiles_agg %>% filter(powertrain == "phev")

      # Get BEV data
      if(!is.null(quantiles_bev) && nrow(quantiles_bev) > 0) {
        bev_nontesla <- quantiles_bev %>% filter(tesla == 0)
        bev_tesla <- quantiles_bev %>% filter(tesla == 1)
      } else {
        bev_nontesla <- data.frame()
        bev_tesla <- data.frame()
      }

      # Create 3 panels using native Plotly with manual domain positioning
      fig <- plot_ly()

      # Panel configurations
      panel_configs <- list(
        list(title = "Hybrid", alt_data = hev_data, alt_label = "Hybrid", alt_color = "#00BA38"),
        list(title = "Plug-in Hybrid", alt_data = phev_data, alt_label = "PHEV", alt_color = "#00BA38"),
        list(title = "Battery Electric", bev_mode = TRUE)
      )

      # Calculate panel dimensions
      n_panels <- 3
      h_gap <- 0.02
      panel_width <- (1 - (n_panels - 1) * h_gap) / n_panels

      xaxes <- list()
      yaxes <- list()
      annotations <- list()

      for(i in seq_along(panel_configs)) {
        config <- panel_configs[[i]]

        # Calculate domain
        x_start <- (i - 1) * (panel_width + h_gap)
        x_end <- x_start + panel_width

        # Create unique axis names
        xaxis_name <- if(i == 1) "x" else paste0("x", i)
        yaxis_name <- if(i == 1) "y" else paste0("y", i)

        # Prepare data for this panel
        if(i == 3) {
          # Battery Electric panel
          groups <- list(
            list(data = conv_data, name = "Conventional", color = "rgba(128, 128, 128, 1)", fill_color = "rgba(128, 128, 128, 0.25)")
          )
          if(nrow(bev_nontesla) > 0) {
            groups[[length(groups) + 1]] <- list(data = bev_nontesla, name = "BEV (Non-Tesla)", color = "#00BA38", fill_color = "rgba(0, 186, 56, 0.25)")
          }
          if(nrow(bev_tesla) > 0) {
            groups[[length(groups) + 1]] <- list(data = bev_tesla, name = "BEV (Tesla)", color = "#619CFF", fill_color = "rgba(97, 156, 255, 0.25)")
          }
        } else {
          # Hybrid or PHEV panel
          groups <- list(
            list(data = conv_data, name = "Conventional", color = "rgba(128, 128, 128, 1)", fill_color = "rgba(128, 128, 128, 0.25)")
          )
          if(nrow(config$alt_data) > 0) {
            groups[[length(groups) + 1]] <- list(data = config$alt_data, name = config$alt_label, color = config$alt_color, fill_color = "rgba(0, 186, 56, 0.25)")
          }
        }

        # Add traces for each group
        for(group in groups) {
          if(nrow(group$data) > 0) {
            # Add ribbon (lower bound)
            fig <- fig %>%
              add_trace(
                data = group$data,
                x = ~age_years,
                y = ~rr25,
                type = "scatter",
                mode = "lines",
                line = list(color = "transparent"),
                showlegend = FALSE,
                xaxis = xaxis_name,
                yaxis = yaxis_name,
                hoverinfo = "none",
                name = group$name
              )

            # Add ribbon (upper bound)
            fig <- fig %>%
              add_trace(
                data = group$data,
                x = ~age_years,
                y = ~rr75,
                type = "scatter",
                mode = "lines",
                fill = "tonexty",
                fillcolor = group$fill_color,
                line = list(color = "transparent"),
                showlegend = FALSE,
                xaxis = xaxis_name,
                yaxis = yaxis_name,
                hoverinfo = "none",
                name = group$name
              )

            # Add median line
            fig <- fig %>%
              add_trace(
                data = group$data,
                x = ~age_years,
                y = ~rr50,
                type = "scatter",
                mode = "lines",
                line = list(color = group$color, width = 2.5),
                showlegend = FALSE,
                xaxis = xaxis_name,
                yaxis = yaxis_name,
                hoverinfo = "none",
                name = group$name
              )
          }
        }

        # Configure axes
        xaxis_config <- list(
          domain = c(x_start, x_end),
          range = c(1, 8),
          title = list(text = "Vehicle age (years)", font = list(size = 11)),
          tickmode = "linear",
          tick0 = 1,
          dtick = 1,
          showticklabels = TRUE
        )

        yaxis_config <- list(
          domain = c(0, 1),
          anchor = xaxis_name,
          range = c(0, 1),
          tickformat = ".0%",
          title = if(i == 1) list(text = "Vehicle value retention rate", font = list(size = 11)) else "",
          showticklabels = TRUE
        )

        if(i == 1) {
          xaxes[["xaxis"]] <- xaxis_config
          yaxes[["yaxis"]] <- yaxis_config
        } else {
          xaxes[[paste0("xaxis", i)]] <- xaxis_config
          yaxes[[paste0("yaxis", i)]] <- yaxis_config
        }

        # Add panel title
        annotations[[length(annotations) + 1]] <- list(
          x = x_start + panel_width / 2,
          y = 1.05,
          text = paste0("<b>", config$title, "</b>"),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 13)
        )
      }

      # Apply layout
      layout_args <- c(
        list(
          annotations = annotations,
          showlegend = FALSE,
          hovermode = "closest",
          margin = list(t = 80, b = 50, l = 80, r = 50)
        ),
        xaxes,
        yaxes
      )

      fig <- do.call(layout, c(list(fig), layout_args))

      fig <- fig %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d"),
          toImageButtonOptions = list(format = "png", filename = "retention_plot")
        )

      return(fig)

    }, error = function(e) {
      return(plot_ly() %>%
               add_annotations(text = paste("Error:", e$message),
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5, showarrow = FALSE,
                             font = list(size = 14, color = "red")))
    })
  })

  # Custom Retention Plot Builder (Plotly Interactive)
  output$custom_retention_plot <- renderPlotly({
    # React to button click OR auto-load on page open
    input$update_custom_plot

    tryCatch({
      # Define colors
      color_cv <- "grey42"
      color_ev <- "#00BA38"
      color_tesla <- "#619CFF"
      color_phev <- "#E69F00"  # Orange for PHEV

      # Get filtered data
      filtered_data <- filtered_retention_data()
      raw_data <- retention_data()

      # Get selected vehicle types
      selected_types <- if(is.null(input$custom_vehicle_types) || length(input$custom_vehicle_types) == 0) {
        c("conventional", "hybrid")
      } else {
        input$custom_vehicle_types
      }

      # Process data for selected types
      plot_data <- list()

      # Handle each selected type
      if("conventional" %in% selected_types) {
        conv_data <- raw_data %>%
          filter(powertrain == "cv") %>%
          mutate(age_years = age_months / 12) %>%
          filter(between(age_years, 1, 8), !is.na(rr25), !is.na(rr50), !is.na(rr75)) %>%
          group_by(age_years) %>%
          summarise(
            rr25 = mean(rr25, na.rm = TRUE),
            rr50 = mean(rr50, na.rm = TRUE),
            rr75 = mean(rr75, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(vehicle_type = "Conventional", type = "conventional")
        plot_data[["conventional"]] <- conv_data
      }

      if("hybrid" %in% selected_types) {
        hybrid_data <- raw_data %>%
          filter(powertrain == "hev") %>%
          mutate(age_years = age_months / 12) %>%
          filter(between(age_years, 1, 8), !is.na(rr25), !is.na(rr50), !is.na(rr75)) %>%
          group_by(age_years) %>%
          summarise(
            rr25 = mean(rr25, na.rm = TRUE),
            rr50 = mean(rr50, na.rm = TRUE),
            rr75 = mean(rr75, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(vehicle_type = "Hybrid", type = "hybrid")
        plot_data[["hybrid"]] <- hybrid_data
      }

      if("phev" %in% selected_types) {
        phev_data <- raw_data %>%
          filter(powertrain == "phev") %>%
          mutate(age_years = age_months / 12) %>%
          filter(between(age_years, 1, 8), !is.na(rr25), !is.na(rr50), !is.na(rr75)) %>%
          group_by(age_years) %>%
          summarise(
            rr25 = mean(rr25, na.rm = TRUE),
            rr50 = mean(rr50, na.rm = TRUE),
            rr75 = mean(rr75, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(vehicle_type = "Plug-in Hybrid", type = "phev")
        plot_data[["phev"]] <- phev_data
      }

      if("bev" %in% selected_types) {
        # Load BEV parquet file for non-Tesla BEVs
        data_paths_bev <- c(
          "data/quantiles_rr_bev.parquet",
          "../data/quantiles_rr_bev.parquet",
          file.path(dirname(getwd()), "data", "quantiles_rr_bev.parquet")
        )

        data_path_bev <- NULL
        for (path in data_paths_bev) {
          if (file.exists(path)) {
            data_path_bev <- path
            break
          }
        }

        if (!is.null(data_path_bev)) {
          bev_data <- read_parquet(data_path_bev) %>%
            filter(tesla == 0) %>%
            mutate(age_years = age_months / 12) %>%
            filter(between(age_years, 1, 8)) %>%
            group_by(age_years) %>%
            summarise(
              rr25 = mean(rr25, na.rm = TRUE),
              rr50 = mean(rr50, na.rm = TRUE),
              rr75 = mean(rr75, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(vehicle_type = "Battery Electric", type = "bev")
          plot_data[["bev"]] <- bev_data
        }
      }

      if("tesla" %in% selected_types) {
        # Load BEV parquet file for Tesla vehicles
        data_paths_bev <- c(
          "data/quantiles_rr_bev.parquet",
          "../data/quantiles_rr_bev.parquet",
          file.path(dirname(getwd()), "data", "quantiles_rr_bev.parquet")
        )

        data_path_bev <- NULL
        for (path in data_paths_bev) {
          if (file.exists(path)) {
            data_path_bev <- path
            break
          }
        }

        if (!is.null(data_path_bev)) {
          tesla_data <- read_parquet(data_path_bev) %>%
            filter(tesla == 1) %>%
            mutate(age_years = age_months / 12) %>%
            filter(between(age_years, 1, 8)) %>%
            group_by(age_years) %>%
            summarise(
              rr25 = mean(rr25, na.rm = TRUE),
              rr50 = mean(rr50, na.rm = TRUE),
              rr75 = mean(rr75, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(vehicle_type = "Tesla", type = "tesla")
          plot_data[["tesla"]] <- tesla_data
        }
      }

      # Combine all selected data
      final_data <- bind_rows(plot_data)

      if(nrow(final_data) == 0) {
        p <- plot_ly() %>%
          layout(
            title = list(text = "No vehicle types selected<br><sub>Please select at least one vehicle type to compare</sub>"),
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
        return(p)
      }

      # Create native plotly plot
      # Define color mapping (line colors)
      color_mapping <- list(
        "Conventional" = color_cv,
        "Hybrid" = color_ev,
        "Plug-in Hybrid" = color_phev,
        "Battery Electric" = color_ev,
        "Tesla" = color_tesla
      )

      # Define fill color mapping (hardcoded rgba to avoid col2rgb issues)
      fill_mapping <- list(
        "Conventional" = 'rgba(107,107,107,0.25)',  # grey42
        "Hybrid" = 'rgba(0,186,56,0.25)',           # #00BA38
        "Plug-in Hybrid" = 'rgba(230,159,0,0.25)',  # #E69F00
        "Battery Electric" = 'rgba(0,186,56,0.25)', # #00BA38
        "Tesla" = 'rgba(97,156,255,0.25)'           # #619CFF
      )

      p <- plot_ly()

      # Get unique vehicle types in the data
      vehicle_types <- unique(final_data$vehicle_type)

      # Determine whether to show confidence ribbons
      show_ribbons <- if(!is.null(input$show_confidence_ribbons)) input$show_confidence_ribbons else FALSE

      # Add ribbons first (if enabled)
      if(show_ribbons) {
        for(vt in vehicle_types) {
          vt_data <- final_data %>% filter(vehicle_type == vt)

          if(nrow(vt_data) > 0) {
            line_color <- color_mapping[[vt]]
            fill_color <- fill_mapping[[vt]]

            p <- p %>%
              add_ribbons(
                data = vt_data,
                x = ~age_years,
                ymin = ~rr25,
                ymax = ~rr75,
                fillcolor = fill_color,
                line = list(width = 0),
                showlegend = FALSE,
                hoverinfo = 'skip',
                name = vt
              )
          }
        }
      }

      # Add lines on top
      for(vt in vehicle_types) {
        vt_data <- final_data %>% filter(vehicle_type == vt)

        if(nrow(vt_data) > 0) {
          line_color <- color_mapping[[vt]]

          p <- p %>%
            add_trace(
              data = vt_data,
              x = ~age_years,
              y = ~rr50,
              type = 'scatter',
              mode = 'lines',
              line = list(color = line_color, width = 2.5),
              name = vt,
              showlegend = TRUE,
              hovertemplate = paste0(
                "<b>", vt, "</b><br>",
                "Age: %{x:.1f} years<br>",
                "Retention Rate: %{y:.2%}<br>",
                "<extra></extra>"
              )
            )
        }
      }

      # Configure layout
      p <- p %>%
        layout(
          title = list(
            text = "Custom Vehicle Retention Rate Comparison",
            font = list(size = 16)
          ),
          xaxis = list(
            title = "Vehicle age (years)",
            range = c(1, 8),
            dtick = 1,
            gridcolor = '#e0e0e0',
            showgrid = TRUE
          ),
          yaxis = list(
            title = "Vehicle value retention rate",
            range = c(0, 1),
            tickformat = ',.0%',
            dtick = 0.2,
            gridcolor = '#e0e0e0',
            showgrid = TRUE
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            title = list(text = "Vehicle Type")
          ),
          hovermode = 'closest',
          plot_bgcolor = 'white',
          paper_bgcolor = 'white'
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c('lasso2d', 'select2d'),
          toImageButtonOptions = list(format = 'png', filename = 'custom_retention_plot')
        )

      return(p)

    }, error = function(e) {
      p <- ggplot() +
        labs(title = paste("Error creating custom plot:", e$message)) +
        theme_minimal()
      return(ggplotly(p))
    })
  })

  # Metrics Table
  output$metrics_table <- DT::renderDataTable({
    data <- filtered_data()
    
    metrics <- data %>%
      group_by(powertrain, vehicle_type) %>%
      summarise(
        Mean_DVMT = round(mean(dvmt), 2),
        Median_DVMT = round(median(dvmt), 2),
        Max_CDF = round(max(quantile), 2),
        .groups = "drop"
      )
    
    DT::datatable(metrics, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Raw Data Table
  output$raw_data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Similarity Analysis Plot (for Nearest Vehicle page)
  output$similarity_plot <- renderPlotly({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Create similarity matrix based on DVMT values
    similarity_data <- data %>%
      group_by(powertrain, vehicle_type) %>%
      summarise(
        mean_dvmt = mean(dvmt),
        median_dvmt = median(dvmt),
        .groups = "drop"
      ) %>%
      mutate(
        vehicle_combo = paste(powertrain, vehicle_type, sep = " - ")
      )
    
    p <- ggplot(similarity_data, aes(x = mean_dvmt, y = median_dvmt)) +
      geom_point(aes(color = powertrain, size = 3), alpha = 0.7) +
      geom_text(aes(label = vehicle_type), vjust = -0.8, size = 3) +
      labs(
        title = "Vehicle Similarity by DVMT Patterns",
        subtitle = "Vehicles with similar mean and median DVMT are clustered together",
        x = "Mean DVMT",
        y = "Median DVMT",
        color = "Fuel Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(size = "none")
    
    ggplotly(p, tooltip = c("x", "y", "colour", "text"))
  })
  
  # Clustering Plot (for Nearest Vehicle page)
  output$cluster_plot <- renderPlotly({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Simple clustering visualization
    cluster_data <- data %>%
      group_by(powertrain, vehicle_type) %>%
      summarise(
        count = n(),
        dvmt_range = max(dvmt) - min(dvmt),
        .groups = "drop"
      )
    
    p <- ggplot(cluster_data, aes(x = count, y = dvmt_range)) +
      geom_point(aes(color = powertrain, size = count), alpha = 0.7) +
      geom_text(aes(label = vehicle_type), vjust = 1.2, size = 3) +
      labs(
        title = "Vehicle Type Clustering",
        subtitle = "Clustering by data availability and DVMT range",
        x = "Number of Data Points",
        y = "DVMT Range",
        color = "Fuel Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(size = "none")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Summary Tables
  output$fuel_summary <- renderTable({
    filtered_data() %>%
      group_by(powertrain) %>%
      summarise(
        Count = n(),
        Mean_DVMT = round(mean(dvmt), 2),
        Std_Dev = round(sd(dvmt), 2),
        .groups = "drop"
      )
  })
  
  output$vehicle_summary <- renderTable({
    filtered_data() %>%
      group_by(vehicle_type) %>%
      summarise(
        Count = n(),
        Mean_DVMT = round(mean(dvmt), 2),
        Std_Dev = round(sd(dvmt), 2),
        .groups = "drop"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
