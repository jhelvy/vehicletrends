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
  
  # Custom CSS for enhanced UI
  header = tags$head(
    tags$style(HTML("
      /* Navbar styling */
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
        transition: all 0.3s ease;
      }
      .navbar-nav .nav-link:hover {
        transform: translateY(-2px);
        background-color: rgba(52, 152, 219, 0.1);
        border-radius: 8px;
      }
      .navbar-nav .nav-link.active {
        border-bottom: 3px solid #3498db;
      }
      .navbar-brand {
        font-weight: 600;
        font-size: 20px;
      }

      /* Card enhancements */
      .card {
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07), 0 1px 3px rgba(0, 0, 0, 0.06);
        border: 1px solid #e9ecef;
        transition: all 0.3s ease;
        margin-bottom: 20px;
      }
      .card:hover {
        box-shadow: 0 10px 20px rgba(0, 0, 0, 0.12), 0 4px 8px rgba(0, 0, 0, 0.08);
        transform: translateY(-2px);
      }
      .card-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white !important;
        font-weight: 600;
        padding: 1.25rem;
        border-radius: 12px 12px 0 0 !important;
        border-bottom: none;
      }

      /* Sidebar styling */
      .bslib-sidebar-layout > .sidebar {
        background: linear-gradient(180deg, #f8f9fa 0%, #ffffff 100%);
        border-right: 1px solid #dee2e6;
        box-shadow: 2px 0 8px rgba(0, 0, 0, 0.05);
      }

      /* Form controls */
      .form-check-input:checked {
        background-color: #3498db;
        border-color: #3498db;
      }
      .form-control:focus, .form-select:focus {
        border-color: #3498db;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }

      /* Plotly plot improvements */
      .js-plotly-plot {
        border-radius: 8px;
      }

      /* Better spacing */
      .bslib-page-navbar {
        background-color: #ffffff;
      }

      /* Headings */
      h5, h6 {
        letter-spacing: 0.5px;
      }
    "))
  ),
  
  # Global sidebar for filters (shared across all pages)
  sidebar = sidebar(
    title = "Global Filters",
    width = 280,

    h5("Dataset Filters", style = "font-weight: bold; color: #2c3e50;"),

    conditionalPanel(
      condition = "input.tabs != 'Make & Model'",
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

      hr()
    ),

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

      checkboxInput("show_aggregated", "Show Aggregated Line", FALSE)
    ),

    conditionalPanel(
      condition = "input.tabs == 'Cumulative VMT'",
      h6("Cumulative VMT Controls", style = "font-weight: 600; color: #34495e;"),

      radioButtons("comparison_category",
                  "Compare by Category:",
                  choices = list("Powertrain" = "powertrain",
                               "Vehicle Type" = "vehicle_type"),
                  selected = "powertrain"),

      hr(style = "border-color: #ddd; margin: 10px 0;"),

      sliderInput("age_range",
                 "Vehicle Age Range (Years):",
                 min = 1, max = 10, value = c(2, 8), step = 0.5),

      checkboxInput("show_confidence_bands", "Show Confidence Bands", TRUE)
    ),

    conditionalPanel(
      condition = "input.tabs == 'Depreciation'",
      h6("Depreciation Controls", style = "font-weight: 600; color: #34495e;"),

      radioButtons("depreciation_category",
                  "Compare by Category:",
                  choices = list("Powertrain" = "powertrain",
                               "Vehicle Type" = "vehicle_type"),
                  selected = "powertrain"),

      hr(style = "border-color: #ddd; margin: 10px 0;"),

      sliderInput("depreciation_age_range",
                 "Age Range for Analysis (Years):",
                 min = 1, max = 8, value = c(1, 8), step = 0.5),

      radioButtons("depreciation_type", "Analysis Type:",
                  choices = list("Retention Rate" = "retention",
                               "Depreciation Rate" = "depreciation",
                               "Both" = "both"),
                  selected = "both")
    ),

    conditionalPanel(
      condition = "input.tabs == 'Make & Model'",
      h6("Make & Model Controls", style = "font-weight: 600; color: #34495e;"),

      selectInput("make_model_metric",
                 "Metric:",
                 choices = list("Cumulative Mileage" = "mileage",
                              "Depreciation" = "depreciation"),
                 selected = "mileage"),

      selectInput("make_model_make",
                 "Select Make:",
                 choices = NULL),

      selectInput("make_model_model",
                 "Select Model:",
                 choices = NULL,
                 multiple = TRUE),

      hr(style = "border-color: #ddd; margin: 10px 0;"),

      sliderInput("make_model_age_range",
                 "Age Range (Years):",
                 min = 1, max = 9, value = c(2, 8), step = 0.5)
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
    card(
      full_screen = TRUE,
      card_header(
        "Vehicle Mileage Trends by Age",
        class = "text-center"
      ),
      plotlyOutput("mileage_plot", height = "700px")
    )
  ),
  
  # Depreciation Page
  nav_panel(
    title = "Depreciation",
    value = "Depreciation",
    icon = icon("chart-line"),

    # Retention Plot
    card(
      full_screen = TRUE,
      card_header(
        "Vehicle Retention Rates by Age",
        class = "text-center"
      ),
      plotlyOutput("retention_plot", height = "700px")
    )
  ),

  # Make & Model Analysis Page
  nav_panel(
    title = "Make & Model",
    value = "Make & Model",
    icon = icon("car"),
    card(
      full_screen = TRUE,
      card_header(
        textOutput("make_model_title"),
        class = "text-center"
      ),
      plotlyOutput("make_model_plot", height = "700px")
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

  # Update filter choices based on loaded data and active tab
  observe({
    # Get current active tab
    current_tab <- input$tabs

    # Determine which datasets are relevant for the current tab
    if (current_tab == "Daily VMT") {
      # Daily VMT uses sample_data (DVMT)
      data_available <- tryCatch({ sample_data() }, error = function(e) NULL)
    } else if (current_tab == "Cumulative VMT") {
      # Cumulative VMT uses mileage_data
      data_available <- tryCatch({ mileage_data() }, error = function(e) NULL)
    } else if (current_tab == "Depreciation") {
      # Depreciation uses retention_data
      data_available <- tryCatch({ retention_data() }, error = function(e) NULL)
    } else {
      # For other tabs, show all available powertrains from DVMT
      data_available <- tryCatch({ sample_data() }, error = function(e) NULL)
    }

    if (!is.null(data_available)) {
      available_powertrains <- sort(unique(data_available$powertrain))
      available_vehicle_types <- sort(unique(data_available$vehicle_type))

      # Update powertrain choices with clear labeling
      powertrain_choices <- setNames(available_powertrains,
                                   case_when(
                                     available_powertrains == "bev" ~ "BEV",
                                     available_powertrains == "bev_non_tesla" ~ "BEV (Non-Tesla)",
                                     available_powertrains == "bev_tesla" ~ "BEV (Tesla)",
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

      # Get currently selected values (use isolate to prevent reactive loop)
      current_fuel_selection <- isolate(input$fuel_types)
      current_vehicle_selection <- isolate(input$vehicle_types)

      # Keep selections that are still valid, or default if none are valid
      new_fuel_selection <- intersect(current_fuel_selection, available_powertrains)
      if (length(new_fuel_selection) == 0) {
        new_fuel_selection <- intersect(c("bev", "cv", "hev"), available_powertrains)
      }

      new_vehicle_selection <- intersect(current_vehicle_selection, available_vehicle_types)
      if (length(new_vehicle_selection) == 0) {
        new_vehicle_selection <- available_vehicle_types
      }

      updateCheckboxGroupInput(session, "fuel_types",
                              choices = powertrain_choices,
                              selected = new_fuel_selection)

      updateCheckboxGroupInput(session, "vehicle_types",
                              choices = vehicle_type_choices,
                              selected = new_vehicle_selection)
    }

    # Update DVMT range based on actual data (only for Daily VMT tab)
    if (current_tab == "Daily VMT") {
      sample_data_available <- tryCatch({ sample_data() }, error = function(e) NULL)
      if (!is.null(sample_data_available) && "dvmt" %in% colnames(sample_data_available)) {
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

    # Note: Daily VMT data has all BEV subtypes, so no mapping needed here
    # Just filter directly with the selected values
    fuel_types <- intersect(fuel_types, unique(data$powertrain))

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

    # Map BEV subtypes for mileage data (which only has "bev", not subtypes)
    if(any(c("bev_tesla", "bev_non_tesla") %in% fuel_types)) {
      fuel_types <- c(fuel_types, "bev")
    }
    fuel_types <- intersect(fuel_types, c("bev", "cv", "diesel", "hev", "phev"))

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

    # Map BEV subtypes for retention data (which only has "bev", not subtypes)
    if(any(c("bev_tesla", "bev_non_tesla") %in% fuel_types)) {
      fuel_types <- c(fuel_types, "bev")
    }
    fuel_types <- intersect(fuel_types, c("bev", "cv", "hev", "phev"))

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
                line = list(color = color_map[vt], width = 2.5),
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

        # Add aggregated line if requested
        if(!is.null(input$show_aggregated) && input$show_aggregated && length(vehicle_types) > 1) {
          # Compute aggregated CDF across all vehicle types
          agg_data <- pt_data %>%
            group_by(dvmt) %>%
            summarise(quantile = mean(quantile, na.rm = TRUE), .groups = "drop") %>%
            arrange(dvmt)

          if(nrow(agg_data) > 0) {
            p <- p %>%
              add_trace(
                data = agg_data,
                x = ~dvmt,
                y = ~quantile,
                type = 'scatter',
                mode = 'lines',
                name = 'Aggregated',
                line = list(color = 'black', width = 3, dash = 'dash'),
                hovertemplate = paste0(
                  "<b>Aggregated</b><br>",
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
            xaxis = list(
              title = list(text = "DVMT", font = list(size = 13, family = "Inter")),
              gridcolor = "#e9ecef",
              showline = TRUE,
              linecolor = "#dee2e6"
            ),
            yaxis = list(
              title = list(text = "Cumulative Probability", font = list(size = 13, family = "Inter")),
              gridcolor = "#e9ecef",
              showline = TRUE,
              linecolor = "#dee2e6"
            ),
            plot_bgcolor = "#fafbfc",
            paper_bgcolor = "#ffffff",
            annotations = list(
              x = 0.5, y = 1.08,
              text = paste("<b>", toupper(pt), "</b>"),
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, family = "Inter", color = "#2c3e50")
            )
          )

        return(p)
      })

      # Combine into subplot with 3 columns for better aspect ratio
      fig <- subplot(plots, nrows = ceiling(length(powertrains)/3),
                    shareX = FALSE, shareY = TRUE, titleX = TRUE, titleY = TRUE,
                    margin = 0.06) %>%
        layout(
          title = list(
            text = "<b>Cumulative Distribution Function (CDF) of Daily VMT</b><br><sub style='color:#6c757d;'>Distribution of daily vehicle miles traveled by powertrain and vehicle type</sub>",
            font = list(size = 18, family = "Inter", color = "#2c3e50")
          ),
          hovermode = 'closest',
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.12,
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "#dee2e6",
            borderwidth = 1,
            font = list(size = 12, family = "Inter")
          ),
          margin = list(t = 100, b = 80, l = 60, r = 40),
          paper_bgcolor = "#ffffff",
          plot_bgcolor = "#fafbfc"
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = c('lasso2d', 'select2d', 'autoScale2d'),
          toImageButtonOptions = list(
            format = 'png',
            filename = 'dvmt_cdf_plot',
            height = 1200,
            width = 1800,
            scale = 2
          )
        )

      return(fig)

    } else if(plot_type == "pdf") {
      # PDF: Compute density from quantile data
      plots <- lapply(powertrains, function(pt) {
        pt_data <- data %>% filter(powertrain == pt)

        p <- plot_ly()

        for(vt in vehicle_types) {
          vt_data <- pt_data %>%
            filter(vehicle_type == vt) %>%
            arrange(dvmt)

          if(nrow(vt_data) > 1) {
            # Compute PDF as derivative of CDF
            pdf_data <- vt_data %>%
              mutate(
                pdf = c(diff(quantile) / diff(dvmt), NA)
              ) %>%
              filter(!is.na(pdf), pdf >= 0)

            if(nrow(pdf_data) > 0) {
              p <- p %>%
                add_trace(
                  data = pdf_data,
                  x = ~dvmt,
                  y = ~pdf,
                  type = 'scatter',
                  mode = 'lines',
                  name = vt,
                  line = list(color = color_map[vt], width = 2),
                  fill = 'tozeroy',
                  fillcolor = paste0(substr(color_map[vt], 1, 7), "30"),
                  hovertemplate = paste0(
                    "<b>", vt, "</b><br>",
                    "DVMT: %{x:.1f} miles<br>",
                    "Density: %{y:.4f}<br>",
                    "<extra></extra>"
                  ),
                  showlegend = (pt == powertrains[1])
                )
            }
          }
        }

        # Add aggregated line if requested
        if(!is.null(input$show_aggregated) && input$show_aggregated && length(vehicle_types) > 1) {
          # Compute aggregated PDF across all vehicle types
          agg_cdf <- pt_data %>%
            group_by(dvmt) %>%
            summarise(quantile = mean(quantile, na.rm = TRUE), .groups = "drop") %>%
            arrange(dvmt)

          if(nrow(agg_cdf) > 1) {
            agg_pdf <- agg_cdf %>%
              mutate(
                pdf = c(diff(quantile) / diff(dvmt), NA)
              ) %>%
              filter(!is.na(pdf), pdf >= 0)

            if(nrow(agg_pdf) > 0) {
              p <- p %>%
                add_trace(
                  data = agg_pdf,
                  x = ~dvmt,
                  y = ~pdf,
                  type = 'scatter',
                  mode = 'lines',
                  name = 'Aggregated',
                  line = list(color = 'black', width = 3, dash = 'dash'),
                  hovertemplate = paste0(
                    "<b>Aggregated</b><br>",
                    "DVMT: %{x:.1f} miles<br>",
                    "Density: %{y:.4f}<br>",
                    "<extra></extra>"
                  ),
                  showlegend = (pt == powertrains[1])
                )
            }
          }
        }

        p <- p %>%
          layout(
            xaxis = list(title = "DVMT"),
            yaxis = list(title = "Probability Density"),
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

      fig <- subplot(plots, nrows = ceiling(length(powertrains)/3),
                    shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(
            text = "<b>Probability Density Function (PDF) of Daily VMT</b><br><sub>Density distribution of daily vehicle miles traveled</sub>",
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
          toImageButtonOptions = list(format = 'png', filename = 'dvmt_pdf_plot')
        )

      return(fig)

    } else if(plot_type == "hist") {
      # Histogram: Create binned counts
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
                type = 'histogram',
                name = vt,
                marker = list(
                  color = color_map[vt],
                  line = list(color = 'white', width = 1)
                ),
                opacity = 0.7,
                hovertemplate = paste0(
                  "<b>", vt, "</b><br>",
                  "DVMT: %{x}<br>",
                  "Count: %{y}<br>",
                  "<extra></extra>"
                ),
                showlegend = (pt == powertrains[1])
              )
          }
        }

        # Add aggregated density line if requested
        if(!is.null(input$show_aggregated) && input$show_aggregated && length(vehicle_types) > 1) {
          # Compute aggregated density from all vehicle types
          agg_data <- pt_data %>%
            group_by(dvmt) %>%
            summarise(quantile = mean(quantile, na.rm = TRUE), .groups = "drop") %>%
            arrange(dvmt)

          if(nrow(agg_data) > 1) {
            # Compute density from CDF
            agg_pdf <- agg_data %>%
              mutate(
                pdf = c(diff(quantile) / diff(dvmt), NA)
              ) %>%
              filter(!is.na(pdf), pdf >= 0)

            if(nrow(agg_pdf) > 0) {
              # Scale PDF to histogram counts (approximate)
              max_pdf <- max(agg_pdf$pdf, na.rm = TRUE)
              if(max_pdf > 0) {
                # Normalize to a reasonable scale
                scale_factor <- nrow(pt_data) * (max(pt_data$dvmt) - min(pt_data$dvmt)) / 30
                agg_pdf <- agg_pdf %>%
                  mutate(pdf_scaled = pdf * scale_factor)

                p <- p %>%
                  add_trace(
                    data = agg_pdf,
                    x = ~dvmt,
                    y = ~pdf_scaled,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Aggregated',
                    line = list(color = 'black', width = 3, dash = 'dash'),
                    yaxis = 'y2',
                    hovertemplate = paste0(
                      "<b>Aggregated Density</b><br>",
                      "DVMT: %{x:.1f} miles<br>",
                      "Density: %{y:.2f}<br>",
                      "<extra></extra>"
                    ),
                    showlegend = (pt == powertrains[1])
                  )
              }
            }
          }
        }

        layout_config <- list(
          xaxis = list(title = "DVMT"),
          yaxis = list(title = "Count"),
          barmode = 'overlay',
          annotations = list(
            x = 0.5, y = 1.05,
            text = paste("<b>", toupper(pt), "</b>"),
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 12)
          )
        )

        # Add secondary y-axis if aggregated line is shown
        if(!is.null(input$show_aggregated) && input$show_aggregated && length(vehicle_types) > 1) {
          layout_config$yaxis2 <- list(
            overlaying = 'y',
            side = 'right',
            title = 'Density',
            showgrid = FALSE
          )
        }

        p <- p %>% layout(layout_config)

        return(p)
      })

      fig <- subplot(plots, nrows = ceiling(length(powertrains)/3),
                    shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(
            text = "<b>Histogram of Daily VMT</b><br><sub>Frequency distribution of daily vehicle miles traveled</sub>",
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
          toImageButtonOptions = list(format = 'png', filename = 'dvmt_histogram_plot')
        )

      return(fig)
    } else {
      # Default to CDF if unknown type
      return(plot_ly() %>%
               add_annotations(text = "Unknown plot type",
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
  
  # Cumulative VMT by Age - Category-based comparison
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

      # Get category selection (default to powertrain)
      selected_category <- if(!is.null(input$comparison_category)) {
        input$comparison_category
      } else {
        "powertrain"
      }

      # Apply filters
      plot_data <- data %>%
        filter(
          age_years >= age_min,
          age_years <= age_max,
          !is.na(miles50), !is.na(miles25), !is.na(miles75)
        )

      # Apply user filters with mapping for BEV subtypes
      if(!is.null(input$fuel_types) && length(input$fuel_types) > 0) {
        # Map BEV subtypes to main BEV category for mileage data
        selected_powertrains <- input$fuel_types
        # If user selected bev_tesla or bev_non_tesla, also include "bev"
        if(any(c("bev_tesla", "bev_non_tesla") %in% selected_powertrains)) {
          selected_powertrains <- c(selected_powertrains, "bev")
        }
        # Filter using only powertrains that exist in mileage data
        available_powertrains <- c("bev", "cv", "diesel", "hev", "phev")
        selected_powertrains <- intersect(selected_powertrains, available_powertrains)

        if(length(selected_powertrains) > 0) {
          plot_data <- plot_data %>% filter(powertrain %in% selected_powertrains)
        } else {
          # No valid powertrains selected, return empty data
          plot_data <- plot_data %>% filter(FALSE)
        }
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

      # Aggregate based on selected category (keeping BOTH dimensions)
      if(selected_category == "powertrain") {
        # Group by powertrain AND vehicle_type, then aggregate within each combination
        plot_data_agg <- plot_data %>%
          group_by(powertrain, vehicle_type, age_years) %>%
          summarise(
            miles25 = mean(miles25, na.rm = TRUE),
            miles50 = mean(miles50, na.rm = TRUE),
            miles75 = mean(miles75, na.rm = TRUE),
            .groups = "drop"
          )
        categories <- sort(unique(plot_data_agg$powertrain))
        category_var <- "powertrain"
        category_label <- "Powertrain"
        secondary_var <- "vehicle_type"
      } else {
        # Group by vehicle_type AND powertrain, then aggregate within each combination
        plot_data_agg <- plot_data %>%
          group_by(vehicle_type, powertrain, age_years) %>%
          summarise(
            miles25 = mean(miles25, na.rm = TRUE),
            miles50 = mean(miles50, na.rm = TRUE),
            miles75 = mean(miles75, na.rm = TRUE),
            .groups = "drop"
          )
        categories <- sort(unique(plot_data_agg$vehicle_type))
        category_var <- "vehicle_type"
        category_label <- "Vehicle Type"
        secondary_var <- "powertrain"
      }

      show_bands <- if(!is.null(input$show_confidence_bands)) {
        input$show_confidence_bands
      } else {
        TRUE
      }

      # Get secondary categories (vehicle types or powertrains to show as lines)
      if(category_var == "powertrain") {
        secondary_cats <- sort(unique(plot_data_agg$vehicle_type))
      } else {
        secondary_cats <- sort(unique(plot_data_agg$powertrain))
      }

      # Create color palette for secondary categories (lines within each panel)
      colors <- viridisLite::viridis(length(secondary_cats))
      color_map <- setNames(colors, secondary_cats)

      # Create single figure with subplots (one per category)
      fig <- plot_ly()

      # Calculate grid dimensions (arrange in 3 columns)
      n_cols <- min(3, length(categories))
      n_rows <- ceiling(length(categories) / n_cols)

      # Margins between subplots
      h_gap <- 0.05
      v_gap <- 0.08

      # Calculate cell dimensions
      cell_width <- (1 - (n_cols - 1) * h_gap) / n_cols
      cell_height <- (1 - (n_rows - 1) * v_gap) / n_rows

      # Build axis layout lists
      xaxes <- list()
      yaxes <- list()
      annotations <- list()

      for(idx in seq_along(categories)) {
        cat <- categories[idx]

        # Get data for this category
        if(category_var == "powertrain") {
          cat_data <- plot_data_agg %>% filter(powertrain == cat)
        } else {
          cat_data <- plot_data_agg %>% filter(vehicle_type == cat)
        }

        # Calculate grid position
        row <- ceiling(idx / n_cols)
        col <- ((idx - 1) %% n_cols) + 1

        # Calculate domain positions
        x_start <- (col - 1) * (cell_width + h_gap)
        x_end <- x_start + cell_width
        y_start <- 1 - row * (cell_height + v_gap)
        y_end <- y_start + cell_height

        # Create unique axis names
        xaxis_name <- if(idx == 1) "x" else paste0("x", idx)
        yaxis_name <- if(idx == 1) "y" else paste0("y", idx)

        if(nrow(cat_data) > 0) {
          # Plot each secondary category as a separate line
          for(sec_cat in secondary_cats) {
            # Filter data for this secondary category
            if(category_var == "powertrain") {
              sec_data <- cat_data %>% filter(vehicle_type == sec_cat)
            } else {
              sec_data <- cat_data %>% filter(powertrain == sec_cat)
            }

            if(nrow(sec_data) > 0) {
              # Add ribbon trace
              if(show_bands) {
                fig <- fig %>%
                  add_trace(
                    data = sec_data,
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
                    data = sec_data,
                    x = ~age_years,
                    y = ~miles75,
                    type = "scatter",
                    mode = "lines",
                    fill = "tonexty",
                    fillcolor = paste0(substr(color_map[sec_cat], 1, 7), "40"),
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
                  data = sec_data,
                  x = ~age_years,
                  y = ~miles50,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = color_map[sec_cat], width = 2.5),
                  name = toupper(sec_cat),
                  showlegend = (idx == 1),
                  legendgroup = sec_cat,
                  xaxis = xaxis_name,
                  yaxis = yaxis_name,
                  hovertemplate = paste0(
                    "<b>", toupper(cat), " - ", toupper(sec_cat), "</b><br>",
                    "Age: %{x:.1f} years<br>",
                    "Mileage: %{y:,.0f} miles<br>",
                    "<extra></extra>"
                  )
                )
            }
          }
        }

        # Configure axes
        xaxis_config <- list(
          domain = c(x_start, x_end),
          range = c(age_min, age_max),
          showticklabels = TRUE,
          title = if(row == n_rows) {
            list(text = "Vehicle Age (years)", font = list(size = 10))
          } else {
            ""
          }
        )

        yaxis_config <- list(
          domain = c(y_start, y_end),
          anchor = xaxis_name,
          showticklabels = TRUE,
          title = if(col == 1) {
            list(text = "Cumulative Miles", font = list(size = 10))
          } else {
            ""
          }
        )

        if(idx == 1) {
          xaxes[["xaxis"]] <- xaxis_config
          yaxes[["yaxis"]] <- yaxis_config
        } else {
          xaxes[[paste0("xaxis", idx)]] <- xaxis_config
          yaxes[[paste0("yaxis", idx)]] <- yaxis_config
        }

        # Add panel title (category name)
        annotations[[length(annotations) + 1]] <- list(
          x = x_start + cell_width / 2,
          y = y_end + 0.02,
          text = paste0("<b>", toupper(cat), "</b>"),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 11)
        )
      }

      # Apply layout
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
            title = list(text = paste0("<b>",
              if(category_var == "powertrain") "Vehicle Type" else "Powertrain",
              "</b>"))
          ),
          hovermode = "closest",
          margin = list(t = 80, b = 100, l = 70, r = 30)
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


  # Vehicle Retention Rates by Age - Category-based comparison
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

      # Get category selection (default to powertrain)
      selected_category <- if(!is.null(input$depreciation_category)) {
        input$depreciation_category
      } else {
        "powertrain"
      }

      # Load main retention data
      quantiles <- read_parquet(data_path_rr) %>%
        mutate(age_years = age_months / 12) %>%
        filter(between(age_years, 1, 8))

      # Apply user filters with mapping for BEV subtypes
      if(!is.null(input$fuel_types) && length(input$fuel_types) > 0) {
        # Map BEV subtypes to main BEV category for retention data
        selected_powertrains <- input$fuel_types
        # If user selected bev_tesla or bev_non_tesla, also include "bev"
        if(any(c("bev_tesla", "bev_non_tesla") %in% selected_powertrains)) {
          selected_powertrains <- c(selected_powertrains, "bev")
        }
        # Filter using only powertrains that exist in retention data
        available_powertrains <- c("bev", "cv", "hev", "phev")
        selected_powertrains <- intersect(selected_powertrains, available_powertrains)

        if(length(selected_powertrains) > 0) {
          quantiles <- quantiles %>% filter(powertrain %in% selected_powertrains)
        } else {
          # No valid powertrains selected, return empty data
          quantiles <- quantiles %>% filter(FALSE)
        }
      }
      if(!is.null(input$vehicle_types) && length(input$vehicle_types) > 0) {
        quantiles <- quantiles %>% filter(vehicle_type %in% input$vehicle_types)
      }

      # Aggregate based on selected category (keeping BOTH dimensions)
      if(selected_category == "powertrain") {
        # Group by powertrain AND vehicle_type
        quantiles_agg <- quantiles %>%
          group_by(powertrain, vehicle_type, age_years) %>%
          summarise(
            rr25 = mean(rr25, na.rm = TRUE),
            rr50 = mean(rr50, na.rm = TRUE),
            rr75 = mean(rr75, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!is.na(rr25), !is.na(rr50), !is.na(rr75))
        categories <- sort(unique(quantiles_agg$powertrain))
        category_var <- "powertrain"
        category_label <- "Powertrain"
        secondary_var <- "vehicle_type"
      } else {
        # Group by vehicle_type AND powertrain
        quantiles_agg <- quantiles %>%
          group_by(vehicle_type, powertrain, age_years) %>%
          summarise(
            rr25 = mean(rr25, na.rm = TRUE),
            rr50 = mean(rr50, na.rm = TRUE),
            rr75 = mean(rr75, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!is.na(rr25), !is.na(rr50), !is.na(rr75))
        categories <- sort(unique(quantiles_agg$vehicle_type))
        category_var <- "vehicle_type"
        category_label <- "Vehicle Type"
        secondary_var <- "powertrain"
      }

      if(nrow(quantiles_agg) == 0) {
        return(plot_ly() %>%
                 add_annotations(text = "No retention data available",
                               xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE))
      }

      # Get secondary categories (vehicle types or powertrains to show as lines)
      if(category_var == "powertrain") {
        secondary_cats <- sort(unique(quantiles_agg$vehicle_type))
      } else {
        secondary_cats <- sort(unique(quantiles_agg$powertrain))
      }

      # Create color palette for secondary categories (lines within each panel)
      colors <- viridisLite::viridis(length(secondary_cats))
      color_map <- setNames(colors, secondary_cats)

      # Create figure with subplots (one per category)
      fig <- plot_ly()

      # Calculate panel dimensions (arrange in 3 columns)
      n_cols <- min(3, length(categories))
      n_rows <- ceiling(length(categories) / n_cols)

      # Margins between subplots
      h_gap <- 0.03
      v_gap <- 0.10

      # Calculate cell dimensions
      panel_width <- (1 - (n_cols - 1) * h_gap) / n_cols
      panel_height <- (1 - (n_rows - 1) * v_gap) / n_rows

      xaxes <- list()
      yaxes <- list()
      annotations <- list()

      for(idx in seq_along(categories)) {
        cat <- categories[idx]

        # Get data for this category
        if(category_var == "powertrain") {
          cat_data <- quantiles_agg %>% filter(powertrain == cat)
        } else {
          cat_data <- quantiles_agg %>% filter(vehicle_type == cat)
        }

        # Calculate grid position
        row <- ceiling(idx / n_cols)
        col <- ((idx - 1) %% n_cols) + 1

        # Calculate domain positions
        x_start <- (col - 1) * (panel_width + h_gap)
        x_end <- x_start + panel_width
        y_start <- 1 - row * (panel_height + v_gap)
        y_end <- y_start + panel_height

        # Create unique axis names
        xaxis_name <- if(idx == 1) "x" else paste0("x", idx)
        yaxis_name <- if(idx == 1) "y" else paste0("y", idx)

        if(nrow(cat_data) > 0) {
          # Plot each secondary category as a separate line
          for(sec_cat in secondary_cats) {
            # Filter data for this secondary category
            if(category_var == "powertrain") {
              sec_data <- cat_data %>% filter(vehicle_type == sec_cat)
            } else {
              sec_data <- cat_data %>% filter(powertrain == sec_cat)
            }

            if(nrow(sec_data) > 0) {
              # Add ribbon (lower bound)
              fig <- fig %>%
                add_trace(
                  data = sec_data,
                  x = ~age_years,
                  y = ~rr25,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "transparent"),
                  showlegend = FALSE,
                  xaxis = xaxis_name,
                  yaxis = yaxis_name,
                  hoverinfo = "none"
                )

              # Add ribbon (upper bound)
              fig <- fig %>%
                add_trace(
                  data = sec_data,
                  x = ~age_years,
                  y = ~rr75,
                  type = "scatter",
                  mode = "lines",
                  fill = "tonexty",
                  fillcolor = paste0(substr(color_map[sec_cat], 1, 7), "40"),
                  line = list(color = "transparent"),
                  showlegend = FALSE,
                  xaxis = xaxis_name,
                  yaxis = yaxis_name,
                  hoverinfo = "none"
                )

              # Add median line
              fig <- fig %>%
                add_trace(
                  data = sec_data,
                  x = ~age_years,
                  y = ~rr50,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = color_map[sec_cat], width = 2.5),
                  name = toupper(sec_cat),
                  showlegend = (idx == 1),
                  legendgroup = sec_cat,
                  xaxis = xaxis_name,
                  yaxis = yaxis_name,
                  hovertemplate = paste0(
                    "<b>", toupper(cat), " - ", toupper(sec_cat), "</b><br>",
                    "Age: %{x:.1f} years<br>",
                    "Retention Rate: %{y:.1%}<br>",
                    "<extra></extra>"
                  )
                )
            }
          }
        }

        # Configure axes
        xaxis_config <- list(
          domain = c(x_start, x_end),
          range = c(1, 8),
          title = if(row == n_rows) {
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
          range = c(0, 1),
          tickformat = ".0%",
          title = if(col == 1) {
            list(text = "Retention rate", font = list(size = 10))
          } else {
            ""
          },
          showticklabels = TRUE
        )

        if(idx == 1) {
          xaxes[["xaxis"]] <- xaxis_config
          yaxes[["yaxis"]] <- yaxis_config
        } else {
          xaxes[[paste0("xaxis", idx)]] <- xaxis_config
          yaxes[[paste0("yaxis", idx)]] <- yaxis_config
        }

        # Add panel title
        annotations[[length(annotations) + 1]] <- list(
          x = x_start + panel_width / 2,
          y = y_end + 0.03,
          text = paste0("<b>", toupper(cat), "</b>"),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 11)
        )
      }

      # Apply layout
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
            title = list(text = paste0("<b>",
              if(category_var == "powertrain") "Vehicle Type" else "Powertrain",
              "</b>"))
          ),
          hovermode = "closest",
          margin = list(t = 80, b = 100, l = 70, r = 30)
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

  # Make & Model Analysis
  # Load make/model data
  make_model_mileage_data <- reactive({
    data_paths <- c(
      "data/mileage_make_model.csv",
      "../data/mileage_make_model.csv",
      file.path(dirname(getwd()), "data", "mileage_make_model.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (!is.null(data_path)) {
      read.csv(data_path)
    } else {
      NULL
    }
  })

  make_model_depreciation_data <- reactive({
    data_paths <- c(
      "data/depreciation_make_model.csv",
      "../data/depreciation_make_model.csv",
      file.path(dirname(getwd()), "data", "depreciation_make_model.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (!is.null(data_path)) {
      read.csv(data_path)
    } else {
      NULL
    }
  })

  # Update make selector based on selected metric
  observe({
    metric <- input$make_model_metric

    if (metric == "mileage") {
      data <- make_model_mileage_data()
    } else {
      data <- make_model_depreciation_data()
    }

    if (!is.null(data)) {
      makes <- sort(unique(data$make))
      updateSelectInput(session, "make_model_make",
                       choices = makes,
                       selected = makes[1])
    }
  })

  # Update model selector based on selected make
  observe({
    req(input$make_model_make)
    metric <- input$make_model_metric

    if (metric == "mileage") {
      data <- make_model_mileage_data()
    } else {
      data <- make_model_depreciation_data()
    }

    if (!is.null(data)) {
      models <- data %>%
        filter(make == input$make_model_make) %>%
        pull(model) %>%
        unique() %>%
        sort()

      updateSelectInput(session, "make_model_model",
                       choices = models,
                       selected = models[1])
    }
  })

  # Dynamic title
  output$make_model_title <- renderText({
    metric_name <- if (input$make_model_metric == "mileage") {
      "Cumulative Mileage"
    } else {
      "Depreciation Rate"
    }
    paste(metric_name, "by Make & Model")
  })

  # Make & Model Plot
  output$make_model_plot <- renderPlotly({
    req(input$make_model_make, input$make_model_model)

    metric <- input$make_model_metric

    if (metric == "mileage") {
      data <- make_model_mileage_data()
      y_col <- "mileage_predicted"
      y_label <- "Cumulative Mileage (miles)"
    } else {
      data <- make_model_depreciation_data()
      y_col <- "rr_predicted"
      y_label <- "Retention Rate"
    }

    if (is.null(data)) {
      return(plot_ly() %>%
               layout(title = "Data not available"))
    }

    # Filter data
    plot_data <- data %>%
      filter(make == input$make_model_make,
             model %in% input$make_model_model,
             age_years >= input$make_model_age_range[1],
             age_years <= input$make_model_age_range[2])

    if (nrow(plot_data) == 0) {
      return(plot_ly() %>%
               layout(title = "No data available for selected filters"))
    }

    # Create plot
    p <- plot_ly()

    models <- unique(plot_data$model)
    colors <- scales::viridis_pal()(length(models))

    for (i in seq_along(models)) {
      model_data <- plot_data %>% filter(model == models[i])

      p <- p %>%
        add_trace(
          data = model_data,
          x = ~age_years,
          y = as.formula(paste0("~", y_col)),
          type = 'scatter',
          mode = 'lines+markers',
          name = paste(model_data$make[1], model_data$model[1]),
          line = list(color = colors[i], width = 2.5),
          marker = list(color = colors[i], size = 6),
          hovertemplate = paste0(
            "<b>", paste(model_data$make[1], model_data$model[1]), "</b><br>",
            "Age: %{x:.1f} years<br>",
            if (metric == "mileage") "Mileage: %{y:,.0f}<br>" else "Retention: %{y:.2%}<br>",
            "<extra></extra>"
          )
        )
    }

    # Configure layout
    p <- p %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Vehicle Age (years)",
          gridcolor = '#e0e0e0',
          showgrid = TRUE
        ),
        yaxis = list(
          title = y_label,
          gridcolor = '#e0e0e0',
          showgrid = TRUE,
          tickformat = if (metric == "depreciation") ",.0%" else ","
        ),
        legend = list(
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 1,
          yanchor = "top"
        ),
        hovermode = 'closest',
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        margin = list(r = 150)
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c('lasso2d', 'select2d'),
        toImageButtonOptions = list(format = 'png', filename = 'make_model_plot')
      )

    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
