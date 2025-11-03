# Vehicle Trends Dashboard - Modern bslib Version
# Multi-page app with horizontal navigation bar

# Load required libraries
library(shiny)
library(bslib)
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
                 min = 1, max = 8, value = c(1, 8), step = 0.5)
    ),

    conditionalPanel(
      condition = "input.tabs == 'Market Concentration'",
      h6("Market Concentration Controls", style = "font-weight: 600; color: #34495e;"),

      radioButtons("hhi_metric",
                  "HHI Metric:",
                  choices = list("Brand (Make)" = "make",
                               "Vehicle Type" = "type",
                               "Price Bin" = "price"),
                  selected = "make")
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
      plotlyOutput("mileage_plot", height = "800px")
    ),
    card(
      card_header(
        "Mileage Summary Table",
        class = "text-center"
      ),
      DT::dataTableOutput("mileage_table")
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
      plotlyOutput("retention_plot", height = "850px")
    ),
    card(
      card_header(
        "Depreciation Summary Table",
        class = "text-center"
      ),
      DT::dataTableOutput("retention_table")
    )
  ),

  # Market Concentration Page
  nav_panel(
    title = "Market Concentration",
    value = "Market Concentration",
    icon = icon("chart-bar"),

    # HHI Explanation Card
    card(
      card_header("About the Herfindahl-Hirschman Index (HHI)", class = "text-center"),
      div(
        style = "padding: 10px 15px; font-size: 13px; line-height: 1.4;",
        div(
          style = "margin-bottom: 8px;",
          strong("What is HHI?"), " The Herfindahl-Hirschman Index (HHI) is a measure of market concentration. It ranges from 0 to 1, where:"
        ),
        tags$ul(
          style = "margin: 8px 0; padding-left: 25px;",
          tags$li(strong("0 (or close to 0):"), " Indicates perfect competition with many small players"),
          tags$li(strong("Closer to 1:"), " Indicates high concentration, dominated by few players")
        ),
        div(
          style = "margin-top: 8px;",
          "The box plots below show HHI distributions by powertrain type, comparing 2018 vs 2024 data. Use the metric controls in the sidebar to switch between Brand, Vehicle Type, and Price Bin views."
        )
      )
    ),

    # HHI Visualization
    card(
      full_screen = TRUE,
      card_header(textOutput("hhi_plot_title", inline = TRUE), class = "text-center"),
      plotlyOutput("hhi_plot", height = "600px")
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

  # Load mileage data (prediction-based)
  mileage_data <- reactive({
    # Try multiple potential data paths
    data_paths <- c(
      "data/mileage_powertrain_type.csv",
      "../data/mileage_powertrain_type.csv",
      file.path(dirname(getwd()), "data", "mileage_powertrain_type.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find mileage_powertrain_type.csv file")
    }

    mileage_pred <- read.csv(data_path)

    # Ensure proper column names and types
    data <- mileage_pred %>%
      filter(!is.na(powertrain), !is.na(vehicle_type))

    return(data)
  })
  
  # Load retention rate data (prediction-based)
  retention_data <- reactive({
    # Try multiple potential data paths
    data_paths <- c(
      "data/depreciation_powertrain_type.csv",
      "../data/depreciation_powertrain_type.csv",
      file.path(dirname(getwd()), "data", "depreciation_powertrain_type.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find depreciation_powertrain_type.csv file")
    }

    depreciation_pred <- read.csv(data_path)

    # Ensure proper column names and types
    data <- depreciation_pred %>%
      filter(!is.na(powertrain), !is.na(vehicle_type))

    return(data)
  })

  # Load HHI (Market Concentration) data
  hhi_make_data <- reactive({
    data_paths <- c(
      "data/hhi_make.csv",
      "../data/hhi_make.csv",
      file.path(dirname(getwd()), "data", "hhi_make.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find hhi_make.csv file")
    }

    read.csv(data_path)
  })

  hhi_type_data <- reactive({
    data_paths <- c(
      "data/hhi_type.csv",
      "../data/hhi_type.csv",
      file.path(dirname(getwd()), "data", "hhi_type.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find hhi_type.csv file")
    }

    read.csv(data_path)
  })

  hhi_price_data <- reactive({
    data_paths <- c(
      "data/hhi_price.csv",
      "../data/hhi_price.csv",
      file.path(dirname(getwd()), "data", "hhi_price.csv")
    )

    data_path <- NULL
    for (path in data_paths) {
      if (file.exists(path)) {
        data_path <- path
        break
      }
    }

    if (is.null(data_path)) {
      stop("Cannot find hhi_price.csv file")
    }

    read.csv(data_path)
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

  mileage_plot_context <- reactive({
    data <- filtered_mileage_data()

    if(is.null(data) || nrow(data) == 0) {
      return(list(status = "empty", message = "No data matches current filters"))
    }

    required_cols <- c("age_years", "mileage_predicted", "coef")
    missing_cols <- setdiff(required_cols, names(data))
    if(length(missing_cols) > 0) {
      return(list(status = "error", message = "Mileage data missing required columns"))
    }

    # Filter for age > 2 years
    data <- data %>%
      filter(age_years > 2)

    age_min <- if(!is.null(input$age_range)) max(2, input$age_range[1]) else 2
    age_max <- if(!is.null(input$age_range)) input$age_range[2] else 8
    show_bands <- FALSE  # No confidence bands in prediction data
    selected_category <- if(!is.null(input$comparison_category)) input$comparison_category else "powertrain"

    data <- data %>%
      filter(
        age_years >= age_min,
        age_years <= age_max,
        !is.na(mileage_predicted)
      )

    if(nrow(data) == 0) {
      return(list(status = "empty", message = "No data matches current filters"))
    }

    if(selected_category == "powertrain") {
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

    if(nrow(aggregated) == 0 || length(categories) == 0 || length(secondary) == 0) {
      return(list(status = "empty", message = "No data matches current filters"))
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
      show_bands = show_bands,
      selected_category = selected_category
    )
  })

  retention_plot_context <- reactive({
    data <- filtered_retention_data()

    if(is.null(data) || nrow(data) == 0) {
      return(list(status = "empty", message = "No data matches current filters"))
    }

    required_cols <- c("age_years", "rr_predicted")
    missing_cols <- setdiff(required_cols, names(data))
    if(length(missing_cols) > 0) {
      return(list(status = "error", message = "Retention data missing required columns"))
    }

    age_min <- if(!is.null(input$depreciation_age_range)) input$depreciation_age_range[1] else 1
    age_max <- if(!is.null(input$depreciation_age_range)) input$depreciation_age_range[2] else 8
    selected_category <- if(!is.null(input$depreciation_category)) input$depreciation_category else "powertrain"

    data <- data %>%
      filter(
        age_years >= age_min,
        age_years <= age_max,
        !is.na(rr_predicted)
      )

    if(nrow(data) == 0) {
      return(list(status = "empty", message = "No data matches current filters"))
    }

    if(selected_category == "powertrain") {
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

    if(nrow(aggregated) == 0 || length(categories) == 0 || length(secondary) == 0) {
      return(list(status = "empty", message = "No data matches current filters"))
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
  smooth_series <- function(df, y_col, age_seq, span = 0.65) {
    df <- df %>% arrange(age_years)
    if (length(unique(df$age_years)) < 3 || nrow(df) < 3) {
      approx_res <- approx(x = df$age_years, y = df[[y_col]], xout = age_seq, rule = 2, ties = mean)
      return(tibble::tibble(age_years = approx_res$x, value = approx_res$y))
    }
    fit <- tryCatch(
      loess(stats::as.formula(paste(y_col, "~ age_years")), data = df, span = span, degree = 2),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      approx_res <- approx(x = df$age_years, y = df[[y_col]], xout = age_seq, rule = 2, ties = mean)
      return(tibble::tibble(age_years = approx_res$x, value = approx_res$y))
    }
    preds <- predict(fit, newdata = data.frame(age_years = age_seq))
    if (all(is.na(preds))) {
      approx_res <- approx(x = df$age_years, y = df[[y_col]], xout = age_seq, rule = 2, ties = mean)
      preds <- approx_res$y
    }
    tibble::tibble(age_years = age_seq, value = preds)
  }

  compute_linear_slope <- function(df, y_col) {
    if (nrow(df) < 2 || length(unique(df$age_years)) < 2) {
      return(NA_real_)
    }
    fit <- tryCatch(
      lm(stats::as.formula(paste(y_col, "~ age_years")), data = df),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(NA_real_)
    }
    slope <- coef(fit)["age_years"]
    unname(ifelse(is.null(slope), NA_real_, slope))
  }

  approx_value <- function(df, y_col, x_value) {
    if (is.null(x_value) || is.na(x_value) || nrow(df) == 0) {
      return(NA_real_)
    }
    approx(x = df$age_years, y = df[[y_col]], xout = x_value, rule = 2, ties = mean)$y
  }

  build_mileage_summary <- function(context) {
    aggregated <- context$aggregated
    if (nrow(aggregated) == 0) {
      return(dplyr::tibble())
    }
    group_cols <- if (context$category_var == "powertrain") c("powertrain", "vehicle_type") else c("vehicle_type", "powertrain")

    aggregated %>%
      group_by(across(all_of(group_cols))) %>%
      arrange(age_years, .by_group = TRUE) %>%
      group_modify(~ {
        slope <- compute_linear_slope(.x, "mileage_predicted")
        start_val <- approx_value(.x, "mileage_predicted", context$age_min)
        end_val <- approx_value(.x, "mileage_predicted", context$age_max)
        coef_val <- mean(.x$coef, na.rm = TRUE)
        tibble::tibble(
          `Predicted Miles (Age Start)` = round(start_val, 0),
          `Predicted Miles (Age End)` = round(end_val, 0),
          `Annual Mileage Change` = round(slope, 1),
          `Coefficient (Annual Rate)` = round(coef_val, 1)
        )
      }) %>%
      ungroup() %>%
      distinct()
  }

  build_retention_summary <- function(context) {
    aggregated <- context$aggregated
    if (nrow(aggregated) == 0) {
      return(dplyr::tibble())
    }
    group_cols <- if (context$category_var == "powertrain") c("powertrain", "vehicle_type") else c("vehicle_type", "powertrain")

    aggregated %>%
      group_by(across(all_of(group_cols))) %>%
      arrange(age_years, .by_group = TRUE) %>%
      group_modify(~ {
        slope <- compute_linear_slope(.x, "rr_predicted")
        start_val <- approx_value(.x, "rr_predicted", context$age_min)
        end_val <- approx_value(.x, "rr_predicted", context$age_max)
        tibble::tibble(
          `Predicted Retention (Age Start)` = scales::percent(start_val, accuracy = 0.1),
          `Predicted Retention (Age End)` = scales::percent(end_val, accuracy = 0.1),
          `Annual Change (pp)` = round(slope * 100, 2)
        )
      }) %>%
      ungroup() %>%
      distinct()
  }

  output$mileage_plot <- renderPlotly({
    context <- mileage_plot_context()

    if (is.null(context) || context$status != "ok") {
      message <- if (!is.null(context$message)) context$message else "No mileage data available"
      return(
        plot_ly() %>%
          add_annotations(
            text = message,
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
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
    show_bands <- context$show_bands

    if (length(categories) == 0 || length(secondary_cats) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data matches current filters",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      )
    }

    colors <- viridisLite::viridis(length(secondary_cats))
    color_map <- setNames(colors, secondary_cats)
    secondary_label <- if (category_var == "powertrain") "Vehicle Type" else "Powertrain"

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

      if (nrow(cat_data) > 0) {
        available_secondary <- if (category_var == "powertrain") {
          sort(unique(cat_data$vehicle_type))
        } else {
          sort(unique(cat_data$powertrain))
        }

        for (sec_cat in available_secondary) {
          sec_data <- if (category_var == "powertrain") {
            cat_data %>% filter(vehicle_type == sec_cat)
          } else {
            cat_data %>% filter(powertrain == sec_cat)
          } %>% arrange(age_years)

          if (nrow(sec_data) == 0) {
            next
          }

          min_age <- max(age_min, min(sec_data$age_years))
          max_age <- min(age_max, max(sec_data$age_years))
          if (min_age >= max_age) {
            next
          }
          age_seq <- seq(min_age, max_age, length.out = max(80, length(sec_data$age_years) * 5))
          age_seq <- unique(age_seq)

          # Use prediction data (single line, no confidence bands)
          mileage_smooth <- smooth_series(sec_data, "mileage_predicted", age_seq) %>%
            rename(mileage_predicted = value)

          if (nrow(mileage_smooth) == 0) {
            next
          }

          fig <- fig %>%
            add_trace(
              data = mileage_smooth,
              x = ~age_years,
              y = ~mileage_predicted,
              type = "scatter",
              mode = "lines",
              line = list(color = color_map[sec_cat], width = 2.5, shape = "spline"),
              name = toupper(sec_cat),
              showlegend = (idx == 1),
              legendgroup = sec_cat,
              xaxis = xaxis_name,
              yaxis = yaxis_name,
              hovertemplate = paste0(
                "<b>", toupper(cat_value), " - ", toupper(sec_cat), "</b><br>",
                "Age: %{x:.1f} years<br>",
                "Mileage: %{y:,.0f} miles<br>",
                "<extra></extra>"
              )
            )
        }
      }

      xaxis_config <- list(
        domain = c(x_start, x_end),
        range = c(age_min, age_max),
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

  # Vehicle Retention Rates by Age - Category-based comparison
  output$retention_plot <- renderPlotly({
    context <- retention_plot_context()

    if (is.null(context) || context$status != "ok") {
      message <- if (!is.null(context$message)) context$message else "No retention data available"
      return(
        plot_ly() %>%
          add_annotations(
            text = message,
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
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
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      )
    }

    colors <- viridisLite::viridis(length(secondary_cats))
    color_map <- setNames(colors, secondary_cats)
    secondary_label <- if (category_var == "powertrain") "Vehicle Type" else "Powertrain"

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

      if (nrow(cat_data) > 0) {
        available_secondary <- if (category_var == "powertrain") {
          sort(unique(cat_data$vehicle_type))
        } else {
          sort(unique(cat_data$powertrain))
        }

        for (sec_cat in available_secondary) {
          sec_data <- if (category_var == "powertrain") {
            cat_data %>% filter(vehicle_type == sec_cat)
          } else {
            cat_data %>% filter(powertrain == sec_cat)
          } %>% arrange(age_years)

          if (nrow(sec_data) == 0) {
            next
          }

          min_age <- max(age_min, min(sec_data$age_years))
          max_age <- min(age_max, max(sec_data$age_years))
          if (min_age >= max_age) {
            next
          }
          age_seq <- seq(min_age, max_age, length.out = max(80, length(sec_data$age_years) * 5))
          age_seq <- unique(age_seq)

          # Use prediction data (single line, no confidence bands)
          rr_smooth <- smooth_series(sec_data, "rr_predicted", age_seq, span = 0.6) %>%
            rename(rr_predicted = value)

          if (nrow(rr_smooth) == 0) {
            next
          }

          fig <- fig %>%
            add_trace(
              data = rr_smooth,
              x = ~age_years,
              y = ~rr_predicted,
              type = "scatter",
              mode = "lines",
              line = list(color = color_map[sec_cat], width = 2.5, shape = "spline"),
              name = toupper(sec_cat),
              showlegend = (idx == 1),
              legendgroup = sec_cat,
              xaxis = xaxis_name,
              yaxis = yaxis_name,
              hovertemplate = paste0(
                "<b>", toupper(cat_value), " - ", toupper(sec_cat), "</b><br>",
                "Age: %{x:.1f} years<br>",
                "Retention Rate: %{y:.1%}<br>",
                "<extra></extra>"
              )
            )
        }
      }

      xaxis_config <- list(
        domain = c(x_start, x_end),
        range = c(age_min, age_max),
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
        range = c(0, 1),
        tickformat = ".0%",
        title = if (col == 1) {
          list(text = "Retention rate", font = list(size = 10))
        } else {
          ""
        },
        showticklabels = TRUE,
        fixedrange = FALSE,
        rangemode = "tozero",
        constrain = "domain",
        constraintoward = "bottom"
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

  output$mileage_table <- DT::renderDataTable({
    context <- mileage_plot_context()

    if (is.null(context) || context$status != "ok") {
      message <- if (!is.null(context$message)) context$message else "No mileage data available"
      table_data <- tibble::tibble(Message = message)
      return(DT::datatable(table_data, options = list(dom = "t"), rownames = FALSE))
    }

    table_data <- build_mileage_summary(context)

    if (nrow(table_data) == 0) {
      table_data <- tibble::tibble(Message = "No mileage data available")
      return(DT::datatable(table_data, options = list(dom = "t"), rownames = FALSE))
    }

    if (context$category_var == "powertrain") {
      table_data <- table_data %>%
        rename(
          Powertrain = powertrain,
          `Vehicle Type` = vehicle_type
        ) %>%
        select(Powertrain, `Vehicle Type`, everything())
    } else {
      table_data <- table_data %>%
        rename(
          `Vehicle Type` = vehicle_type,
          Powertrain = powertrain
        ) %>%
        select(`Vehicle Type`, Powertrain, everything())
    }

    DT::datatable(
      table_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$retention_table <- DT::renderDataTable({
    context <- retention_plot_context()

    if (is.null(context) || context$status != "ok") {
      message <- if (!is.null(context$message)) context$message else "No retention data available"
      table_data <- tibble::tibble(Message = message)
      return(DT::datatable(table_data, options = list(dom = "t"), rownames = FALSE))
    }

    table_data <- build_retention_summary(context)

    if (nrow(table_data) == 0) {
      table_data <- tibble::tibble(Message = "No retention data available")
      return(DT::datatable(table_data, options = list(dom = "t"), rownames = FALSE))
    }

    if (context$category_var == "powertrain") {
      table_data <- table_data %>%
        rename(
          Powertrain = powertrain,
          `Vehicle Type` = vehicle_type
        ) %>%
        select(Powertrain, `Vehicle Type`, everything())
    } else {
      table_data <- table_data %>%
        rename(
          `Vehicle Type` = vehicle_type,
          Powertrain = powertrain
        ) %>%
        select(`Vehicle Type`, Powertrain, everything())
    }

    DT::datatable(
      table_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
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

  # HHI Market Concentration Plot Title
  output$hhi_plot_title <- renderText({
    metric <- input$hhi_metric
    titles <- list(
      "make" = "Brand (Make) Concentration by Powertrain",
      "type" = "Vehicle Type Concentration by Powertrain",
      "price" = "Price Bin Concentration by Powertrain"
    )
    return(titles[[metric]])
  })

  # Unified HHI Plot
  output$hhi_plot <- renderPlotly({
    # Get selected metric
    metric <- input$hhi_metric

    # Load appropriate dataset
    hhi_data <- switch(metric,
      "make" = hhi_make_data(),
      "type" = hhi_type_data(),
      "price" = hhi_price_data(),
      hhi_make_data()
    )

    if (nrow(hhi_data) == 0) {
      return(plotly_empty())
    }

    # Convert powertrain to factor with clean labels and create display names
    hhi_data <- hhi_data %>%
      mutate(
        powertrain_lower = tolower(powertrain),
        powertrain_label = case_when(
          powertrain_lower == "bev" ~ "BEV",
          powertrain_lower == "cv" ~ "CV",
          powertrain_lower == "diesel" ~ "Diesel",
          powertrain_lower == "flex" ~ "Flex",
          powertrain_lower == "hev" ~ "HEV",
          powertrain_lower == "phev" ~ "PHEV",
          TRUE ~ toupper(powertrain)
        )
      )

    # Create box plot using quartile statistics - side by side comparison
    fig <- plot_ly()

    # Get unique powertrains in order
    powertrains_ordered <- unique(hhi_data$powertrain_label)

    for (pt in powertrains_ordered) {
      pt_data <- hhi_data %>% filter(powertrain_label == pt)

      for (yr in c("2018", "2024")) {
        yr_data <- pt_data %>% filter(year == yr)

        if (nrow(yr_data) > 0) {
          # Color scheme: Coral for 2018, Teal for 2024
          color <- if (yr == "2018") "rgba(255, 107, 107, 0.7)" else "rgba(78, 205, 196, 0.7)"
          line_color <- if (yr == "2018") "rgb(200, 50, 50)" else "rgb(40, 150, 140)"

          fig <- fig %>%
            add_trace(
              type = "box",
              x = rep(pt, nrow(yr_data)),
              q1 = list(yr_data$q25),
              median = list(yr_data$median),
              q3 = list(yr_data$q75),
              lowerfence = list(pmax(0, yr_data$lower)),
              upperfence = list(pmin(1, yr_data$upper)),
              name = yr,
              marker = list(
                color = color,
                line = list(color = line_color, width = 2)
              ),
              line = list(color = line_color, width = 2),
              legendgroup = yr,
              showlegend = (pt == powertrains_ordered[1]),
              boxmean = FALSE,
              hovertemplate = paste0(
                "<b>", pt, " (", yr, ")</b><br>",
                "Median HHI: ", round(yr_data$median, 3), "<br>",
                "Q1: ", round(yr_data$q25, 3), "<br>",
                "Q3: ", round(yr_data$q75, 3), "<br>",
                "IQR: ", round(yr_data$IQR, 3), "<br>",
                "<extra></extra>"
              )
            )
        }
      }
    }

    fig <- fig %>%
      layout(
        xaxis = list(
          title = "Powertrain Type",
          categoryorder = "array",
          categoryarray = powertrains_ordered
        ),
        yaxis = list(
          title = "HHI (0 = Perfect Competition, 1 = Monopoly)",
          range = c(0, 1),
          tickformat = ".2f",
          gridcolor = "rgba(200, 200, 200, 0.3)"
        ),
        boxmode = "group",
        boxgap = 0.2,
        boxgroupgap = 0.1,
        hovermode = "closest",
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.12,
          title = list(text = "<b>Year</b>", font = list(size = 14))
        ),
        margin = list(t = 50, b = 100, l = 90, r = 30),
        plot_bgcolor = "rgba(240, 240, 240, 0.5)",
        paper_bgcolor = "white"
      )

    return(fig)
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
