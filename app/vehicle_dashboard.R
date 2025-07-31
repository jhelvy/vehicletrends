# Vehicle Trends Dashboard
# Interactive Shiny dashboard for vehicle VMT analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(colourpicker)
library(arrow)
library(here)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Vehicle Trends Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("VMT Distribution", tabName = "vmt_dist", icon = icon("chart-line")),
      menuItem("Vehicle Comparison", tabName = "comparison", icon = icon("car")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Summary Stats", tabName = "summary", icon = icon("calculator"))
    ),
    
    # Sidebar controls
    hr(),
    h4("Filter Options", style = "margin-left: 15px;"),
    
    checkboxGroupInput("fuel_types", 
                      "Select Fuel Types:",
                      choices = list("BEV" = "bev",
                                   "BEV Non-Tesla" = "bev_non_tesla", 
                                   "BEV Tesla" = "bev_tesla",
                                   "Conventional" = "cv",
                                   "Diesel" = "diesel",
                                   "Flex Fuel" = "flex",
                                   "Hybrid" = "hev",
                                   "PHEV" = "phev"),
                      selected = c("bev", "cv", "hev")),
    
    checkboxGroupInput("vehicle_types",
                      "Select Vehicle Types:",
                      choices = list("Car" = "car",
                                   "CUV" = "cuv", 
                                   "Minivan" = "minivan",
                                   "Pickup" = "pickup",
                                   "SUV" = "suv"),
                      selected = c("car", "cuv", "minivan", "pickup", "suv")),
    
    sliderInput("dvmt_range",
               "DVMT Range:",
               min = 0, max = 100, value = c(0, 100))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # VMT Distribution Tab
      tabItem(tabName = "vmt_dist",
        fluidRow(
          box(
            title = "CDF of Daily VMT by Fuel Type", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            plotlyOutput("cdf_plot", height = "550px")
          )
        ),
        
        fluidRow(
          box(
            title = "Distribution Controls",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            radioButtons("plot_type", "Plot Type:",
                        choices = list("CDF" = "cdf", 
                                     "PDF" = "pdf",
                                     "Histogram" = "hist"),
                        selected = "cdf"),
            
            checkboxInput("show_aggregated", "Show Aggregated (Black Line)", TRUE)
          ),
          
          box(
            title = "Plot Settings",
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            colourpicker::colourInput("theme_color", "Accent Color:", "#3498db"),
            selectInput("theme_style", "Plot Theme:",
                       choices = list("Light" = "light",
                                    "Dark" = "dark", 
                                    "Minimal" = "minimal"),
                       selected = "light")
          )
        )
      ),
      
      # Vehicle Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Vehicle Type Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            plotlyOutput("comparison_plot", height = "550px")
          )
        ),
        
        fluidRow(
          box(
            title = "Comparison Metrics",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("metrics_table")
          )
        )
      ),
      
      # Data Table Tab
      tabItem(tabName = "data_table",
        fluidRow(
          box(
            title = "Raw Data Explorer",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("raw_data_table")
          )
        )
      ),
      
      # Summary Stats Tab  
      tabItem(tabName = "summary",
        fluidRow(
          valueBoxOutput("total_vehicles"),
          valueBoxOutput("avg_dvmt"),
          valueBoxOutput("fuel_types_count")
        ),
        
        fluidRow(
          box(
            title = "Summary Statistics by Fuel Type",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tableOutput("fuel_summary")
          ),
          
          box(
            title = "Summary Statistics by Vehicle Type", 
            status = "success",
            solidHeader = TRUE,
            width = 6,
            tableOutput("vehicle_summary")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load real vehicletrends data
  sample_data <- reactive({
    # Set the path to the vehicletrends data
     data_path <- file.path(dirname(getwd()), "data", "quantiles_dvmt.parquet")
    
    # If the relative path doesn't work, try absolute path
    if (!file.exists(data_path)) {
      data_path <- "../data/quantiles_dvmt.parquet"
    }
    
    # Load the parquet file
    quantiles_dvmt <- read_parquet(data_path)
    
    # Keep original column names to match professor's code
    data <- quantiles_dvmt %>%
      filter(powertrain != 'all', vehicle_type != 'all')
    
    return(data)
  })
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    data <- sample_data()
    
    data <- data %>%
      filter(powertrain %in% input$fuel_types,
             vehicle_type %in% input$vehicle_types,
             dvmt >= input$dvmt_range[1],
             dvmt <= input$dvmt_range[2])
    
    return(data)
  })
  
  # Main Plot (CDF/PDF/Histogram)
  output$cdf_plot <- renderPlotly({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Create different plot types based on selection
    if(input$plot_type == "cdf") {
      # Original CDF plot
      p <- ggplot(data, aes(x = dvmt, y = quantile)) +
        geom_line(aes(color = vehicle_type), linewidth = 1, alpha = 0.8) +
        facet_wrap(vars(powertrain)) +
        labs(
          title = "CDF of daily VMT",
          subtitle = "Black line is all vehicles aggregated, which is dominated by conventional vehicles",
          x = "DVMT",
          y = "%",
          color = "vehicle_type"
        ) +
        theme_bw() +
        theme(legend.position = 'right')
      
      # Add aggregated line if selected
      if(input$show_aggregated) {
        data_path <- "../data/quantiles_dvmt.parquet"
        agg_data <- read_parquet(data_path) %>%
          filter(powertrain == 'all', vehicle_type == 'all') %>%
          select(quantile, dvmt)
        
        p <- p + geom_line(data = agg_data, aes(x = dvmt, y = quantile), 
                          color = "black", linewidth = 1.2, inherit.aes = FALSE)
      }
      
    } else if(input$plot_type == "pdf") {
      # PDF plot (derivative of CDF)
      pdf_data <- data %>%
        arrange(powertrain, vehicle_type, quantile) %>%
        group_by(powertrain, vehicle_type) %>%
        mutate(
          pdf_value = c(diff(quantile), 0) / c(diff(dvmt), 1)
        ) %>%
        filter(pdf_value > 0, pdf_value < Inf)
      
      p <- ggplot(pdf_data, aes(x = dvmt, y = pdf_value)) +
        geom_line(aes(color = vehicle_type), linewidth = 1, alpha = 0.8) +
        facet_wrap(vars(powertrain)) +
        labs(
          title = "PDF of daily VMT",
          subtitle = "Probability density function showing usage distribution",
          x = "DVMT",
          y = "Density",
          color = "vehicle_type"
        ) +
        theme_bw() +
        theme(legend.position = 'right')
      
    } else if(input$plot_type == "hist") {
      # Histogram approximation
      # Convert quantiles back to approximate raw data for histogram
      hist_data <- data %>%
        group_by(powertrain, vehicle_type) %>%
        # Create approximate data points based on quantiles
        do({
          dvmt_vals <- .$dvmt
          quantile_vals <- .$quantile
          # Generate points proportional to density
          n_points <- 1000
          sample_data <- approx(quantile_vals/100, dvmt_vals, 
                               xout = seq(0.01, 0.99, length.out = n_points))$y
          data.frame(dvmt_sample = sample_data[!is.na(sample_data)])
        })
      
      p <- ggplot(hist_data, aes(x = dvmt_sample)) +
        geom_histogram(aes(fill = vehicle_type), alpha = 0.7, bins = 30, position = "identity") +
        facet_wrap(vars(powertrain)) +
        labs(
          title = "Histogram of daily VMT",
          subtitle = "Distribution of daily vehicle miles traveled",
          x = "DVMT",
          y = "Count",
          fill = "vehicle_type"
        ) +
        theme_bw() +
        theme(legend.position = 'right')
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Comparison Plot
  output$comparison_plot <- renderPlotly({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(plotly_empty())
    }
    
    comparison_data <- data %>%
      group_by(powertrain, vehicle_type) %>%
      summarise(
        mean_dvmt = mean(dvmt[quantile <= 50]),
        max_cdf = max(quantile),
        .groups = "drop"
      )
    
    p <- ggplot(comparison_data, aes(x = powertrain, y = mean_dvmt, fill = vehicle_type)) +
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
  
  # Value Boxes
  output$total_vehicles <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Data Points",
      icon = icon("car"),
      color = "blue"
    )
  })
  
  output$avg_dvmt <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$dvmt), 1),
      subtitle = "Average DVMT",
      icon = icon("road"),
      color = "green"
    )
  })
  
  output$fuel_types_count <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$powertrain)),
      subtitle = "Fuel Types",
      icon = icon("gas-pump"),
      color = "yellow"
    )
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