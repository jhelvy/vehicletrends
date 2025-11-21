# Nearest Vehicles Analysis Dashboard
# Displays vehicle similarity and clustering analysis

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)

# Define UI
ui <- page_sidebar(
  title = "Nearest Vehicles Analysis",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#ffffff",
    fg = "#2c3e50"
  ),
  fillable = TRUE,

  # Sidebar with filters
  sidebar = sidebar(
    title = "Search Parameters",
    width = 280,

    h5("Target Vehicle Selection:", style = "font-weight: bold; color: #2c3e50; margin-top: 10px;"),

    selectInput(
      "target_make",
      "Vehicle Make:",
      choices = c("Toyota", "Honda", "Ford", "Tesla", "Chevrolet", "BMW", "Hyundai", "Kia"),
      selected = "Toyota"
    ),

    selectInput(
      "target_type",
      "Vehicle Type:",
      choices = c("Car", "CUV", "Pickup", "SUV", "Minivan"),
      selected = "CUV"
    ),

    sliderInput(
      "price_range",
      "Target Price Range ($):",
      min = 10000,
      max = 100000,
      value = c(25000, 50000),
      step = 5000
    ),

    hr(),

    h5("Display Options:", style = "font-weight: bold; color: #2c3e50; margin-top: 15px;"),

    sliderInput(
      "num_similar",
      "Number of Similar Vehicles:",
      min = 3,
      max = 20,
      value = 10,
      step = 1
    ),

    radioButtons(
      "view_type",
      "View Type:",
      choices = list("Scatter Plot" = "scatter", "Table" = "table"),
      selected = "scatter"
    )
  ),

  # Main content area
  card(
    full_screen = TRUE,
    card_header("Vehicle Similarity Analysis", class = "bg-primary text-white"),
    uiOutput("content_ui")
  )
)

# Define server logic
server <- function(input, output, session) {

  # Generate sample similarity data
  similarity_data <- reactive({
    # This would load from GitHub in actual implementation
    # For now, create synthetic data for Shinylive compatibility
    set.seed(123)
    expand.grid(
      make = c("Toyota", "Honda", "Ford", "Tesla", "Chevrolet", "BMW", "Hyundai", "Kia"),
      type = c("Car", "CUV", "Pickup", "SUV")
    ) %>%
      mutate(
        similarity = runif(n(), 0.5, 1),
        price = runif(n(), 15000, 85000),
        mileage = runif(n(), 5000, 150000)
      ) %>%
      arrange(desc(similarity)) %>%
      head(input$num_similar)
  })

  # Render content based on view type
  output$content_ui <- renderUI({
    if (input$view_type == "scatter") {
      plotlyOutput("similarity_plot", height = "700px")
    } else {
      dataTableOutput("similar_vehicles_table")
    }
  })

  # Render scatter plot
  output$similarity_plot <- renderPlotly({
    tryCatch({
      data <- similarity_data()

      if (nrow(data) == 0) {
        return(plot_ly() %>%
          add_annotations(text = "No data available", xref = "paper", yref = "paper",
                         x = 0.5, y = 0.5, showarrow = FALSE))
      }

      p <- plot_ly(data = data, x = ~price, y = ~similarity,
                  color = ~type, size = ~similarity,
                  type = 'scatter', mode = 'markers',
                  marker = list(sizemode = 'diameter', sizeref = 1),
                  text = ~make,
                  hovertemplate = paste0(
                    "<b>%{text}</b><br>",
                    "Type: %{customdata}<br>",
                    "Price: $%{x:,.0f}<br>",
                    "Similarity Score: %{y:.2f}<extra></extra>"
                  ),
                  customdata = ~type) %>%
        layout(
          xaxis = list(title = "Vehicle Price ($)", tickformat = "$,.0f"),
          yaxis = list(title = "Similarity Score (0-1)"),
          hovermode = 'closest',
          plot_bgcolor = "#fafbfc",
          paper_bgcolor = "#ffffff",
          showlegend = TRUE
        )

      return(p)
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(text = paste("Error:", e$message), xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5, showarrow = FALSE, font = list(color = "red"))
    })
  })

  # Render data table
  output$similar_vehicles_table <- renderDataTable({
    data <- similarity_data() %>%
      mutate(
        similarity_pct = paste0(round(similarity * 100, 1), "%"),
        price_fmt = paste0("$", format(round(price), big.mark = ",")),
        mileage_fmt = format(round(mileage), big.mark = ",")
      ) %>%
      select(
        Make = make,
        Type = type,
        `Similarity` = similarity_pct,
        Price = price_fmt,
        Mileage = mileage_fmt
      )

    datatable(data, options = list(
      pageLength = 10,
      searching = TRUE,
      ordering = TRUE
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
