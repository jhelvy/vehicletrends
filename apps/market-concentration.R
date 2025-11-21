# Market Concentration (HHI) Dashboard - Standalone
# Displays Herfindahl-Hirschman Index box plots for market concentration

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

# Load HHI data from jsDelivr CDN (CORS-enabled for Shinylive)
hhi_make_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/hhi_make.csv"
hhi_type_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/hhi_type.csv"
hhi_price_url <- "https://cdn.jsdelivr.net/gh/jhelvy/vehicletrends@main/data/hhi_price.csv"

hhi_make <- read.csv(hhi_make_url)
hhi_type <- read.csv(hhi_type_url)
hhi_price <- read.csv(hhi_price_url)

# Define UI
ui <- page_sidebar(
  title = "Market Concentration Analysis (HHI)",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#ffffff",
    fg = "#2c3e50"
  ),
  fillable = TRUE,

  sidebar = sidebar(
    title = "Market Concentration Controls",
    width = 250,

    radioButtons(
      "hhi_metric",
      "HHI Metric:",
      choices = list(
        "Brand (Make)" = "make",
        "Vehicle Type" = "type",
        "Price Bin" = "price"
      ),
      selected = "make"
    ),

    checkboxGroupInput(
      "market_concentration_hhi_powertrains",
      "Show Powertrains:",
      choices = list(
        "BEV" = "bev",
        "Conventional" = "cv",
        "Hybrid" = "hev",
        "PHEV" = "phev"
      ),
      selected = c("bev", "cv", "hev", "phev")
    )
  ),

  # Main content
  div(
    # HHI Explanation
    div(
      style = "padding: 15px 0; text-align: center;",
      h4("About the Herfindahl-Hirschman Index (HHI)"),
      p("We use the Herfindahl-Hirschman Index (HHI) as a measure of ",
        "market concentration. It ranges from 0 to 1. ",
        tags$a(href = "https://jhelvy.github.io/hhi/",
               target = "_blank",
               "Click HERE",
               style = "color: #3498db; text-decoration: underline;"),
        " to view an interactive app explaining HHI in more detail.")
    ),

    # HHI Visualization
    div(
      style = "padding: 15px 0;",
      h4(textOutput("hhi_plot_title", inline = TRUE),
         style = "text-align: center;"),
      plotlyOutput("hhi_plot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Get data based on metric selection
  hhi_data <- reactive({
    switch(input$hhi_metric,
      "make" = hhi_make,
      "type" = hhi_type,
      "price" = hhi_price,
      hhi_make
    )
  })

  # Dynamic title
  output$hhi_plot_title <- renderText({
    metric <- input$hhi_metric
    titles <- list(
      "make" = "Brand (Make) Concentration by Powertrain",
      "type" = "Vehicle Type Concentration by Powertrain",
      "price" = "Price Bin Concentration by Powertrain"
    )
    return(titles[[metric]])
  })

  # Unified HHI Plot (from app.R lines 3090-3211)
  output$hhi_plot <- renderPlotly({
    # Get selected metric
    metric <- input$hhi_metric
    hhi_data_raw <- hhi_data()

    if (nrow(hhi_data_raw) == 0) {
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

    # Convert powertrain to factor with clean labels and create display names
    hhi_data_filtered <- hhi_data_raw %>%
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
      ) %>%
      filter(powertrain_lower %in% input$market_concentration_hhi_powertrains)

    # Create box plot using quartile statistics - side by side comparison
    fig <- plot_ly()

    # Get unique powertrains in order
    powertrains_ordered <- unique(hhi_data_filtered$powertrain_label)

    # Define year colors only (2 colors total)
    year_colors <- list(
      "2018" = "rgba(200, 200, 200, 0.7)",  # Light gray for 2018
      "2024" = "rgba(44, 62, 80, 0.8)"      # Dark slate blue for 2024
    )

    for (pt in powertrains_ordered) {
      pt_data <- hhi_data_filtered %>% filter(powertrain_label == pt)

      for (yr in c("2018", "2024")) {
        yr_data <- pt_data %>% filter(year == yr)

        if (nrow(yr_data) > 0) {
          # Aggregate quartile statistics across all makes for this year/powertrain
          agg_q1 <- median(yr_data$q25, na.rm = TRUE)
          agg_median <- median(yr_data$median, na.rm = TRUE)
          agg_q3 <- median(yr_data$q75, na.rm = TRUE)
          agg_lower <- max(0, median(yr_data$lower, na.rm = TRUE))
          agg_upper <- min(1, median(yr_data$upper, na.rm = TRUE))

          # All lines are black, fill color varies only by year
          fig <- fig %>%
            add_trace(
              type = "box",
              y = pt,
              q1 = agg_q1,
              median = agg_median,
              q3 = agg_q3,
              lowerfence = agg_lower,
              upperfence = agg_upper,
              name = yr,
              fillcolor = year_colors[[yr]],
              marker = list(
                line = list(color = "rgb(0, 0, 0)", width = 2)
              ),
              line = list(color = "rgb(0, 0, 0)", width = 2),
              legendgroup = yr,
              showlegend = (pt == powertrains_ordered[1]),
              boxmean = FALSE,
              orientation = "h",
              hovertemplate = paste0(
                "<b>", pt, " (", yr, ")</b><br>",
                "Median HHI: ", round(agg_median, 3), "<br>",
                "Q1: ", round(agg_q1, 3), "<br>",
                "Q3: ", round(agg_q3, 3), "<br>",
                "<extra></extra>"
              )
            )
        }
      }
    }

    fig <- fig %>%
      layout(
        yaxis = list(
          title = "Powertrain Type",
          categoryorder = "array",
          categoryarray = powertrains_ordered
        ),
        xaxis = list(
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
}

# Run app
shinyApp(ui = ui, server = server)
