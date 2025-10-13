# Interactive HHI Calculator - Using Decimal Scale (0 to 1)
# Market shares expressed as fractions, not percentages

library(shiny)
library(tidyverse)
library(scales)

# Define UI
ui <- fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    
    titlePanel("Interactive HHI Calculator - Understanding Market Concentration"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Set Production Volumes"),
            p("Adjust the sliders to see how market share affects competition"),
            
            sliderInput("companyA",
                        "Company A (Cars Produced):",
                        min = 0,
                        max = 10000,
                        value = 5000,
                        step = 100),
            
            sliderInput("companyB",
                        "Company B (Cars Produced):",
                        min = 0,
                        max = 10000,
                        value = 3000,
                        step = 100),
            
            sliderInput("companyC",
                        "Company C (Cars Produced):",
                        min = 0,
                        max = 10000,
                        value = 2000,
                        step = 100),
            
            hr(),
            actionButton("reset", "Reset to Default", class = "btn-secondary"),
            hr(),
            
            h4("Quick Scenarios:"),
            actionButton("scenario1", "Equal Competition", class = "btn-success btn-sm"),
            actionButton("scenario2", "Moderate Concentration", class = "btn-warning btn-sm"),
            actionButton("scenario3", "High Concentration", class = "btn-danger btn-sm"),
            actionButton("scenario4", "Near Monopoly", class = "btn-dark btn-sm")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Dashboard",
                         h3("Market Overview"),
                         fluidRow(
                             column(4, wellPanel(
                                 h4("Total Market Size"),
                                 textOutput("totalCars"),
                                 tags$style(type="text/css", "#totalCars {font-size: 24px; font-weight: bold; color: #2c3e50;}")
                             )),
                             column(4, wellPanel(
                                 h4("HHI Score (Decimal)"),
                                 textOutput("hhiScore"),
                                 tags$style(type="text/css", "#hhiScore {font-size: 24px; font-weight: bold; color: #e74c3c;}")
                             )),
                             column(4, wellPanel(
                                 h4("Market Status"),
                                 textOutput("marketStatus"),
                                 tags$style(type="text/css", "#marketStatus {font-size: 18px; font-weight: bold;}")
                             ))
                         ),
                         
                         h3("Market Share Distribution"),
                         plotOutput("pieChart", height = "300px"),
                         
                         h3("Company Details"),
                         tableOutput("companyTable"),
                         
                         h3("HHI Visualization"),
                         plotOutput("hhiBar", height = "250px")
                ),
                
                tabPanel("What is HHI?",
                         h3("Understanding the Herfindahl-Hirschman Index (HHI)"),
                         
                         h4("What does HHI measure?"),
                         p("HHI measures market concentration - how much power a few companies have in an industry. 
                   It helps us understand if a market is competitive (many companies) or monopolistic (one dominant company)."),
                         
                         h4("How is it calculated?"),
                         p("1. Calculate each company's market share as a fraction (their production ÷ total market production)"),
                         p("2. Square each market share fraction"),
                         p("3. Add all the squared values together"),
                         
                         div(style = "background-color: #ecf0f1; padding: 15px; margin: 10px 0; border-radius: 5px;",
                             strong("Formula:"), br(),
                             "HHI = (Market Share A)² + (Market Share B)² + (Market Share C)²", br(), br(),
                             strong("Note:"), " Market shares are expressed as decimals (0.25 for 25%, not 25)"
                         ),
                         
                         div(style = "background-color: #d1ecf1; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 5px solid #0c5460;",
                             strong("🔢 Two Ways to Express HHI:"), br(),
                             "• ", strong("Decimal Scale (0 to 1):"), " Market shares as fractions → HHI ranges 0 to 1", br(),
                             "• ", strong("Points Scale (0 to 10,000):"), " Market shares as percentages → HHI ranges 0 to 10,000", br(), br(),
                             "This app uses the ", strong("decimal scale."), " An HHI of 0.25 = 2,500 points."
                         ),
                         
                         h4("What do the scores mean?"),
                         tags$ul(
                             tags$li(strong("Below 0.15:"), " Competitive Market - Many companies competing, good for consumers!"),
                             tags$li(strong("0.15 - 0.25:"), " Moderately Concentrated - A few companies are getting stronger"),
                             tags$li(strong("Above 0.25:"), " Highly Concentrated - One or two companies dominate, potential monopoly concerns")
                         ),
                         
                         h4("Why does this matter?"),
                         p("Government regulators (like the Department of Justice) use HHI to:"),
                         tags$ul(
                             tags$li("Decide if company mergers should be allowed"),
                             tags$li("Ensure markets stay competitive"),
                             tags$li("Protect consumers from monopolies and high prices")
                         ),
                         
                         div(style = "background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px;",
                             strong("💡 Try this:"), " Use the sliders to make Company A much larger than the others. 
                     Watch how the HHI score increases dramatically! This shows why big mergers raise concerns."
                         )
                ),
                
                tabPanel("Interpretation Guide",
                         h3("How to Read Your Results"),
                         
                         h4("The HHI Score Color Guide (Decimal Scale):"),
                         div(style = "background-color: #d4edda; padding: 10px; margin: 5px 0; border-left: 5px solid #28a745;",
                             strong("GREEN (HHI < 0.15):"), " Healthy Competition", br(),
                             "The market is competitive with no dominant players. Good for innovation and consumer prices!", br(),
                             em("Equivalent to < 1,500 points")
                         ),
                         
                         div(style = "background-color: #fff3cd; padding: 10px; margin: 5px 0; border-left: 5px solid #ffc107;",
                             strong("YELLOW (HHI 0.15-0.25):"), " Moderate Concentration", br(),
                             "Some companies are gaining power. Regulators would review major mergers carefully.", br(),
                             em("Equivalent to 1,500-2,500 points")
                         ),
                         
                         div(style = "background-color: #f8d7da; padding: 10px; margin: 5px 0; border-left: 5px solid #dc3545;",
                             strong("RED (HHI > 0.25):"), " High Concentration", br(),
                             "Market is dominated by one or few companies. Mergers would likely be blocked!", br(),
                             em("Equivalent to > 2,500 points")
                         ),
                         
                         hr(),
                         
                         h4("Understanding Why We Square Market Shares:"),
                         p("The HHI squares market shares, which means bigger companies have disproportionate influence:"),
                         
                         div(style = "background-color: #e7f3ff; padding: 15px; margin: 10px 0; border-radius: 5px;",
                             strong("Example with 3 companies:"), br(), br(),
                             "• Company with 10% share (0.10): contributes 0.01 to HHI (0.10² = 0.01)", br(),
                             "• Company with 20% share (0.20): contributes 0.04 to HHI (0.20² = 0.04) - 4x more!", br(),
                             "• Company with 40% share (0.40): contributes 0.16 to HHI (0.40² = 0.16) - 16x more!", br(), br(),
                             "This means the HHI heavily weights dominant players, making it very sensitive to market concentration."
                         ),
                         
                         hr(),
                         
                         h4("Real-World Examples:"),
                         p(strong("Competitive Market (Low HHI):"), 
                           " Imagine 10 equally-sized companies each with 10% market share (0.10 each). 
                   HHI = 10 × (0.10)² = 10 × 0.01 = 0.10. This is a healthy, competitive market."),
                         
                         p(strong("Concentrated Market (High HHI):"), 
                           " If one company has 70% share (0.70) and two others have 15% each (0.15), 
                   HHI = (0.70)² + (0.15)² + (0.15)² = 0.49 + 0.0225 + 0.0225 = 0.535. This is highly concentrated!"),
                         
                         hr(),
                         
                         h4("Converting Between Scales:"),
                         p("To convert between decimal and points scale:"),
                         tags$ul(
                             tags$li("Decimal → Points: Multiply by 10,000 (e.g., 0.25 × 10,000 = 2,500 points)"),
                             tags$li("Points → Decimal: Divide by 10,000 (e.g., 2,500 ÷ 10,000 = 0.25)")
                         )
                )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values for calculations using tidyverse
    marketData <- reactive({
        # Create data frame
        companies_df <- tibble(
            company = c("Company A", "Company B", "Company C"),
            production = c(input$companyA, input$companyB, input$companyC)
        )
        
        total <- sum(companies_df$production)
        
        if(total == 0) {
            return(list(
                total = 0,
                hhi = 0,
                data = companies_df %>%
                    mutate(
                        market_share_fraction = 0,
                        market_share_percent = 0,
                        hhi_contribution = 0
                    )
            ))
        }
        
        # Calculate market shares and HHI using tidyverse
        companies_df <- companies_df %>%
            mutate(
                market_share_fraction = production / total,
                market_share_percent = market_share_fraction * 100,
                hhi_contribution = market_share_fraction^2
            )
        
        hhi <- sum(companies_df$hhi_contribution)
        
        list(
            total = total,
            hhi = hhi,
            data = companies_df
        )
    })
    
    # Output: Total cars
    output$totalCars <- renderText({
        data <- marketData()
        format(data$total, big.mark = ",", scientific = FALSE)
    })
    
    # Output: HHI score
    output$hhiScore <- renderText({
        data <- marketData()
        sprintf("%.4f", data$hhi)
    })
    
    # Output: Market status with color
    output$marketStatus <- renderText({
        data <- marketData()
        hhi <- data$hhi
        
        if(hhi < 0.15) {
            "✓ Competitive"
        } else if(hhi < 0.25) {
            "⚠ Moderate"
        } else {
            "⚠ Highly Concentrated"
        }
    })
    
    # Output: Pie chart using ggplot2
    output$pieChart <- renderPlot({
        data <- marketData()
        
        if(data$total == 0) {
            ggplot() +
                annotate("text", x = 0, y = 0, label = "No production data", size = 6, color = "gray50") +
                theme_void()
        } else {
            data$data %>%
                mutate(label = paste0(company, "\n", round(market_share_percent, 1), "%")) %>%
                ggplot(aes(x = "", y = market_share_percent, fill = company)) +
                geom_col(width = 1, color = "white", size = 2) +
                coord_polar("y", start = 0) +
                geom_text(aes(label = label), 
                          position = position_stack(vjust = 0.5),
                          size = 5, fontface = "bold", color = "white") +
                scale_fill_manual(values = c("#3498db", "#e74c3c", "#2ecc71")) +
                theme_void() +
                theme(
                    legend.position = "bottom",
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 14, face = "bold")
                ) +
                labs(fill = "Company")
        }
    })
    
    # Output: Company table
    output$companyTable <- renderTable({
        data <- marketData()
        
        data$data %>%
            mutate(
                Production = format(production, big.mark = ","),
                `Market Share (%)` = sprintf("%.2f%%", market_share_percent),
                `Market Share (Fraction)` = sprintf("%.4f", market_share_fraction),
                `HHI Contribution` = sprintf("%.4f", hhi_contribution)
            ) %>%
            select(Company = company, Production, `Market Share (%)`, 
                   `Market Share (Fraction)`, `HHI Contribution`)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Output: HHI bar chart
    output$hhiBar <- renderPlot({
        data <- marketData()
        hhi <- data$hhi
        
        # Determine color based on thresholds
        color <- case_when(
            hhi < 0.15 ~ "#28a745",
            hhi < 0.25 ~ "#ffc107",
            TRUE ~ "#dc3545"
        )
        
        status <- case_when(
            hhi < 0.15 ~ "Competitive",
            hhi < 0.25 ~ "Moderately Concentrated",
            TRUE ~ "Highly Concentrated"
        )
        
        # Create visualization
        tibble(category = "Current HHI", value = hhi) %>%
            ggplot(aes(x = category, y = value)) +
            geom_col(fill = color, width = 0.5) +
            geom_hline(yintercept = 0.15, linetype = "dashed", color = "#28a745", size = 1) +
            geom_hline(yintercept = 0.25, linetype = "dashed", color = "#dc3545", size = 1) +
            annotate("text", x = 1.35, y = 0.15, 
                     label = "Competitive Threshold\n(0.15 = 1,500 pts)", 
                     size = 3.5, color = "#28a745", fontface = "bold") +
            annotate("text", x = 1.35, y = 0.25, 
                     label = "High Concentration\n(0.25 = 2,500 pts)", 
                     size = 3.5, color = "#dc3545", fontface = "bold") +
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                               labels = label_number(accuracy = 0.1)) +
            labs(
                title = sprintf("HHI Score: %.4f (%s)", hhi, status),
                subtitle = sprintf("Equivalent to %s points on 0-10,000 scale", 
                                   format(round(hhi * 10000), big.mark = ",")),
                y = "HHI Value (Decimal Scale)", 
                x = ""
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 11),
                axis.title.y = element_text(size = 13, face = "bold"),
                panel.grid.major.x = element_blank()
            )
    })
    
    # Reset button
    observeEvent(input$reset, {
        updateSliderInput(session, "companyA", value = 5000)
        updateSliderInput(session, "companyB", value = 3000)
        updateSliderInput(session, "companyC", value = 2000)
    })
    
    # Scenario buttons
    observeEvent(input$scenario1, {
        # Equal competition: HHI = 3 × (1/3)² = 0.333
        updateSliderInput(session, "companyA", value = 3333)
        updateSliderInput(session, "companyB", value = 3333)
        updateSliderInput(session, "companyC", value = 3334)
    })
    
    observeEvent(input$scenario2, {
        # Moderate concentration: HHI ≈ 0.38
        updateSliderInput(session, "companyA", value = 5000)
        updateSliderInput(session, "companyB", value = 3000)
        updateSliderInput(session, "companyC", value = 2000)
    })
    
    observeEvent(input$scenario3, {
        # High concentration: HHI ≈ 0.54
        updateSliderInput(session, "companyA", value = 7000)
        updateSliderInput(session, "companyB", value = 2000)
        updateSliderInput(session, "companyC", value = 1000)
    })
    
    observeEvent(input$scenario4, {
        # Near monopoly: HHI ≈ 0.82
        updateSliderInput(session, "companyA", value = 9000)
        updateSliderInput(session, "companyB", value = 700)
        updateSliderInput(session, "companyC", value = 300)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
