# Interactive HHI Explorer - Clean, Modern Educational Tool
# Understanding market concentration visually

library(shiny)
library(tidyverse)
library(scales)
library(bslib)

# Function to generate realistic market shares for a given HHI and number of companies
generate_market_shares <- function(target_hhi, n_companies) {
    
    if(n_companies <= 0) {
        return(list(shares = numeric(0), n_companies = 0, actual_hhi = 0, 
                    min_hhi = 0, max_hhi = 0, is_achievable = FALSE))
    }
    
    if(n_companies == 1) {
        return(list(shares = 1, n_companies = 1, actual_hhi = 1,
                    min_hhi = 1, max_hhi = 1, is_achievable = (target_hhi >= 0.99)))
    }
    
    min_hhi <- 1 / n_companies
    max_hhi <- 1.0
    is_achievable <- (target_hhi >= min_hhi - 0.001 && target_hhi <= max_hhi + 0.001)
    
    if(!is_achievable) {
        shares <- rep(1/n_companies, n_companies)
        return(list(shares = shares, n_companies = n_companies, actual_hhi = min_hhi,
                    min_hhi = min_hhi, max_hhi = max_hhi, is_achievable = FALSE))
    }
    
    if(target_hhi <= min_hhi + 0.001) {
        shares <- rep(1/n_companies, n_companies)
    } else if(target_hhi >= 0.999) {
        shares <- c(0.9999, rep(0.0001/(n_companies-1), n_companies-1))
    } else if(n_companies == 2) {
        discriminant <- 2 * target_hhi - 1
        if(discriminant >= 0) {
            s1 <- (1 + sqrt(discriminant)) / 2
            s2 <- 1 - s1
            shares <- c(s1, s2)
        } else {
            shares <- c(0.5, 0.5)
        }
    } else {
        hhi_normalized <- (target_hhi - min_hhi) / (max_hhi - min_hhi)
        exponent <- 0.2 + hhi_normalized * 4
        
        ranks <- 1:n_companies
        shares <- (1/ranks)^exponent
        shares <- shares / sum(shares)
        
        max_iterations <- 2000
        tolerance <- 0.00001
        learning_rate <- 0.1
        
        for(iteration in 1:max_iterations) {
            current_hhi <- sum(shares^2)
            error <- current_hhi - target_hhi
            
            if(abs(error) < tolerance) break
            
            if(error > 0) {
                transfer_amount <- min(shares[1] * learning_rate, abs(error) * 0.5)
                shares[1] <- shares[1] - transfer_amount
                shares[n_companies] <- shares[n_companies] + transfer_amount
            } else {
                transfer_amount <- min(shares[n_companies] * learning_rate, abs(error) * 0.5)
                if(shares[n_companies] > transfer_amount) {
                    shares[1] <- shares[1] + transfer_amount
                    shares[n_companies] <- shares[n_companies] - transfer_amount
                }
            }
            
            shares <- pmax(shares, 0.00001)
            shares <- shares / sum(shares)
            
            if(iteration %% 100 == 0) {
                learning_rate <- learning_rate * 0.9
            }
        }
    }
    
    actual_hhi <- sum(shares^2)
    
    return(list(shares = shares, n_companies = n_companies, actual_hhi = actual_hhi,
                min_hhi = min_hhi, max_hhi = max_hhi, is_achievable = TRUE))
}

# Define UI with modern theme
ui <- page_sidebar(
    title = "HHI Explorer: Understanding Market Concentration",
    theme = bs_theme(
        version = 5,
        preset = "bootstrap",
        bg = "#ffffff",
        fg = "#2c3e50",
        primary = "#3498db",
        secondary = "#7f8c8d",
        success = "#27ae60",
        warning = "#f39c12",
        danger = "#e74c3c",
        base_font = font_google("Inter"),
        "value-box-bg-gradient-blue-purple" = "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
        "value-box-bg-gradient-blue-cyan" = "linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)"
    ),
    
    sidebar = sidebar(
        width = 320,
        
        h4("Market Parameters", class = "mb-3"),
        p("Explore how market concentration changes with different HHI levels and company counts.", 
          class = "text-muted small"),
        
        sliderInput("n_companies",
                    "Number of Companies:",
                    min = 1,
                    max = 10,
                    value = 5,
                    step = 1,
                    width = "100%"),
        
        sliderInput("hhi_target",
                    "HHI Level:",
                    min = 0,
                    max = 1,
                    value = 0.30,
                    step = 0.01,
                    width = "100%"),
        
        hr(),
        
        h5("Quick Scenarios", class = "mb-2"),
        actionButton("scenario1", "Perfect Competition", 
                     class = "btn-success btn-sm w-100 mb-2"),
        actionButton("scenario2", "Moderate Market", 
                     class = "btn-warning btn-sm w-100 mb-2"),
        actionButton("scenario3", "Concentrated Market", 
                     class = "btn-danger btn-sm w-100 mb-2"),
        actionButton("scenario4", "Duopoly", 
                     class = "btn-dark btn-sm w-100 mb-2"),
        
        hr(),
        
        div(class = "alert alert-info small",
            icon("lightbulb"), " ", 
            strong("Tip:"), " Lower HHI = more competition. Higher HHI = more concentration."
        )
    ),
    
    # Main content area
    navset_card_tab(
        nav_panel(
            "Visualize",
            
            # Market status cards
            layout_column_wrap(
                width = 1/3,
                
                value_box(
                    title = "HHI Level",
                    value = htmlOutput("hhi_display"),
                    showcase = icon("chart-line"),
                    theme = "bg-gradient-blue-purple",
                    class = "text-center text-white"
                ),
                
                value_box(
                    title = "Companies",
                    value = textOutput("num_companies_display"),
                    showcase = icon("building"),
                    theme = "bg-gradient-blue-cyan",
                    class = "text-center text-white"
                ),
                
                value_box(
                    title = "Market Type",
                    value = textOutput("market_status_display"),
                    showcase = icon("scale-balanced"),
                    theme = "secondary",
                    class = "text-center text-white"
                )
            ),
            
            # Error message if applicable
            uiOutput("hhi_warning"),
            
            # Main visualization
            card(
                card_header("Market Share Distribution"),
                plotOutput("pieChart", height = "500px")
            )
        ),
        
        nav_panel(
            "Learn",
            
            card(
                card_header("What is HHI?"),
                markdown("
**The Herfindahl-Hirschman Index (HHI)** measures how concentrated a market is.

### How it Works
1. Calculate each company's market share (as a decimal, like 0.25 for 25%)
2. Square each market share
3. Add them all together

### The Scale (0 to 1)
- **0.00 to 0.15** = 🟢 Competitive Market (many companies, good for consumers)
- **0.15 to 0.25** = 🟡 Moderately Concentrated (a few larger players emerging)
- **0.25 to 1.00** = 🔴 Highly Concentrated (one or two companies dominate)

### Why It Matters
Government regulators use HHI to:
- Evaluate merger proposals
- Protect consumers from monopolies
- Ensure fair competition

**A market with HHI above 0.25 typically raises antitrust concerns.**
        ")
            ),
            
            card(
                card_header("Understanding the Math"),
                markdown("
### Why Square Market Shares?

Squaring gives **much more weight to larger companies**:

| Market Share | Contribution to HHI |
|--------------|---------------------|
| 10% (0.10)   | 0.01                |
| 20% (0.20)   | 0.04 (4× more)      |
| 40% (0.40)   | 0.16 (16× more!)    |

This makes HHI very sensitive to market dominance.

### Examples

**Equal Competition (10 companies @ 10% each):**
- HHI = 10 × (0.10)² = 0.10 ✓ Competitive!

**One Dominant Player (70%, plus smaller competitors):**
- HHI ≈ (0.70)² + others = 0.49+ ⚠️ Highly concentrated!
        ")
            )
        )
    )
)

# Server logic
server <- function(input, output, session) {
    
    # Reactive market data
    marketData <- reactive({
        result <- generate_market_shares(input$hhi_target, input$n_companies)
        
        if(length(result$shares) > 0 && result$n_companies > 0) {
            companies_df <- tibble(
                company = LETTERS[1:result$n_companies],
                market_share_fraction = result$shares,
                market_share_percent = result$shares * 100
            )
        } else {
            companies_df <- tibble()
        }
        
        list(
            target_hhi = input$hhi_target,
            actual_hhi = result$actual_hhi,
            n_companies = result$n_companies,
            min_hhi = result$min_hhi,
            max_hhi = result$max_hhi,
            is_achievable = result$is_achievable,
            data = companies_df
        )
    })
    
    # HHI Display with color
    output$hhi_display <- renderText({
        hhi <- input$hhi_target
        sprintf("%.2f", hhi)
    })
    
    # Number of companies
    output$num_companies_display <- renderText({
        as.character(input$n_companies)
    })
    
    # Market status
    output$market_status_display <- renderText({
        hhi <- input$hhi_target
        if(hhi < 0.15) {
            "Competitive"
        } else if(hhi < 0.25) {
            "Moderate"
        } else {
            "Concentrated"
        }
    })
    
    # Warning message
    output$hhi_warning <- renderUI({
        data <- marketData()
        
        if(!data$is_achievable) {
            div(
                class = "alert alert-danger",
                icon("exclamation-triangle"), " ",
                strong("Impossible Combination"), br(),
                sprintf("With %d companies, HHI must be between %.2f and %.2f. ", 
                        data$n_companies, data$min_hhi, data$max_hhi),
                sprintf("Minimum HHI (%.2f) = all companies equal.", data$min_hhi)
            )
        }
    })
    
    # Pie chart - modern and clean
    output$pieChart <- renderPlot({
        data <- marketData()
        
        if(nrow(data$data) == 0) {
            ggplot() +
                annotate("text", x = 0, y = 0, 
                         label = "Set parameters to view market distribution", 
                         size = 6, color = "#95a5a6") +
                theme_void()
        } else {
            # Modern color palette
            colors <- c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6", 
                        "#f39c12", "#1abc9c", "#34495e", "#e67e22",
                        "#95a5a6", "#16a085")
            
            data$data %>%
                arrange(desc(market_share_percent)) %>%
                mutate(
                    label = if_else(market_share_percent >= 4, 
                                    paste0(company, "\n", round(market_share_percent, 1), "%"),
                                    company),
                    company = factor(company, levels = company)
                ) %>%
                ggplot(aes(x = "", y = market_share_percent, fill = company)) +
                geom_col(width = 1, color = "white", linewidth = 2) +
                coord_polar("y", start = 0) +
                geom_text(aes(label = label), 
                          position = position_stack(vjust = 0.5),
                          size = 5, fontface = "bold", color = "white") +
                scale_fill_manual(values = colors[1:nrow(data$data)]) +
                theme_void(base_size = 14) +
                theme(
                    legend.position = "right",
                    legend.text = element_text(size = 13),
                    legend.title = element_text(size = 14, face = "bold"),
                    plot.background = element_rect(fill = "white", color = NA)
                ) +
                labs(fill = "Company")
        }
    }, bg = "white")
    
    # Scenario buttons
    observeEvent(input$scenario1, {
        updateSliderInput(session, "n_companies", value = 10)
        updateSliderInput(session, "hhi_target", value = 0.10)
    })
    
    observeEvent(input$scenario2, {
        updateSliderInput(session, "n_companies", value = 7)
        updateSliderInput(session, "hhi_target", value = 0.20)
    })
    
    observeEvent(input$scenario3, {
        updateSliderInput(session, "n_companies", value = 5)
        updateSliderInput(session, "hhi_target", value = 0.35)
    })
    
    observeEvent(input$scenario4, {
        updateSliderInput(session, "n_companies", value = 2)
        updateSliderInput(session, "hhi_target", value = 0.65)
    })
}

shinyApp(ui = ui, server = server)