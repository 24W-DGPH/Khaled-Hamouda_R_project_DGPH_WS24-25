library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(DT)
library(scales)  
library(dplyr)   

# Save your processed data as RDS files
saveRDS(life_exp_longer, "life_exp_data.rds")
saveRDS(health_xp_longer, "health_xp_data.rds")

# Then in your app, load the data
life_exp_longer <- readRDS("life_exp_data.rds")
health_xp_longer <- readRDS("health_xp_data.rds")
# Create combined data
combined_data <- left_join(
  life_exp_longer %>% rename(life_expectancy = value),
  health_xp_longer %>% rename(health_expenditure = value),
  by = c("country", "year")
)

# Let's verify the cleaning worked
print("Unique countries in the dataset:")
print(unique(combined_data$country)[1:10])  # Print first 10 countries as example

# Now you can use this cleaned combined_data in your Shiny dashboard

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Health Comparisons"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Country vs Global", tabName = "comparison", icon = icon("globe")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    ),
    # Add country selector
    selectizeInput("country_select", "Select Countries to Highlight:",
                   choices = unique(combined_data$country),
                   multiple = TRUE,
                   selected = c("United States", "Japan", "Germany"))
  ),
  
  dashboardBody(
    tabItems(
      # Comparison tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(plotlyOutput("scatter_plot"), width = 12,
                    title = "Country Averages vs Global Trend")
              ),
              fluidRow(
                box(plotlyOutput("deviation_plot"), width = 6,
                    title = "Deviation from Global Average"),
                box(plotlyOutput("time_trend"), width = 6,
                    title = "Trends Over Time")
              )
      ),
      
      # Data table tab
      tabItem(tabName = "data",
              fluidRow(
                box(DT::dataTableOutput("comparison_table"), width = 12,
                    title = "Country vs Global Average Statistics")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Calculate global averages
  global_averages <- reactive({
    combined_data %>%
      group_by(year) %>%
      summarise(
        avg_expenditure = mean(health_expenditure, na.rm = TRUE),
        avg_life_exp = mean(life_expectancy, na.rm = TRUE)
      )
  })
  
  # Calculate country averages
  country_averages <- reactive({
    combined_data %>%
      group_by(country) %>%
      summarise(
        avg_expenditure = mean(health_expenditure, na.rm = TRUE),
        avg_life_exp = mean(life_expectancy, na.rm = TRUE)
      )
  })
  
  # Main scatter plot
  output$scatter_plot <- renderPlotly({
    # Create base plot
    p <- ggplot() +
      # Global average trend
      geom_smooth(data = global_averages(), 
                  aes(x = avg_expenditure, y = avg_life_exp),
                  color = "grey80", size = 2, se = FALSE) +
      # All country points
      geom_point(data = country_averages(), 
                 aes(x = avg_expenditure, y = avg_life_exp,
                     text = country,
                     color = country %in% input$country_select)) +
      scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
      labs(x = "Healthcare Expenditure (USD)",
           y = "Life Expectancy (years)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Deviation plot
  output$deviation_plot <- renderPlotly({
    # Calculate deviations
    global_means <- global_averages() %>%
      summarise(
        global_exp = mean(avg_expenditure),
        global_life = mean(avg_life_exp)
      )
    
    deviations <- country_averages() %>%
      mutate(
        exp_diff = avg_expenditure - global_means$global_exp,
        life_diff = avg_life_exp - global_means$global_life,
        highlighted = country %in% input$country_select
      )
    
    p <- ggplot(deviations, aes(x = exp_diff, y = life_diff, 
                                text = country,
                                color = highlighted)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      geom_point() +
      scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
      labs(x = "Difference from Global Average Expenditure (USD)",
           y = "Difference from Global Average Life Expectancy (years)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Time trend plot
  output$time_trend <- renderPlotly({
    # Filter for selected countries
    highlighted_data <- combined_data %>%
      filter(country %in% input$country_select)
    
    p <- ggplot() +
      # Global average line
      geom_line(data = global_averages(),
                aes(x = year, y = avg_life_exp),
                color = "grey80", size = 1) +
      # Selected country lines
      geom_line(data = highlighted_data,
                aes(x = year, y = life_expectancy, color = country)) +
      labs(x = "Year",
           y = "Life Expectancy (years)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Comparison table
  output$comparison_table <- DT::renderDataTable({
    global_means <- global_averages() %>%
      summarise(
        global_exp = mean(avg_expenditure),
        global_life = mean(avg_life_exp)
      )
    
    country_stats <- country_averages() %>%
      mutate(
        exp_diff = avg_expenditure - global_means$global_exp,
        life_diff = avg_life_exp - global_means$global_life,
        exp_diff_pct = (exp_diff / global_means$global_exp) * 100,
        life_diff_pct = (life_diff / global_means$global_life) * 100
      ) %>%
      select(
        Country = country,
        `Avg Expenditure` = avg_expenditure,
        `Avg Life Expectancy` = avg_life_exp,
        `Exp. Diff from Global` = exp_diff,
        `Life Exp. Diff from Global` = life_diff,
        `Exp. % Diff` = exp_diff_pct,
        `Life Exp. % Diff` = life_diff_pct
      )
    
    DT::datatable(country_stats,
                  options = list(pageLength = 10),
                  rownames = FALSE) %>%
      formatCurrency(columns = c("Avg Expenditure", "Exp. Diff from Global")) %>%
      formatRound(columns = c("Avg Life Expectancy", "Life Exp. Diff from Global"), digits = 2) %>%
      formatRound(columns = c("Exp. % Diff", "Life Exp. % Diff"), digits = 1)
  })
}

# Run the app
shinyApp(ui = ui, server = server)