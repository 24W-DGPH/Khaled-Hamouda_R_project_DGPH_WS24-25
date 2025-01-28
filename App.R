# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  shiny,
  shinythemes
)

# Create the Shiny app
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Life Expectancy vs Health Expenditure Analysis (1995-2010)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", 
                  "Select a Country:",
                  choices = sort(unique(combined_data$country)),
                  selected = "United States"),
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("global_plot")),
        br(),
        column(12, plotOutput("trend_plot"))
      ),
      width = 9
    )
  )
)

server <- function(input, output) {
  
  # Global scatter plot with selected country highlighted
  output$global_plot <- renderPlot({
    # Filter data for selected country
    country_data <- combined_data %>%
      filter(country == input$country)
    
    ggplot() +
      # All countries points in background
      geom_point(data = combined_data,
                 aes(x = health_expenditure, y = life_expectancy),
                 color = "grey80", alpha = 0.3) +
      # Selected country points highlighted
      geom_point(data = country_data,
                 aes(x = health_expenditure, y = life_expectancy),
                 color = "red", size = 3) +
      # Add line for selected country
      geom_path(data = country_data,
                aes(x = health_expenditure, y = life_expectancy),
                color = "red", arrow = arrow()) +
      # Labels
      labs(
        title = paste("Health Expenditure vs Life Expectancy:", input$country, "Highlighted"),
        subtitle = "Red points show selected country's trajectory",
        x = "Health Expenditure",
        y = "Life Expectancy (years)"
      ) +
      theme_minimal()
  })
  
  # Trend comparison plot
  output$trend_plot <- renderPlot({
    # Filter data for selected country
    country_data <- combined_data %>%
      filter(country == input$country) %>%
      group_by(year) %>%
      summarise(
        life_expectancy = mean(life_expectancy, na.rm = TRUE),
        health_expenditure = mean(health_expenditure, na.rm = TRUE)
      )
    
    # Global averages
    global_data <- combined_data %>%
      group_by(year) %>%
      summarise(
        life_expectancy = mean(life_expectancy, na.rm = TRUE),
        health_expenditure = mean(health_expenditure, na.rm = TRUE)
      )
    
    ggplot() +
      # Global average line
      geom_line(data = global_data,
                aes(x = year, group = 1,
                    y = life_expectancy, color = "Global Average"),
                size = 1) +
      # Selected country line
      geom_line(data = country_data,
                aes(x = year, group = 1,
                    y = life_expectancy, color = "Selected Country"),
                size = 1) +
      # Add points
      geom_point(data = global_data,
                 aes(x = year, y = life_expectancy),
                 color = "blue", size = 3) +
      geom_point(data = country_data,
                 aes(x = year, y = life_expectancy),
                 color = "red", size = 3) +
      # Customize colors
      scale_color_manual(values = c("Global Average" = "blue",
                                    "Selected Country" = "red")) +
      # Labels
      labs(
        title = paste("Life Expectancy Over Time:", input$country, "vs Global Average"),
        x = "Year",
        y = "Life Expectancy (years)",
        color = "Legend"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the app
shinyApp(ui = ui, server = server)