# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  shiny,
  shinythemes,
  tidyverse,      # Load tidyverse first as it's the primary package
  rio,            # for importing/exporting data
  here,           # for relative file paths
  skimr,          # for reviewing the data
  janitor,        # for cleaning and tabulating data
  epikit,         # for creating age categories
  kableExtra   
)

# Import datasets with no headers ----
life_exp <- read_csv(here("data", "life_exp.csv"), col_names = FALSE)
health_xp <- read_csv(here("data", "health_xp.csv"), col_names = FALSE)
# Data cleaning ----

# Clean life expectancy data
life_exp_longer <- life_exp %>%
  # Set column names
  set_names(c("country", as.character(1800:2100))) %>%
  # Remove the first row (which contains "country")
  slice(-1) %>%  # This removes the first row that was incorrectly read as data
  # Pivot longer
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "life_expectancy",
    names_transform = list(year = as.numeric)
  ) %>%
  # Filter years
  filter(between(year, 1995, 2010)) %>%
  # Remove NA values
  drop_na() %>%
  # Arrange data
  arrange(country, year)

# Clean health expenditure data
health_xp_longer <- health_xp %>%
  # Set column names
  set_names(c("country", as.character(1995:2010))) %>%
  # Remove the first row (which contains "country")
  slice(-1) %>%  # This removes the first row that was incorrectly read as data
  # Pivot longer
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "health_expenditure",
    names_transform = list(year = as.numeric)
  ) %>%
  # Remove NA values
  drop_na() %>%
  # Arrange data
  arrange(country, year)

# Join the datasets
combined_data <- life_exp_longer %>%
  left_join(health_xp_longer, by = c("country", "year"))

# Create the Shiny app ----
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