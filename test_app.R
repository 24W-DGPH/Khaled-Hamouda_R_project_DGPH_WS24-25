# test_app.R
library(shiny)
library(tidyverse)

# Load and prepare minimal data
life_exp_longer <- life_exp %>%
  setNames(life_exp_columns) %>%
  filter(country != "country") %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "value",
    names_transform = list(year = as.numeric)
  ) %>%
  filter(year >= 1995 & year <= 2010) %>%
  drop_na() %>%
  arrange(country, year)

health_xp_longer <- health_xp %>%
  setNames(health_xp_columns) %>%
  filter(country != "country") %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "value",
    names_transform = list(year = as.numeric)
  ) %>%
  drop_na() %>%
  arrange(country, year)

# Create minimal combined data
combined_data <- left_join(
  life_exp_longer %>% rename(life_expectancy = value),
  health_xp_longer %>% rename(health_expenditure = value),
  by = c("country", "year")
)

# Simple UI
ui <- fluidPage(
  titlePanel("Health and Life Expectancy Test"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country:",
                  choices = unique(combined_data$country))
    ),
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Simple Server
server <- function(input, output) {
  output$scatter_plot <- renderPlot({
    ggplot(combined_data %>% filter(country == input$country),
           aes(x = health_expenditure, y = life_expectancy)) +
      geom_point() +
      theme_minimal() +
      labs(x = "Health Expenditure",
           y = "Life Expectancy")
  })
}

# Run the app
shinyApp(ui = ui, server = server)