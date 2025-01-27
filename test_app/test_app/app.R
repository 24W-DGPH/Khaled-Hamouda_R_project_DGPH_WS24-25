
library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
    titlePanel("Test Dashboard"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Choose a country:",
                       choices = c("USA", "Japan", "Germany"))
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Server
server <- function(input, output) {
    output$plot <- renderPlot({
        # Simple plot
        plot(1:10, main = input$country)
    })
}

# Run the app
shinyApp(ui = ui, server = server)

