library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Elephants: Range of Variables by Sex"
  ),
  
  dashboardSidebar(
    
    selectInput("y",
                "Select Variable",
                choices = c("age", "height"),
                selected = "age")
  ),
  
  dashboardBody(  
    plotOutput("plot", width = "600px", height = "500px")
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    
    elephants %>% 
      ggplot(aes(x = sex,
                 y = .data[[input$y]],
                 fill = sex)) + 
      geom_boxplot(alpha = 0.75) +
      labs(title = "Variables by Sex",
           x = "Sex",
           fill = "Sex") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_minimal()
    
  })
}

shinyApp(ui, server)