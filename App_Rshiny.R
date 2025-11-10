library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Analyse du sommeil et du style de vie"),
  sidebarLayout(
    sidebarPanel(
      selectInput("varX", "Variable X :", choices = c("Age", "Stress.Level", "Sleep.Duration")),
      selectInput("varY", "Variable Y :", choices = c("Sleep.Duration", "Physical.Activity.Level", "BMI.Category")),
      selectInput("colorVar", "Couleur par :", choices = c("Gender", "Occupation", "Sleep.Disorder"))
    ),
    mainPanel(
      plotOutput("graph"),
      tableOutput("summary")
    )
  )
)

server <- function(input, output) {
  data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
  
  output$graph <- renderPlot({
    ggplot(data, aes_string(x = input$varX, y = input$varY, color = input$colorVar)) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(x = input$varX, y = input$varY, color = input$colorVar)
  })
  
  output$summary <- renderTable({
    data |>
      select(all_of(c(input$varX, input$varY))) |>
      summary() |>
      as.data.frame()
  })
}

shinyApp(ui, server)

