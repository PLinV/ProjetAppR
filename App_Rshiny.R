# app.R
library(shiny)
library(ggplot2)
library(dplyr)

# Chargement des données
heart <- read.csv("heart.csv")

# Interface utilisateur
ui <- fluidPage(
  titlePanel("🫀 Étude sur les maladies cardiaques"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtres"),
      
      selectInput("sex", "Sexe :", 
                  choices = c("Tous", unique(heart$Sex)), 
                  selected = "Tous"),
      
      selectInput("smoke", "Fumeur :", 
                  choices = c("Tous", unique(heart$Smoking)), 
                  selected = "Tous"),
      
      selectInput("asthma", "Asthme :", 
                  choices = c("Tous", unique(heart$Asthma)), 
                  selected = "Tous"),
      
      br(),
      actionButton("reset", "🔄 Réinitialiser les filtres")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique",
                 
           tabsetPanel(
             tabPanel("Répartition des maladies cardiaques selon l'âge",
               plotOutput("agePlot"),
               br(),
               textOutput("summaryText")
             ),
             tabPanel("Proportion d'individus selon les maladies")
           )
        ),
        tabPanel("Tableau",
                 h4("Données filtrées"),
                 dataTableOutput("filteredTable")
        )
        
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  filteredData <- reactive({
    data <- heart
    
    if (input$sex != "Tous") {
      data <- data[data$Sex == input$sex, ]
    }
    if (input$smoke != "Tous") {
      data <- data[data$Smoking == input$smoke, ]
    }
    if (input$asthma != "Tous") {
      data <- data[data$Asthma == input$asthma, ]
    }
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "sex", selected = "Tous")
    updateSelectInput(session, "smoke", selected = "Tous")
    updateSelectInput(session, "asthma", selected = "Tous")
  })
  
  output$filteredTable <- renderDataTable({
    filteredData()
  })
  
  output$agePlot <- renderPlot({
    data <- filteredData()
    
    # Calcul de la proportion de malades par tranche d'âge
    prop_data <- data %>%
      group_by(AgeCategory) %>%
      summarise(
        total = n(),
        malades = sum(HeartDisease == "Yes"),
        proportion = 100 * malades / total
      )
    
    ggplot(prop_data, aes(x = AgeCategory, y = proportion)) +
      geom_col(fill = "#E53935") +
      theme_minimal() +
      labs(
        x = "Catégorie d'âge",
        y = "Proportion de malades (%)",
        title = "Proportion d'individus atteints d'une maladie cardiaque selon l'âge"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summaryText <- renderText({
    data <- filteredData()
    total <- nrow(data)
    diseased <- sum(data$HeartDisease == "Yes")
    percent <- round(100 * diseased / total, 1)
    
    paste("Sur", total, "individus filtrés,", diseased, 
          "ont une maladie cardiaque soit", percent, "%.")
  })
  
}

# Lancement de l'app
shinyApp(ui = ui, server = server)
