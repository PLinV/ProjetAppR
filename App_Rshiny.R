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
      
      selectInput("age", "Catégorie d'âge :", 
                  choices = c("Toutes", unique(heart$AgeCategory)), 
                  selected = "Toutes"),
      
      br(),
      actionButton("reset", "🔄 Réinitialiser les filtres")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique",
                 h4("Proportion de maladies cardiaques selon les filtres"),
                 plotOutput("heartPlot"),
                 br(),
                 textOutput("summaryText")
        ),
        tabPanel("Tableau",
                 h4("Données filtrées"),
                 dataTableOutput("filteredTable")
        ),
        tabPanel("Âge et maladies cardiaques",
                 h4("Répartition des maladies cardiaques selon l'âge"),
                 plotOutput("agePlot")
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
    if (input$age != "Toutes") {
      data <- data[data$AgeCategory == input$age, ]
    }
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "sex", selected = "Tous")
    updateSelectInput(session, "smoke", selected = "Tous")
    updateSelectInput(session, "asthma", selected = "Tous")
    updateSelectInput(session, "age", selected = "Toutes")
  })
  
  output$heartPlot <- renderPlot({
    data <- filteredData()
    
    ggplot(data, aes(x = HeartDisease, fill = HeartDisease)) +
      geom_bar() +
      theme_minimal() +
      labs(x = "Présence de maladie cardiaque", 
           y = "Nombre d'individus",
           fill = "HeartDisease") +
      scale_fill_manual(values = c("No" = "#4CAF50", "Yes" = "#E53935"))
  })
  
  output$summaryText <- renderText({
    data <- filteredData()
    total <- nrow(data)
    diseased <- sum(data$HeartDisease == "Yes")
    percent <- round(100 * diseased / total, 1)
    
    paste("Sur", total, "individus filtrés,", diseased, 
          "ont une maladie cardiaque soit", percent, "%.")
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
  
}

# Lancement de l'app
shinyApp(ui = ui, server = server)
