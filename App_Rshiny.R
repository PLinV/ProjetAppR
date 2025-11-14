# app.R
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)

# Chargement des données
heart <- read.csv("heart.csv")

heart <- heart %>%
  mutate(
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal weight",
      BMI >= 25 & BMI < 30 ~ "Class 1 obesity",
      BMI >= 30 & BMI < 40 ~ "Class 2 obesity",
      BMI >= 40 ~ "Class 3+ obesity"
    )
  )

# Interface utilisateur
ui <- fluidPage(
  useShinyjs(),   # activation de shinyjs
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
      
      selectInput("bmi", "IMC :", 
                  choices = c("Tous", unique(heart$BMI_Category)), 
                  selected = "Tous"),
      
      selectInput("physical", "Activité physique :", 
                  choices = c("Tous", unique(heart$PhysicalActivity)), 
                  selected = "Tous"),
      
      br(),
      actionButton("reset", "🔄 Réinitialiser les filtres")
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Graphique",
                           tabsetPanel(id = "graph_tabs",
                                       tabPanel("Répartition des maladies cardiaques selon l'âge",
                                                plotOutput("agePlot"),
                                                br(),
                                                textOutput("summaryText")
                                       ),
                                       tabPanel("Proportion d'individus selon les maladies"),
                                       tabPanel("Camembert", plotOutput("piePlot"))  # camembert commenté
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
  
  observe({
      if(input$graph_tabs == "Camembert") {
          disable("sex")
          disable("smoke")
          disable("asthma")
          disable("bmi")
          disable("physical")
      } else {
          enable("sex")
          enable("smoke")
          enable("asthma")
          enable("bmi")
          enable("physical")
      }
  })
  
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
    if (input$bmi != "Tous") {
      data <- data[data$BMI_Category == input$bmi, ]
    }
    if (input$physical != "Tous") {
      data <- data[data$PhysicalActivity == input$physical, ]
    }
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "sex", selected = "Tous")
    updateSelectInput(session, "smoke", selected = "Tous")
    updateSelectInput(session, "asthma", selected = "Tous")
    updateSelectInput(session, "bmi", selected = "Tous")
    updateSelectInput(session, "physical", selected = "Tous")
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 60)
  })
  
  output$summaryText <- renderText({
    data <- filteredData()
    total <- nrow(data)
    diseased <- sum(data$HeartDisease == "Yes")
    percent <- round(100 * diseased / total, 1)
    
    paste("Sur", total, "individus filtrés,", diseased, 
          "ont une maladie cardiaque soit", percent, "%.")
  })
  
  output$piePlot <- renderPlot({  # camembert commenté
      data <- heart %>% filter(HeartDisease == "Yes")
      factor_counts <- c(
          Fumeur = sum(data$Smoking == "Yes", na.rm = TRUE),
          Asthme = sum(data$Asthma == "Yes", na.rm = TRUE),
          Obese = sum(data$BMI_Category %in% c("Class 1 obesity","Class 2 obesity","Class 3+ obesity"), na.rm = TRUE),
          FaibleActivite = sum(data$PhysicalActivity == "No", na.rm = TRUE)
          
      )
      df <- data.frame(
          Factor = names(factor_counts),
          Count = as.numeric(factor_counts)
      )
      ggplot(df, aes(x = "", y = Count, fill = Factor)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          theme_void() +
          labs(title = "Répartition des malades cardiaques selon les facteurs") +
          scale_fill_brewer(palette = "Set2")
   })
  
}

# Lancement de l'app
shinyApp(ui = ui, server = server)
