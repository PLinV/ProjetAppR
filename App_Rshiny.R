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
                  ),
                  
                  tabPanel("Bilan", 
                           h4("Impact des filtres sur le risque de maladie cardiaque par tranche d'âge"),
                           plotOutput("bilanPlot"),  # graphique des deux courbes
                           br(),
                           textOutput("bilanText")   # texte explicatif
                  )
                  
                  
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  observe({
      if(input$main_tabs == "Graphique" && input$graph_tabs == "Camembert") {
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
  
  # Reactive qui calcule le tableau des proportions par tranche d'âge
  bilanAgeData <- reactive({
    data_filtre <- filteredData()  # données filtrées par l'utilisateur
    data_sain <- heart %>%
      filter(Smoking == "No",
             Asthma == "No",
             BMI_Category == "Normal weight",
             PhysicalActivity == "Yes")
    
    age_levels <- unique(heart$AgeCategory)
    
    # Calcul des proportions par tranche d'âge
    df <- data.frame(AgeCategory = age_levels)
    df$Prop_sain <- sapply(age_levels, function(a){
      temp <- data_sain[data_sain$AgeCategory == a, ]
      if(nrow(temp) == 0) return(0)
      mean(temp$HeartDisease == "Yes") * 100
    })
    
    df$Prop_filtre <- sapply(age_levels, function(a){
      temp <- data_filtre[data_filtre$AgeCategory == a, ]
      if(nrow(temp) == 0) return(0)
      mean(temp$HeartDisease == "Yes") * 100
    })
    
    df$Risque_supp <- df$Prop_filtre - df$Prop_sain
    
    df
  })
  
  # Graphique avec deux courbes
  output$bilanPlot <- renderPlot({
    df <- bilanAgeData()
    
    ggplot(df, aes(x = AgeCategory)) +
      geom_line(aes(y = Prop_sain, color = "Individu sain"), size = 1.2, group = 1) +
      geom_point(aes(y = Prop_sain, color = "Individu sain"), size = 2) +
      geom_line(aes(y = Prop_filtre, color = "Filtre actuel"), size = 1.2, group = 1) +
      geom_point(aes(y = Prop_filtre, color = "Filtre actuel"), size = 2) +
      theme_minimal() +
      labs(
        x = "Tranche d'âge",
        y = "Proportion de malades (%)",
        title = "Comparaison entre un individu sain et vos filtres",
        color = ""
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, max(c(df$Prop_sain, df$Prop_filtre)) * 1.1)
  })
  
  # Texte explicatif du risque général
  output$bilanText <- renderText({
    
    # Données filtrées par l'utilisateur
    data_filtre <- filteredData()
    
    # Données d'un "individu sain"
    data_sain <- heart %>%
      filter(Smoking == "No",
             Asthma == "No",
             BMI_Category == "Normal weight",
             PhysicalActivity == "Yes")
    
    # Calcul de la proportion moyenne de malades
    prop_filtre <- if(nrow(data_filtre) > 0) mean(data_filtre$HeartDisease == "Yes") * 100 else NA
    prop_sain   <- if(nrow(data_sain) > 0)   mean(data_sain$HeartDisease == "Yes") * 100 else NA
    
    # Risque supplémentaire global
    risque_sup <- round(prop_filtre - prop_sain, 1)
    
    # Création du texte simple
    if(is.na(risque_sup)){
      "Impossible de calculer le risque : aucune donnée disponible pour ces filtres."
    } else if(risque_sup > 0){
      paste0("La personne filtrée a +", risque_sup, 
             "% de risque de maladie cardiaque par rapport à une personne qui n'a aucun facteur agravant.")
    } else if(risque_sup < 0){
      paste0("La personne filtrée a ", risque_sup, 
             "% de risque de maladie cardiaque par rapport à une personne qui n'a aucun facteur agravant.")
    } else {
      "La personne filtrée a le même risque qu'une personne qui n'a aucun facteur agravant."
    }
  })
  
  
  
  
  
}

# Lancement de l'app
shinyApp(ui = ui, server = server)
