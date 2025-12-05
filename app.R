# Cointrin Questionnaire - Shiny Application
# This application collects survey data about the Cointrin area

library(shiny)
library(shinyjs)

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Questionnaire Cointrin"),
  
  # Introduction
  wellPanel(
    h3("Bienvenue / Welcome"),
    p("Ce questionnaire vise à recueillir vos opinions et expériences concernant la zone de Cointrin."),
    p("This questionnaire aims to collect your opinions and experiences regarding the Cointrin area."),
    hr()
  ),
  
  # Sidebar with navigation
  sidebarLayout(
    sidebarPanel(
      h4("Informations / Information"),
      p("Temps estimé: 10-15 minutes"),
      p("Estimated time: 10-15 minutes"),
      hr(),
      actionButton("submit", "Soumettre / Submit", class = "btn-primary"),
      br(), br(),
      textOutput("status")
    ),
    
    # Main panel with questions
    mainPanel(
      h3("Section 1: Informations démographiques / Demographic Information"),
      
      selectInput("age_group",
                  "Groupe d'âge / Age group:",
                  choices = c("Sélectionner / Select" = "",
                              "18-24",
                              "25-34",
                              "35-44",
                              "45-54",
                              "55-64",
                              "65+"),
                  selected = ""),
      
      selectInput("residence",
                  "Lieu de résidence / Place of residence:",
                  choices = c("Sélectionner / Select" = "",
                              "Cointrin",
                              "Genève / Geneva",
                              "Canton de Genève / Canton of Geneva",
                              "Autre Suisse / Other Switzerland",
                              "France voisine / Neighboring France",
                              "Autre / Other"),
                  selected = ""),
      
      hr(),
      
      h3("Section 2: Utilisation de la zone / Area Usage"),
      
      selectInput("visit_frequency",
                  "À quelle fréquence visitez-vous la zone de Cointrin? / How often do you visit the Cointrin area?",
                  choices = c("Sélectionner / Select" = "",
                              "Quotidiennement / Daily",
                              "Plusieurs fois par semaine / Several times a week",
                              "Une fois par semaine / Once a week",
                              "Quelques fois par mois / A few times a month",
                              "Rarement / Rarely",
                              "Jamais / Never"),
                  selected = ""),
      
      checkboxGroupInput("visit_reasons",
                        "Raisons de visite / Reasons for visiting (sélectionner tout ce qui s'applique / select all that apply):",
                        choices = c("Travail / Work" = "work",
                                   "Voyages (aéroport) / Travel (airport)" = "airport",
                                   "Shopping / Shopping" = "shopping",
                                   "Loisirs / Leisure" = "leisure",
                                   "Visite à des proches / Visiting family/friends" = "visiting",
                                   "Transit / Transit" = "transit",
                                   "Autre / Other" = "other")),
      
      hr(),
      
      h3("Section 3: Transport / Transportation"),
      
      checkboxGroupInput("transport_modes",
                        "Modes de transport utilisés / Transportation modes used (sélectionner tout ce qui s'applique / select all that apply):",
                        choices = c("Voiture personnelle / Personal car" = "car",
                                   "Transport public / Public transport" = "public",
                                   "Vélo / Bicycle" = "bike",
                                   "À pied / Walking" = "walk",
                                   "Moto/Scooter / Motorcycle/Scooter" = "motorcycle",
                                   "Taxi/Uber" = "taxi",
                                   "Autre / Other" = "other")),
      
      hr(),
      
      h3("Section 4: Perception de la zone / Perception of the Area"),
      
      sliderInput("accessibility",
                  "Accessibilité de la zone / Area accessibility:",
                  min = 1, max = 5,
                  value = 3,
                  step = 1),
      tags$div(style = "margin-left: 20px;",
               "1 = Très mauvaise / Very poor, 5 = Excellente / Excellent"),
      
      br(),
      
      sliderInput("safety",
                  "Sentiment de sécurité / Feeling of safety:",
                  min = 1, max = 5,
                  value = 3,
                  step = 1),
      tags$div(style = "margin-left: 20px;",
               "1 = Très peu sûr / Very unsafe, 5 = Très sûr / Very safe"),
      
      br(),
      
      sliderInput("attractiveness",
                  "Attractivité de la zone / Area attractiveness:",
                  min = 1, max = 5,
                  value = 3,
                  step = 1),
      tags$div(style = "margin-left: 20px;",
               "1 = Pas du tout attrayante / Not attractive at all, 5 = Très attrayante / Very attractive"),
      
      hr(),
      
      h3("Section 5: Suggestions et commentaires / Suggestions and Comments"),
      
      textAreaInput("improvements",
                    "Quelles améliorations souhaiteriez-vous voir dans la zone de Cointrin? / What improvements would you like to see in the Cointrin area?",
                    placeholder = "Vos suggestions... / Your suggestions...",
                    rows = 4,
                    width = "100%"),
      
      textAreaInput("additional_comments",
                    "Commentaires additionnels / Additional comments:",
                    placeholder = "Tout autre commentaire... / Any other comments...",
                    rows = 3,
                    width = "100%"),
      
      hr(),
      
      p(tags$em("Merci de votre participation! / Thank you for your participation!"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store submission status
  values <- reactiveValues(submitted = FALSE)
  
  # Handle form submission
  observeEvent(input$submit, {
    # Validate required fields
    if (input$age_group == "" || input$residence == "" || input$visit_frequency == "") {
      showModal(modalDialog(
        title = "Champs manquants / Missing Fields",
        "Veuillez remplir tous les champs obligatoires (âge, résidence, fréquence de visite). / Please fill in all required fields (age, residence, visit frequency).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Collect all responses
    responses <- data.frame(
      timestamp = Sys.time(),
      age_group = input$age_group,
      residence = input$residence,
      visit_frequency = input$visit_frequency,
      visit_reasons = paste(input$visit_reasons, collapse = "; "),
      transport_modes = paste(input$transport_modes, collapse = "; "),
      accessibility = input$accessibility,
      safety = input$safety,
      attractiveness = input$attractiveness,
      improvements = input$improvements,
      additional_comments = input$additional_comments,
      stringsAsFactors = FALSE
    )
    
    # Save to CSV file (append mode)
    file_name <- "questionnaire_responses.csv"
    
    # Create file with headers if it doesn't exist
    if (!file.exists(file_name)) {
      write.csv(responses, file_name, row.names = FALSE)
    } else {
      write.table(responses, file_name, sep = ",", 
                  append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    
    # Show success message
    showModal(modalDialog(
      title = "Succès / Success",
      "Merci! Vos réponses ont été enregistrées. / Thank you! Your responses have been recorded.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
    # Update status
    output$status <- renderText({
      "Soumis avec succès! / Successfully submitted!"
    })
    
    values$submitted <- TRUE
    
    # Reset form after a delay
    shinyjs::delay(2000, {
      shinyjs::reset("age_group")
      shinyjs::reset("residence")
      shinyjs::reset("visit_frequency")
      shinyjs::reset("visit_reasons")
      shinyjs::reset("transport_modes")
      shinyjs::reset("accessibility")
      shinyjs::reset("safety")
      shinyjs::reset("attractiveness")
      shinyjs::reset("improvements")
      shinyjs::reset("additional_comments")
      output$status <- renderText({""})
      values$submitted <- FALSE
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
