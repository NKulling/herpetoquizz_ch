
# packages ----------------------------------------------------------------

library(shiny)
library(jpeg)
library(RCurl)
library(data.table)
library(magrittr)
library(dplyr)
library(leaflet)

# 1) local paths ----------------------------------------------------------

dt<-fread("data/allsp.csv")

# Initialize score
score <- 0

df <- dt
# French names for the taxa

taxa <- c(
  "Zamenis longissimus",
  "Vipera aspis",
  "Vipera berus",
  "Natrix natrix",
  "Natrix helvetica",
  "Coronella austriaca",
  "Hierophis viridiflavus",
  "Natrix maura",
  "Natrix tessellata"
)

taxa_french <- c(
  "Zamenis longissimus (Couleuvre d'Esculape)",
  "Vipera aspis (Vipère aspic)",
  "Vipera berus (Vipère péliade)",
  "Natrix natrix (Couleuvre à collier)",
  "Natrix helvetica (Couleuvre à collier helvétique)",
  "Coronella austriaca (Coronelle lisse)",
  "Hierophis viridiflavus (Couleuvre verte et jaune)",
  "Natrix maura (Couleuvre vipérine)",
  "Natrix tessellata (Couleuvre tessellée)"
)


# Define UI
ui <- fluidPage(
  titlePanel("Herpeto Quiz"),
  sidebarLayout(
    sidebarPanel(
      h4(""),
      checkboxGroupInput(
        "selected_taxa",
        label = "Quels serpents veux-tu reconnaître?",
        choices = taxa_french,
        selected = taxa_french  # Default to selecting all
      ),
      actionButton("start_game", "Start Game"),
      hr(),
      h4("Instructions"),
      p("Sélectionne l'espèce correcte dans la liste ci-après."),
      selectInput("answer", "choisis l'espèce:", choices = NULL),
      actionButton("submit", "Submit"),
      textOutput("feedback"),
      hr(),
      textOutput("score"),
      br(),
      actionButton("restart", "Restart Game")
    ),
    mainPanel(
      uiOutput("image"),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize reactive values
  current_index <- reactiveVal(1)
  score <- reactiveVal(0)
  game_over <- reactiveVal(FALSE)
  df <- reactiveVal(data.frame())  # Empty reactive dataframe initially
  
  # Observe the start game button to filter taxa and start the game
  observeEvent(input$start_game, {
    selected_taxa <- taxa[match(input$selected_taxa, taxa_french)]
    filtered_dt <- dt[dt$scientific_name %in% selected_taxa, ]
    
    # Generate a new random sample
    random_number <- sample(0:90000, 1)
    set.seed(random_number)
    new_sample <- filtered_dt %>%
      group_by(scientific_name) %>%
      slice_sample(n = ceiling(100 / length(selected_taxa))) %>%
      ungroup() %>%
      slice_sample(n = 100) %>%
      slice_sample(n = 15)  # Updated sample size to 15
    
    df(new_sample)  # Update the reactive dataframe
    
    # Reset game state
    current_index(1)
    score(0)
    game_over(FALSE)
    output$feedback <- renderText("")
    
    # Update dropdown options based on selected taxa
    updateSelectInput(session, "answer", choices = input$selected_taxa)
  })
  
  # Display the current image
  output$image <- renderUI({
    if (!game_over() && nrow(df()) > 0) {
      i <- current_index()
      image_url <- df()[i, ]$image_url
      
      tags$div(
        style = "text-align: center;",
        tags$img(
          src = image_url,
          style = "max-width: 90%; max-height: 600px; border: 2px solid #ccc; cursor: zoom-in;",
          onclick = "this.style.cursor='zoom-out'; this.style.maxWidth='none'; this.style.maxHeight='none';"
        )
      )
    } else {
      # Define the final message and image based on the score
      final_score <- score()
      message <- ""
      image_url <- ""
      
      if (final_score <= 5) {
        message <- "bravo, tu as le niveau d'un orvet."
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/280317402/medium.jpg" 
      } else if (final_score <= 10) {
        message <- "tu es l'équivalent de ce crapaud "
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/88307277/medium.jpeg"  # Replace with your image URL
      } else if (final_score <= 14) {
        message <- "t'es pas plus con que t'en as l'air!"
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/448385907/large.jpg"  # Replace with your image URL
      } else {
        message <- "tu es l'équivalent d'un anaconda dans ce jeu, c'est a dire une soupière tropicale"
        image_url <- "https://inaturalist-open-data.s3.amazonaws.com/photos/145377510/original.jpg"  # Replace with your image URL
      }
      
      tags$div(
        style = "text-align: center; font-size: 20px; color: red;",
        tags$h3("Fini!"),
        tags$p(paste("ton score est de :", score(), "/", nrow(df()), "points. ", message)),
        tags$img(
          src = image_url,
          style = "max-width: 90%; max-height: 600px; border: 2px solid #ccc;"
        )
      )
    }
  })
  
  # Render the map
  output$map <- renderLeaflet({
    i <- current_index()
    if (!game_over() && nrow(df()) > 0 && i <= nrow(df())) {
      lat <- df()[i, ]$latitude
      lon <- df()[i, ]$longitude
      
      leaflet() %>%
        addTiles() %>%
        setView(lng = lon, lat = lat, zoom = 10) %>%
        addMarkers(lng = lon, lat = lat, popup = paste("Observation:", df()[i, ]$scientific_name))
    } else {
      leaflet() %>%
        addTiles()
    }
  })
  
  # Check the user's input
  observeEvent(input$submit, {
    i <- current_index()
    if (!game_over() && nrow(df()) > 0 && i <= nrow(df())) {
      correct_species <- df()[i, ]$scientific_name
      correct_species_french <- taxa_french[which(taxa == correct_species)]
      if (input$answer == correct_species_french) {
        score(score() + 1)
        output$feedback <- renderText("Bien joué tartempion!")
      } else {
        output$feedback <- renderText(paste("...m'enfin? l'espèce était:", correct_species_french))
      }
      current_index(i + 1)
      if (i == nrow(df())) game_over(TRUE)
    }
  })
  
  # Display the score
  output$score <- renderText({
    paste("Score:", score(), "/", nrow(df()))
  })
  
  # Restart the game
  observeEvent(input$restart, {
    # Reset game state and reinitialize taxa selection
    updateCheckboxGroupInput(session, "selected_taxa", selected = taxa_french)
    df(data.frame())  # Reset the dataframe
    current_index(1)
    score(0)
    game_over(FALSE)
    output$feedback <- renderText("")
  })
}

# Run the app
shinyApp(ui, server)

