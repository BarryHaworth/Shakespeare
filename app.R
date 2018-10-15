# Shiny App for Shakespeare Word Cloud
# Options are to filter the wordcloud by 
# Play type, by play and by character 
# Using code I already had, but a sample from https://shiny.rstudio.com/gallery/word-cloud.html

library(shiny)

library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Set directory and environment vaiables

# HOME        <- "/data/anet11.2/tatf/work/uca78"
HOME        <- "c:/R"
PROJECT_DIR <- paste0(HOME,"/Shakespeare")
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

load(file=paste0(DATA_DIR,"/works.RData"))

ui <- fluidPage(
  titlePanel("Shakespearean Word Clouds",windowTitle = "Shakespeare"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "type", 
                     label = "Play Type",
                     sort(unique(as.character(works$Type))), 
                     selected = NULL,
                     multiple=TRUE),
      
      selectizeInput(inputId = "play", 
                     label = "Play",
                     sort(unique(as.character(works$Play))), 
                     selected =  NULL,
                     multiple=TRUE),
      
      selectizeInput(inputId = "player", 
                     label = "Character",
                     sort(unique(as.character(works$Player))), 
                     selected =  NULL,
                     multiple=TRUE)
    ),
      mainPanel(
        plotOutput("plot",
                   width="500px",
                   height="500px")
      )
    )
  )

    
server <- function(input, output) {

  # Filter Works > types > Plays > Players > text
  types <- reactive({
    types <- works
    if (is.null(input$type) == FALSE){
      types <- types %>% filter(Type %in% input$type)
    }
    return(types)
  })
  plays <- reactive({
    plays <- types()
    if (is.null(input$play) == FALSE){
      plays <- plays %>% filter(Play %in% input$play)
    }
    return(plays)
  })
  players <- reactive({
    players <- plays()
    if (is.null(input$player) == FALSE){
      players <- players %>% filter(Player %in% input$player)
    }
    return(players)
  })
  text <- reactive({
    return(players()$PlayerLine)
  })  
  
  # if players is selected
  # then play and play type are conditional on that
  # else if type is selected
  # then play and player are conditional on that
  # else if play is selected
  # then type and player are conditional on that
  
  
  # Create Docs for Word Cloud
  docs <- reactive({
    docs  <- Corpus(VectorSource(text()))
    docs <- tm_map(docs, content_transformer(tolower))          # Convert the text to lower case
    docs <- tm_map(docs, removeNumbers)                         # Remove numbers
    docs <- tm_map(docs, removeWords, stopwords("english"))     # Remove english common stopwords
    docs <- tm_map(docs, removeWords, c("thee", "thou","thy","shall","will"))  # Remove your own stopwords
    docs <- tm_map(docs, removePunctuation)                     # Remove punctuation
    docs <- tm_map(docs, stripWhitespace)                       # Eliminate extra white spaces
    return(docs)
  })
  # Create the Word Cloud

  output$plot <- renderPlot({
    wordcloud(words = docs(), min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)