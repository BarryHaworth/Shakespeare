# Shiny App for Shakespeare Word Cloud
# Options are to filter the wordcloud by 
# Play type, by play and by character 
# Using code I already had, but a sample from https://shiny.rstudio.com/gallery/word-cloud.html

# Next thing to try: reduce filter options based on other filter options.
# Maybe one of these:
#  https://sites.temple.edu/psmgis/2017/07/26/r-shiny-task-create-an-input-select-box-that-is-dependent-on-a-previous-input-choice/
# https://groups.google.com/forum/#!topic/shiny-discuss/Q1ZvnjDCzUM


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
# HOME        <- "h:/R"
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
      
      selectizeInput(inputId = "character", 
                     label = "Character",
                     sort(unique(as.character(works$Character))), 
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

    
server <- function(input, output, session) {

  # Filter Works > types > plays > characters > text
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
  characters <- reactive({
    characters <- plays()
    if (is.null(input$character) == FALSE){
      characters <- characters %>% filter(Character %in% input$character)
    }
    return(characters)
  })
  text <- reactive({
    return(characters()$PlayerLine)
  })  
  
  # if character is selected
  # then play and play type are conditional on that
  # else if type is selected
  # then play and character are conditional on that
  # else if play is selected
  # then type and character are conditional on that
  
  observe({
    x <- input$type

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectizeInput(session, "type",
                         label = paste("Play Type"),
                         choices = sort(unique(as.character(characters()$Type ))),
                         selected = input$type
    )
    updateSelectizeInput(session, "play",
                         label = paste("Play"),
                         choices = sort(unique(as.character(characters()$Play ))),
                         selected = input$play
    )
    updateSelectizeInput(session, "character",
                         label = paste("Character"),
                         choices = sort(unique(as.character(characters()$Character ))),
                         selected = input$character
    )
    
  })


  
  customStop <- c("thee", "thou","thy","shall","will")   # Custom Stopwords
  
  # Create Docs for Word Cloud
  docs <- reactive({
    docs  <- Corpus(VectorSource(text()))
    docs <- tm_map(docs, content_transformer(tolower))          # Convert the text to lower case
    docs <- tm_map(docs, removeNumbers)                         # Remove numbers
    docs <- tm_map(docs, removeWords, stopwords("english"))     # Remove common english stopwords
    docs <- tm_map(docs, removeWords, customStop)               # Remove custom stopwords
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