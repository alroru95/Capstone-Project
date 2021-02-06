suppressPackageStartupMessages(c(
    library(stringr),
    library(stylo),
    library(tm)
))

source("./Word Predictor Function.R")

shinyServer(function(input, output) {
    nextWord <- reactive({
        text <- input$text
        cleanText <- cleanInput_text(text)
        WordCount <- length(cleanText)
        nextWord <- nextWordPred(WordCount,cleanText)})
    
    output$WordPrediction <- renderPrint(nextWord())
    output$inputWords <- renderText({input$text}, quoted = FALSE)
    
})