#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

searchsource <- read.csv(file="searchsource4.csv")[,-1]
nextword <- function(s,data=searchsource){
  s <- as.character(s)
  termsindex <- grep(paste("^",s,sep=""),searchsource$term,value=FALSE)
  term <- termsindex[which.max(searchsource[termsindex,"freq"])]
  if(length(term)<1) {
    value <- NULL
  } else {
    tValue <- as.character(searchsource[termsindex[which.max(searchsource[termsindex,"freq"])],1])
    value <- strsplit(tValue,split=" ")[[1]][2]
  }
  return(value)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  resultText <- reactive(nextword(input$text))
  
  output$textResult <- renderText({resultText()})
  
})
