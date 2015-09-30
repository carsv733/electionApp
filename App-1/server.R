
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  counts <- tapply(myData$Percent, list(as.factor(myData$Party), as.factor(myData$Kommun)), mean)
  colours <- c("green","pink", "blue", "purple","darkblue", "seagreen","red" ,"lightblue",  "darkred")

  
 distplot <- eventReactive(input$action, {
   if (10 %in% input$select) { 
     
     barplot(counts, main="Votes, by municipal and party",
             xlab="Municipal", col=colours,
             legend = rownames(counts) , beside=TRUE)
     
   } else {
     
     partyIndex <- 1:9
     toDelete <- partyIndex[-as.numeric(input$select)]
     
     counts <- counts[-toDelete,]
     
     
     barplot(counts, main="Votes, by municipal and party",
             xlab="Municipal", col=colours[seq(length(input$select))],
             legend = rownames(counts) , beside=TRUE)
   }
  })
  
  output$distPlot <- renderPlot({
    
  distplot()
    
  })
})