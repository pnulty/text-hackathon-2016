require(magrittr)
require(quanteda)
require(dplyr)
setwd("~/Dropbox/hackathon")
shinyServer(function(input, output) {
    live <- reactive({
        nt <- dfm(input$speech, keptFeatures = repDfm) %>% removeFeatures(c(stopwords('english'), extra)) %>% as.matrix
        return(predict(eModelTrump, nt, s = "lambda.min", type = "response"))
    })
    #output$nText <- renderText({
     #   live()
#    })
    # Render a barplot
    output$trumpBar <- renderPlot({
    barplot(as.numeric(live()), 
            main="Trumpiness", ylim=c(0,1))
})
})    