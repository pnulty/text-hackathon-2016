require(magrittr)
require(quanteda)
require(dplyr)
shinyServer(function(input, output) {
    liveParty <- reactive({
        nt <- dfm(input$speech,  ngrams=c(1,2,3), keptFeatures = mainDfm) %>% removeFeatures(c(stopwords('english'), extra)) %>% as.matrix
        resp <- as.numeric(predict(eModelParty, nt, s = "lambda.min", type = "response"))
        return(c(1-resp, resp))
    })
    liveMain <- reactive({
        nt <- dfm(input$speech, ngrams=c(1,2,3), keptFeatures = mainDfm) %>% removeFeatures(c(stopwords('english'), extra)) %>% as.matrix
        return(predict(eModelMain, nt, s = "lambda.min", type = "response"))
    })

    # Render a barplot
    output$partyBar <- renderPlot({
    barplot(as.numeric(liveParty()), 
            main="Party", ylim=c(0,1), names.arg = c("Dem", "Rep"), col=c("Blue", "Red"))
})
    
    output$mainBar <- renderPlot({
        barplot(as.numeric(liveMain()), 
                main="Candidate", ylim=c(0,1), names.arg = colnames(liveMain()))
    })
    
    
})    