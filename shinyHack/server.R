setwd("~/Dropbox/hackathon")
load(file='trumpModel.RData')
shinyServer(function(input, output) {
    live <- reactive({
        nt <- dfm(input$speech, keptFeatures = repDfm) %>% removeFeatures(c(stopwords('english'), extra)) %>% as.matrix
        return(predict(eModel, nt, s = "lambda.min", type = "response"))
    })
    output$nText <- renderText({
        live()
    })
})