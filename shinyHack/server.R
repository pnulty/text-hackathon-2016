library(magrittr)
library(quanteda)
library(dplyr)
library(glmnet)
   
shinyServer(function(input, output) {
    load('CandidateN123a.01.RData')
    load('PartyN123a.01.RData')
    load('mainDfmN123MinCount5.RData')
    # additional custom stopwords
    extra <- c('going', 'said', 'need', 'know', 'say', 'many', 'now', 'will', 'actually', 'want', 'like',
               'mean', 'well', 'can')
    
    # make a new dfm from the input in the text area. 'keptFeatures' must be from the training dfm so that
    # new dfm has same same dimensions as the one the model was trained on
    liveParty <- reactive({
        newText <- dfm(input$speech,  ngrams=c(1,2,3))%>% dfm_select(mainDfm) %>% dfm_remove(c(stopwords('english'), extra)) %>% as.matrix
        print(dim(newText))
        #print(eModelMain$glmnet.fit$dim)
        resp <- as.numeric(predict(eModelParty, newText, s = "lambda.min", type = "response"))
        return(c(1-resp, resp))
    })
    liveMain <- reactive({
        newText <- dfm(input$speech, ngrams=c(1,2,3))%>% dfm_select(mainDfm) %>% dfm_remove(c(stopwords('english'), extra)) %>% as.matrix
        print(dim(newText))
        #print(eModelParty$glmnet.fit$dim)
        return(predict(eModelMain, newText, s = "lambda.min", type = "response"))
    })
    output$partyBar <- renderPlot({
    barplot(as.numeric(liveParty()), 
            main="Party", ylim=c(0,1), names.arg = c("Dem", "Rep"), col=c("Blue", "Red"))
    })
    output$mainBar <- renderPlot({
        barplot(as.numeric(liveMain()), 
                main="Candidate", ylim=c(0,1), names.arg = colnames(liveMain()))
    })
})