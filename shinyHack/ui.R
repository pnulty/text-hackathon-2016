shinyUI(fluidPage(
    headerPanel("Live multinomial logistic glmnet predictions"),
    sidebarPanel(
        tags$textarea(id = 'speech', placeholder = 'Type here', rows = 8,style="width:800px;", ""), width=12
    ),
    mainPanel(
        fluidRow(
            splitLayout(cellWidths = c("80%", "30%"), plotOutput("mainBar"), plotOutput("partyBar"))
        )
        
       # plotOutput("mainBar") ,
        #plotOutput("partyBar") 
    )
))


