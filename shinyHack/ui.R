
shinyUI(fluidPage(
    headerPanel(h6("Live text prediction")),
    sidebarPanel(
        tags$textarea(id = 'speech', placeholder = 'Type here', rows = 8,style="width:800px;", ""), 
        h6('Live text category prediction using elastic net regression trained on 2016 US Presidential candidate 
           debates. Hackathon Entry by Paul Nulty, for Visual Text Analytics Workshop 2016 at LSE.',
           a(href="https://github.com/pnulty/text-hackathon-2016", "Code and slides. \n"),
           a(href="http://www.textasdata.com/2016/03/visualization-workshop-hackathon-challenge/",
             "Dataset for challenge")),
        width=12
    ),
    mainPanel(
        fluidRow(
            splitLayout(cellWidths = c("80%", "30%"), plotOutput("mainBar"), plotOutput("partyBar"))
        )
    )
))