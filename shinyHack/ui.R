
shinyUI(fluidPage(
    headerPanel("Live text prediction"),
    sidebarPanel(
        tags$textarea(id = 'speech', placeholder = 'Type here', rows = 8,style="width:800px;", ""), 
        width=12
    ),
    mainPanel(
        fluidRow(
            splitLayout(cellWidths = c("80%", "30%"), plotOutput("mainBar"), plotOutput("partyBar"))
        ),
        h6('Hackathon Entry by Paul Nulty, for Visual Text Analytics Workshop 2016 at LSE.',
           a(href="https://github.com/pnulty/text-hackathon-2016", "Code and slides here."))
    )
))