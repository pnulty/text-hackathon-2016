
shinyUI(fluidPage(
    headerPanel("Live text prediction"),
    sidebarPanel(
        tags$textarea(id = 'speech', placeholder = 'Type here', rows = 8,style="width:800px;", ""), width=12
    ),
    mainPanel(
        fluidRow(
            splitLayout(cellWidths = c("80%", "30%"), plotOutput("mainBar"), plotOutput("partyBar"))
        )
    )
))