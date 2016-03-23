shinyUI(pageWithSidebar(
    headerPanel("actionButton test"),
    sidebarPanel(
        textInput("speech", "Speech")
    ),
    mainPanel(
        verbatimTextOutput("nText")
    )
))