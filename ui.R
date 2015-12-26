shinyUI(pageWithSidebar(
  headerPanel(h2("Linear regression in mtcars dataset")),
  sidebarPanel(
    uiOutput("predictors"),
    actionButton("calculate", label = "Estimate model"),
    width=4
  ), 
  mainPanel(
    textOutput('text'),    
    dataTableOutput('table'),
    plotOutput('model'),
    textOutput('r2'),
    dataTableOutput('tablevif')
  )
))