# ui.R

shinyUI(fluidPage(
  tags$head(tags$style(
    "body { word-wrap: break-word; }"
  )),
  titlePanel("Welcome to Forecasting Project "),
  helpText("This project forecasts data, using different Forecasting Techniques."),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        
        column(5, wellPanel(
          selectInput("input_type", "Select Technique",
                      c("Mean", "Moving Average", "Exponential Smoothing", "Halt's Method",
                        "Halt Winter's Method","Simple Regression", "ARIMA", "ANN"
                      )
          )),
          fileInput('file1', 'Choose File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          #tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       '\t')
        ),
        column(5, wellPanel(
          # This outputs the dynamic UI component
          uiOutput("ui_1"),
          uiOutput("ui_2"),
          uiOutput("ui_3"),
          uiOutput("ui_4"),
          uiOutput("ui_5")
        ))
      ),
      fluidRow(
        column(4
        ),
        column(6,
               submitButton("Submit")
        )
        
      )
    ),
    
    mainPanel(
      fluidRow(
        column(5, 
               tableOutput('contents')
        ),
        column(5,
               lapply(1:50, function(i) {
                 uiOutput(paste(i))
               }),
               br(),
               htmlOutput("text1"),
               tags$style(type='text/css', '#text1 {background-color: rgb(130,222,253); padding:20px 20px 20px 20px;}')
        )
        
      ),
      fluidRow(plotOutput('dynamicPlot'))
    )
  )
))