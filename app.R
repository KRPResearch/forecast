#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('TextFile', 'Choose Text file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain'
                )
      ),
      tags$hr(),
      radioButtons('skipper', 'Lines to skip',
                   c(Zero=0,
                     One=1
                   )),
      tags$hr(),
      numericInput('year','Enter Starting Year',value =2018 ),
      tags$hr(),
      numericInput('month','Enter Starting month',value=01),
      tags$hr(),
      numericInput('frequency','Enter Frequency',value=12)
    ),
    
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      fluidRow("ARIMA",
               splitLayout(cellWidths = c("50%", "30%","20%"), 
                           plotOutput("contents1"), plotOutput("contents2"),
                           verbatimTextOutput("contents3"))
               
      ))
  )
)

# Define server logic required to draw a histogram
library(forecast)
server <- function(input, output) {
  data_l<-reactive({
    inFile <- input$TextFile
    if (is.null(inFile))
      return(NULL)
    data<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
             frequency = input$frequency)
    return(data)
  })
  
  output$contents1 <- renderPlot({
    data<-data_l()
    M2=auto.arima(data)
    M2F=forecast(M2,h=12)
    plot(M2F,main="ARIMA Forecast")
    
  })
  output$contents2 <- renderPlot({
    data<-data_l()
    tsdisplay(diff(data,lag=1,differences = 1),lag.max=12,main="Is Data Stationary?")  })
  
  output$contents3 = renderPrint({
    data<-data_l()
    M2=auto.arima(data)
    summary(M2)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

