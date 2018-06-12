library(shiny)
library(PerformanceAnalytics)

ui <- fluidPage(
  fileInput("csvfile", "Choose CSV file", buttonLabel = "Browse", placeholder = "No file selected",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  checkboxInput("header", "Header", TRUE),
  uiOutput("variables"),
  tableOutput("corrmat")
)

server <- function(input, output) {
  filedata <- reactive({
    infile <- input$csvfile
    
    if (is.null(infile))
      return(NULL)
    
    read.csv(infile$datapath, header = input$header)
  })
  
  output$variables <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df[,16:21])
    names(items)=items
    selectInput("response", "Response variable",items)
    
  })
  
  output$corrmat <- renderTable({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    cor(df[,16:21])
  })
}

shinyApp(ui = ui, server = server)