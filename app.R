library(shiny)
library(PerformanceAnalytics)

ui <- fluidPage(
  titlePanel("Estimate above ground biomass (AGB) from lidar data"),

  sidebarPanel(
    h2("Open data file"),
    fileInput("csvfile", "Choose a CSV file to begin", buttonLabel = "Browse", placeholder = "No file selected",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    h2("Select variables for model"),
    uiOutput("response"),
    uiOutput("predictor"),
    h2("Model"),
    actionButton("lmbutton", "Linear model"), 
    actionButton("rfbutton", "RF model")
  ),
  mainPanel(
    h2("Model fit"),
    plotOutput("model"),
    verbatimTextOutput("summary")
  )
)

server <- function(input, output) {
  filedata <- reactive({
    infile <- input$csvfile
    
    if (is.null(infile))
      return(NULL)
    
    read.csv(infile$datapath, header = input$header)
  })
  
  output$response <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("response", "Response variable",items)
    
  })
  
  output$predictor <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("predictor", "Predictor variable",items)
    
  })
  
  observeEvent(input$lmbutton, {
    df <- filedata()
    if (is.null(df)) return(NULL)
    lm1 <- lm(df[,input$response] ~ df[,input$predictor])
    lm1sum <- summary(lm1)
    
    output$model <- renderPlot({
      plot(df[,input$predictor], df[,input$response], xlab=input$predictor, ylab=input$response,
           main=NA)
      abline(lm1$coefficients[1], lm1$coefficients[2])
      })
    
    output$summary <- renderPrint({
      summary(lm1)
    })
  })
  
  observeEvent(input$rfbutton, {
    
  })
}

shinyApp(ui = ui, server = server)