library(shiny)
library(raster)

ui <- fluidPage(
  titlePanel("Estimate above ground biomass (AGB) from lidar data"),

  fluidRow(
    column(3,
      h3("Variable selection"),
      fileInput("csvfile", "Choose a CSV file to begin", buttonLabel = "Browse", placeholder = "No file selected",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("header", "Header", TRUE),
      
      h3("Select variables for model"),
      uiOutput("response"),
      uiOutput("predictor"),
      
      h3("Predict AGB"),
      fileInput("rasfile", "Choose a raster file", buttonLabel = "Browse", placeholder = "No file selected"),
      # actionButton("predbutton", "Predict"),
      
      h3("Model"),
      actionButton("lmbutton", "Linear model"), 
      actionButton("nlbutton", "Non-linear model")
      
    ),
    column(9,
           fluidRow(
             column(6,
                    h3("Model fit"),
                    plotOutput("model")
                    ),
             column(3,
                    h3("Model summary"),
                    verbatimTextOutput("summary")
                    )
             ),
           fluidRow(
             column(6,
                    h3("Model predictions"),
                    plotOutput("map")
                    ),
             column(3,
                    h3("Prediction summary"),
                    verbatimTextOutput("predtext")
                    )
             )
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=10*1024^2)
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
    names(lm1$coefficients) <- c("(Intercept)", input$predictor)
    lm1sum <- summary(lm1)
    rasfile <- input$rasfile
    rasdat <- brick(rasfile$datapath)
    names(rasdat) <- input$predictor
    raspred <- lm1$coefficients[1][1] + rasdat * lm1$coefficients[2][1]
    agbinfo <- list(mean=paste("Mean AGB (Mg/ha)", as.character(cellStats(raspred, 'mean'))),
                    sum=paste("Total AGB (Mg)", 
                              as.character(cellStats(raspred, 'sum')*xres(raspred)*yres(raspred)*0.0001)))
    
    output$model <- renderPlot({
      plot(df[,input$predictor], df[,input$response], xlab=input$predictor, ylab=input$response,
           main=NA)
      abline(lm1$coefficients[1], lm1$coefficients[2])
      })
    
    output$summary <- renderPrint({
      summary(lm1)
    })
    
    output$map <- renderPlot({
      plot(raspred)
    })
    
    output$predtext <- renderPrint({
      agbinfo
    })
  })
  
  observeEvent(input$nlbutton, {
    df <- filedata()
    if (is.null(df)) return(NULL)
    mod.df <- data.frame(x=df[,input$predictor], y=df[,input$response])
    nl1 <- nls(y ~ a * x^b,
               data=mod.df,
               start=list(a=0.1,b=1.01))
    nl1sum <- summary(lm1)
    rasfile <- input$rasfile
    rasdat <- brick(rasfile$datapath)
    names(rasdat) <- input$predictor
    raspred <- coef(nl1)[1] * rasdat^coef(nl1)[2]
    agbinfo <- list(mean=paste("Mean AGB (Mg/ha)", as.character(cellStats(raspred, 'mean'))),
                    sum=paste("Total AGB (Mg)", 
                              as.character(cellStats(raspred, 'sum')*xres(raspred)*yres(raspred)*0.0001)))
    
    output$model <- renderPlot({
      predictor.df <- data.frame(x=seq(0,round(max(mod.df$x, na.rm=T))+1,1))
      predagb <- predict(nl1, newdata=predictor.df)
      plot(mod.df$x, mod.df$y, xlab=input$predictor, ylab=input$response,
           main=NA)
      lines(predictor.df$x, predagb)
    })
    
    output$summary <- renderPrint({
      summary(nl1)
    })
    
    output$map <- renderPlot({
      plot(raspred)
    })
    
    output$predtext <- renderPrint({
      agbinfo
    })
  })
  
}

shinyApp(ui = ui, server = server)