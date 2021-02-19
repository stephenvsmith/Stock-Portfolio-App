# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Author: Stephen Smith

library(shiny)
library(pdfetch)

ui <- fluidPage(
   
   # Application title
   titlePanel("Stock Portfolio"),
   p("Created by Stephen Smith"),
   p("PhD Student, UCLA Statistics"),
   p("Data for this page is gathered from"),
   a(href="https://finance.yahoo.com/","Yahoo Finance"),
   sidebarLayout(

      sidebarPanel(
        dateRangeInput('dateRange',
                       label = 'Date range input:',
                       start = Sys.Date() - 31, end = Sys.Date(),
                       format = "yyyy-mm-dd"
        ),
        textInput('stockChoices','Stock Choices (separate with commas)','^GSPC'),
        selectInput('frequency',
                    'Frequency',
                    choices = c('Daily','Weekly','Monthly'),
                    selected = 'Monthly'),
        submitButton('Get Adjusted Close Prices'),
        downloadButton("download","Download Data Table"),
        
        textOutput('unaccepted'),
        textOutput('badtickers')
      ),
      
      
      
      mainPanel(
         dataTableOutput('table')
      )
   )
)


server <- function(input, output) {

  unaccepted <- reactive({
    
    stocks <- input$stockChoices
    stocks <- gsub("\\s", "", stocks)
    choices <- unlist(strsplit(stocks,","))
    
    int <- input$frequency
    if (int == "Daily"){
      int <- "1d"
    }else if (int == "Weekly"){
      int <- "1wk"
    } else int <- "1mo"
    
    n <- nrow(
      na.omit(
        pdfetch_YAHOO('^GSPC',
                      from = input$dateRange[1],
                      to=input$dateRange[2],
                      interval = int,
                      fields = 'adjclose')))
    unaccepted <- c()
    badtickers <- c()
    accepted <- c()
    for (i in choices) {
      if (i == "") next
      df <- pdfetch_YAHOO(i,
                          from = input$dateRange[1],
                          to=input$dateRange[2],
                          interval = int,
                          fields = 'adjclose')

      if (is.null(df)){
        badtickers <- c(badtickers,i)
      } else if (nrow(na.omit(df))!=n) { # Inappropriate submission
        unaccepted <- c(unaccepted,i)
      } else accepted <- c(accepted,i)
    }
    return(list("unaccepted"=unaccepted,"badtickers"=badtickers,"accepted"=accepted))
  })
  
  stockInput <- reactive({
    # Save appropriate tickers
    choices <- unaccepted()[["accepted"]]
    
    stockdf <- data.frame()
    int <- input$frequency
    if (int == "Daily"){
      int <- "1d"
    }else if (int == "Weekly"){
      int <- "1wk"
    } else int <- "1mo"
    
    for (i in choices) {
      df <- pdfetch_YAHOO(i,
                          from = input$dateRange[1],
                          to=input$dateRange[2],
                          interval = int,
                          fields = 'adjclose')
      df <- na.omit(df)
      
      if (i==choices[1]) stockdf <- data.frame("Date" = as.character(time(df)))
      stockdf[,i] <- df[,1]
    }
    return(stockdf)
  })
  output$table <- renderDataTable(stockInput())
  output$download <- downloadHandler(
    filename = "stockData.csv",
    content = function(file){
      write.csv(stockInput(),file)
    }
  )
  
  output$unaccepted <- renderText({
    tickers <- unaccepted()[[1]]
    
    if (length(tickers)>1) {
      val <- "There was insufficient data for "
      val2 <- paste(tickers,collapse = ", ")
      val3 <- ". These stocks were dropped from the final table."
      return(paste0(val,val2,val3))
    } else if (length(tickers)==1) {
      return(paste0("\nThere was insufficient data for\n",
                    tickers,
                    "\nThis stock was dropped from the final table."))
    } else return("")
  })
  
  output$badtickers <- renderText({
    tickers <- unaccepted()[[2]]
    
    if (length(tickers)>1) {
      val <- "The following tickers were not found: "
      val2 <- paste(tickers,collapse = ", ")
      val3 <- "."
      return(paste0(val,val2,val3))
    } else if (length(tickers)==1) {
      return(paste0("The following ticker was not found: ",tickers,"."))
    } else return("")
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

