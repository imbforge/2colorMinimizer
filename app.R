options(shiny.maxRequestSize=100*1024^2) # set max upload size to 100MB
options(stringsAsFactors=FALSE)
library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("2-color distance minimizer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("uploadTable", "Choose tab separated file", accept=c("text/tab-separated-values", "text/plain", ".tsv", ".txt")),
        downloadButton("downloadMinimized", "Download minimized data")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Original data", DT::dataTableOutput("tableOriginal")),
          tabPanel("Minimized", DT::dataTableOutput("tableMinimized"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #' Stop app when session is ended (when user closes the browser)
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ##
  ## Helper functions
  ##
  #' 2 color distance minimizer
  #' 
  #' Minimizes 2 color distances in the same cell.
  #' 
  #' @param x input data.frame, expected to have the folowing columns:
  #'             -"Green Red spot pairs - distance"
  #'             -"Green Red spot pairs - GR_NucIndex"
  #'             -"Green Red spot pairs - GR_Spot1Index"
  #'             -"Green Red spot pairs - GR_Spot2Index"
  #' @return the minimized dataset
  minimize <- function(x) {
    # get columns containing the color spot information
    colorCols <- list(distance  =colnames(x)[grepl("\\w+ \\w+ spot pairs - distance"     , colnames(x))],
                      DistIndex =colnames(x)[grepl("\\w+ \\w+ spot pairs - .+_DistIndex" , colnames(x))],
                      NucIndex  =colnames(x)[grepl("\\w+ \\w+ spot pairs - .+_NucIndex"  , colnames(x))],
                      Spot1Index=colnames(x)[grepl("\\w+ \\w+ spot pairs - .+_Spot1Index", colnames(x))],
                      Spot2Index=colnames(x)[grepl("\\w+ \\w+ spot pairs - .+_Spot2Index", colnames(x))])
    
    # validate data is properly formated
    keyCols <- c("Row", "Column", "Timepoint", "Field", colorCols[["NucIndex"]], "experiment")
    if(!is.data.frame(x) |
       !all(keyCols   %in% colnames(x)) |
       !all(colorCols %in% colnames(x))
    ) {
      return(NULL)
    }
    
    # remove incomplete cases
    x <- withProgress(message="removing incomplete cases", value=0, {
      x <- x[x[, colorCols[["Spot1Index"]]] %in% 1:2 & x[, colorCols[["Spot2Index"]]] %in% 1:2, ]
      x$key <- apply(x[, keyCols], 1, paste, collapse=" - ")
      completeKeys <- table(x$key)
      completeKeys <- names(completeKeys[completeKeys == 4])
      x[x$key %in% completeKeys, ]
    })
   
    #  minimize distances within each cell
    x.min <- withProgress(message="minimizing distances", value=1, {
      data.table::rbindlist(mclapply(split(x, x$key), function(x) {  # this guy runs in parallel
        data.table::rbindlist(lapply(split(x, x[, colorCols[["Spot1Index"]]]), function(x) {
          x[which.min(x[, colorCols[["distance"]]]), , drop=FALSE]
        }))
      }, mc.cores=getOption("mc.cores", 4L))) # use 4 cores by default
    })
    x.min <- as.data.frame(x.min)
    
    x.min[order(x.min$experiment, x.min$key), !grepl("^\\bkey\\b$", colnames(x.min))]
  }

  ##
  ## reactive content/UI fill
  ##
  #' Read in uploaded TSV file
  #' 
  #' Reads the file uploaded to the server, from input$uploadTable. Needs the file
  #' to be TSV (no checks, read.delim directly called).
  #' 
  #' @return a data.frame with the read data. Column names are not checked.
  originalData <- reactive({
    if(is.null(input$uploadTable))
      return(NULL)
    else
      withProgress(message="reading input file", value=0, {
        read.delim(input$uploadTable$datapath, check.names=FALSE)
      })
  })
  
  #' Minimize the distances from the original table
  #' 
  #' Minimize the distances from the original tables, removing incomplete cases
  #' and cells with different numbers of green/red spots detected.
  #' 
  #' @return a data.frame with the minimized data.
  minimizedData <- reactive({
    if(is.null(originalData()))
      return(NULL)
    else
      minimize(originalData())
  })
  
  #' Original data uploaded by the user
  #' 
  #' Fills out the UI space for the original data uploaded by the user. Since the
  #' table will usually be huge, it's stored on the server side. User cannot
  #' interact with the table (no sorting/filtering).
  output$tableOriginal <- DT::renderDataTable({
    originalData()
  }, rownames=FALSE, options=list(searching=FALSE, ordering=FALSE, pageLength=25))
  
  #' Minimized data
  #' 
  #' Fills out the UI space for the minimized data uploaded by the user. Since the
  #' table will usually be huge, it's stored on the server side. User cannot
  #' interact with the table (no sorting/filtering).
  output$tableMinimized <- DT::renderDataTable({
    minimizedData()
  }, rownames=FALSE, options=list(searching=FALSE, ordering=FALSE, pageLength=25))
  
  #' Download minimized data
  #' 
  #' Download minimized data as tab-separated file
  output$downloadMinimized <- downloadHandler(
    filename="output.txt",
    content=function(file) {
      write.table(minimizedData(), file, sep="\t", row.names=FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
