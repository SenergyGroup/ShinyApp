library(shiny)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
# options(shiny.trace=T)

server <- shinyServer(function(input,output) {
  
  ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
  # this reactive output display the content of the input$file dataframe
  output$filedf <- renderTable({
    if(is.null(input$file)){return ()}
    input$file # the file input data frame object that contains the file attributes
    
  })
  
  # Extract the file path for file
  output$filedf2 <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath # the file input data frame object that contains the file attributes
    
  })
  
  ## Below code to display the structure of the input file object
  output$fileob <- renderPrint({
    if(is.null(input$file)){return ()}
    str(input$file)
    
  })
  
  
  # Merging the data files using rbind
  # assumption that all files have same columns
  datamerge <- reactive({
    
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      csv = list()
      for (i in 1 : nfiles)
      {
        
        csv[[i]] = read.csv(input$file$datapath[i])
      }
      do.call(rbind, csv) # rbind the datasets
      
    }
  })
  
  ## Display the merged data
  output$newdata <- renderTable({
    datamerge()
  })
  
  
  
  
  ## DownloadHandler to download the merged dataset to local system
  output$download <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      fileext = switch(input$sep,
                       "," = "csv",
                       ";" = "csv",
                       "\t" = "txt",
                       " " = "doc")
      paste("merged", fileext, sep = ".") # example : iris.csv, iris.doc, iris.txt
      
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      # Write to a file specified by the 'file' argument
      write.table(datamerge(), file, sep = input$sep,
                  row.names = FALSE)
    }
  )
  
  
  
  
  ## Side bar select input widget coming through renderUI()
  # Following code displays the select input widget with the list of file loaded by the user
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Select the files for which you need to see data and summary stats"),
         selectInput("Select", "Select", choices=input$file$name)
    )
    
    
  })
  
  ## Summary Stats code ##
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$summ <- renderPrint({
    if(is.null(input$file)){return()}
    summary(read.table(file=input$file$datapath[input$file$name==input$Select], 
                       sep=input$sep, 
                       header = input$header, 
                       stringsAsFactors = input$stringAsFactors))})
  
  
  
  ## Dataset code ##
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({ 
    if(is.null(input$file)){return()}
    read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  ## MainPanel tabset renderUI code ##
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
  # Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Input File Object DF ", tableOutput("filedf"), tableOutput("filedf2")),
        tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
        tabPanel("Dataset", tableOutput("table")),
        tabPanel("Summary Stats", verbatimTextOutput("summ")),
        tabPanel("Merged Dataset", downloadButton("download", "Download" ), tableOutput("newdata"))
      )
  })
})



