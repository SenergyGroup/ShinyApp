library(shiny)
library(DT)
library(dplyr)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #upload call
      fileInput("user_input_file","Please select a file to upload", multiple = T),
      selectInput("us_manipulation_1", "Please select what you would like to do to your data",
                  choices = c("", "Round", "Use a formula", "Graph", "Merge"),
                  selected = NULL, multiple = FALSE, selectize = FALSE),
      conditionalPanel("input.us_manipulation_1 == 'Round'",
                       # Add a checkbox button group to select the variables to round
                       checkboxGroupInput("user_df_column", "Select the columns you want to round",
                                          choices = c("please upload data first"),
                                          selected = NULL)),
      conditionalPanel("input.us_manipulation_1 == 'Round'",
                       # Add a checkbox button group to select the variables to round
                       textInput("rounding_digits", "How many decimal places would you like your data to have", value = "")),
      
      
      textOutput(outputId = "us_selected_man_1")
    ),
    mainPanel(
      #name of user upload file
      textOutput(outputId = "fileName"),
      #display of user upload file
      tableOutput(outputId = "contents")
    )
  )
) 



server <- function(input, output){
  
  
  #This chunk loads the data and creates the filename object
  user_df <- reactive({
    req(input$user_input_file)
    # Loading in the user data
    if (endsWith(input$user_input_file$datapath, ".csv")) {
      user_df <- read.csv(input$user_input_file$datapath)
    } else if (endsWith(input$user_input_file$datapath, ".xlsx")) {
      user_df <-read_xlsx(input$user_input_file$datapath)
    } else if (endsWith(input$user_input_file$datapath, ".rdata")) {
      user_df <-load(input$user_input_file$datapath)
    } else if (endsWith(input$user_input_file$datapath, ".rds")) {
      user_df <-readRDS(input$user_input_file$datapath)
    } else {
      # If the file type is not recognized, return NULL
      return(NULL)
    }
  })
  
  rounded_df <- reactive({
    req(input$user_input_file)
    df <- user_df()
    if(!is.null(input$user_df_column) & input$rounding_digits != ""){
      # Select only numeric columns in df
      col_order <- names(user_df())
      cols_to_round <- c(input$user_df_column)
      #for whatever reason odd number of inputer$user_df_column throws an error
      if (length(cols_to_round) %% 2 != 0) {
        cols_to_round <- c(cols_to_round, cols_to_round[1])
      }
      other_cols <- col_order[!col_order %in% cols_to_round]
      if(length(other_cols) == 1){
        other_cols <- c(other_cols, other_cols)
      }
      
      df1<-round(df[,cols_to_round], as.numeric(input$rounding_digits))
      df2<- df[,other_cols]
      rounded_df <- bind_cols(df1,df2)[, col_order]
      return(rounded_df)
      
    } else {return(user_df())}
  })
  
  max_digits <-reactive({
    req(input$user_input_file)
    if(is.null(rounded_df)){
      df <- as.data.frame(user_df())
      max_digits <- as.numeric(max(nchar(as.character(round(
        df[, sapply(df, is.numeric)], digits = 0)))))
    } else {
      df <- as.data.frame(rounded_df())
      max_digits <- as.numeric(max(nchar(as.character(round(
        df[, sapply(df, is.numeric)], digits = 0)))))
    }
    if(max_digits > 10){
      max_digits <- 10
    }
    
  })
  
  #This is for when users select round
  #displays the rounded_df 
  if(is.null(rounded_df)){
    output$contents <- renderTable({
      req(input$user_input_file)
      #loading in the user data
      user_df()
    }, digits = max_digits)
  } else {
    output$contents <- renderTable({
      req(input$user_input_file)
      #loading in the user data
      rounded_df()
    },digits = max_digits)
  }
  
  observe({
    # Use the updateCheckboxGroupInput() function to update the choices
    # for the checkbox buttons based on the column names in the dataset
    updateCheckboxGroupInput(getDefaultReactiveDomain(), "user_df_column", choices = colnames(user_df()))
  })
  
  # Render the file name
  output$fileName <- renderText({
    req(input$user_input_file)
    file <- input$user_input_file
    if (is.null(file)) {
      return("No file selected")
    } else {
      return(file$name)
    }
  })
  
  
  
}
shinyApp(ui = ui, server = server)
