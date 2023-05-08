library(shiny)
library(DT)
library(tidyverse)
library(shinyalert)
library(shinyvalidate)
library(shinyFeedback)


load("data/zp_example.RData")

## Source functions to perform data checks and categorical variable identification
source("source/func/initial_data_check.R")
source("source/func/get_categorical_variables.R")
source("source/func/check_selected_variables.R")


server <- function(input, output, session) {
  
  ###
  # NAVIGATION ----
  ###
  tab <- reactiveValues(page = 1, min = 1, max = 7)
  
  observe({
    toggleState(id = "prevBtn_1", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_2", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_3", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_4", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_5", condition = tab$page > tab$min)
    toggleState(id = "nextBtn_1", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_2", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_3", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_4", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_5", condition = tab$page < tab$max)
  })
  
  observe({
    if (input$mynavlist == "home") tab$page = 1
    if (input$mynavlist == "dataupload") tab$page = 2
    if (input$mynavlist == "missing") tab$page = 3
    if (input$mynavlist == "psmodel") tab$page = 4
    if (input$mynavlist == "psresults") tab$page = 5
    if (input$mynavlist == "cfmethod") tab$page = 6
    if (input$mynavlist == "results") tab$page = 7
  })
  
  navPage <- function(direction) {
    if("None" %in% input$psm & ((tab$page == 4 & direction>0)|(tab$page == 6 & direction<0))){ 
      tab$page <- tab$page + 2*direction 
    } else {
      tab$page <- tab$page + direction
    }
  }
  
  observeEvent(input$nextBtn_1, {
    
    ## Remove error message if any present from previous upload
    feedbackDanger("file1", show = FALSE)
    feedbackDanger("outcome", show = FALSE)
    feedbackDanger("treatment", show = FALSE)
    feedbackDanger("matchvars", show = FALSE)
    
    ## Create variables to track issues with input
    required_input_missing <- FALSE
    required_input_missmatched <- FALSE
    
    ## Check if all required input has been specified, if not give error
    if (is.null(input$outcome) | is.null(input$treatment) | is.null(input$matchvars)){
      
      required_input_missing <- TRUE
      if (is.null(input$outcome)){feedbackDanger("outcome", show = TRUE, "Please select outcome before proceeding")}
      if (is.null(input$treatment)){feedbackDanger("treatment", show = TRUE, "Please select treatment before proceeding")}
      if (is.null(input$matchvars)){feedbackDanger("matchvars", show = TRUE, "Please select matching variables before proceeding")}
    }
    
    ## If all required input selected, check if there are input conflicts
    if (!required_input_missing){
      if (input$outcome == input$treatment | input$outcome %in% input$matchvars | input$treatment %in% input$matchvars){
        
        required_input_missmatched <- TRUE
        if (input$outcome == input$treatment){feedbackDanger("treatment", show = TRUE, "Outcome and treatment cannot be the same")}
        if (input$outcome %in% input$matchvars){feedbackDanger("matchvars", show = TRUE, "Outcome and matching variables cannot be the same")}
        if (input$treatment %in% input$matchvars){feedbackDanger("matchvars", show = TRUE, "Treatment and matching variables cannot be the same")}
      }
    }
    
    ## If there are no input issues, proceed
    if(!required_input_missmatched & !required_input_missing){
      
      navPage(1)
      updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
      cat(tab$page)
      
    }
  })
  
  observeEvent(input$prevBtn_1 |  input$prevBtn_2 |  input$prevBtn_3 | input$prevBtn_4 | input$prevBtn_5, {
    navPage(-1)
    updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
    cat(tab$page)
  })
  
  observeEvent(input$start | input$nextBtn_2 |  input$nextBtn_3 | input$nextBtn_4 | input$nextBtn_5, {
    navPage(1)
    updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
    cat(tab$page)
  })
  
  ## If input variable(s) is changed, remove any warnings that may be present for variable selection
  observeEvent(c(input$outcome, input$treatment, input$matchvars), {
    feedbackDanger("outcome", show = FALSE)
    feedbackDanger("treatment", show = FALSE)
    feedbackDanger("matchvars", show = FALSE)
  })
  
  ####
  # DATA UPLOAD ----
  ####
  
  ## Save data as a reactive variable
  inputData <- reactiveValues()
  inputData$rawdata <- NULL
  inputData$source <- NULL
  
  ## Update app when file uploaded
  observeEvent(input$file1, {
    
    ## Remove error message if any present from previous upload
    feedbackDanger("file1", show = FALSE)
    feedbackDanger("outcome", show = FALSE)
    feedbackDanger("treatment", show = FALSE)
    feedbackDanger("matchvars", show = FALSE)
    
    ## Save data source
    inputData$source <- "own"
    
    inputData$rawdata <- read.csv(input$file1$datapath)
  })
  observeEvent(input$Btn_sampledata, {
    inputData$rawdata <- read.csv("data/zp_eg.csv")
  })
  
  observe({
    if(!is.null(inputData$rawdata)){
      updatePickerInput(session, "outcome", choices = names(isolate(inputData$rawdata)))
      updatePickerInput(session, "treatment", choices = names(isolate(inputData$rawdata)))
      updatePickerInput(session, "matchvars", choices = names(isolate(inputData$rawdata)))
      updatePickerInput(session, "covars", choices = names(isolate(inputData$rawdata)))
    }
    if(input$Btn_sampledata){
      
      updatePickerInput(session, "outcome", selected="Anxiety_age17")
      updatePickerInput(session, "treatment", selected="Reading_age15")
      updatePickerInput(session, "matchvars", selected=names(read.csv("data/zp_eg.csv"))[-c(2:3)])
      updatePickerInput(session, "covars", choices = names(read.csv("data/zp_eg.csv"))[-c(2:3)])
        size = "l",
        footer=tagList(
          actionButton('recode_NA', 'Recode as "NA"'),
          modalButton('Continue'))))
      
      ## If "recode as NA" selected remove all "-999" values from data
      observeEvent(input$recode_NA, {
        inputData$rawdata[inputData$rawdata == -999] <- NA
        removeModal() ## remove modal
      })
    }
    
    ## Get variable classes
    categorical_variables <- get_categorical_variables(inputData$rawdata)
    continuous_variables <- names(isolate(inputData$rawdata))[!names(isolate(inputData$rawdata)) %in% categorical_variables]
    
    ## Reset variable inputs
    updatePickerInput(session, "categorical_vars", choices = names(isolate(inputData$rawdata)), selected=categorical_variables)
    updatePickerInput(session, "outcome", choices=continuous_variables, selected = NULL)
    updatePickerInput(session, "treatment", choices=names(isolate(inputData$rawdata)), selected = NULL)
    updatePickerInput(session, "matchvars", choices=names(isolate(inputData$rawdata)), selected = NULL, clearOptions = TRUE)
    updatePickerInput(session, "covars", choices=names(isolate(inputData$rawdata)), selected = NULL, clearOptions = TRUE)
  })
  
  ## When categorical variable selection changed, update what can be selected as the outcome variable
  observeEvent(input$categorical_vars, {
    
    if(inputData$source == "sample"){}else{
      ## Get names of continuous variables
      continuous_variables <- names(isolate(inputData$rawdata))[!names(isolate(inputData$rawdata)) %in% input$categorical_vars]
      ## Only allow selection from continuous variables
      updatePickerInput(session, "outcome", selected=NULL, choices = continuous_variables) 
      ## Clear all following pickers
      updatePickerInput(session, "treatment", selected=character(0))
      updatePickerInput(session, "matchvars", selected=character(0))
      updatePickerInput(session, "covars", selected=character(0))
    }
  })
  
  
  ## Update app when sample data selected
  observeEvent(input$Btn_sampledata, {
    
    ## Save data source
    inputData$source <- "sample"
    
    ## Remove error message if any present from previous upload
    feedbackDanger("file1", show = FALSE)
    feedbackDanger("outcome", show = FALSE)
    feedbackDanger("treatment", show = FALSE)
    feedbackDanger("matchvars", show = FALSE)
    
    ## If "sample data" is selected, upload sample data
    inputData$rawdata <- read.csv("data/zp_eg.csv")
    
    ## Update variable selection
    updatePickerInput(session, "categorical_vars", choices = c("Gender", "Reading_age15", "SubstanceUse1_age13", 
                                                               "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13"), 
                      selected=c("Gender", "Reading_age15", "SubstanceUse1_age13", 
                                 "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13"))
    updatePickerInput(session, "outcome", choices = "Anxiety_age17", selected="Anxiety_age17")
    updatePickerInput(session, "treatment", choices = "Reading_age15", selected="Reading_age15")
    updatePickerInput(session, "matchvars", choices = names(isolate(inputData$rawdata))[-c(2:3)], selected=names(isolate(inputData$rawdata))[-c(2:3)])
    updatePickerInput(session, "covars", choices = names(isolate(inputData$rawdata))[-c(2:3)])
  })
  
  output$contents <- DT::renderDataTable({
    DT::datatable(inputData$rawdata, options = list(scrollX = TRUE), style = "bootstrap", selection = "none")
  })
  
  
  # Source server side 
  # source("source/server_missing.R",local=T)
  
  
}