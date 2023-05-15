load("data/zp_example.RData")

## Source functions to perform data checks and categorical variable identification
source("source/func/initial_data_check.R")
source("source/func/get_categorical_variables.R")
source("source/func/get_descriptives.R")
source("source/func/reset_upload_page.R")
source("source/func/check_selected_variables.R")

server <- function(input, output, session) {
  
  ###
  # NAVIGATION ----
  ###
  tab <- reactiveValues(page = 1, min = 1, max = 5)
  
  observe({
    toggleState(id = "prevDU_btn", condition = tab$page > tab$min)
    toggleState(id = "prevCM_btn", condition = tab$page > tab$min)
    toggleState(id = "prevPSR_btn", condition = tab$page > tab$min)
    toggleState(id = "nextDU_btn", condition = tab$page < tab$max)
    toggleState(id = "nextCM_btn", condition = tab$page < tab$max)
    toggleState(id = "nextPSR_btn", condition = tab$page < tab$max)
  })
  
  observe({
    if (input$mynavlist == "home") tab$page = 1
    if (input$mynavlist == "dataupload") tab$page = 2
    if (input$mynavlist == "method") tab$page = 3
    if (input$mynavlist == "psresults") tab$page = 4
    if (input$mynavlist == "results") tab$page = 5
  })
  
  navPage <- function(direction) {
    tab$page <- tab$page + direction
  }
  
  observeEvent(input$prevDU_btn | input$prevCM_btn | input$prevPSR_btn, {
    navPage(-1)
    updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
    cat(tab$page)
  })
  observeEvent(input$start_btn | input$nextCM_btn | input$nextPSR_btn, {
    navPage(1)
    updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
    cat(tab$page)
  })
  
  
  observeEvent(input$nextDU_btn, {
    # Remove error message if any present from previous upload
    reset_upload_page(hide_descriptives = FALSE)

    ## Check inputed variables. If there is an issue give informative error message, otherwise, continue to next page
    ## First check if data has been uploaded
    if (!isTruthy(inputData$rawdata)) { ## If there is no data, give informative error
      feedbackDanger(inputId = "file1", show=TRUE, text = "Upload data and pick variables")

    }else{variable_check_info <- check_selected_variables(outcome = input$outcome, treatment = input$treatment, matchvars = input$matchvars, covars = input$covars)
    ## If there is no missing data and no variable mismatched, proceed to next tab
      if(all(!c(variable_check_info$required_input_missmatched, variable_check_info$required_input_missing))){
        navPage(1)
        updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
        cat(tab$page)
      }
    }
  })
  
  
  ## If input variable(s) is changed, remove any warnings that may be present for variable selection
  observeEvent(c(input$outcome, input$treatment, input$matchvars, input$covars), {
    reset_upload_page(reset_errors = TRUE)
  })
  
  
  ####
  # DATA UPLOAD ----
  ####
  
  ## Hide descriptives tab initially
  hideTab(inputId = "Tab_data", target = "descriptives")
  
  inputData <- reactiveValues(rawdata=NULL)
  
  observeEvent(input$file1, {
    inputData$rawdata <- read.csv(input$file1$datapath)
  })
  observeEvent(input$Btn_sampledata, {
    inputData$rawdata <- na.omit(read.csv("data/zp_eg.csv"))
  })
  
  ## If input variable(s) is changed, remove any warnings that may be present for variable selection
  observeEvent(c(input$outcome, input$treatment, input$matchvars, input$covars), {
    reset_upload_page(reset_errors = TRUE)
  })
  
  ## Hide descriptives tab
  hideTab(inputId = "Tab_data", target = "descriptives")
  
  ## Save data as a reactive variable
  inputData <- reactiveValues(rawdata = NULL, descriptives = NULL, source = NULL)
  
  ## Update app when file uploaded
  observeEvent(input$file1, {
    
    ## Reset any input errors
    reset_upload_page(reset_errors = TRUE)
    
    ## Go back to raw data view and delete data descriptive
    updateTabsetPanel(session, "Tab_data",selected = "raw_data")
    inputData$descriptives <- NULL
    
    ## Save data source
    inputData$source <- "own"
    
    ## Reset inputs that changes data 
    reset("recode_NA", asis = TRUE)
    suppressWarnings(rm(categorical_variables, warn))
    
    ## Load in own data
    inputData$rawdata <- read.csv(input$file1$datapath)
    
    ## Check data upon upload
    initial_data_check_ls <- initial_data_check(inputData$rawdata)
    
    ## If data is too small or contains non numeric values, give error message and delete
    if(initial_data_check_ls$too_small){
      feedbackDanger(inputId = "file1",
                     show=initial_data_check_ls$too_small,
                     text = "Data too small! (<10 rows)")
    }
    
    if(initial_data_check_ls$some_nonnumeric){
      feedbackDanger(inputId = "file1",
                     show=initial_data_check_ls$some_nonnumeric,
                     text = "Non numeric values detected!")
    }
    
    if(any(c(initial_data_check_ls$too_small, initial_data_check_ls$some_nonnumeric))){
      ## Remove uploaded data and list with initial data checks
      inputData$rawdata <- NULL
      initial_data_check_ls <- list(some_nonnumeric = FALSE,
                                    impossible_value = FALSE,
                                    too_small = FALSE)
    }
    
    
    ## If data contains contains "-999" give warning and option to recode as NA
    if(initial_data_check_ls$impossible_value == TRUE){
      
      showModal(modalDialog(
        title = 'Warning: "-999" value detected in data',
        'Would you like to recode "-999" as "NA" or continue without alteration?',
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
    
    ## Reset any input errors and hide descriptives (these depend on categorical var selection)
    reset_upload_page(reset_errors = TRUE, hide_descriptives = TRUE)
    
    ## Go back to raw data view and delete data descriptive
    updateTabsetPanel(session, "Tab_data",selected = "raw_data")
    inputData$descriptives <- NULL
    
    if(inputData$source == "sample"){
      
    }else{
      ## Get names of continuous variables
      continuous_variables <- names(isolate(inputData$rawdata))[!names(isolate(inputData$rawdata)) %in% input$categorical_vars]
      ## Only allow selection from continuous variables
      updatePickerInput(session, "outcome", selected=NULL, choices = continuous_variables) 
      ## Clear all following pickers
      updatePickerInput(session, "treatment", selected=character(0))
      updatePickerInput(session, "matchvars", selected=character(0))
      updatePickerInput(session, "covars", selected=character(0))
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  ## Update app when sample data selected
  observeEvent(input$Btn_sampledata, {
    
    ## Reset any input errors and hide descriptives tab
    reset_upload_page(reset_errors = TRUE)
    
    ## Save data source
    inputData$source <- "sample"
    
    ## If "sample data" is selected, upload sample data
    inputData$rawdata <- na.omit(read.csv("data/zp_eg.csv"))
    
    ## Update variable selection
    updatePickerInput(session, "categorical_vars", selected=c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13"), choices = names(isolate(inputData$rawdata)))
    updatePickerInput(session, "outcome", selected="Anxiety_age17", choices = names(isolate(inputData$rawdata))[!names(isolate(inputData$rawdata)) %in% c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13")])
    updatePickerInput(session, "treatment", selected="Reading_age15", choices = names(isolate(inputData$rawdata)))
    updatePickerInput(session, "matchvars", selected=names(isolate(inputData$rawdata))[-c(2:3)], choices = names(isolate(inputData$rawdata)))
    updatePickerInput(session, "covars", choices = names(isolate(inputData$rawdata)))
  })
  
  ## Generate data descriptive and switch to descriptive tab
  observeEvent(input$Btn_descriptives, {
    
    ## Check if data has been uploaded
    if (isTruthy(inputData$rawdata)) { ## `If so, generate descriptives and move to descriptives tab
      ## Get descriptive
      inputData$descriptives <- get_description(inputData$rawdata, input$categorical_vars)
      ## Show and switch to descriptive tab
      showTab(inputId = "Tab_data", target = "descriptives", select = FALSE, session = getDefaultReactiveDomain())
      updateTabsetPanel(session, "Tab_data", selected = "descriptives")
    }else{  ## Otherwise, give error
      feedbackDanger(inputId = "file1", show=TRUE, text = "Upload data first")}
  }) 
  
  
  output$contents <- DT::renderDataTable({
    DT::datatable(inputData$rawdata, options = list(scrollX = TRUE))     
  })
  output$data_description <- renderUI(inputData$descriptives)
  

  
  # Source server side 
  source("source/outcome_model.R",local=T)
  source("source/ps_model.R",local=T)
  react_PSmodel <- reactiveValues(psmodel=NULL)
  react_outcomemodel <- reactiveValues(outcomemodel=NULL)
  observeEvent(input$nextCM_btn, {
    react_PSmodel$psmodel <- ps_model(.data = inputData$rawdata, 
                                      treatment = input$treatment,
                                      matchvars = input$matchvars,
                                      model = input$psm,
                                      method = input$counterfactual)
    print(react_PSmodel$psmodel)
    
    output$PSmodel_baltab <- renderPrint({cobalt::bal.tab(react_PSmodel$psmodel)})
    output$PSmodel_balplot <- renderPlot({cobalt::bal.plot(react_PSmodel$psmodel)})
    output$PSmodel_loveplot <- renderPlot({cobalt::love.plot(react_PSmodel$psmodel)})
    
  })
  
  observeEvent(input$resshow_btn, {
    react_outcomemodel$outcomemodel <- outcome_model(.data=inputData$rawdata,
                                                     outcome = input$outcome,
                                                     treatment = input$treatment,
                                                     covars = input$covars,
                                                     matchvars = input$matchvars,
                                                     PSmodel = react_PSmodel$psmodel, 
                                                     method = input$counterfactual,
                                                     doubly = input$drobust)
    output$outcome_plot <- renderPlot({react_outcomemodel$outcomemodel$plot})
    output$outcome_table <- renderTable({react_outcomemodel$outcomemodel$est})
    output$outcome_resid <- renderPlot({performance::check_model(react_outcomemodel$outcomemodel$mod)})
  
    })
}









