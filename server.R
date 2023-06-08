load("data/zp_example.RData")

## Source functions to perform data checks and categorical variable identification
source("source/func/initial_data_check.R")
source("source/func/get_categorical_variables.R")
source("source/func/reset_upload_page.R")
source("source/func/check_selected_variables.R")
source("source/func/get_validation.R")


server <- function(input, output, session) {
 
  
  ####
  # App theme ----
  ####
output$style <- renderUI({
  if (!is.null(input$style)){
    if (input$style) {
      includeCSS("./www/themes/dark.css")
    } else {
      includeCSS("./www/themes/light.css")
    }}
  })
  
  
  
  ####
  # Start Page ----
  ####

  
  ## When "Get Started!" selected on home page check if user has agreed to T&Cs, if so, proceed, if not, ask again 
  observeEvent(input$start_btn,{
    
    ## If user has already agreed to T&Cs, proceed to upload page
    if (isTruthy(input$start_agree)){
      updateTabsetPanel(session, inputId = "Tab_analysis", selected = "upload")
    } else{ ## If they have not yet agreed, ask (again)

      ## Pop up agreement
      showModal(modalDialog(
        HTML("<center>"),
        h4("Before you get started:"),
        br(),
        tags$div("Have you read and agree to the terms of the", actionLink("TCs_link", "DigiCAT Customer Agreement"), "?"),
        footer=tagList(
          div(style = "text-align:center",
          actionButton('start_dont_agree', "No, I don't agree", style="color: white; background: #4f78dc"),
          actionButton('start_agree', 'Yes, I agree', style="color: white; background: green"))),
        HTML("<center>")))
    }
  })
  
  ## T&Cs agreement: If terms and conditions link clicked, close modal and switch to T&Cs tab
  observeEvent(input$TCs_link, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = 'TC')
    removeModal() ## remove modal
  })
  
  ## T&Cs agreement: If 'No, I don't agree', remove modal and remain in start page
  observeEvent(input$start_dont_agree, {
    removeModal() ## remove modal
  })
      
  ## T&Cs agreement: If 'Yes, I agree', continue to data upload page
  observeEvent(input$start_agree, {
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "upload")
    removeModal() ## remove modal
  })
  
  ## If tutorial link clicked, close modal and switch to T&Cs tab
  observeEvent(input$tutorial_link, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = 'tutorial')
    removeModal() ## remove modal
  })
  
  ####
  # Data upload ----
  ####
  #* Data upload: setup ----
  
  ## Hide data and validation tabs initially
  hideTab(inputId = "Tab_data", target = "raw_data")
  hideTab(inputId = "Tab_data", target = "data_validation")
  
  
  ## Save data and source as a reactive variable
  inputData <- reactiveValues(rawdata = NULL, data_source = NULL, validation = NULL)

  
  #* Data upload: Navigation ----
  ## If "Prev" selected on data upload page, go back to start page
  observeEvent(input$prevDU_btn,{
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "home")
  }
  )
  
  
  observeEvent(input$validate_btn,{
    
    ## Check if data has been validated
    if (is.null(inputData$validation)){
      ## Do nothing
    } else{ ## Continue to model config page
      updateTabsetPanel(session, inputId = "Tab_analysis", selected = "methods")
    }
  }
  )
  
  
  #* Data upload: Variable Selection ----
  
  ## If input variable(s) change(s), remove any warnings that may be present for variable selection
  observeEvent(c(input$outcome, input$treatment, input$matchvars, input$covars), {
    reset_upload_page(reset_errors = TRUE, hide_validation = TRUE)
    inputData$validation <- NULL ## Remove validation info
    updateActionButton(session, "validate_btn", label = "Validate Data", icon = NULL) ## Relabel "Next" button with "Validate"
    output$no_data_warning <- NULL ## Remove "no data" warning
  })
  
  ## When categorical variable selection changed, update what can be selected as the outcome variable
  observeEvent(input$categorical_vars, {
    
    ## Reset any input errors
    reset_upload_page(reset_errors = TRUE)
    output$no_data_warning <- NULL ## Remove "no data" warning
    
    if(inputData$data_source == "sample"){
      
    }else{
      ## Get names of continuous variables
      continuous_variables <- names(isolate(inputData$rawdata))[!names(isolate(inputData$rawdata)) %in% input$categorical_vars]
      ## Only allow selection from continuous variables
      updatePickerInput(session, "outcome", selected=NULL, choices = continuous_variables)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  #* Data upload: Data Upload ----
  
  ## Update app when file uploaded
  observeEvent(input$file1, {
    
    ## Show and switch to data tab
    showTab(inputId = "Tab_data", target = "raw_data", select = FALSE, session = getDefaultReactiveDomain())
    updateTabsetPanel(session, "Tab_data", selected = "raw_data")
    
    ## Load in data
    inputData$rawdata <- read.csv(input$file1$datapath)
    
    ## Reset any input errors and hide validation tab
    reset_upload_page(reset_errors = TRUE, hide_validation = TRUE)
    inputData$validation <- NULL ## Remove validation info
    updateActionButton(session, "validate_btn", label = "Validate Data", icon = NULL) ## Relabel "Next" button with "Validate"
    output$no_data_warning <- NULL ## Remove "no data" warning
    
    ## Save data source
    inputData$data_source <- "own"
    
    ## Check data upon upload
    initial_data_check_ls <- initial_data_check(inputData$rawdata)
    
    ## If data is too small give error message and delete
    if(initial_data_check_ls$small_rows){
      feedbackDanger(inputId = "file1",
                     show=initial_data_check_ls$small_rows,
                     text = "Data has too few rows! (<10 rows)")
      inputData$rawdata <- NULL
    }
    if(initial_data_check_ls$small_cols){
      feedbackDanger(inputId = "file1",
                     show=initial_data_check_ls$small_cols,
                     text = "Data has too few columns! (<2 columns)")
      inputData$rawdata <- NULL
    }
    
    ## If data is contains nonnumeric values, give option to delete data or delete just columns
    if(initial_data_check_ls$some_nonnumeric){
      feedbackDanger(inputId = "file1",
                     show=initial_data_check_ls$some_nonnumeric,
                     text = "Non numeric values detected!")
    }
    
    if(any(c(initial_data_check_ls$too_small, initial_data_check_ls$some_nonnumeric))){
      ## Remove uploaded data and list with initial data checks
      inputData$rawdata <- NULL
      initial_data_check_ls <- list(some_nonnumeric = FALSE,
                                    too_small = FALSE)
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
  
  ## Update app when sample data selected
  observeEvent(input$Btn_sampledata, {
    
    ## Load in sample data
    inputData$rawdata <- na.omit(read.csv("data/zp_eg.csv"))
    
    ## Reset any input errors and hide validate tab
    reset_upload_page(reset_errors = TRUE, hide_validation = TRUE)
    inputData$validation <- NULL ## Remove validation info
    updateActionButton(session, "validate_btn", label = "Validate Data", icon = NULL) ## Relabel "Next" button with "Validate"
    output$no_data_warning <- NULL ## Remove "no data" warning
    
    ## Save data source
    inputData$data_source <- "sample"
    
    ## Show and switch to data tab
    showTab(inputId = "Tab_data", target = "raw_data", select = FALSE, session = getDefaultReactiveDomain())
    updateTabsetPanel(session, "Tab_data", selected = "raw_data")
    
    ## Update variable selection
    updatePickerInput(session, "categorical_vars", selected=c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13"), choices = names(isolate(inputData$rawdata)))
    updatePickerInput(session, "outcome", selected="Anxiety_age17", choices = names(isolate(inputData$rawdata))[!names(isolate(inputData$rawdata)) %in% c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13")])
    updatePickerInput(session, "treatment", selected="Reading_age15", choices = names(isolate(inputData$rawdata)))
    updatePickerInput(session, "matchvars", selected=names(isolate(inputData$rawdata))[-c(2:3)], choices = names(isolate(inputData$rawdata)))
    updatePickerInput(session, "covars", choices = names(isolate(inputData$rawdata)))
  })
  
  ## Clear data when "Clear Data" button is pressed
  observeEvent(input$clear_btn,{
    inputData$rawdata <- NULL ## Remove data
    reset_upload_page(reset_errors = TRUE, hide_data = TRUE, hide_validation = TRUE) ## Remove errors and hide data and validate tabs
    inputData$validation <- NULL ## Remove validation info
    updateActionButton(session, "validate_btn", label = "Validate Data", icon = NULL) ## Relabel "Next" button with "Validate"
    output$no_data_warning <- NULL ## Remove "no data" warning
    
    ## Clear input pickers
    updatePickerInput(session, "categorical_vars", choices = character(0), selected=character(0))
    updatePickerInput(session, "outcome", choices = character(0), selected=character(0))
    updatePickerInput(session, "treatment", choices = character(0), selected=character(0))
    updatePickerInput(session, "matchvars", choices = character(0), selected=character(0))
    updatePickerInput(session, "covars", choices = character(0), selected=character(0))
  })
  
  #* Data upload: Data Validation ----
  
  ## When "Validate" Selected on data upload page, check required input first, validate if present, flag if not
  observeEvent(input$validate_btn, {
    
    if (is.null(inputData$validation)){ ## Only validate if validation has not get been carried out
      
      # Remove error message if any present from previous upload
      reset_upload_page(reset_errors = TRUE)
      output$no_data_warning <- NULL ## Remove "no data" warning
      
      ## Check inputed variables. If there is an issue give informative error message, otherwise, continue to next page
      ## First check if data has been uploaded
      if (!isTruthy(inputData$rawdata)) { ## If there is no data, give informative error
        
        output$no_data_warning <- renderUI(h5("Please upload some data first!", style = "color:red"))
        
      }else{variable_check_info <- check_selected_variables(outcome = input$outcome, treatment = input$treatment, matchvars = input$matchvars, covars = input$covars)
      ## If there is no missing data and no variable mismatched, proceed to next tab
      if(all(!c(variable_check_info$required_input_missmatched, variable_check_info$required_input_missing))){
        ## Move to validate tab
        ## Show and switch to validate tab
        showTab(inputId = "Tab_data", target = "data_validation", select = FALSE, session = getDefaultReactiveDomain())
        updateTabsetPanel(session, "Tab_data", selected = "data_validation")
        
        ## Validate data
        inputData$validation  <- get_validation(.data = inputData$rawdata, outcome = input$outcome, matchvars = input$matchvars, covars = input$covars)
        
        ## Change "Validate" button to "Next" button
        updateActionButton(session, "validate_btn", label = "Next", icon = NULL)
        }
      }}
  })

  #* Data upload: Show data and validation ----
  ## Show uploaded data
  output$contents <- DT::renderDataTable({DT::datatable(inputData$rawdata, options = list(scrollX = TRUE))})
  output$data_validation <- renderUI(inputData$validation)
  
  
  ####
  # Model configuration ----
  ####
  
  ## When "BUILD!" selected on model configuration, switch to PS results tab
  observeEvent(input$nextCM_btn,{
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "psres")
  }
  )
  
  ## When "Prev" selected on model configuration, go back to data upload page
  observeEvent(input$prevCM_btn,{
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "upload")
  }
  )
  
  
  ####
  # PS results ----
  ####
  
  
  ## When "Prev" selected on PS results page, go back to model configuration page
  observeEvent(input$prevPSR_btn,{
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "methods")
  }
  )
  
  ## When "Next" selected on PS results page, go back to get results page
  observeEvent(input$nextPSR_btn,{
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "results")
  }
  )
  
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
  
  # Get results ----
  ####
  
  ## When "Prev" selected on get results page, go back to PS results page
  observeEvent(input$resPrev_btn,{
    updateTabsetPanel(session, inputId = "Tab_analysis", selected = "psres")
  }
  )
  
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





