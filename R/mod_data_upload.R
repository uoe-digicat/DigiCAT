# home module ----

data_upload_ui <- function(id) {
  ns <- NS(id)
  
  require(shinyFeedback)
  require(shinycssloaders)
  
  
  tabPanel(title = "",
           value = NS(id, "tab"),
           useShinyFeedback(), # include shinyFeedback
           br(),
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5("GET STARTED")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("DATA UPLOAD")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("APPROACH", style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("BALANCING", style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("OUTCOME", style="color: #607cc4;"))
           ),
           br(), 
           ## Add buttons to move back to home page and validate uploaded data
           div(align="center",
               actionButton(NS(id,"prevDU_btn"), "Prev", class = "default_button"),
               actionButton(NS(id,"validate_btn"), "Validate Data", class = "default_button"),
               actionButton(NS(id, 'nextDU_btn'), 'Next', class = "default_button")
           ), 
           br(),
           sidebarLayout(
             sidebarPanel(id=ns("sidebarPanel"),
                          
                          ## Add space to give no data warning
                          tags$h2(style="text-align: centre; margin-bottom:20px","Upload data:"),
                          uiOutput(ns("no_data_warning")), ## Give "no data" warning
                          ## Give instructions to get data
                          tags$h4(style="text-align: left;", "Choose CSV File or Use Example Data"),
                          uiOutput(ns("local_disabled")),
                          ## Add button to load sample data
                          div(class = "buttonagency",
                              style="max-width:40%; float:right;", ## position to right
                              actionButton(NS(id,"Btn_sampledata"), "Load Example Data")),
                          
                          ## Add file input for user to upload own data
                          div(style=";max-width:55%; float:left;", ## position to left
                              fileInput(ns("file1"), label = NULL, placeholder = "No file selected",
                                        accept=c("text/csv","text/comma-separated-values,text/plain",".csv")
                              )),
                          
                          ## Give instructions to select/check variable class
                          br(),
                          
                          pickerInput(inputId=ns("categorical_vars"), label ="Select categorical variables *", multiple = TRUE, 
                                      choices=NULL, selected=NULL
                          )  %>% ## add info about variables being classed automatically
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(title = "Any variables with 5 or fewer unique values are automatically classed as categorical, please alter where necessary", placement = "right")
                            ),
                          
                          
                          pickerInput(inputId=ns("outcome"), label ="Select your outcome variable *",
                                      choices=NULL, selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)) %>% ## Multiple choices allowed but max set to 1 so input can be reset to NULL
                            shinyInput_label_embed(
                              shiny_iconlink() %>% ## Add information about outcome having to be a continuous variable
                                bs_embed_popover(title = "Please select your outcome. Please note you can only select a continuous variable (i.e. not catagorical)", placement = "right")
                            ),
                          
                          pickerInput(inputId= ns("treatment"), label ="Select your treatment variable *",
                                      choices=NULL,selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F) ## Multiple choices allowed but max set to 1 so input can be reset to NULL
                          ),
                          pickerInput(inputId= ns("matchvars"), label ="Select your matching variables *",
                                      choices=NULL,selected=NULL,multiple=TRUE, options = pickerOptions(dropupAuto = F)
                          ),
                          pickerInput(inputId= ns("covars"), label ="Select any additional covariates",
                                      choices=NULL,selected=NULL,multiple=TRUE,  options = pickerOptions(dropupAuto = F)
                          ) %>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(title = "covariates are characteristics (excluding the treatment) of the participants, that may also affect the outcome", placement = "right")
                            ),
                          ## Add checkbox asking if "survey weight" should be specified
                          div(style = "background-color: #F4F4F3; padding: 10px; border-radius: 3px;",
                            checkboxInput(ns("survey_weight_checkbox"), "Select if data includes survey weights"),
                            ## If check box selected, show picker input
                            uiOutput(ns("survey_weight_var"))
                          ),
                          br(),
                          ## Add checkbox asking if "clustering variable" should be specified
                          div(style = "background-color: #F4F4F3; padding: 10px; border-radius: 3px;",
                            checkboxInput(ns("clustering_checkbox"), "Select if data includes a clustering variable"),
                            ## If check box selected, show picker input
                            uiOutput(ns("clustering_var"))
                          ),
                          br(),
                          ## Add checkbox asking if "stratification variables" should be specified
                          div(style = "background-color: #F4F4F3; padding: 10px; border-radius: 3px;",
                            checkboxInput(ns("stratification_checkbox"), "Select if data includes stratification variables"),
                            ## If check box selected, show picker input
                            uiOutput(ns("stratification_var"))
                          ),
                          br(),
                          ## Give warning that validation and subsequnt analysis rerun required upon re-selection of data/variables
                          uiOutput(ns("data_upload_rerun_message"), style = "color: grey;"),
                          
                          ## Add buttons to clear/validate data  
                          div(align="center",
                              ## Add button to clear data upload page
                              actionButton(NS(id,"clear_btn"), "Clear Data", class = "default_button")
                          )
             ),
             mainPanel(wellPanel(id = "well_panel",
                                 tabsetPanel(id = NS(id,"Tab_data"),
                                             tabPanel(title = "Requirements", value = NS(id,"data_requirements"),
                                                      uiOutput(ns("upload_error")),
                                                      br(),
                                                      h4(strong("Data requirements:")),
                                                      h5(strong("File type:"), "CSV"),
                                                      h5(strong("Maximum file size:"), "5MB"),
                                                      h5(strong("Number of rows:")," 10-10,000"),
                                                      h5(strong("Number of columns:")," 2-100"),
                                                      h5(strong("Variable type:"), " All data must be numeric, categorical data should be coded as index variables"),
                                                      h5(strong("Outcome Variable:"), " Outcome variable must be continuous"),
                                                      h5(strong("Missing data:"), "Missing values should be coded as 'NA'"),
                                                      br(),
                                                      h4(strong("Input requirements:")),
                                                      h5(strong("Required input:"), "Data, categorical variables, outcome, treatment, matching variables"),
                                                      br(),
                                                      h4(strong("Required actions:")),
                                                      h5(strong("1. Upload data: "), "Upload your own data or use our sample data"),
                                                      h5(strong("2. Select input variables ")),
                                                      h5(strong("3. Validate input:"), "Ensure inputed data and variables meet requirments"),
                                                      h5(strong("4. Process to model configuration"))
                                             ),
                                             tabPanel(title = "Data", value = NS(id,"raw_data"), br(), withSpinner(DT::dataTableOutput(ns("contents")))),
                                             tabPanel(title = "Validation", value = NS(id,"data_validation"), br(), div(style="width:auto;height:570px;overflow-y: scroll;",
                                                                                                                 withSpinner(uiOutput((ns("data_validation"))))))))
             ))
           )
}

data_upload_server <- function(id, parent, enableLocal) {
  
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   shinyjs::toggleState("file1", enableLocal)
                 })
                 if(enableLocal==FALSE){
                   output$local_disabled = renderUI({
                     p("Please install DigiCAT locally to enable file upload. See ",a("https://github.com/josiahpjking/DigiCAT"))
                   })
                 }
                 
                 ## Define Reactives ----
                 
                 ## Save data, data source and validation as a reactive variable
                 data_upload_values <- reactiveValues(rawdata = NULL, 
                                                      data_source = NULL, 
                                                      validation = NULL)
                 
                 # Setup Page ----
                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("nextDU_btn")
                 
                 ## Hide data and validation tabs initially
                 hideTab(session = parent, inputId = NS(id, "Tab_data"), target = NS(id, "raw_data"))
                 hideTab(session = parent, inputId = NS(id, "Tab_data"), target = NS(id, "data_validation"))
                 
                 ## Create reactive value to store rerun message
                 data_upload_output <- reactiveValues(data_upload_rerun_message = NULL)
                 
                 # Navigation ----
                 ## If "Prev" selected on data upload page, go back to start page
                 observeEvent(input$prevDU_btn,{
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "home-tab")
                 }
                 )

                 # Variable Selection ----
                 
                 ## If input variable(s) change(s), remove any warnings that may be present for variable selection
                 observeEvent(c(input$categorical_vars, input$outcome, input$treatment, input$matchvars, input$covars, input$survey_weight_var, input$non_response_weight_checkbox), {
                   reset_upload_page(reset_errors = TRUE, hide_validation = TRUE, parent = parent)
                   data_upload_values$validation <- NULL
                   shinyjs::disable("nextDU_btn")
                   
                   output$no_data_warning <- NULL ## Remove "no data" warning
                 })
                 
                 ## When categorical variable selection changed, update what can be selected as the outcome variable
                 observeEvent(input$categorical_vars, {
                   
                   if(data_upload_values$data_source == "sample"){
                     
                   }else{
                     ## Get names of continuous variables
                     continuous_variables <- names(isolate(data_upload_values$rawdata))[!names(isolate(data_upload_values$rawdata)) %in% input$categorical_vars]
                     ## Only allow selection from continuous variables
                     updatePickerInput(session, "outcome", selected=NULL, choices = continuous_variables)
                   }
                 }, ignoreNULL = FALSE, ignoreInit = TRUE)
                 
                 ## If "non-response weight" checked, show picker selection
                 observeEvent(input$survey_weight_checkbox, {
                   
                   if (input$survey_weight_checkbox){
                     output$survey_weight_var <- renderUI(
                       ## If survey weights present, allow user to select from varaibles in data and indicate if these wights are non-response weights also
                       div(
                       pickerInput(session$ns("survey_weight_var"), label ="Select survey weight",
                                   choices=names(isolate(data_upload_values$rawdata)), selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)),
                       checkboxInput(session$ns("non_response_weight_checkbox"), "Select if survey weights above compensates for non-response")
                       )
                       )
                   } else{
                     output$survey_weight_var <- NULL ## If checkbox unselceted, show nothing
                   }
                   })
                 
                 ## If "clustering variable" checked, show picker selection
                 observeEvent(input$clustering_checkbox, {
                   
                   if (input$clustering_checkbox){
                     output$clustering_var <- renderUI(
                       pickerInput(session$ns("clustering_var"), label ="Select a clustering variable",
                                   choices=names(isolate(data_upload_values$rawdata)), selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)))
                   } else{
                     output$clustering_var <- NULL ## If checkbox unseleceted, show nothing
                   }
                 })
                 
                 ## If "stratification variable" checked, show picker selection
                 observeEvent(input$stratification_checkbox, {
                   
                   if (input$stratification_checkbox){
                     output$stratification_var <- renderUI(
                       pickerInput(session$ns("stratification_var"), label ="Select stratification variables",
                                   choices=names(isolate(data_upload_values$rawdata)), selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)))
                   } else{
                     output$stratification_var <- NULL ## If checkbox unselected, show nothing
                   }
                 })
                 
                 # Data Upload ----
                 
                 ## Update app when file uploaded
                 observeEvent(input$file1, {
                   
                   ## Reset any input errors and hide validation tab
                   reset_upload_page(reset_errors = TRUE, hide_validation = TRUE, hide_data = TRUE, parent = parent)
                   ## Remove validation info
                   data_upload_values$validation <- NULL
                   ## Disable Next button
                   shinyjs::disable("nextDU_btn")
                   ## Remove "no data" warning
                   output$no_data_warning <- NULL
                   ## Reset "survey weights", "clustering" and stratification" checkboxes to null
                   updateCheckboxInput(session, inputId = "survey_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "clustering_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "stratification_checkbox", value = FALSE)
                
                   ## Load in data
                   ## Save potential error to check for running of code dependent on data upload
                   error_check <- NA
                   error_check <- tryCatch({
                   data_upload_values$rawdata <- read.csv(input$file1$datapath)},
                   
                   ## If data does not upload, return error message
                   error = function(cond) {
                     ## Output error message
                     data_upload_values$upload_error <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                     
                   })
                   
                   ## Carry out data checks if no error in data upload
                   if (all(!grepl("Error:", error_check))){
                     try({
                       
                       ## Show and switch to data tab
                       showTab(session = parent, inputId = NS(id,"Tab_data"), target = NS(id, "raw_data"), select = TRUE)
                       ## Remove data upload error if present
                       data_upload_values$upload_error <- NULL
                      
                       ## Save data source
                       data_upload_values$data_source <- "own"
                       
                       ## Check data upon upload
                       initial_data_check_ls <- initial_data_check(data_upload_values$rawdata)
                       
                       ## If data is too small give error and delete
                       if(initial_data_check_ls$small_rows){
                         data_upload_values$upload_error <- p("Error: Data has too few rows! (<10 rows)", style = "color:red")
                         data_upload_values$rawdata <- NULL
                       }
                       
                       if(initial_data_check_ls$small_cols){
                         data_upload_values$upload_error <- p("Error: Data has too few columns! (<2 columns)", style = "color:red")
                         data_upload_values$rawdata <- NULL
                       }
                       
                       ## If data is contains nonnumeric values, give error, delete and remove data tab
                       if(initial_data_check_ls$some_nonnumeric){
                         data_upload_values$upload_error <- p("Error: Non numeric values detected!", style = "color:red")
                         data_upload_values$rawdata <- NULL
                       }
                       
                       ## If inappropriate data is uploaded:
                       if(any(c(initial_data_check_ls$small_cols, initial_data_check_ls$small_rows, initial_data_check_ls$some_nonnumeric))){
                         
                         ## Move back to data requirements page
                         updateTabsetPanel(session = parent, inputId = "data_upload-Tab_data", selected = "data_upload-data_requirements")
                         
                         ## Hide data tab
                         hideTab(session = parent, inputId = NS(id, "Tab_data"), target = NS(id, "raw_data"))
                         
                         ## Reset variable inputs
                         updatePickerInput(session, "categorical_vars", choices = character(0))
                         updatePickerInput(session, "outcome", choices=character(0))
                         updatePickerInput(session, "treatment", choices=character(0))
                         updatePickerInput(session, "matchvars", choices=character(0))
                         updatePickerInput(session, "covars", choices=character(0))
                       }
                       
                       
                       ## Get variable classes
                       categorical_variables <- get_categorical_variables(data_upload_values$rawdata)
                       continuous_variables <- names(isolate(data_upload_values$rawdata))[!names(isolate(data_upload_values$rawdata)) %in% categorical_variables]
                       
                       ## Reset variable inputs
                       updatePickerInput(session, "categorical_vars", choices = names(isolate(data_upload_values$rawdata)), selected=categorical_variables)
                       updatePickerInput(session, "outcome", choices=continuous_variables, selected = NULL)
                       updatePickerInput(session, "treatment", choices=names(isolate(data_upload_values$rawdata)), selected = NULL)
                       updatePickerInput(session, "matchvars", choices=names(isolate(data_upload_values$rawdata)), selected = NULL, clearOptions = TRUE)
                       updatePickerInput(session, "covars", choices=names(isolate(data_upload_values$rawdata)), selected = NULL, clearOptions = TRUE)
                       
                       updatePickerInput(session, "survey_weight_var", choices = names(isolate(data_upload_values$rawdata)))
                       updatePickerInput(session, "clustering_var", choices = names(isolate(data_upload_values$rawdata)))
                       updatePickerInput(session, "stratification_var", choices = names(isolate(data_upload_values$rawdata)))

                     })
                   }
                 })
                 
                 ## Update app when sample data selected
                 observeEvent(input$Btn_sampledata, {
                   
                   ## Load in sample data
                   data_upload_values$rawdata <- zp_eg
                   
                   ## Reset any input errors and hide validate tab
                   reset_upload_page(reset_errors = TRUE, hide_validation = TRUE, parent = parent)
                   ## Remove validation info
                   data_upload_values$validation <- NULL
                   ## Disable Next button
                   shinyjs::disable("nextDU_btn")
                   ## Remove "no data" warning
                   output$no_data_warning <- NULL 
                   ## Reset "survey weights", "clustering" and stratification" checkboxes to null
                   updateCheckboxInput(session, inputId = "survey_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "clustering_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "stratification_checkbox", value = FALSE)
                   
                   ## Save data source
                   data_upload_values$data_source <- "sample"
                   
                   ## Show and switch to data tab
                   showTab(session = parent, inputId = NS(id,"Tab_data"), target = NS(id, "raw_data"), select = TRUE)
                   
                   ## Update variable selection
                   updatePickerInput(session, "categorical_vars", selected=c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13"), choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "outcome", selected="Anxiety_age17", choices = names(isolate(data_upload_values$rawdata))[!names(isolate(data_upload_values$rawdata)) %in% c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13")])
                   updatePickerInput(session, "treatment", selected="Reading_age15", choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "matchvars", selected=names(isolate(data_upload_values$rawdata))[-c(2:3)], choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "covars", choices = names(isolate(data_upload_values$rawdata)))
                   
                   updatePickerInput(session, "survey_weight_var", choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "clustering_var", choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "stratification_var", choices = names(isolate(data_upload_values$rawdata)))
                 })
                 
                 ## Clear data when "Clear Data" button is pressed
                 observeEvent(input$clear_btn,{
                   ## Remove data
                   data_upload_values$rawdata <- NULL 
                   ## Remove errors and hide data and validate tabs
                   reset_upload_page(reset_errors = TRUE, hide_data = TRUE, hide_validation = TRUE, parent = parent) 
                   ## Remove validation info
                   data_upload_values$validation <- NULL
                   ## Disable "Next" button
                   shinyjs::disable("nextDU_btn")
                   
                   ## Reset "survey weights", "clustering" and stratification" checkboxes to null
                   updateCheckboxInput(session, inputId = "survey_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "clustering_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "stratification_checkbox", value = FALSE)
                   
                   output$no_data_warning <- NULL ## Remove "no data" warning
                   
                   ## Clear input pickers
                   updatePickerInput(session, "categorical_vars", choices = character(0))
                   updatePickerInput(session, "outcome", choices = character(0))
                   updatePickerInput(session, "treatment", choices = character(0))
                   updatePickerInput(session, "matchvars", choices = character(0))
                   updatePickerInput(session, "covars", choices = character(0))
                   
                   updatePickerInput(session, "survey_weight_var", choices = character(0))
                   updatePickerInput(session, "clustering_var", choices = character(0))
                   updatePickerInput(session, "stratification_var", choices = character(0))
                 })
                 
                 # Data Validation ----
                 
                 ## When "Validate" Selected on data upload page, check required input first, validate if present, flag if not
                 observeEvent(input$validate_btn, {
                   
                   if (is.null(data_upload_values$validation)){ ## Only validate if validation has not get been carried out
                     
                     # Remove error message if any present from previous upload
                     reset_upload_page(reset_errors = TRUE, parent = parent)
                     output$no_data_warning <- NULL ## Remove "no data" warning
                     
                     ## Check inputed variables. If there is an issue give informative error message, otherwise, continue to next page
                     ## First check if data has been uploaded
                     if (!isTruthy(data_upload_values$rawdata)) { 
                       ## If there is no data, give informative error
                       output$no_data_warning <- renderUI(h5("Please upload some data first!", style = "color:red"))
                       
                     }else{
                       variable_check_info <- check_selected_variables(outcome = input$outcome,
                                                                           treatment = input$treatment,
                                                                           matchvars = input$matchvars,
                                                                           covars = input$covars)
                     
                       ## If there is no missing data and no variable mismatched, proceed to next tab
                     if(all(!c(variable_check_info$required_input_missmatched, variable_check_info$required_input_missing))){
                       ## Show and switch to validate tab
                       showTab(inputId = NS(id,"Tab_data"), target = NS(id,"data_validation"), select = TRUE, session = parent)

                       ## Validate data
                       data_upload_values$validation  <- get_validation(.data = data_upload_values$rawdata, outcome = input$outcome, 
                                                                        treatment = input$treatment, matchvars = input$matchvars, 
                                                                        covars = input$covars, survey_weight_var = input$survey_weight_var,
                                                                        non_response_weight = input$non_response_weight_checkbox)
                       
                       ## Change "Validate" button to "Next" button
                       shinyjs::enable("nextDU_btn")
                       
                       ## Display warning under data/variable selection warning about reselection requiring revalidation/rerun of subsequent analysis
                       data_upload_output$data_upload_rerun_message <- p("Note: Changing data/variable selection will require reruning validation and all subsequent analysis steps.")
                     }
                     }}
                   else{ ## If validation is present, proceed to next page
                     # updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "CF_approach-tab")
                   }
                 })

                 observeEvent(input$nextDU_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "CF_approach-tab")
                 })
                 
                 # Show data and validation ----
                 ## Show uploaded data
                 output$contents <- DT::renderDataTable({DT::datatable(data_upload_values$rawdata, options = list(scrollX = TRUE))})
                 output$data_validation <- renderUI(data_upload_values$validation)
                 output$data_upload_rerun_message <- renderUI(data_upload_output$data_upload_rerun_message)
                 output$upload_error <- renderUI(data_upload_values$upload_error)
                 
                 # Return data and variables ----
                 
                 ## Output list containing: dataset, categorical variables, treatment variable, outcome variable,
                 ## matching variable, covariates
                 
                 data_upload_output <- reactiveValues(data = NULL,
                                                     categorical_vars = NULL,
                                                     outcome = NULL,
                                                     treatment = NULL,
                                                     matchvars = NULL,
                                                     covars = NULL,
                                                     survey_weight_var = NULL,
                                                     non_response_weight = NULL,
                                                     cluster_var = NULL,
                                                     stratification_var = NULL)
                 
                 observe({
                   data_upload_output$data <- data_upload_values$rawdata
                   data_upload_output$categorical_vars <- data_upload_values$categorical_vars
                   data_upload_output$outcome <- input$outcome
                   data_upload_output$treatment <- input$treatment
                   data_upload_output$matchvars <- input$matchvars
                   data_upload_output$covars <- input$covars
                   
                   data_upload_output$survey_weight_var <- input$survey_weight_var
                   data_upload_output$non_response_weight <- input$non_response_weight_checkbox
                   data_upload_output$cluster_var <- input$clustering_var
                   data_upload_output$stratification_var <- input$stratification_var
                 })
                 
                 return(data_upload_output)

               })
}
