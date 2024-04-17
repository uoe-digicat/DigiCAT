data_upload_ui <- function(id, i18n) {
  ns <- NS(id)
  
  require(shinyFeedback)
  require(shinycssloaders)
  
  ## Tab for data upload and variable selection
  tabPanel(title = "",
           value = NS(id, "tab"),
           useShinyFeedback(), # include shinyFeedback
           br(),
           ## Navigation bar ----
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5(i18n$t("GET STARTED"))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("DATA UPLOAD"), style="color: #607cc4;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("APPROACH"), style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("BALANCING"), style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("OUTCOME"), style="color: #607cc4;"))
           ),
           br(), 
           
           ## Navigation ----
           ## Add buttons to move back to home page and validate uploaded data
           div(align="center",
               actionButton(NS(id,"prevDU_btn"), i18n$t("Button Previous"), class = "default_button"),
               actionButton(NS(id,"validate_btn"), i18n$t("Upload Button validate"), class = "default_button"),
               actionButton(NS(id, 'nextDU_btn'), i18n$t("Button Next"), class = "default_button")
           ), 
           br(),
           
           ## Data upload input ----
           sidebarLayout(
             sidebarPanel(id=ns("sidebarPanel"),
                          
                          ## Add space to give no data warning
                          tags$h2(style="text-align: centre; margin-bottom:20px",i18n$t("Upload Upload data")),
                          uiOutput(ns("data_upload_rerun_message"), style = "color: grey;"), ## Give warning that validation and subsequnt analysis rerun required upon re-selection of data/variables
                          uiOutput(ns("no_data_warning")), ## Give "no data" warning
                          ## Give instructions to get data
                          tags$h4(style="text-align: left;", "Choose CSV File or Use Example Data"),
                          uiOutput(ns("local_disabled")),
                          div(style="margin: 0 auto;",
                              actionButton(NS(id,"Btn_sampledata"), i18n$t("Upload Button example data"), width = "100%"), ## Add button to load sample data
                              p(i18n$t("Upload OR"), style="text-align: center; padding-top:5px;"),
                              fileInput(ns("file1"), label = NULL, buttonLabel = i18n$t("Upload Upload data"), placeholder = "", accept=c("text/csv","text/comma-separated-values,text/plain",".csv"))),  ## Add file input for user to upload own data 
                          ## Add variable inputs
                          pickerInput(inputId=ns("categorical_vars"), label = i18n$t("Upload Select categorical"), multiple = TRUE, options = pickerOptions(title = "---"),
                                      choices=NULL, selected=NULL
                          ), ## add info about variables being classed automatically
                          
                          pickerInput(inputId=ns("outcome"), label = i18n$t("Upload Select outcome"),
                                      choices=NULL, selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F, title = "---")), ## Multiple choices allowed but max set to 1 so input can be reset to NULL

                          pickerInput(inputId= ns("treatment"), label = i18n$t("Upload Select treatment"),
                                      choices=NULL,selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F, title = "---") ## Multiple choices allowed but max set to 1 so input can be reset to NULL
                          ),
                          uiOutput(ns("treatment_name_change")),
                          pickerInput(inputId= ns("matchvars"), label = i18n$t("Upload Select matching"),
                                      choices=NULL,selected=NULL,multiple=TRUE, options = pickerOptions(dropupAuto = F, title = "---")),

                          pickerInput(inputId= ns("covars"), label = i18n$t("Upload Select covariates"),
                                      choices=NULL,selected=NULL,multiple=TRUE,  options = pickerOptions(dropupAuto = F, title = "---")
                          ),

                          ## Add checkbox asking if "survey weight" should be specified
                          div(class = "tab_panel_feature",
                              checkboxInput(ns("survey_weight_checkbox"), p(i18n$t("Upload Survey weights selection"), tags$sup("†"))),
                              ## If check box selected, show picker input
                              uiOutput(ns("survey_weight_var")),
                              uiOutput(ns("survey_weight_name_change")),
                              ## Add checkbox to see if weights compensates for non-response
                              checkboxInput(ns("non_response_weight_checkbox"), p(i18n$t("Upload nonresponse selection"), tags$sup("†")))
                          ),
                          br(),
                          ## Add checkbox asking if "clustering variable" should be specified
                          div(class = "tab_panel_feature",
                              checkboxInput(ns("clustering_checkbox"), p(i18n$t("Upload Clustering"), tags$sup("†"))),
                              ## If check box selected, show picker input
                              uiOutput(ns("clustering_var"))
                          ),
                          br(),
                          ## Add checkbox asking if "stratification variables" should be specified
                          div(class = "tab_panel_feature",
                              checkboxInput(ns("stratification_checkbox"), p(i18n$t("Upload Stratification"), tags$sup("†"))),
                              ## If check box selected, show picker input
                              uiOutput(ns("stratification_var"))
                          ),
                          p(i18n$t("Upload Required fields")),
                          p(tags$sup("†"), i18n$t("Upload Design warning")),
                          br(),
                          
                          ## Add buttons to clear/validate data  
                          div(align="center",
                              ## Add button to clear data upload page
                              actionButton(NS(id,"clear_btn"), i18n$t("Upload Button clear data"), class = "default_button")
                          )),
             mainPanel(wellPanel(id = "well_panel",
                                 tabsetPanel(id = NS(id,"data_panel"),
                                             tabPanel(title = i18n$t("Upload Requirements"), value = NS(id,"data_requirements"),
                                                      uiOutput(ns("upload_error")),
                                                      br(),
                                                      div(h4(strong(i18n$t("Upload Data requirements"))),
                                                          h5(strong(i18n$t("Upload Requirements file type")), "CSV"),
                                                          h5(strong(i18n$t("Upload Requirements file size")), "5MB"),
                                                          h5(strong(i18n$t("Upload Requirements rows"))," 10-10,000"),
                                                          h5(strong(i18n$t("Upload Requirements columns"))," 2-100"),
                                                          h5(strong(i18n$t("Upload Requirements variable type")), i18n$t("Upload Requirements variable type description")),
                                                          h5(strong(i18n$t("Upload Data requirements missing data")), i18n$t("Upload Data requirements missing data description")),
                                                          br(),
                                                          h4(strong(i18n$t("Upload Data requirements required action"))),
                                                          h5(strong(i18n$t("Upload Data requirements upload data")), i18n$t("Upload Data requirements upload data description")),
                                                          h5(strong(i18n$t("Upload Data requirements select variables"))),
                                                          h5(strong(i18n$t("Upload Data requirements categorical")), i18n$t("Upload Data requirements categorical description")),
                                                          h5(strong(i18n$t("Upload Data requirements outcome")), i18n$t("Upload Data requirements outcome description")),
                                                          h5(strong(i18n$t("Upload Data requirements treatment")), i18n$t("Upload Data requirements treatment description")),
                                                          h5(strong(i18n$t("Upload Data requirements matching")), i18n$t("Upload Data requirements matching description")),
                                                          h5(strong(i18n$t("Upload Data requirements covariates")), i18n$t("Upload Data requirements covariates description")),
                                                          h5(strong(i18n$t("Upload Data requirements validate")), i18n$t("Upload Data requirements validate description")),
                                                          h5(strong(i18n$t("Upload Data requirements proceed"))),
                                                          br(), h4(strong(i18n$t("Upload Data requirements input requirements"))),
                                                          h5(i18n$t("Upload Data requirements input requirements description")),
                                                          p(a(id = "link", i18n$t("Upload Data requirements tutorial"), href="https://uoe-digicat.github.io/#overview-of-tutorials", target="_blank")))
                                             ),
                                             tabPanel(title = i18n$t("Upload Data"), value = NS(id,"raw_data"), br(), withSpinner(DT::dataTableOutput(ns("contents")))),
                                             tabPanel(title = i18n$t("Upload Validation"), value = NS(id,"data_validation"), br(), div(style="width:auto;height:570px;overflow-y: scroll;",
                                                                                                                        withSpinner(uiOutput((ns("data_validation"))))))))
             ))
  )
}

data_upload_server <- function(id, parent, enableLocal, analysis_tab, i18n, selected_language) {
  
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   shinyjs::toggleState("file1", enableLocal)
                 })
                 if(enableLocal==FALSE){
                   output$local_disabled = renderUI({
                     p("Please install DigiCAT locally to enable file upload. See ",  a(id = "link", "the DigiCAT github page.", href = "https://github.com/uoe-digicat/DigiCAT"))
                   })
                 }
                 
                 ## Define Reactives ----
                 
                 ## Save data, data source and validation as a reactive variable
                 data_upload_values <- reactiveValues()
                 
                 # Setup Page ----
                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("nextDU_btn")
                 
                 ## Hide data and validation tabs initially
                 hideTab(session = parent, inputId = NS(id, "data_panel"), target = NS(id, "raw_data"))
                 hideTab(session = parent, inputId = NS(id, "data_panel"), target = NS(id, "data_validation"))
                 
                 ## Disable "non-response weight" checkbox initially
                 shinyjs::disable("non_response_weight_checkbox")
                 
                 ## Create reactive value to store rerun message
                 data_upload_output <- reactiveValues(data_upload_rerun_message = NULL)
                 
                 # Navigation ----
                 ## If "Prev" selected on data upload page, go back to start page
                 observeEvent(input$prevDU_btn,{
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "home-tab")
                   shinyjs::removeClass(selector = "body", class = "sidebar-collapse") ## Expand NavBar
                 }
                 )
                 
                 # Variable Selection ----
                 
                 ## If input variable(s) change(s), reset page
                 observeEvent(c(input$categorical_vars, input$outcome, input$treatment, input$matchvars, input$covars, input$survey_weight_checkbox, input$survey_weight_var, input$non_response_weight_checkbox, input$clustering_var, input$clustering_checkbox, input$stratification_var, input$stratification_checkbox), {
                   reset_upload_page(reset_errors = TRUE, hide_validation = TRUE, parent = parent)
                   data_upload_values$validation <- NULL
                   shinyjs::disable("nextDU_btn")
                   
                   ## Remove "no data" warning
                   output$no_data_warning <- NULL 
                 })
                 
                 ## If treatment variable is labelled "treatment" flag and rename
                 observeEvent(input$treatment, {
                   
                   if (input$treatment == "treatment"){
                     ## Change treatment variable name in uploaded data
                     data_upload_values$rawdata[, "uploaded_treatment"] <- data_upload_values$rawdata[, input$treatment]
                     data_upload_values$treatment_name <- "uploaded_treatment"
                   } else{
                     data_upload_values$treatment_name <- input$treatment
                     output$treatment_name_change <- NULL
                   }
                   
                 })
                 
                 observeEvent(input$survey_weight_var, {
                   
                   if (input$survey_weight_var == "weights"){
                     ## Change survey weights variable name in uploaded data
                     data_upload_values$rawdata[, "uploaded_weights"] <- data_upload_values$rawdata[, input$survey_weight_var]
                     data_upload_values$survey_weight_var <- "uploaded_weights"
                   } else{
                     data_upload_values$survey_weight_var <- input$survey_weight_var
                     output$survey_weight_name_change <- NULL
                   }
                   
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
                 
                 ## If "survey weight" checked, show picker selection
                 observeEvent(input$survey_weight_checkbox, {
                   
                   if (input$survey_weight_checkbox){
                     output$survey_weight_var <- renderUI(
                       ## If survey weights present, allow user to select from variables in data
                       div(
                         pickerInput(session$ns("survey_weight_var"), label = i18n$t("Upload Survey weights"),
                                     choices=names(isolate(data_upload_values$rawdata)), selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F))
                       )
                     )
                     
                   } else{
                     output$survey_weight_var <- NULL ## If checkbox unselected, disable "non-response" checkbox
                     shinyjs::disable("non_response_weight_checkbox")
                   }
                 })
                 
                 ## If survey weight variable is selected, enable "non-response" ckeckbox 
                 observeEvent(input$survey_weight_var, {
                   
                   if (is.null(input$survey_weight_var)){
                     updateCheckboxInput(session, inputId = "non_response_weight_checkbox", value = FALSE)
                     shinyjs::disable("non_response_weight_checkbox")
                   } else{
                     shinyjs::enable("non_response_weight_checkbox")
                   }
                 }, ignoreNULL = FALSE)
                 
                 
                 ## If "clustering variable" checked, show picker selection
                 observeEvent(input$clustering_checkbox, {
                   
                   if (input$clustering_checkbox){
                     output$clustering_var <- renderUI(
                       pickerInput(session$ns("clustering_var"), label = i18n$t("Upload Clustering selection"),
                                   choices=names(isolate(data_upload_values$rawdata)), selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)))
                   } else{
                     output$clustering_var <- NULL ## If checkbox unseleceted, show nothing
                   }
                 })
                 
                 ## If "stratification variable" checked, show picker selection
                 observeEvent(input$stratification_checkbox, {
                   
                   if (input$stratification_checkbox){
                     output$stratification_var <- renderUI(
                       pickerInput(session$ns("stratification_var"), label = i18n$t("Upload Stratification selection"),
                                   choices=names(isolate(data_upload_values$rawdata)), selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)))
                   } else{
                     output$stratification_var <- NULL ## If checkbox unselected, show nothing
                   }
                 })
                 
                 # Data Upload ----
                 
                 ## Update app when file uploaded
                 observeEvent(input$file1, {
                   
                   # Reset any input errors and hide validation tab
                   reset_upload_page(reset_errors = TRUE, hide_validation = TRUE, hide_data = TRUE, parent = parent)
                   ## Remove validation info
                   data_upload_values$validation <- NULL
                   ## Disable Next button
                   shinyjs::disable("nextDU_btn")
                   ## Remove "no data" warning
                   output$no_data_warning <- NULL
                   ## Reset "survey weights", "clustering" and stratification" checkboxes to null
                   updateCheckboxInput(session, inputId = "survey_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "non_response_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "clustering_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "stratification_checkbox", value = FALSE)
                   ## Disable "non-response weight" checkbox
                   shinyjs::disable("non_response_weight_checkbox")
                   
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
                       showTab(session = parent, inputId = NS(id,"data_panel"), target = NS(id, "raw_data"), select = TRUE)

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

                         ## Reset variable inputs
                         updatePickerInput(session, "categorical_vars", choices = names(isolate(data_upload_values$rawdata)), selected=categorical_variables)
                         updatePickerInput(session, "outcome", choices=continuous_variables, selected = NULL)
                         updatePickerInput(session, "treatment", choices=names(isolate(data_upload_values$rawdata)), selected = NULL)
                         updatePickerInput(session, "matchvars", choices=names(isolate(data_upload_values$rawdata)), selected = NULL, clearOptions = TRUE)
                         updatePickerInput(session, "covars", choices=names(isolate(data_upload_values$rawdata)), selected = NULL, clearOptions = TRUE)
                         
                         updatePickerInput(session, "survey_weight_var", choices = names(isolate(data_upload_values$rawdata)))
                         updatePickerInput(session, "clustering_var", choices = names(isolate(data_upload_values$rawdata)))
                         updatePickerInput(session, "stratification_var", choices = names(isolate(data_upload_values$rawdata)))
                         
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
                   updateCheckboxInput(session, inputId = "non_response_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "clustering_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "stratification_checkbox", value = FALSE)
                   ## Disable "non-response weight" checkbox
                   shinyjs::disable("non_response_weight_checkbox")
                   
                   ## Save data source
                   data_upload_values$data_source <- "sample"
                   
                   ## Show and switch to data tab
                   showTab(session = parent, inputId = NS(id,"data_panel"), target = NS(id, "raw_data"), select = TRUE)
                   
                   ## Update variable selection
                   updatePickerInput(session, "categorical_vars", selected=c("Gender", "Reading_age15", "ReadingO_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13"), choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "outcome", selected="Anxiety_age17", choices = names(isolate(data_upload_values$rawdata))[!names(isolate(data_upload_values$rawdata)) %in% c("Gender", "Reading_age15", "SubstanceUse1_age13", "SubstanceUse2_age13", "SubstanceUse3_age13", "SubstanceUse4_age13")])
                   updatePickerInput(session, "treatment", selected="Reading_age15", choices = names(isolate(data_upload_values$rawdata)))
                   updatePickerInput(session, "matchvars", selected=names(isolate(data_upload_values$rawdata))[-c(2:4)], choices = names(isolate(data_upload_values$rawdata)))
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
                   ## Disable "non-response weight" checkbox
                   shinyjs::disable("non_response_weight_checkbox")
                   
                   ## Reset "survey weights", "clustering" and stratification" checkboxes to null
                   updateCheckboxInput(session, inputId = "survey_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "non_response_weight_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "clustering_checkbox", value = FALSE)
                   updateCheckboxInput(session, inputId = "stratification_checkbox", value = FALSE)
                   
                   output$no_data_warning <- NULL ## Remove "no data" warning
                   output$upload_error <- NULL ## Remove upload error
                   
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
                       output$no_data_warning <- renderUI(h5(i18n$t("Upload Data warning"), style = "color:red"))
                       
                     }else{
                       variable_check_info <- check_selected_variables(outcome = isolate(input$outcome),
                                                                       treatment = isolate(input$treatment),
                                                                       matchvars = isolate(input$matchvars),
                                                                       covars = isolate(input$covars),
                                                                       i18n = i18n)
                       
                       ## If there is no missing data and no variable mismatched, proceed to next tab
                       if(all(!c(variable_check_info$required_input_missmatched, variable_check_info$required_input_missing))){
                         ## Show and switch to validate tab
                         showTab(inputId = NS(id,"data_panel"), target = NS(id,"data_validation"), select = TRUE, session = parent)
                         
                         ## Validate data
                         ## First, ensure all data is numeric as required for cor estimate, categorical variables will later be factorised
                         data_upload_values$rawdata <- mutate_all(data_upload_values$rawdata, function(x) as.numeric(as.character(x)))
                         
                         data_upload_values$validation  <- get_validation(.data = data_upload_values$rawdata, 
                                                                          outcome = isolate(input$outcome), 
                                                                          treatment = isolate(input$treatment), 
                                                                          matchvars = isolate(input$matchvars), 
                                                                          covars = isolate(input$covars), 
                                                                          survey_weight_var = isolate(input$survey_weight_var),
                                                                          non_response_weight = isolate(input$non_response_weight_checkbox),
                                                                          clustering_var = isolate(input$clustering_var),
                                                                          stratification_var = isolate(input$stratification_var),
                                                                          i18n = i18n)
                         
                         
                         ## If survey design weight, clustering or stratification unsuitable (based on missingness or error in survey design), overwrite variable name with NULL or output
                         if (data_upload_values$validation$log$survey_weight_no_missingness == FALSE){
                           data_upload_values$survey_weight_var <- NULL
                         } else{ ## otherwise, overwrite with survey weight variable name to be output - check that variable name is not "weights", if it is, rename
                           if (input$survey_weight_var == "weights"){
                             data_upload_values$survey_weight_var <- "uploaded_weights"
                           } else{
                             data_upload_values$survey_weight_var <- input$survey_weight_var
                           }
                         }
                         
                         if (data_upload_values$validation$log$clustering_no_missingness == FALSE){
                           data_upload_values$clustering_var <- NULL
                         } else{ ## otherwise, overwrite with clustering variable name to be output
                           data_upload_values$clustering_var <- input$clustering_var
                         }
                         
                         if (data_upload_values$validation$log$stratification_no_missingness == FALSE){
                           data_upload_values$survey_weight <- NULL
                         } else{ ## otherwise, overwrite with stratification variable name to be output
                           data_upload_values$survey_weight <- input$stratification_var
                         }
                         
                         ## If validation did not catch any fatal errors, enable "Next" button
                         if (data_upload_values$validation$log$treatment_variable_error == FALSE){
                           shinyjs::enable("nextDU_btn")
                         }
                         
                         ## Display warning under data/variable selection warning about reselection requiring revalidation/rerun of subsequent analysis
                         data_upload_output$data_upload_rerun_message <- p(i18n$t("Upload Warning rerun"))
                         
                         ## Factorise categorical variables
                         data_upload_values$rawdata <- data_upload_values$rawdata %>%
                           mutate(across(isolate(input$categorical_vars), (as.factor)))
                       }
                     }}
                 })
                 
                 observeEvent(input$nextDU_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "CF_approach-tab")
                 })
                 
                 ## Pass output to UI ----
                 
                 output$contents <- DT::renderDataTable({DT::datatable(round(mutate_all(data_upload_values$rawdata, function(x) as.numeric(as.character(x))), 2), options = list(dom = 't', scrollX = TRUE))})
                 output$data_validation <- renderUI(p(data_upload_values$validation$print))
                 output$data_upload_rerun_message <- renderUI(data_upload_output$data_upload_rerun_message)
                 output$upload_error <- renderUI(data_upload_values$upload_error)
                 
                 ## Return data upload output to server ----
                 
                 ## Output list containing: dataset and input variables
                 data_upload_output <- reactiveValues(data = NULL,
                                                      data_source = NULL,
                                                      file_path = NULL,
                                                      categorical_vars = NULL,
                                                      outcome = NULL,
                                                      treatment = NULL,
                                                      matchvars = NULL,
                                                      covars = NULL,
                                                      survey_weight_var = NULL,
                                                      cluster_var = NULL,
                                                      stratification_var = NULL,
                                                      validation_log = NULL)
                 
                 observe({
                   data_upload_output$data <- data_upload_values$rawdata[,names(data_upload_values$rawdata)  %in% unique(c(data_upload_values$treatment_name, input$outcome, input$matchvars, input$covars, data_upload_values$survey_weight_var, data_upload_values$clustering_var, data_upload_values$stratification_var))]
                   data_upload_output$data_source <- data_upload_values$data_source
                   data_upload_output$file_path <- input$file1$datapath
                   data_upload_output$categorical_vars <- input$categorical_vars
                   data_upload_output$outcome <- input$outcome
                   data_upload_output$treatment <- data_upload_values$treatment_name
                   data_upload_output$matchvars <- input$matchvars
                   data_upload_output$covars <- input$covars
                   
                   data_upload_output$survey_weight_var <- data_upload_values$survey_weight_var
                   data_upload_output$cluster_var <- data_upload_values$clustering_var
                   data_upload_output$stratification_var <- data_upload_values$stratification_var
                   data_upload_output$validation_log <- data_upload_values$validation$log
                 })
                 
                 return(data_upload_output)
                 
               })
}
