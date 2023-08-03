# counterfactual approach module ----

CF_approach_ui <- function(id) {
  
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5("GET STARTED", style="color: white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))), style="color: white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("APPROACH", style="color: white; border-bottom: solid 2px white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("BALANCING", style="color: #607cc4;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("OUTCOME", style="color: #607cc4;"))
           ),
           div(align="center",
               actionButton(NS(id, 'prev_CF_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), 'Next', class = "default_button")),
           br(),
           
           ## CF approach choices
           div(style = "display: flex;",
               div(style = "width: 32.67%;",
                   class = "text_blocks",
                   uiOutput(ns("approach_selection")), ## Based on treatment variable type, display approach choices
                   uiOutput(ns("approach_missing_message")), ## If run is clicked before approach selected, issue warning 
                   uiOutput(ns("approach_rerun_message")), ## Give warning that rerun required upon re-selection
                   br(),
                   ## Description of selected counterfactual approach
                   uiOutput(ns("approach_description")) ## Add description of approach selected
                   
               ),
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   uiOutput(ns("missingness_selection")), ## Based on presence of complete non-response weights, display approach choices
                   uiOutput(ns("missingness_missing_message"), style = "color: grey;"), ## If run is clicked before missingness selected, issue warning 
                   uiOutput(ns("missingness_rerun_message")), ## Give warning that rerun required upon re-selection
                   br(),
                   uiOutput(ns("missingness_description")) ## Add description of missingness selected
                   
               ),
               
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   uiOutput(ns("balancing_model_selection")), ## Based on missingness choices, display balancing model choices
                   uiOutput(ns("balancing_model_missing_message")), ## If run is clicked before model selected, issue warning 
                   uiOutput(ns("balancing_model_rerun_message")), ## Give warning that rerun required upon re-selection
                   br(),
                   uiOutput(ns("balancing_model_description")), ## Description of selected balancing model
                   
                   p("For more information on balancing model options, visit our ", actionLink(ns("balancing_model_tab_tutorial_link"), "tutorial"), ".")
               ))
  )
}

CF_approach_server <- function(id, parent, raw_data, outcome_variable, treatment_variable, matching_variables, categorical_variables, covariates, NRW_var, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()))})
                 
                 ## Create reactive value for approach description
                 CF_approach_values <- reactiveValues(
                   approach_description = descriptions$cfapproach,
                   approach_rerun_message = NULL,
                   approach_missing_message = NULL,
                   missingness_description = descriptions$missingness,
                   missingness_missing_message = NULL,
                   missingness_rerun_message = NULL,
                   balancing_model_description = descriptions$balancing_model,
                   balancing_model_missing_message = NULL,
                   balancing_model_rerun_message = NULL,
                   page_complete = NULL
                 )
                 
                 
                 
                 ## When "Prev is selected", go back to data upload page
                 observeEvent(input$prev_CF_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_CF_btn, {
                   ## Check required inputs have been selected
                   if(is.null(input$CF_radio)){
                     CF_approach_values$approach_missing_message <- p("Please select an approach before proceeding", style = "color:red")
                   }
                   if(is.null(input$missingness_radio)){
                     CF_approach_values$missingness_missing_message <- p("Please select a method of dealing with missing data before proceeding", style = "color:red")
                   }
                   if(is.null(input$balancing_model_radio)){
                     CF_approach_values$balancing_model_missing_message <- p("Please select a method of dealing with missing data before proceeding", style = "color:red")
                   }
                   else{
                     ## Record page completion
                     CF_approach_values$page_complete <- 1
                     ## Add rerun warning to each parameter
                     CF_approach_values$approach_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                     CF_approach_values$missingness_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                     CF_approach_values$balancing_model_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                     ## Proceed to balancing page
                     updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing-tab")
                   }
                 })
                 
                 
                 ## If data/variable selection has changed since previous approach selection, add question asking if current approach is still
                 ## appropriate to approach description and reset page
                 observeEvent(c(raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates(), NRW_var()), {
                   
                   ## Remove missing input warnings
                   CF_approach_values$approach_missing_message <- NULL
                   CF_approach_values$missingness_missing_message <- NULL
                   CF_approach_values$balancing_model_missing_message <- NULL

                   ## If page has already been completed, add message stating reselection is required
                   if (!is.null(CF_approach_values$page_complete)){
                     ## Have rerun message indicating data/variables have been changed
                     CF_approach_values$approach_rerun_message <- p(strong("Something has changed on the data uplaod page, reselect the approach you would like to take."))
                     CF_approach_values$missingness_rerun_message <- p(strong("Something has changed on the data uplaod page, reselect how you would like to deal with missingness."))
                     CF_approach_values$balancing_model_rerun_message <- p(strong("Something has changed on the data uplaod page, reselect the model you would like to run."))
                   }
                   
                   ## Update approach based on treatment variable (binary/ordinal)
                   if (length(unique(na.omit(raw_data()[,treatment_variable()]))) == 2){
                     
                     output$approach_selection <- renderUI(radioButtons(NS(id, "CF_radio"), 
                                                                        label = h4("1. Choose a Counterfactual Approach:"), 
                                                                        choices = list("Propensity Matching" = "matching",
                                                                                       "Inverse probability of treatment weighting (IPTW)" = "iptw"),
                                                                        selected = character(0)))
                     
                   }
                   if (length(unique(na.omit(raw_data()[,treatment_variable()]))) > 2 & length(unique(na.omit(raw_data()[,treatment_variable()]))) < 6){
                     
                     output$approach_selection <- renderUI(radioButtons(NS(id, "CF_radio"), 
                                                                        label = h4("1. Choose a Counterfactual Approach:"), 
                                                                        choices = list("Non-bipartite Matching" = "NBP"),
                                                                        selected = character(0)))
                   }
                   
                   ## If treatment variable detected as continuous, give warning message and disable other inputs
                   if (length(unique(na.omit(raw_data()[,treatment_variable()]))) > 5){
                     
                     output$approach_selection <- renderUI(strong(p("You have selected ",treatment_variable(), " as your treatment variable. 
                     This has been detected as a continuous variable and cannot be used in the current counterfactual approaches 
                     offered by DigiCAT. Please reselect or categorize your current treatment variable."), style = "color: red;"))
                     
                     output$missingness_selection <- renderUI(strong(p("Please choose a non-continuous treatment variable before continuing with counterfactual
                                                              analysis."), style = "color: red;"))
                     
                     output$balancing_model_selection <- renderUI(strong(p("Please choose a non-continuous treatment variable before continuing with counterfactual
                                                              analysis."), style = "color: red;"))
                   }
                   
                   
                   ## Check presents on non-response variable, update missingness and add initial balancing model message if treatment variable is non-continuous
                   if (length(unique(na.omit(raw_data()[,treatment_variable()]))) < 6){
                     
                     ## If non-response weights provided (with no missingness), include weighting as a missingness option
                     if (!(is.null(NRW_var()) & any(is.na(raw_data()[,NRW_var()])))){
                       
                       output$missingness_selection <- renderUI(
                         radioButtons(NS(id, "missingness_radio"), label = h4("2. Choose a Method of Dealing with Missingess:"),
                                      choices = list(
                                        "Multiple Imputation" = "mi",
                                        "Weighting" = "weighting",
                                        "Complete Case" = "complete"),
                                      selected = character(0))
                       )
                     }
                     ## If non-response weight provided, but missingness detected, add note
                     if ((!is.null(NRW_var())) & any(is.na(raw_data()[,NRW_var()]))){
                       
                       output$missingness_selection <-renderUI(
                         p(
                           radioButtons(NS(id, "missingness_radio"), label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Multiple Imputation" = "mi",
                                          "Complete Case" = "complete"),
                                        selected = character(0)),
                           br(),
                           p("As missing data was detected in the non-response weights you provided, these cannot be used to handle missingness.
                         Providing complete non-response weights will allow weighting to be carried out, otherwise, please select from the other
                         missingness options available above.", style = "color: grey;")
                         )
                       )
                     }
                     ## If non-response weight not provided, don't add weighting
                     if (is.null(NRW_var())){
                       
                       output$missingness_selection <- renderUI(
                         radioButtons(NS(id, "missingness_radio"), label = h4("2. Choose a Method of Dealing with Missingess:"),
                                      choices = list(
                                        "Multiple Imputation" = "mi",
                                        "Complete Case" = "complete"),
                                      selected = character(0))
                       )
                     }

                     ## Add message stating balancing model depends on missingness and choices will be displayed after missingness selection
                     output$balancing_model_selection <- renderUI(p("As the choice of balancing model depends on the counterfactual approach 
                     and method of dealing with missingness, balancing model options will appear once these choices have been selected."))
                   }
                 })


                 
                 ## Update balancing model choices based on appraoch and missingness selected
                 observeEvent(c(input$CF_radio, input$missingness_radio), {
                   
                   ## Do nothing if approach and missingess have not both been selected
                   if(is.null(input$CF_radio) | is.null(input$missingness_radio)){
                     
                   }else{
                     
                     ## If approach is NBP - only balancing model option in "OLR"
                     if (input$CF_radio == "NBP"){
                       
                       output$balancing_model_selection <- renderUI(
                         radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                      choices = list(
                                        "Ordinal Logistic Regression (OLR)" = "orl"),
                         selected = character(0))
                       )
                     }
                     
                     ## If approach other than NBP selected, base balancing model choice off of missingness
                     else {
                       if (input$missingness_radio == "complete"){
                         
                         output$balancing_model_selection <- renderUI(
                           radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                        choices = list(
                                          "Gradient Boosting Machine (GBM)" = "gbm",
                                          "Random Forest" = "rforest",
                                        "Probit Regression" = "glm"),
                           selected = character(0)))
                       }
                       
                       if (input$missingness_radio == "mi" | input$missingness_radio == "weighting"){
                         output$balancing_model_selection <- renderUI(
                           radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                        choices = list(
                                        "Probit Regression" = "glm"),
                           selected = character(0)))
                       }
                     }
                   }
                 })
                 
                 
                 ## Update guide information based on choice of approach
                 observeEvent(input$CF_radio,{
                   if(input$CF_radio == "matching"){
                     CF_approach_values$approach_description <- descriptions$prop_matching
                   }
                   if(input$CF_radio == "weighting"){
                     CF_approach_values$approach_description <- descriptions$iptw
                   }
                   
                   CF_approach_values$approach_missing_message <- NULL
                 })
                 
                 observeEvent(input$missingness_radio,{
                   
                   if(input$missingness_radio == "mi"){
                     CF_approach_values$missingness_description <- descriptions$mi
                   }
                   if(input$missingness_radio == "weighting"){
                     CF_approach_values$missingness_description <- descriptions$weighting
                   }
                   if(input$missingness_radio == "complete"){
                     CF_approach_values$missingness_description <- descriptions$completecase
                   }
                   ## Remove message with no missingness method selected error if present
                   CF_approach_values$missingness_missing_message <- NULL
                 })
                 
                 
                 
                 ## Update model description and parameters based on choice of approach
                 observeEvent(input$balancing_model_radio,{
                   
                   if(input$balancing_model_radio == "glm"){
                     CF_approach_values$balancing_model_description <- descriptions$glm
                   }
                   
                   if(input$balancing_model_radio == "gbm"){
                     CF_approach_values$balancing_model_description <- descriptions$gbm
                   }
                   
                   if(input$balancing_model_radio == "rforest"){
                     CF_approach_values$balancing_model_description <- descriptions$rforest
                   }
                   
                   ## Remove message with no balancing method selected error if present
                   CF_approach_values$balancing_model_missing_message <- NULL
                 })
                 
                 
                 ## Display information for choosing counterfactual approach, missingness and balancing model
                 output$approach_description <- renderUI(CF_approach_values$approach_description)
                 output$approach_missing_message <- renderUI(CF_approach_values$approach_missing_message)
                 output$approach_rerun_message <- renderUI(CF_approach_values$approach_rerun_message)
                 
                 output$missingness_description <- renderUI(CF_approach_values$missingness_description)
                 output$missingness_rerun_message <- renderUI(CF_approach_values$missingness_rerun_message)
                 output$missingness_missing_message <- renderUI(CF_approach_values$missingness_missing_message)
                 
                 output$balancing_model_description <- renderUI(CF_approach_values$balancing_model_description)
                 output$balancing_model_rerun_message <- renderUI(CF_approach_values$balancing_model_rerun_message)
                 output$balancing_model_missing_message <- renderUI(CF_approach_values$balancing_model_missing_message)
                 
                 
                 ## Return choices to server to pass to other tool pages
                 CF_approach_output <- reactiveValues(missingness = NULL,
                                                      CF_radio = NULL,
                                                      balancing_model = NULL
                 )
                 
                 observe({
                   CF_approach_output$missingness <- input$missingness_radio
                   CF_approach_output$CF_radio <- input$CF_radio
                   CF_approach_output$balancing_model <- input$balancing_model_radio
                 })
                 
                 return(CF_approach_output)
                 
               })
}
