# counterfactual approach module ----

CF_approach_ui <- function(id, descriptions) {
  
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           useShinyjs(),
           br(),
           
           ## Navigation bar ----
           
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5("GET STARTED")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("APPROACH", style="color: white; border-bottom: solid 2px white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("BALANCING", style="color: #607cc4;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("OUTCOME", style="color: #607cc4;"))
           ),
           
           ## Navigation ----
           
           div(align="center",
               actionButton(NS(id, 'prev_CF_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), 'Next', class = "default_button")),
           br(),
           
           ## Approach selection ----
           
           div(style = "display: flex;",
               ## CF approach choices
               div(style = "width: 32.67%;",
                   class = "text_blocks",
                   uiOutput(ns("approach_selection")), ## Based on treatment variable type, display approach choices
                   uiOutput(ns("approach_missing_message")), ## If run is clicked before approach selected, issue warning 
                   uiOutput(ns("approach_rerun_message")), ## Give warning that rerun required upon re-selection
                   br(),
                   ## Description of genral and selected counterfactual approach
                   uiOutput(ns("approach_description_general")), ## Add description of approach selected
                   br(),
                   uiOutput(ns("approach_description")) ## Add description of approach selected
                   
               ),
               ## Missingness choices
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   uiOutput(ns("missingness_selection")), ## Based on presence of complete non-response weights, display approach choices
                   uiOutput(ns("missingness_missing_message"), style = "color: grey;"), ## If run is clicked before missingness selected, issue warning 
                   uiOutput(ns("missingness_rerun_message")), ## Give warning that rerun required upon re-selection
                   br(),
                   p(descriptions$missingness), ## Add general description
                   br(),
                   uiOutput(ns("missingness_description")) ## Add description of missingness selected
                   
               ),
               ## Balancing model choices
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   uiOutput(ns("balancing_model_selection")), ## Based on missingness choices, display balancing model choices
                   uiOutput(ns("balancing_model_missing_message")), ## If run is clicked before model selected, issue warning 
                   uiOutput(ns("balancing_model_rerun_message")), ## Give warning that rerun required upon re-selection
                   br(),
                   p(descriptions$balancing_model), ## Add general description
                   br(),
                   uiOutput(ns("balancing_model_description")) ## Description of selected balancing model
               ))
  )
}

CF_approach_server <- function(id, parent, enableLocal, raw_data, outcome_variable, treatment_variable, matching_variables, categorical_variables, covariates, survey_weight_var, cluster_var, stratification_var, validation_log, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- NS(id)
                 
                 ## Navigation bar ----
                 
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()), style="width: 200%; margin-left: -50%")})
                 
                 ## Define Reactives ----
                 
                 ## Create reactive value for approach description
                 CF_approach_values <- reactiveValues(
                   approach_description = descriptions$cfapproach_temp,
                   approach_rerun_message = NULL,
                   approach_missing_message = NULL,
                   missingness_description = NULL,
                   missingness_missing_message = NULL,
                   missingness_rerun_message = NULL,
                   balancing_model_description = NULL,
                   balancing_model_missing_message = NULL,
                   balancing_model_rerun_message = NULL,
                   page_complete = NULL
                 )
                 
                 ## Navigation ----
                 
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
                   ## Only continue if all required input is given
                   if(!is.null(input$CF_radio) & !is.null(input$missingness_radio) & !is.null(input$balancing_model_radio)){
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
                 
                 ## Reset page if counterfactual approach inputs have changed ----
                 
                 ## If data/variable selection has changed since previous approach selection, add question asking if current approach is still
                 ## appropriate to approach description and reset page
                 observeEvent(c(raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates(), survey_weight_var(), cluster_var(), stratification_var(), validation_log()), {
                   
                   ## Only run if validation has been carried out
                   if (!is.null(validation_log())){
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
                                                                            choices = list("Propensity Matching (PSM)" = "psm",
                                                                                           "Inverse probability of treatment weighting (IPTW)" = "iptw"),
                                                                            selected = character(0))
                       )
                       
                       output$approach_description_general <- renderUI(descriptions$cfapproach_binary)
                       
                     }
                     if (length(unique(na.omit(raw_data()[,treatment_variable()]))) > 2){
                       
                       if (enableLocal){
                         output$approach_selection <- renderUI(p("Non-bipartite Matching (NBP) coming soon." ,style = "color:grey"))
                       } else{
                         output$approach_selection <- renderUI(radioButtons(ns("CF_radio"),
                                                                            label = h4("1. Choose a Counterfactual Approach:"),
                                                                            choices = list("Non-bipartite Matching" = "nbp"),
                                                                            selected = character(0)))
                       }
                       
                       output$approach_description_general <- renderUI(descriptions$cfapproach_ordinal)

                       ## If no missingness detected and no non repose weight, only offer complete case
                       if(validation_log()$no_missingness_no_non_response){
                         
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Complete Case" = "complete"),
                                        selected = "complete"),
                           br(),
                           p("No missing data detected, please proceed with complete case", style = "color:gray;")
                         )
                         )
                       }
                       
                       ## If no missingness detected but non reponse weight, offer complete case and weighting
                       if(validation_log()$no_missingness_but_non_response){
                         
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Weighting" = "weighting",
                                          "Complete Case" = "complete"),
                                        selected = character(0)),
                           br(),
                           p("No missing data detected, please proceed with complete case or weighting", style = "color:gray;")
                         )
                         )
                       }
                       
                       ## If there is data missingness and non response weights provided, include weighting as a missingness option
                       if (validation_log()$some_missingness_but_non_response){
                         
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Weighting" = "weighting",
                                          "Multiple Imputation" = "mi",
                                          "Complete Case" = "complete"),
                                        selected = character(0))
                         )
                         )
                       }
                       
                       ## If there is data missingness and no non response weight provided, don't add weighting
                       if (validation_log()$some_missingness_no_non_response){
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Complete Case" = "complete",
                                          "Multiple Imputation" = "mi"),
                                        selected = character(0))
                         ))
                       }
                       
                       
                       ## OLR available as balancing model
                       output$balancing_model_selection <- renderUI(p(
                         radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                      choices = list(
                                        "Ordinal Logistic Regression (OLR)" = "poly"),
                                      selected = character(0))
                       )
                       )
                     }
                     
                     
                     ## Check presents of missing data and non-response variable, update missingness accoridingly and add initial balancing model message if treatment variable is binary
                     if (length(unique(na.omit(raw_data()[,treatment_variable()]))) == 2){
                       
                       ## If no missingness detected and no non repose weight, only offer complete case
                       if(validation_log()$no_missingness_no_non_response){
                         
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Complete Case" = "complete"),
                                        selected = "complete"),
                           br(),
                           p("No missing data detected, please proceed with complete case", style = "color:gray;")
                         )
                         )
                       }
                       
                       ## If no missingness detected but non reponse weight, offer complete case and weighting
                       if(validation_log()$no_missingness_but_non_response){
                         
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Weighting" = "weighting",
                                          "Complete Case" = "complete"),
                                        selected = character(0)),
                           br(),
                           p("No missing data detected, please proceed with complete case or weighting", style = "color:gray;")
                         )
                         )
                       }
                       
                       ## If there is data missingness and non response weights provided, include weighting as a missingness option
                       if (validation_log()$some_missingness_but_non_response){
                         
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Multiple Imputation" = "mi",
                                          "Weighting" = "weighting",
                                          "Complete Case" = "complete"),
                                        selected = character(0))
                         )
                         )
                       }
                       
                       ## If there is data missingness and no non response weight provided, don't add weighting
                       if (validation_log()$some_missingness_no_non_response){
                         output$missingness_selection <- renderUI(p(
                           radioButtons(NS(id, "missingness_radio"), 
                                        label = h4("2. Choose a Method of Dealing with Missingess:"),
                                        choices = list(
                                          "Multiple Imputation" = "mi",
                                          "Complete Case" = "complete"),
                                        selected = character(0))
                         ))
                       }
                       
                       ## Add message stating balancing model depends on missingness and choices will be displayed after missingness selection
                       output$balancing_model_selection <- renderUI(p("As the choices of balancing model depends on the counterfactual approach 
                                                                      and method of dealing with missingness, balancing model options will appear 
                                                                      once these choices have been selected."))
                     }}
                 })
                 
                 
                 ## Update balancing model choice when approach/missingness changes ----
                 
                 observeEvent(c(input$CF_radio, input$missingness_radio), {
                   
                   ## Do nothing if approach and missingess have not both been selected
                   if(is.null(input$CF_radio) | is.null(input$missingness_radio)){
                     
                   }else{
                     
                     ## If approach is NBP:
                     if (input$CF_radio == "nbp"){
                       ## Do nothing as options are already displayed
                     }
                     
                     ## If approach other than NBP selected, base balancing model choice off of missingness
                     else {
                       if (input$missingness_radio == "complete" | input$missingness_radio == "mi"){
                         
                         ## If there are more than 50 or more rows in data, include GBM
                         if (!validation_log()$no_GBM){
                           
                           output$balancing_model_selection <- renderUI(p(
                             radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                          choices = list(
                                            "Generalized Boosted Models (GBM)" = "gbm",
                                            "Random Forest" = "rf",
                                            "Logistic Regression" = "glm"),
                                          selected = character(0)))
                           )
                         }
                         else{
                           output$balancing_model_selection <- renderUI(p(
                             radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                          choices = list(
                                            "Random Forest" = "rf",
                                            "Logistic Regression" = "glm"),
                                          selected = character(0)),
                             br(),
                             p("Generalized Boosted Models (GBM) not available for small datasets (< 50 rows)", style = "color:gray;"))
                           )
                           
                         }
                       }
                       
                       if (input$missingness_radio == "weighting"){
                         output$balancing_model_selection <- renderUI(p(
                           radioButtons(NS(id, "balancing_model_radio"), label = h4("3. Choose a Balancing Model:"),
                                        choices = list(
                                          "Logistic Regression" = "glm"),
                                        selected = character(0)))
                         )
                       }
                     }
                   }
                 })
                 
                 
                 ## Update guide information based on choice of approach
                 observeEvent(input$CF_radio,{
                   if(input$CF_radio == "iptw"){
                     CF_approach_values$approach_description <- descriptions$iptw
                   }
                   if(input$CF_radio == "psm"){
                     CF_approach_values$approach_description <- descriptions$prop_matching
                   }
                   if(input$CF_radio == "nbp"){
                     CF_approach_values$approach_description <- NULL
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
                   
                   if(input$balancing_model_radio == "rf"){
                     CF_approach_values$balancing_model_description <- descriptions$rf
                   }
                   
                   if(input$balancing_model_radio == "gbm"){
                     CF_approach_values$balancing_model_description <- descriptions$gbm
                   }
                   
                   if(input$balancing_model_radio == "poly"){
                     CF_approach_values$balancing_model_description <- descriptions$olr
                   }
                   
                   ## Remove message with no balancing method selected error if present
                   CF_approach_values$balancing_model_missing_message <- NULL
                 })
                 
                 
                 ## Pass output to UI ----
                 
                 output$approach_description <- renderUI(CF_approach_values$approach_description)
                 output$approach_missing_message <- renderUI(CF_approach_values$approach_missing_message)
                 output$approach_rerun_message <- renderUI(CF_approach_values$approach_rerun_message)
                 
                 output$missingness_description <- renderUI(CF_approach_values$missingness_description)
                 output$missingness_rerun_message <- renderUI(CF_approach_values$missingness_rerun_message)
                 output$missingness_missing_message <- renderUI(CF_approach_values$missingness_missing_message)
                 
                 output$balancing_model_description <- renderUI(CF_approach_values$balancing_model_description)
                 output$balancing_model_rerun_message <- renderUI(CF_approach_values$balancing_model_rerun_message)
                 output$balancing_model_missing_message <- renderUI(CF_approach_values$balancing_model_missing_message)
                 
                 ## Return counterfactual approach output to server ----
                 
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
