# counterfactual approach module ----

CF_approach_ui <- function(id) {
  
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", p("GET STARTED", style="color: white;")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: white; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p(p("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))), style="color: white;")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: white; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p("APPROACH", style="color: white; border-bottom: solid 2px white; border-radius: 5px")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: #607cc4; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p("BALANCING", style="color: #607cc4;")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: #607cc4; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p("OUTCOME", style="color: #607cc4;"))
           ),
           div(align="center",
               actionButton(NS(id, 'prev_CF_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), 'Next', class = "default_button")),
           br(),
           
           ## CF approach choices
           div(style = "display: flex;",
               div(style = "width: 32.67%;",
                   class = "text_blocks",
                   
                   radioButtons(NS(id, "CF_radio"), label = h4("Choose a Counterfactual Approach:"),
                                choices = list("Propensity Matching" = "matching", 
                                               "Inverse probability of treatment weighting (IPTW)" = "iptw",
                                               "Non-Bipartite (NBP)" = "NBP"),
                                selected = character(0)),
                   
                   ## If no approach is selected before pressing "Next", error message will be displayed
                   uiOutput(ns("approach_missing_message")),
                   
                   ## Once approach has been selected, message warning about analysis rerun will be displayed
                   uiOutput(ns("approach_missing_message")),
                   uiOutput(ns("approach_rerun_message")),

                   ## Description of selected counterfactual approach
                   uiOutput(ns("approach_description"))
                   
               ),
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   radioButtons(NS(id, "missingness_radio"), label = h4("Missingess:"),
                                choices = list(
                                  #"Full Information Maximum Likelihood (FIML)" = "fiml", 
                                  "Multiple Imputation" = "mi",
                                  #"Weighting" = "weighting",
                                  "Complete Case" = "complete"),
                                selected = character(0)),
                   uiOutput(ns("missingness_missing_message"), style = "color: red;"), ## If no model missingness selected when "Run" pressed, give warning
                   uiOutput(ns("missingness_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                   br(),
                   uiOutput(ns("missingness_description"))
                   
               ),
               
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "balancing_model_radio"), label = h4("Choose a Balancing Model:"),
                                choices = list(
                                  #"Gradient Boosting Machine (GBM)" = "gbm",
                                  #"Random Forest" = "rforest"),
                                  "Probit Regression" = "glm"),
                                selected = character(0)),
                   uiOutput(ns("balancing_model_missing_message"), style = "color: red;"), ## If no model selected when "Run" pressed, give warning
                   uiOutput(ns("balancing_model_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                   br(),
                   uiOutput(ns("balancing_model_description")), ## Description of selected balancing model
                   
                   p("For more information on balancing model options, visit our ", actionLink(ns("balancing_model_tab_tutorial_link"), "tutorial"), ".")
               ))
  )
}

CF_approach_server <- function(id, parent, raw_data, outcome_variable, treatment_variable, matching_variables, categorical_variables, covariates, descriptions) {
  
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
                   balancing_model_rerun_message = NULL
                   )
                 
                 
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
                     missingness_values$missingness_description <- descriptions$mi
                   }
                   if(input$missingness_radio == "weighting"){
                     missingness_values$missingness_description <- descriptions$weighting
                   }
                   if(input$missingness_radio == "complete"){
                     missingness_values$missingness_description <- descriptions$completecase
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
                 

                 
                 ## When "Prev is selected", go back to data upload page
                 observeEvent(input$prev_CF_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_CF_btn, {
                   
                   if(is.null(input$CF_radio)){
                     CF_approach_values$approach_missing_message <- p("Please select an approach before proceeding", style = "color:red")
                   }
                   if(is.null(input$missingness_radio)){
                     missingness_values$missingness_missing_message <- p("Please select a method of dealing with missing data before proceeding", style = "color:red")
                   }
                   if(is.null(input$balancing_model_radio)){
                     missingness_values$balancing_model_message <- p("Please select a method of dealing with missing data before proceeding", style = "color:red")
                   }
                   else{
                     
                     ## Add rerun warning to each parameter
                     CF_approach_values$approach_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                     CF_approach_values$missingness_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                     CF_approach_values$balancing_model_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                     
                     ## Proceed to balancing page
                     updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing-tab")
                   }
                 })


                                  ## If data/variable selection has changed since previous approach selection, add question asking if current approach is still
                 ## appropriate to approach description
                 observeEvent(c(raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates()), {
                   
                   ## First check if an approach has been selected yet, if yes, undo approach selection and add informative message to approach description
                   if (!is.null(input$CF_radio)){
                     ## Informative message indicating data/variables have been changed
                     CF_approach_values$approach_missing_message <- p(strong("It looks like your data and/or variable selection has changed since first choosing an counterfactual approach. Please pick the counterfactual approach that you would like to carry out with your new data."))
                   }
                   
                   ## First check if an missingness has been selected yet, if yes, undo missingness selection and add informative message to missingness description
                   if (!is.null(input$CF_radio)){
                     ## Informative message indicating data/variables have been changed
                     CF_missingness_values$missingness_missing_message <- p(strong("It looks like your data and/or variable selection has changed since first choosing a missingness approach. Please pick the missingness method that you would like to apply to your new data."))
                   }
                   
                   ## First check if an approach has been selected yet yet, if yes, undo approach selection and add informative message to approach description
                   if (!is.null(input$CF_radio)){
                     ## Informative message indicating data/variables have been changed
                     CF_approach_values$approach_missing_message <- p(strong("It looks like your data and/or variable selection has changed since first choosing an approach. Please pick the balancing model that you would like to run with your new data."))
                   }
                   
                   
                 })
                 
                 ## Display information for choosing counterfactual approach, missingness and balancing model
                 output$approach_description <- renderUI(CF_approach_values$description)
                 output$approach_missing_message <- renderUI(CF_approach_values$approach_missing_message)
                 output$approach_rerun_message <- renderUI(CF_approach_values$approach_rerun_message)
                 
                 output$missingness_description <- renderUI(CF_approach_values$missingness_description)
                 output$missingness_rerun_message <- renderUI(CF_approach_values$missingness_rerun_message)
                 output$missingness_missing_message <- renderUI(CF_approach_values$missingness_missing_message)
                 
                 output$balancing_model_description <- renderUI(CF_approach_values$balancing_model_description)
                 output$balancing_model_rerun_message <- renderUI(CF_approach_values$balancing_model_rerun_message)
                 output$balancing_model_missing_message <- renderUI(CF_approach_values$balancing_model_missing_message)
                 
                 
                 ## Return choices to server to pass to other tool pages
                 return(
                   list(
                     missingness_radio = reactive({input$missingness_radio}),
                     cfapproach_radio = reactive({input$CF_radio}),
                     balancing_model_radio = reactive({input$balancing_model_radio}))
                   )
                 
               })
}
