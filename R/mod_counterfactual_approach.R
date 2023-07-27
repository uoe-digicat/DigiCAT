# counterfactual approach module ----

CF_approach_ui <- function(id) {
  
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/CF_approach.png" width="1000px"></center>'),
           # div(style="display: flex; align: center; width: '1000px'; margin:auto",
           #     div(style="width: 160px; text-align: center;", p("GET STARTED")),
           #     div(style="width: 160px; text-align: center;", p("DATA UPLOAD"),uiOutput(ns("prog_choiceDU"))),
           #     div(style="width: 160px; text-align: center;", p("APPROACH", style="border-bottom: solid 5px red;")),
           #     div(style="width: 160px; text-align: center;", p("BALANCING MOD")),
           #     div(style="width: 160px; text-align: center;", p("BALANCING")),
           #     div(style="width: 160px; text-align: center;", p("OUTCOME"))
           # ),
           div(align="center",
               actionButton(NS(id, 'prev_CF_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), 'Next', class = "default_button")),
           br(),
           
           ## CF approach choices
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   
                   radioButtons(NS(id, "CF_radio"), label = h4("Choose a Counterfactual Approach:"),
                                choices = list("Propensity Matching" = "matching", 
                                               " Inverse probability of treatment weighting (IPTW)" = "iptw"),
                                selected = character(0)),
                   
                   ## If no approach is selected before pressing "Next", error message will be displayed
                   uiOutput(ns("approach_missing_message")),
                   
                   ## Once approach has been selected, message warning about analysis rerun will be displayed
                   uiOutput(ns("approach_rerun_message")),
                   uiOutput(ns("approach_rerun_warning")),
                   
                   ## Description of selected counterfactual approach
                   uiOutput(ns("approach_description"))
                   
               ),
               div(style = "width: 49%; margin-left: 2%;",
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
                   
               ))
           
  )
}

CF_approach_server <- function(id, parent, raw_data, outcome_variable, treatment_variable, matching_variables, categorical_variables, covariates, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()))})
                 
                 
                 ## Create reactive value for approach description
                 CF_approach_values <- reactiveValues(
                   description = descriptions$cfapproach,
                   approach_rerun_message = p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey"),
                   approach_rerun_warning = NULL)
                 
                 missingness_values <- reactiveValues(missingness_description = descriptions$missingness)
                 
                 observeEvent(input$missingness_radio,{
                   
                   if(input$missingness_radio == "fiml"){
                     missingness_values$missingness_description <- descriptions$fiml
                     missingness_values$missingness_parameters <- p(h4("Model Missingness Parameters: Full Information Maximum Likelihood (FIML)"),p("Information on parameters in use."))
                   }
                   if(input$missingness_radio == "mi"){
                     missingness_values$missingness_description <- descriptions$mi
                     missingness_values$missingness_parameters <- p(h4("Model Missingness Parameters: Multiple Imputation"),p("Information on parameters in use."))
                   }
                   if(input$missingness_radio == "weighting"){
                     missingness_values$missingness_description <- descriptions$weighting
                     missingness_values$missingness_parameters <- p(h4("Model Missingness Parameters: Weighting"),p("Information on parameters in use."))
                   }
                   if(input$missingness_radio == "complete"){
                     missingness_values$missingness_description <- descriptions$completecase
                     missingness_values$missingness_parameters <- p(h4("Model Missingness Parameters: Complete Cases"),p("Information on parameters in use."))
                   }
                   ## Remove message with no missingness method selected error if present
                   missingness_values$missingness_missing_message <- NULL
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
                   else{
                     updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing_model-tab")
                   }
                 })
                 

                 ## Update guide information based on choice of approach
                 observeEvent(input$CF_radio,{
                   if(input$CF_radio == "matching"){
                     CF_approach_values$description <- descriptions$prop_matching
                   }
                   if(input$CF_radio == "weighting"){
                     CF_approach_values$description <- descriptions$iptw
                   }
                   
                   CF_approach_values$approach_missing_message <- NULL
                   CF_approach_values$approach_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                   CF_approach_values$approach_rerun_warning <- NULL
                   
                 })
                 
                 ## If data/variable selection has changed since previous approach selection, add question asking if current approach is still
                 ## appropriate to appraoch description
                 observeEvent(c(raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates()), {
                   
                   ## First check if an approach has been selected yet yet, if yes, undo approach selection and add informative message to approach description
                   if (!is.null(input$CF_radio)){
                     
                     ## Update approach radio button so nothing is selected
                     updateRadioButtons(session, "CF_radio", selected = character(0))
                     updateRadioButtons(session, "missingness_radio", selected = character(0))
                     ## Informative message indicating data/variables have been changed
                     CF_approach_values$approach_rerun_warning <- p(strong("It looks like your data and/or variable selection has changed since first choosing an approach. Please pick the counterfactual approach that you would like to carry out with your new data."))
                     ## Remove message about rerunning all subsequent analysis upon reselection
                     CF_approach_values$approach_rerun_message <- NULL
                     missingness_values$missingness_rerun_message <- NULL
                     
                   }
                 })
                 
                 ## Display information for choosing counterfactual approach
                 output$approach_description <- renderUI(CF_approach_values$description)
                 output$approach_missing_message <- renderUI(CF_approach_values$approach_missing_message)
                 output$approach_rerun_message <- renderUI(CF_approach_values$approach_rerun_message)
                 output$approach_rerun_warning <- renderUI(CF_approach_values$approach_rerun_warning)
                 
                 output$missingness_description <- renderUI(missingness_values$missingness_description)
                 output$missingness_rerun_message <- renderUI(missingness_values$missingness_rerun_message)
                 output$missingness_missing_message <- renderUI(missingness_values$missingness_missing_message)
                 
                 
                 return(
                   list(
                     missingness_radio = reactive({input$missingness_radio}),
                     cfapproach_radio = reactive({input$CF_radio}))
                   )
                 
               })
}
