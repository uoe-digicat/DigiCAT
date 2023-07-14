# counterfactual approach module ----

CF_approach_ui <- function(id) {
  
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/CF_approach.png" width="1000"></center>'),
           
           br(), br(),
           
           ## CF approach choices
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   
                   radioButtons(NS(id, "CF_radio"), label = h4("Choose a Counterfactual Approach:"),
                                choices = list("Propensity Matching" = "matching", 
                                               " Inverse probability of treatment weighting (IPTW)" = "weighting"),
                                selected = character(0)),
                   
                   ## If no approach is selected before pressing "Next", error message will be displayed
                   uiOutput(ns("approach_missing_message")),
                   
                   ## Once approach has been selected, message warning about analysis rerun will be displayed
                   uiOutput(ns("approach_rerun_message"))
                   
               ),
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   ## Description of selected counterfactual approach
                   uiOutput(ns("approach_description"))
                   
               )),
           
           br(), br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_CF_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), 'Next', class = "default_button"))
           
  )
}

CF_approach_server <- function(id, parent, raw_data, outcome_variable, treatment_variable, matching_variables, categorical_variables, covariates) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Create reactive value for approach description
                 CF_approach_values <- reactiveValues(description = div(
                   h4("Description:"),
                   p("The aim of counterfactual analysis is to estimate the causal effects of 'exposures' or 'treatments' by comparing what 
                     actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual 
                     outcomes). Because we can never directly observe counterfactual outcomes, we compare groups who differ in their treatment. 
                     However, in observational settings where random allocation into different treatments is not possible, researchers may employ methods that such as 'matching' or 'weighting' of participants to ensure that the different treatment groups 
                     being compared are 'balanced' with respect to other characteristics."),
                   br(),
                   p("Please click on the approach you would like to take. We will give you more information about each approach as you select them.")))
                 
                 
                 ## When "Prev is selected", go back to data upload page
                 observeEvent(input$prev_CF_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_CF_btn, {
                   
                   if(is.null(input$CF_radio)){
                     CF_approach_values$approach_missing_message <- p("Please select an approach before proceeding", style = "color:red")
                   }
                   else{
                     updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing_model-tab")
                   }
                 })
                 

                 ## Update guide information based on choice of approach
                 observeEvent(input$CF_radio,{
                   
                   if(input$CF_radio == "matching"){
                     CF_approach_values$description <- p(h4("Propensity Matching:"), 
                                                      br(),
                                                      p("You've chosen propensity matching. This approach involves creating balanced comparison
                                                        groups by matching treated individuals with similar untreated individuals based on their propensity 
                                                        scores (probability of someone having a specific treatment based on observed characteristics). 
                                                        This aims to ensure that the groups are comparable in terms of potential confounding variables. 
                                                        The treatment effect is then estimated by comparing outcomes between the matched groups."))
                   }
                   if(input$CF_radio == "weighting"){
                     CF_approach_values$description <- p(h4("Inverse probability of treatment weighting (IPTW):"), 
                                                      br(),
                                                      p("You've choosen IPTW, this is why is may/may not be a good choice."))
                   }
                   
                   CF_approach_values$approach_missing_message <- NULL
                   CF_approach_values$approach_rerun_message <- p("Note: If counterfactual analysis has already been run, changing this parameter will require the rerun of all subsequent steps.", style = "color:grey")
                   
                 })
                 
                 ## If data/variable selection has changed since previous approach selection, add question asking if current approach is still
                 ## appropriate to appraoch description
                 observeEvent(c(raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates()), {
                   
                   ## First check if an approach has been selected yet yet, if yes, undo approach selection and add informative message to approach description
                   if (!is.null(input$CF_radio)){
                     
                     ## Update approach radio button so nothing is selected
                     updateRadioButtons(session, "CF_radio", selected = character(0))
                     
                     ## Informative message indicating data/variables have been changed
                     CF_approach_values$description <- p(h4("Propensity Matching:"),
                     p("The aim of counterfactual analysis is to estimate the causal effects of 'exposures' or 'treatments' by comparing what 
                     actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual 
                     outcomes). Because we can never directly observe counterfactual outcomes, we compare groups who differ in their treatment. 
                     However, in observational settings where random allocation into different treatments is not possible, researchers may employ methods that such as 'matching' or 'weighting' of participants to ensure that the different treatment groups 
                     being compared are 'balanced' with respect to other characteristics."),
                     br(),
                     p("Please click on the approach you would like to take. We will give you more information about each approach as you select them."),
                     br(),
                     p(strong("It looks like your data and/or variable selection has changed since first choosing an approach. Please pick the counterfactual approach that
                                                                  you would like to carry out with your new data.")))
                     

                     ## Remove message about rerunning all subsequent analysis upon reselection
                     CF_approach_values$approach_rerun_message <- NULL
  
                     
                   }
                 })
                 
                 ## Display information for choosing counterfactual approach
                 output$approach_description <- renderUI(CF_approach_values$description)
                 output$approach_missing_message <- renderUI(CF_approach_values$approach_missing_message)
                 output$approach_rerun_message <- renderUI(CF_approach_values$approach_rerun_message)
                 
                 
                 return(reactive({input$CF_radio}))
                 
               })
}
