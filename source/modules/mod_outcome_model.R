# outcome model module ----

outcome_model_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'outcome_model_tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/outcome_model.png" width="1000"></center>'),
           
           br(), br(),
           
           ## matching method
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "outcome_model_radio"), label = h4("Choose an Outcome Model:"),
                                choices = c("Linear Regression" = "LR"),
                                selected = character(0))
               ),
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   ## Description of selected outcome model
                   uiOutput(ns("outcome_model_description_method")),
                   br(),br(),
                   p("For more information, visit our ", actionLink(ns("outcome_model_tab_tutorial_link"), "tutorial"), ".")
                   
               )),
           
           br(), br(),
           
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   ## Parameters of selected outcome_model model
                   uiOutput(ns("outcome_model_parameters_method")),
                   ),
               
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   ## Output of selected outcome_model model
                   uiOutput(ns("outcome_model_output")))
           ),
           
           
           br(), br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_outcome_model_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_outcome_model_btn'), 'Next', class = "default_button"))
           
  )
}

outcome_model_server <- function(id, parent, treatment_variable, outcome_variable, matching_variables, balancing_results) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_outcome_model_btn, {
                   updateTabsetPanel(session = session, inputId = 'tabs', selected = NS(id, 'balancing_tab'))
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_outcome_model_btn, {
                   updateTabsetPanel(session = session, inputId = 'tabs', selected = NS(id, 'get_results_tab'))
                 })
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$outcome_model_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "tutorial")
                 })
                 
                 ## Create reactive value for approach description
                 outcomeModel <- reactiveValues(
                   description_method = p(h4("Outcome Model:"),
                                          p("The outcome model within counterfactual analysis method provides the estimate of the effect of the ‘treatment’. 
                                            To do this, DigiCAT uses a linear regression to predict the mental health outcome of interest from the candidate 
                                            treatment variable, using the ‘re-balanced’ data. It is recommended that the matching variables are also included 
                                            in this model. It may also include other covariates to estimate the effect of the ‘treatment’ net of other influences 
                                            on the outcome. See the outcome model tutorial for more details.")
                                          ),
                   parameters_method = p(h4("Outcome Model Parameters:"),
                                         p("Information on parameters in use.")),
                   output = p(h4("Output:"),
                              p("Once you have selected your outcome model, press'Run' to get results."))
                   )
                 
                 ## Update matching method description and parameters based on choice of appraoch
                 observeEvent(input$outcome_model_radio,{
                   
                   if(input$outcome_model_radio == "LR"){
                     outcomeModel$description_method <- p(h4("Outcome Model: Linear Regression:"),
                                                            br(),
                                                            p("You've choosen Linear Regression, this is why is may/may not be a good choice."))
                     
                     outcomeModel$parameters_method <- p(h4("Outcome Model Parameters: Linear Regression"),
                                                           br(),
                                                           p("Information on parameters in use."))
                     }
                 })
                 
                 ## Run outcome model 
                 source("source/func/outcome_model.R")
                
                 observeEvent(input$run_outcome_model_btn, {
                   
                   outcome_res <- outcome_analysis("unweighted", 
                                                     y_var = outcome_variable,
                                                     t_var = treatment_variable,
                                                     m_vars = matching_variables,
                                                     balanced_data = balancing_results,
                                                     ids = NULL, weights = NULL, strata = NULL, fpc = NULL, 
                                                     cf_method = "matching")
                   
                   
                   ## Output estimate
                   outcomeModel$output <- p(h4("Model Output"),
                                              p(paste0("Estimate: ", round(outcome_res[2,2], 3))),
                                              p(paste0("Standard Error: ", round(outcome_res[2,3], 3))),
                                              p(paste0("P-value: ", round(outcome_res[2,6], 3)))
                                              )
                   

                 })
                 
                 ## Display information for choosing counterfactual approach, relevent parameters and model output
                 output$outcome_model_description_method <- renderUI(outcomeModel$description_method)
                 output$outcome_model_parameters_method <- renderUI(outcomeModel$parameters_method)
                 output$outcome_model_output <- renderUI(outcomeModel$output)
                 
               })
}
