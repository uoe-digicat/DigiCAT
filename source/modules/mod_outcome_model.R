# outcome model module ----

outcome_model_ui <- function(id) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/outcome_model.png" width="1000"></center>'),
           
           br(), br(),
           
           ## matching method
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "outcome_model_radio"), label = h4("Choose an Outcome Model:"),
                                choices = c("Linear Regression" = "LR"),
                                selected = character(0)),
                   uiOutput(ns("outcome_model_missing_message"), style = "color: red;"), ## If no model selected when "Run" pressed, give warning
                   uiOutput(ns("outcome_model_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                   
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
                   withSpinner(uiOutput(ns("outcome_model_output"))))
           ),
           
           
           br(), br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_outcome_model_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_outcome_model_btn'), 'Next', class = "default_button"))
           
  )
}

outcome_model_server <- function(id, parent, treatment_variable, outcome_variable, matching_variables, balancing_results, approach) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("next_outcome_model_btn")
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "balancing-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "get_results-tab")
                 })
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$outcome_model_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = "main_tabs", selected = "tutorial")
                 })
                 
                 ## Create reactive value for approach description
                 outcome_model_values <- reactiveValues(
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
                     outcome_model_values$description_method <- p(h4("Outcome Model: Linear Regression:"),
                                                            br(),
                                                            p("Linear regression is a way of modelling the associations between exploratory variable(s) 
                                                              and a continuous outcome variable. The model takes the form y = bX + e, where y and x are our 
                                                              outcome and explanatory variables respectively, and e is the random error. Of interest here is 
                                                              B, which is the estimated effect of our explanatory variable on our outcome. By fitting a model 
                                                              to a sample that is either matched or weighted according to the propensity of treatment, we can 
                                                              better estimate the **causal** effect of the treatment on our outcome variable."))
                     
                     outcome_model_values$parameters_method <- p(h4("Outcome Model Parameters: Linear Regression"),
                                                           br(),
                                                           p("Information on parameters in use."))
                     
                     ## Remove missing parameter message is present
                     outcome_model_values$model_missing_message  <- NULL
                   }
                   
                   
                   ## If outcome model has already been run, give informative message about rerun and disable "Next" button to force rerun
                   if (!is.null(outcome_model_values$results)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     outcome_model_values$output <- p(h4("Output:"),
                                                        p(
                                                          strong("It looks like the outcome model will have to be rerun, this is because some of the required inputs have been changed since the 
                     previous run."), "Once you have selected your outcome model, press 'Run' to get results."))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_outcome_model_btn")
                     
                   }
               })
                 
                 ## Run outcome model 
                 source("source/func/outcome_model.R")
                
                 observeEvent(input$run_outcome_model_btn, {
                   
                   ## If no outcome model has been selected, give error message
                   if(is.null(input$outcome_model_radio)){
                     outcome_model_values$model_missing_message <- p("Please select an outcome model before proceeding")
                   }
                   
                   ## If outcome model has been selected, run model
                   if (!is.null(input$outcome_model_radio)){
                     
                     ## Remove default output message
                     outcome_model_values$output <- NULL
                     
                     ## Save potential error to check for running of code dependent on outcome model
                     error_check <- NA
                     error_check <- tryCatch(
                       outcome_model_values$results <- outcome_analysis("unweighted", 
                                                       y_var = outcome_variable(),
                                                       t_var = treatment_variable(),
                                                       m_vars = matching_variables(),
                                                       balanced_data = balancing_results(),
                                                       ids = NULL, weights = NULL, strata = NULL, fpc = NULL, 
                                                       cf_method = approach()),
                       
                       ## If outcome model does not run, return error message and enable run button 
                       error = function(cond) {
                         ## Enable "Run" button
                         shinyjs::enable("run_outcome_model_btn")
                         ## Output error message
                         outcome_model_values$output <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                       })
                     
                     
                     # Display output if no error in outcome model
                     
                     if (all(!grepl("Error:", error_check))){
                       try({
                     ## Output estimate
                         outcome_model_values$output <- p(h4("Model Output"),
                                                  p("In counterfactual analysis, the estimate can be used to quantify the potential causal effect of specific factors, 
                                                    interventions, or treatments on mental health outcomes."),
                                                  strong(p(paste0("Estimate: ", round(outcome_model_values$results[2,2], 3)))),
                                                  br(),
                                                  p("The standard error is a statistical measure that quantifies the variability or uncertainty associated 
                                                    with the estimate. It provides a measure of how much the estimate is likely to vary from the true 
                                                    population value. "),
                                                  strong(p(paste0("Standard Error: ", round(outcome_model_values$results[2,3], 3)))),
                                                  br(),
                                                  p("In null-hypothesis significance testing, the p-value represents the the probability of obtaining a test 
                                                    statistic as extreme or more extreme than the one observed, assuming that the null hypothesis is true. 
                                                    Typically, if the p-value is below a predetermined significance level (often 0.05), the null hypothesis is 
                                                    rejected in favour of an alternative hypothesis, implying that there is a statistically significant effect 
                                                    or relationship in the data."),
                                                  strong(p(paste0("P-value: ", round(outcome_model_values$results[2,6], 3))))
                                                    )
                         
                         ## Add message noting that parameter reselection will require rerun
                         outcome_model_values$model_rerun_message <- p("Note: Changing this parameter will require outcome model to be rerun")
                         
                         
                         ## Enable 'Run' and 'Next' buttons
                         shinyjs::enable("next_outcome_model_btn")
                         shinyjs::enable("run_outcome_model_btn")
                       })
                     }
                     }
                   })
                 
                 
                 ## Remove outcome model output and force rerun if previous steps have changed since previous run
                 observeEvent(balancing_results(), {
                   ## First check if outcome model has been run yet, if yes, print informative message in output
                   if (!is.null(outcome_model_values$results)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     outcome_model_values$output <- p(h4("Output:"),
                                                        p(
                                                          strong("It looks like the outcome model will have to be rerun, this is because some of the required inputs have been changed since the 
                       previous run."), "Once you have selected your outcome model, press 'Run' to get results."))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_outcome_model_btn")
                     
                   }
                 })
                 
                 ## Display information for choosing counterfactual approach, relevent parameters and model output
                 output$outcome_model_description_method <- renderUI(outcome_model_values$description_method)
                 output$outcome_model_missing_message <- renderUI(outcome_model_values$model_missing_message)
                 output$outcome_model_rerun_message <- renderUI(outcome_model_values$model_rerun_message)
                 output$outcome_model_parameters_method <- renderUI(outcome_model_values$parameters_method)
                 output$outcome_model_output <- renderUI(outcome_model_values$output)
                 
               })
}
