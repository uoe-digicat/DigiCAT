outcome_model_ui <- function(id) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", p("GET STARTED", style="color: white;")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: white; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p(p("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))), style="color: white;")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: white; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p(p("APPROACH"), p(uiOutput(ns("prog_choiceCF"))), style="color: white")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: white; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p(p("BALANCING"), p(uiOutput(ns("prog_choiceBM"))), style="color: white")),
               div(style="width: 12%; text-align: center; height: 2px; background-color: white; margin-top:10px;"),
               div(style="width: 12%; text-align: center;", p("OUTCOME", style="color: white; border-bottom: solid 2px white; border-radius: 5px;"))
           ),
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_outcome_model_btn'), 'Run', class = "default_button")
               ## actionButton(NS(id, 'next_outcome_model_btn'), 'Next', class = "default_button")
               ),
           br(),
           
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
           )
           
  )
}

outcome_model_server <- function(id, parent, treatment_variable, outcome_variable, matching_variables, approach, missingness, balancing_model, balancing_method, balancing_ratio, balancing_res, balancing_model_res, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()))})
                 output$prog_choiceCF <- renderUI({p(paste0("Counterfactual Approach: ", approach()),br(),paste0("Missingness: ", missingness()),br(),paste0("Model: ", balancing_model()))})
                 
                 observeEvent(approach() | missingness() | balancing_model(), {
                   
                   ## First check balancing has been run
                   if (!is.null(balancing_model_res)){
                     
                     if (approach() == "matching" | approach() == "NBP"){
                       
                       output$prog_choiceBM <- renderUI({p(paste0("Balancing Method: ",balancing_method()), br(), paste0("Balancing Ratio: ", balancing_ratio()))})
                     }
                     if (approach() == "iptw"){
                       output$prog_choiceBM <- NULL
                     }
                   }
                 })

                 ## Disable 'Next' button initially
                 #shinyjs::disable("next_outcome_model_btn")
                 
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
                   description_method = descriptions$outcome_model,
                   parameters_method = p(h4("Outcome Model Parameters:"),
                                         p("Information on parameters in use.")),
                   output = p(h4("Output:"),
                              p("Once you have selected your outcome model, press'Run' to get results."))
                   )
                 
                 ## Update matching method description and parameters based on choice of appraoch
                 observeEvent(input$outcome_model_radio,{
                   
                   if(input$outcome_model_radio == "LR"){
                     outcome_model_values$description_method <- decriptions$linear_regression
                     
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
                     #shinyjs::disable("next_outcome_model_btn")
                     
                   }
               })
                 
                 ## Run outcome model 
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
                                                       cf_method = approach$cfapproach_radio(),
                                                       psmod = balancing_model$results),
                       
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
                                                  descriptions$estimate,
                                                  strong(p(paste0("Estimate: ", round(outcome_model_values$results[2,2], 4)))),
                                                  br(),
                                                  descriptions$standard_error,
                                                  strong(p(paste0("Standard Error: ", round(outcome_model_values$results[2,3],3)))),
                                                  br(),
                                                  descriptions$pvalue,
                                                  strong(p(paste0("P-value: ", round(outcome_model_values$results[2,6], 3))))
                                                    )
                         
                         ## Add message noting that parameter reselection will require rerun
                         outcome_model_values$model_rerun_message <- p("Note: Changing this parameter will require outcome model to be rerun")
                         
                         
                         ## Enable 'Run' and 'Next' buttons
                         #shinyjs::enable("next_outcome_model_btn")
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
                     #shinyjs::disable("next_outcome_model_btn")
                     
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
