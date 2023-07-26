# balancing model module ----

balancing_model_ui <- function(id) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for balancing model
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           #HTML('<center><img src="progress_bar/new/balancing_model.png" width="1000px"></center>'),
           div(style="display: flex; align: center; width: '1000px'; margin: auto",
               div(style="width: 160px; text-align: center;", p("GET STARTED")),
               div(style="width: 160px; text-align: center;", p("DATA UPLOAD"),uiOutput(ns("prog_choiceDU"))),
               div(style="width: 160px; text-align: center;", p("APPROACH"),uiOutput(ns("prog_choiceCF"))),
               div(style="width: 160px: text-align: center;", p("BALANCING MOD", style="border-bottom: solid 5px red;")),
               div(style="width: 160px; text-align: center;", p("BALANCING")),
               div(style="width: 160px; text-align: center;", p("OUTCOME"))
           ),
           div(align="center",
               actionButton(NS(id, 'prev_balancing_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_balancing_model_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_balancing_model_btn'), 'Next', class = "default_button")),
           br(),
           
           ## balancing model choices
           div(style = "display: flex;",
               div(style = "width: 49%;",
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
               ),
               
               div(style = "width: 49%; margin-left: 2%; overflow-y: scroll;",
                   class = "text_blocks",
                   ## Output of selected balancing model
                   withSpinner(uiOutput(ns("balancing_model_output")))
               )
           ),
           
           br(), br(),
           
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   ## Parameters of selected balancing model and missingness
                   uiOutput(ns("balancing_model_parameters")),
                   br(),
                   uiOutput(ns("balancing_missingness_parameters"))),
               
               div(style = "width: 49%; margin-left: 2%;"
                   ## EMPTY DIV
                   )
           )
           
  )
}

balancing_model_server <- function(id, parent, raw_data, treatment_variable, outcome_variable, matching_variables, covariates, approach, descriptions) {
  
  
  moduleServer(id,
               function(input, output, session) {
                 
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()))})
                 output$prog_choiceCF <- renderUI({p(paste0(approach$cfapproach_radio()),br(),paste0(approach$missingness_radio()))})
                 
                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("next_balancing_model_btn")
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_balancing_model_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "CF_approach-tab")
                 })

                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_balancing_model_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing-tab")
                 })

                 ## Create reactive value for approach description
                 balancing_model_values <- reactiveValues(
                   model_description = p("In order to balance our dataset, we must first calculate the probability each individual has of being in our treatment
                                   group, based on their observed characteristics, i.e., their propensity score. On this page, you will select a balancing model
                                   to train on your dataset to predict the likelihood of an individual being treated. Different modelling approaches can be used 
                                   for this step and the selection of specific model depends on the characteristics of the data and the research question at hand. 
                                   Please visit our tutorial if you would like more guidance on choosing a balancing model."),
                   model_parameters = p(h4("Balancing Model Parameters:"),
                                  p("Once you have selected a balancing model, we will show you the parameters in use here.")),
                   output = p(h4("Output:"),
                              p("Once you have selected your balancing model and method of dealing with missingness, press
                                'Run' to get results.")),
                   result = NULL)
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$balancing_model_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "tutorial")
                 })

                 ## Update model description and parameters based on choice of approach
                 observeEvent(input$balancing_model_radio,{

                   if(input$balancing_model_radio == "glm"){
                     balancing_model_values$model_description <- p(h5("Probit Regression"),
                                                      p("Probit regression is a method of modelling binary outcomes (such as whether or not someone is 
                                                        in the treatment group). This assumes that the probability of the outcome follows a cumulative 
                                                        distribution function of a normal distribution. This 'probit' function links the linear 
                                                        combination of independent variables into the probability space between 0 and 1."))
                     
                     balancing_model_values$model_parameters <- p(h4("Balancing Model Parameters: Probit Regression"),
                                                          p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_radio == "gbm"){
                     balancing_model_values$model_description <- p(h5("Gradient Boosting Machine (GBM)"),
                                                          p("You've choosen GBM, this is why is may/may not be a good choice."))
                     
                     balancing_model_values$model_parameters <- p(h4("Balancing Model Parameters: Gradient Boosting Machine (GBM)"),
                                                         p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_radio == "rforest"){
                     balancing_model_values$model_description <- p(h5("Random Forest"),
                                                          p("You've choosen random forest, this is why is may/may not be a good choice."))
                     
                     balancing_model_values$model_parameters <- p(h4("Balancing Model Parameters: Random Forest"),
                                                         p("Information on parameters in use."))
                   }
                   
                   
                   ## Remove message with no balancing method selected error if present
                   balancing_model_values$balancing_model_missing_message <- NULL
                   
                   ## Check if balancing model has already been run, if so, remove give informative message and force rerun
                   if (!is.null(balancing_model_values$result)){
                     
                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_model_values$output <- p(h4("Output:"),
                                                     p(
                                                       strong("It looks like balancing model will have to be rerun, this is because some of the required inputs have been changed since the 
                         previous run."), "Once you have selected your balancing model and method of dealing with missingness, press
                                  'Run' to get results."))
                     
                     ## Disable 'Next' button
                     shinyjs::disable("next_balancing_model_btn")
                   }
                 })
                 
                 
                 
                 ## Run balancing model 
                 observeEvent(input$run_balancing_model_btn,{
                   
                   ## If no balancing method has been selected, give error message
                   if(is.null(input$balancing_model_radio)){
                     balancing_model_values$balancing_model_missing_message <- p("Please select a balancing model before proceeding", style = "color:red")
                   }
                   
                   if (!is.null(input$balancing_model_radio)){
                   ## Disable 'Run' button
                   shinyjs::disable("run_balancing_model_btn")
                   
                   ## Remove general output message
                   balancing_model_values$output <- NULL
                   
                   ## Remove balancing model results, if any present
                   balancing_model_values$result <- NULL
                   
                   ## Run balancing model
                   ## Save potential error to check for running of code dependent on balancing model
                   error_check <- NA
                   error_check <- tryCatch(
                     balancing_model_values$result <- get_score(
                       psmodel = input$balancing_model_radio,
                       .data = raw_data(),
                       t_var = treatment_variable(),
                       y_var = outcome_variable(),
                       covars = covariates(),
                       m_vars = matching_variables(),
                       missing = approach$missingness_radio()
                     ),
                     
                     ## If balancing model does not run, return error message and enable run button 
                     error = function(cond) {
                       ## Enable "Run" button
                       shinyjs::enable("run_balancing_model_btn")
                       ## Output error message
                       balancing_model_values$output <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                     })
                   
                   ## Display ROC curve as balancing model output if no error in balancing model
                   if (all(!grepl("Error:", error_check))){
                     try({balancing_model_values$output <- p(
                         h4("The Receiver Operating Characteristic (ROC) curve:"),
                         renderPlot(performance_plot(psmodel_obj = balancing_model_values$result,
                                                     t_var =treatment_variable(),
                                                     treattype = "binary")),
                         p("The Receiver Operating Characteristic (ROC) curve is a plotting method used to assess 
                                                       the performance of a binary classifier (such as a probit regression model) across 
                                                       various discrimination thresholds. The curve plots the true positive rate (sensitivity) 
                                                       against the false positive rate (1 - specificity) at different threshold values. 
                                                       Examining the shape and steepness of the curve shows the classifier's ability 
                                                       to distinguish between the two outcomes. The Area Under the Curve (AUC) summarizes 
                                                       the overall performance, taking values between 0.5 and 1, with higher values indicating better 
                                                       discrimination. A value of 0.5 suggests the classifier performs no better than random guessing, 
                                                       and the corresponding curve would be a diagonal line from bottom-left to top-right.")
                         )
                         
                         ## Enable 'Run' and 'Next' buttons 
                         shinyjs::enable("run_balancing_model_btn")
                         shinyjs::enable("next_balancing_model_btn")
                         
                         ## Add message noting that parameter reselection will require rerun
                         balancing_model_values$model_rerun_message <- p("Note: Changing this parameter will require balancing model to be rerun along with all subsequent steps.")
              
                     })
                   }
                 }})
                 
                 
                 ## Remove balancing model output and force rerun if previous steps have changed since previous run
                 observeEvent(c(approach$cfapproach_radio(), approach$missingness_radio(), raw_data(), treatment_variable(), matching_variables()), {
                   ## First check if balancing model has been run yet, if yes, print informative message in output
                   if (!is.null(balancing_model_values$result)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_model_values$output <- p(h4("Output:"),
                       p(
                       strong("It looks like balancing model will have to be rerun, this is because some of the required inputs have been changed since the 
                       previous run."), "Once you have selected your balancing model and method of dealing with missingness, press
                                'Run' to get results."))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_model_btn")
                     
                     ## Remove balancing model results, if any present
                     balancing_model_values$result <- NULL
                     }
                   })

                 
                 ## Display information for choosing counterfactual approach, relevant parameters and model output
                 output$balancing_model_description <- renderUI(balancing_model_values$model_description)
                 output$balancing_model_rerun_message <- renderUI(balancing_model_values$model_rerun_message)
                 output$balancing_model_parameters <- renderUI(balancing_model_values$model_parameters)
                 output$balancing_model_output <- renderUI(balancing_model_values$output)
                 output$balancing_model_missing_message <- renderUI(balancing_model_values$balancing_model_missing_message)
                 
                 ## Return output of balancing model: balancing model choice, missingness choice and balancing model res
                 balancing_model_output <- reactiveValues(balancing_model = NULL,
                                                          missingness = NULL,
                                                          results = NULL)
                 
                 observe({
                   balancing_model_output$balancing_model <- input$balancing_model_radio
                   balancing_model_output$results <- balancing_model_values$result
                 })
                 
                 return(balancing_model_output)
               })
}
               
               
               
