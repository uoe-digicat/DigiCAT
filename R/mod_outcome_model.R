outcome_model_ui <- function(id) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for choosing outcome model
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           
           ## Navigation bar ----
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5("GET STARTED", style="color: white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("APPROACH"), p(uiOutput(ns("prog_choiceCF"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("BALANCING"), p(uiOutput(ns("prog_choiceBM"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("OUTCOME", style="color: white; border-bottom: solid 2px white;"))
           ),
           
           ## Navigation ----
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_outcome_model_btn'), 'Run', class = "default_button")
               ## actionButton(NS(id, 'next_outcome_model_btn'), 'Next', class = "default_button")
               ),
           br(),
           
           ## Outcome model selection ----
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "outcome_model_radio"), label = h4("Choose an Outcome Model:"),
                                choices = list(
                                  "Linear Regression (Outcome ~ Treatment * Matching Variables)" = "linear_regression_w_mvars_interactions",
                                  "Linear Regression (Outcome ~ Treatment + Matching Variables)" = "linear_regression_w_mvars",
                                  "Linear Regression (Outcome ~ Treatment)" = "linear_regression_wo_mvars"),
                                selected = character(0),
                                width = "200%"),
                   uiOutput(ns("outcome_model_missing_message"), style = "color: red;"), ## If no model selected when "Run" pressed, give warning
                   uiOutput(ns("outcome_model_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                   br(),
                   uiOutput(ns("outcome_model_description_method")),
                   br(),
                   uiOutput(ns("outcome_model_description_method_selected"))
               ),
               
               ## Outcome model output ----
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   ## Output of selected outcome_model model
                   withSpinner(uiOutput(ns("outcome_model_output")))
                   )
               ),
           br(),
           
           ## Downloadable Output ----
           div(align="center",
               uiOutput(ns("download_options"))
               )
           )
}

outcome_model_server <- function(id, parent, data_source, file_path, categorical_variables, treatment_variable, outcome_variable, matching_variables, covariates, survey_weight_var, cluster_var, stratification_var, approach, missingness, balancing_model, matching_method, matching_ratio, estimation_stage_res, balancing_stage_res, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0("Approach: ", approach()),br(),paste0("Missingness: ", missingness()),br(),paste0("Model: ", balancing_model()), style="width: 200%; margin-left: -50%")})
                 
                 ## If approach, missingness or balancing model changes, update what is displayed as balancing stage choices
                 observeEvent(c(approach(), missingness(), balancing_model()), {
                   
                   if (!is.null(approach())){
                     ## If IPTW selected, display nothing
                     if (approach() == "iptw"){
                       output$prog_choiceBM <- NULL
                     }
                     else{
                       ## If NBP or PSM selected, display matching method and ratio
                       output$prog_choiceBM <- renderUI({p(paste0("Matching Method: ", matching_method()), br(), paste0("Matching Ratio: 1:", matching_ratio()), style="width: 200%; margin-left: -50%")})
                     }}
                   })
                 
                 ## Define reactives ----
                 ## Create reactive value for approach description
                 outcome_model_values <- reactiveValues(
                   description_method = descriptions$outcome_model,
                   description_method_selected = NULL,
                   R_script = NULL,
                   output = p(h4("Output:"),
                              p("Once you have selected your outcome model, press'Run' to get results."))
                 )

                 ## Navigation ----
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "balancing-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "get_results-tab")
                 })
                 
                 
                 ## Update choice of outcome model when approach is changed
                 observeEvent(approach(),{
                   
                   ## If NBP selected, remove choice with interaction
                   if(approach() == "nbp"){
                     updateRadioButtons(session, 
                                        inputId = "outcome_model_radio", 
                                        choices = list(
                                          "Linear Regression (Outcome ~ Treatment + Matching Variables)" = "linear_regression_w_mvars",
                                          "Linear Regression (Outcome ~ Treatment)" = "linear_regression_wo_mvars"),
                                        selected = character(0))
                   }
                   ## Give choice with interaction for other approaches
                   if(approach() == "psm" | approach() == "iptw"){
                     updateRadioButtons(session, 
                                        inputId = "outcome_model_radio", 
                                        choices = list(
                                          "Linear Regression (Outcome ~ Treatment * Matching Variables)" = "linear_regression_w_mvars_interactions",
                                          "Linear Regression (Outcome ~ Treatment + Matching Variables)" = "linear_regression_w_mvars",
                                          "Linear Regression (Outcome ~ Treatment)" = "linear_regression_wo_mvars"),
                                        selected = character(0))
                   }
                 })
                 
                 ## Update descriptions and rerun message when input changes ----
                 observeEvent(input$outcome_model_radio,{
                   
                   if(input$outcome_model_radio == "linear_regression_w_mvars_interactions"){
                     outcome_model_values$description_method_selected <- descriptions$linear_regression_w_mvars_interactions
                   }
                   
                   if(input$outcome_model_radio == "linear_regression_w_mvars"){
                     outcome_model_values$description_method_selected <- descriptions$linear_regression_w_mvars
                   }
                   
                   if(input$outcome_model_radio == "linear_regression_wo_mvars"){
                     outcome_model_values$description_method_selected <- descriptions$linear_regression_wo_mvars
                   }
                     
                   ## Remove missing parameter message if present
                   outcome_model_values$model_missing_message  <- NULL
        
                   
                   
                   ## If outcome model has already been run, give informative message about rerun and disable "Next" button to force rerun
                   if (!is.null(outcome_model_values$outcome_analysis_stage_res)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     outcome_model_values$output <- p(h4("Output:"),
                                                        p(
                                                          strong("It looks like the outcome model will have to be rerun, this is because some of the required inputs have been changed since the 
                     previous run."), "Once you have selected your outcome model, press 'Run' to get results."))
                     
                   }
               })
                 

                 
                 ## Run outcome model ----
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
                     error_check <- tryCatch({
                       
                       if(approach() == "psm" | approach() == "iptw"){
                         outcome_model_values$outcome_analysis_stage_res <- outcome_analysis_stage(
                           balanced_data = balancing_stage_res(),
                           counterfactual_method = approach(),
                           outcome_variable = outcome_variable(),
                           treatment_variable = treatment_variable(),
                           matching_variable = matching_variables(), 
                           psmodel_obj = estimation_stage_res(),
                           cluster_variable = cluster_var(),
                           nonresponse_weights = survey_weight_var(),
                           sampling_weights = survey_weight_var(),
                           missing_method = missingness(),
                           weighting_variable = survey_weight_var())
                       }
                       
                       if(approach() == "nbp"){
                         outcome_model_values$outcome_analysis_stage_res <- outcome_analysis_stage(
                           balanced_data = balancing_stage_res(), 
                           counterfactual_method = approach(),
                           outcome_variable = outcome_variable(),
                           treatment_variable = treatment_variable(),
                           matching_variable = matching_variables(), 
                           psmodel_obj = estimation_stage_res(),
                           missing_method = missingness())
                         
                         
                       }
                       },
                       
                       ## If outcome model does not run, return error message and enable run button 
                       error = function(cond) {
                         ## Enable "Run" button
                         shinyjs::enable("run_outcome_model_btn")
                         ## Output error message
                         outcome_model_values$output <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                       })
                     
                     
                     # Display output and show download button if no error in outcome model
                     if (all(!grepl("Error:", error_check))){
                       try({
                     ## Output estimate
                         
                         if(approach() == "psm" | approach() == "iptw"){
                         outcome_model_values$output <- p(h4("Model Output"),
                                                  descriptions$estimate,
                                                  strong(p(paste0("Estimate: ", round(outcome_model_values$outcome_analysis_stage_res[1,1], 4)))),
                                                  br(),
                                                  descriptions$standard_error,
                                                  strong(p(paste0("Standard Error: ", round(outcome_model_values$outcome_analysis_stage_res[1,2], 4)))),
                                                  br(),
                                                  descriptions$p_value,
                                                  strong(p(paste0("P-value: ", round(outcome_model_values$outcome_analysis_stage_res[1,3], 4)))),
                                                  br(),
                                                  if (missingness() == "complete"){
                                                    strong(p(paste0("95% Confidence Interval: ", round(outcome_model_values$outcome_analysis_stage_res[1,5], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res[1,4], 4))))
                                                  }
                                                  else{
                                                    strong(p(paste0("95% Confidence Interval: ", round(outcome_model_values$outcome_analysis_stage_res[1,4], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res[1,5], 4))))
                                                  }
                                                  )
                         }
                         
                         if(approach() == "nbp"){
                           outcome_model_values$output <- p(h4("Model Output"),
                                                            descriptions$estimate,
                                                            strong(p(paste0("Estimate: ", round(outcome_model_values$outcome_analysis_stage_res[2,1], 4)))),
                                                            br(),
                                                            descriptions$standard_error,
                                                            strong(p(paste0("Standard Error: ", round(outcome_model_values$outcome_analysis_stage_res[2,2], 4)))),
                                                            br(),
                                                            descriptions$p_value,
                                                            strong(p(paste0("P-value: ", round(outcome_model_values$outcome_analysis_stage_res[2,4], 4))))
                           )
                         }
                         
                         ## Add message noting that parameter reselection will require rerun
                         outcome_model_values$model_rerun_message <- p("Note: Changing this parameter will require outcome model to be rerun")

                         ## Enable 'Run' button
                         shinyjs::enable("run_outcome_model_btn")
                         
                         ## Generate R script
                         
                         outcome_model_values$R_script <- get_R_script(
                           data_source = data_source(),
                           file_path = file_path(),
                           categorical_variables = categorical_variables(),
                           outcome_variable = outcome_variable(),
                           treatment_variable = treatment_variable(),
                           matching_variables = matching_variables(),
                           covariates = covariates(),
                           weighting_variable = survey_weight_var(),
                           cluster_variable = cluster_var(),
                           stratification_variable = stratification_var(),
                           CF_approach = approach(),
                           missingness = missingness(),
                           balancing_model = balancing_model(),
                           matching_method = matching_method(),
                           matching_ratio = matching_ratio(),
                           outcome_model = input$outcome_model_radio)
                         
                         
                         ## Add download script button
                         output$download_options <- renderUI({
                           downloadButton(session$ns("download_script"), "Download R Script", class = "default_button")
                         })
                       })
                     }
                     }
                   })
                 
                 ## Reset if outcome model input changes ----
                 ## Remove outcome model output and force rerun if previous steps have changed since previous run
                 observeEvent(c(estimation_stage_res(), balancing_stage_res()), {
                   ## First check if outcome model has been run yet, if yes, print informative message in output
                   if (!is.null(outcome_model_values$outcome_analysis_stage_res)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     outcome_model_values$output <- p(h4("Output:"),
                                                        p(
                                                          strong("It looks like the outcome model will have to be rerun, this is because some of the required inputs have been changed since the 
                       previous run."), "Once you have selected your outcome model, press 'Run' to get results."))
                     
                     ## Remove download button
                     output$download_options <- NULL
                     
                   }
                 })
                 
                 ## Download output ----
                 output$download_options <- renderUI({

                 })
                 
                 ## Download script and analysis functions when download clicked
                 output$download_script <- downloadHandler(

                   filename = function() {
                     paste("DigiCAT.R", sep = "")
                   },
                   content = function(file) {
                     write.table(
                       isolate(outcome_model_values$R_script),
                       file, 
                       quote = FALSE,
                       row.names = FALSE, 
                       col.names = FALSE)
                   }
                 )
                   
                 
                 ## Pass output to UI ----
                 ## Display information for choosing counterfactual approach, relevant parameters and model output
                 output$outcome_model_description_method <- renderUI(outcome_model_values$description_method)
                 output$outcome_model_description_method_selected <- renderUI(outcome_model_values$description_method_selected)
                 output$outcome_model_missing_message <- renderUI(outcome_model_values$model_missing_message)
                 output$outcome_model_rerun_message <- renderUI(outcome_model_values$model_rerun_message)
                 output$outcome_model_parameters_method <- renderUI(outcome_model_values$parameters_method)
                 output$outcome_model_output <- renderUI(outcome_model_values$output)
                 
               })
}
