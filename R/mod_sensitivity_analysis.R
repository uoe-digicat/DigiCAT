#'@import rmarkdown
#'@import knitr

sensitivity_analysis_ui <- function(id, i18n) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for running sensitivity analysis
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           
           ## Navigation bar ----
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5(i18n$t("GET STARTED"))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5(i18n$t("DATA UPLOAD")), p(uiOutput(ns("prog_choiceDU"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5(i18n$t("APPROACH")), p(uiOutput(ns("prog_choiceCF"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5(i18n$t("BALANCING")), p(uiOutput(ns("prog_choiceBM"))))),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", p(h5(i18n$t("OUTCOME")), p(uiOutput(ns("prog_choiceOM"))))),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", h5(i18n$t("SENSITIVITY ANALYSIS"), style="color: white; border-bottom: solid 2px white;"))
           ), 
           
           
           ## Navigation ----
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_sensitivity_analysis_btn'), i18n$t("Sensitivity Analysis Button Run"), class = "default_button")
           ),
           br(),
           
           br(),
           div(style = "display: flex;",
               class = "text_blocks",
               div(
                   p(
                     h4(i18n$t("Sensitivity Analysis title")),
                     br(),
                     p(i18n$t("Sensitivity Analysis description")),
                     br(),
                     withSpinner(uiOutput(ns("sensitivity_analysis_output"))),
                     p(uiOutput(ns("sensitivity_analysis_output_error"))),
                     p(uiOutput(ns("sensitivity_analysis_rerun")))
                   )
               )
           ),
           br(),
           
           ## Downloadable Output ----
           div(align="center",
               uiOutput(ns("download_options"))
           )
           
  )
}

sensitivity_analysis_server <- function(id, parent, data_source, raw_data, file_path, categorical_variables, outcome_variable, treatment_variable, matching_variables, covariates, survey_weight_var, cluster_var, stratification_var, validation_log, approach, missingness, balancing_model, approach_display, missingness_display, balancing_model_display, matching_method, matching_method_display, matching_ratio, estimation_stage_res, balancing_stage_res, common_support_plot, observation_table, love_plot, balance_table, outcome_model, outcome_model_display, outcome_model_res, outcome_variable_type, analysis_tab, i18n, selected_language) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----
                 output$prog_choiceDU <- renderUI({p(paste0(i18n$t("Tabs DU outcome"), outcome_variable()),br(),paste0(i18n$t("Tabs DU treatment"), treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0(i18n$t("Tabs Approach approach"), approach_display()),br(),paste0(i18n$t("Tabs Approach missingness"), missingness_display()),br(),paste0(i18n$t("Tabs Approach model"), balancing_model_display()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceOM <- renderUI({p(paste0(i18n$t("Tabs Outcome model"), " ", outcome_model_display()), style="width: 200%; margin-left: -50%")})
                 
                 ## If approach, missingness or balancing model changes, update what is displayed as balancing stage choices
                 observeEvent(c(approach(), missingness(), balancing_model()), {
                   
                   if (!is.null(approach())){
                     ## If IPTW selected, display nothing
                     if (approach() == "iptw"){
                       output$prog_choiceBM <- NULL
                     }
                     else{
                       ## If NBP or PSM selected, display matching method and ratio
                       output$prog_choiceBM <- renderUI({p(paste0(i18n$t("Balancing Matching method")," ", matching_method_display()), br(), paste0(i18n$t("Balancing Matching ratio"), "1:", matching_ratio()), style="width: 200%; margin-left: -50%")})
                     }}
                 })
                 
                 
                 ## Navigation ----
                 ## When "Prev is selected", show and move to last tab
                 observeEvent(input$prev_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "outcome_model-tab")
                 })
                 
                 ## Define reactives ----
                 ## Create reactive value for approach description
                 sensitivity_analysis_values <- reactiveValues(
                   output_RB = NULL,
                   output_EV = NULL,
                   output = NULL
                 )
                 
                 ## Reset if sensitivity analysis input changes ----
                 ## Remove SA output and force rerun if previous steps have changed since previous run
                 observeEvent(c(outcome_model_res(), outcome_model()), {
                   
                     ## Only reset if SA has been run
                     if(!is.null(sensitivity_analysis_values$output)){
  
                     ## Remove output and error messages
                     sensitivity_analysis_values$output  <- NULL
                     sensitivity_analysis_values$output_error <- NULL
                     
                     ## Remove download button
                     output$download_options <- NULL
                     
                     ## Add rerun message
                     sensitivity_analysis_values$rerun  <- p(i18n$t("Sensitivity Analysis rerun"))
                     
                     ## Remove download buttons 
                     }
                   }
                 )
                 
                 
                 ## Run sensitivity analysis ----
                 observeEvent(input$run_sensitivity_analysis_btn, {
                   
                   ## Remove rerun message
                   
                   sensitivity_analysis_values$rerun <- NULL
                   
                   ## Save potential error to check for running of code dependent on sensitivity analysis
                   error_check <- NA
                   error_check <- tryCatch({

                     if(approach() == "psm"){

                       sensitivity_analysis_values$output_RB <- run_sensitivity(
                         PS_estimation_object = estimation_stage_res(),
                         balanced_data = balancing_stage_res(),
                         missing_method = missingness(),
                         outcome_variable =  outcome_variable(),
                         sensitivity_type = "rosenbaum_sensitivity",
                         outcome_results = outcome_model_res(),
                         outcome_type = outcome_variable_type())
                       
                       sensitivity_analysis_values$output_EV <- run_sensitivity(
                         PS_estimation_object = estimation_stage_res(),
                         balanced_data = balancing_stage_res(),
                         missing_method = missingness(),
                         outcome_variable =  outcome_variable(),
                         sensitivity_type = "VW_Evalue",
                         outcome_results = outcome_model_res(),
                         outcome_type = outcome_variable_type())

                     } else{
                       
                       sensitivity_analysis_values$output_RB <- NULL
                       
                       sensitivity_analysis_values$output_EV <- run_sensitivity(
                         PS_estimation_object = estimation_stage_res(),
                         balanced_data = balancing_stage_res(),
                         missing_method = missingness(),
                         outcome_variable =  outcome_variable(),
                         sensitivity_type = "VW_Evalue",
                         outcome_results = outcome_model_res(),
                         outcome_type = outcome_variable_type())

                       }
                 },
                     
                     ## If sensitivity analysis does not run, return error message and enable run button 
                     error = function(cond) {
                       ## Enable "Run" button
                       sensitivity_analysis_values$output_error <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                     })
                     
                     # Display output and show download buttons if no error in outcome model
                     if (all(!grepl("Error:", error_check))){
                       try({
                        
                         ## Display description and output of Rosenbaum bounds analysis
                         if(approach() == "psm"){
                           
                           sensitivity_analysis_values$output <-p(
                             h3(i18n$t("Sensitivity Analysis RR title")),
                             br(),
                             i18n$t("Sensitivity Analysis RR description"),
                             br(),
                             br(),
                             HTML(sprintf("<pre>%s</pre>", paste(capture.output(sensitivity_analysis_values$output_RB), collapse = "\n"))),
                             br(),
                             h3(i18n$t("Sensitivity Analysis EV title")),
                             br(),
                             i18n$t("Sensitivity Analysis EV description"),
                             br(),
                             renderTable(round(sensitivity_analysis_values$output_EV, digits = 3), rownames = TRUE)
                           )
                           
                         } else{
                         
                         sensitivity_analysis_values$output <- p(
                           h3(i18n$t("Sensitivity Analysis EV title")),
                           br(),
                           i18n$t("Sensitivity Analysis EV description"),
                           br(),
                           renderTable(round(sensitivity_analysis_values$output_EV, digits = 3), rownames = TRUE)
                         )}
                         
                         ### Generate R script ----
                         sensitivity_analysis_values$R_script <- get_R_script(
                           data_source = data_source(),
                           file_path = file_path(),
                           df = raw_data(),
                           categorical_variables = categorical_variables(),
                           outcome_variable = outcome_variable(),
                           treatment_variable = treatment_variable(),
                           matching_variables = matching_variables(),
                           covariates = covariates(),
                           weighting_variable = survey_weight_var(),
                           cluster_variable = cluster_var(),
                           strata_variable = stratification_var(),
                           counterfactual_method = approach(),
                           missing_method = missingness(),
                           balancing_model = balancing_model(),
                           matching_method = matching_method(),
                           matching_ratio = matching_ratio(),
                           outcome_formula = outcome_model(),
                           outcome_type = outcome_variable_type(),
                           DigiCAT_balanced_data = balancing_stage_res(),
                           DigiCAT_extracted_balanced_data = outcome_model_res()$extracted_balanced_data,
                           DigiCAT_fitted_model = outcome_model_res()$fitted_model,
                           DigiCAT_extracted_outcome_results = outcome_model_res()$extracted_outcome_results,
                           include_sensitivity = TRUE)
                         
                         ### Add download buttons ----
                         output$download_options <- renderUI({
                           div(
                             downloadButton(session$ns("download_script"), i18n$t("Outcome Button download script"), class = "default_button"),
                             downloadButton(session$ns("download_report"), i18n$t("Outcome Button download report"), class = "default_button"))
                         })
                         
                       })
                     }
                     })
                 
                 ## Download output ----
                 output$download_options <- renderUI({
                   
                 })
                 
                 ## Download script when "download R script" clicked
                 output$download_script <- downloadHandler(
                   
                   filename = function() {
                     paste("DigiCAT_with_sensitivity.R", sep = "")
                   },
                   content = function(file) {
                     write.table(
                       isolate(sensitivity_analysis_values$R_script),
                       file, 
                       quote = FALSE,
                       row.names = FALSE, 
                       col.names = FALSE)
                   }
                 )
                 
                 ## Download report when "download report" clicked
                 output$download_report <- 
                   
                   downloadHandler(
                     filename = "DigiCAT_report_with_sensitivity.pdf",
                     content =
                       function(file) {
                         shinyjs::disable("download_report")
                         on.exit(shinyjs::enable("download_report"))
                         output <- render(
                           input = "report_template.Rmd",
                           output_format = "pdf_document",
                           params = list(data_name = file_path(),
                                         data = raw_data(),
                                         outcome_variable = outcome_variable(),
                                         treatment_variable = treatment_variable(),
                                         matching_variables = matching_variables(),
                                         covariates = covariates(),
                                         weighting_variable = survey_weight_var(),
                                         non_response_weight = validation_log()$non_response_weight_no_missingness,
                                         cluster_variable = cluster_var(),
                                         stratification_variable = stratification_var(),
                                         CF_approach = approach(),
                                         missingness = missingness(),
                                         balancing_model = balancing_model(),
                                         matching_method = matching_method(),
                                         matching_ratio = matching_ratio(),
                                         common_support_plot = common_support_plot(),
                                         observation_table = observation_table(),
                                         love_plot = love_plot(),
                                         balance_table = balance_table(),
                                         outcome_formula = outcome_model(),
                                         outcome_variable_type = outcome_variable_type(),
                                         outcome_res = outcome_model_res()$standardised_format,
                                         include_sensitivity = TRUE,
                                         sensitivity_results = list(EV = sensitivity_analysis_values$output_EV, 
                                                                    RB = sensitivity_analysis_values$output_RB))
                         )
                         file.copy(output, file)
                       }
                   )
                 
                 ## Pass output to UI ----
                 output$sensitivity_analysis_output <- renderUI(sensitivity_analysis_values$output)
                 output$sensitivity_analysis_output_error <- renderUI(sensitivity_analysis_values$output_error)
                 output$sensitivity_analysis_rerun <- renderUI(sensitivity_analysis_values$rerun)
                 
                 ## Return sensitivity analysis output to server ----
                 
                 sensitivity_analysis_output <- reactiveValues()
                 
                 observe({
                   sensitivity_analysis_output$output  <-  sensitivity_analysis_values$output
                 })
                 
                 return(sensitivity_analysis_output)
                 
                 })
}
