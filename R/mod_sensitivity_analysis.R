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
               actionButton(NS(id, 'run_sensitivity_analysis_btn'), i18n$t("Sensitivity Abalysis Button Run"), class = "default_button")
           ),
           br(),
           mainPanel(wellPanel(id = "well_panel",
                               tabsetPanel(id = NS(id,"results_panel"),
                                           tabPanel(title = i18n$t("Sensitivity Analysis title"),
                                                    value = NS(id,'sensitivity_analysis'),
                                                    ## Sensitivity analysis information 
                                                    p(br(),
                                                      i18n$t("Sensitivity Analysis description")
                                                      ),
                                                    withSpinner(uiOutput(ns("sensitivity_analysis_output"))),
                                                    p(uiOutput(ns("sensitivity_analysis_output_error")))
                                           )
                               )
           )),
           br(),
           p("Report + script download options to reappear")
           
  )
}

sensitivity_analysis_server <- function(id, parent, outcome_variable, treatment_variable, approach, missingness, balancing_model, approach_display, missingness_display, balancing_model_display, matching_method, matching_method_display, matching_ratio, estimation_stage_res, balancing_stage_res, outcome_model_display, outcome_model_res, outcome_variable_type, outcome_model, analysis_tab, i18n, selected_language) {
  
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
                   output_EV = NULL
                 )
                 
                 ## Run sensitivity analysis ----
                 observeEvent(input$run_sensitivity_analysis_btn, {
                   
                     ## Save potential error to check for running of code dependent on sensitivity analysis
                     error_check <- NA
                     error_check <- tryCatch({

                       if(approach() == "psm"){

                         sensitivity_analysis_values$output_RB <- run_sensitivity(
                           PS_object = estimation_stage_res(),
                           balanced_data = balancing_stage_res(),
                           missing_method = missingness(),
                           outcome_variable =  outcome_variable(),
                           sensitivity_type = "rosenbaum_sensitivity",
                           outcome_results = outcome_model_res(),
                           outcome_type = outcome_variable_type())
                         
                         sensitivity_analysis_values$output_EV <- run_sensitivity(
                           PS_object = estimation_stage_res(),
                           balanced_data = balancing_stage_res(),
                           missing_method = missingness(),
                           outcome_variable =  outcome_variable(),
                           sensitivity_type = "VW_Evalue",
                           outcome_results = outcome_model_res(),
                           outcome_type = outcome_variable_type())

                       } else{
                         
                         sensitivity_analysis_values$output_RB <- NULL
                         
                         sensitivity_analysis_values$output_EV <- run_sensitivity(
                           PS_object = estimation_stage_res(),
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
                           
                           sensitivity_analysis_values$output <- p(h3(i18n$t("Sensitivity Analysis RR title")),
                                                                   br(),
                                                                   i18n$t("Sensitivity Analysis RR description"),
                                                                   br(),
                                                                   renderPrint({print(sensitivity_analysis_values$output_RB)}),
                                                                   br(),
                                                                   h3(i18n$t("Sensitivity Analysis EV title")),
                                                                   br(),
                                                                   i18n$t("Sensitivity Analysis EV description"),
                                                                   br(),
                                                                   sensitivity_analysis_values$output_EV)
                           
                         } else{
                         
                         sensitivity_analysis_values$output <- p(h3(i18n$t("Sensitivity Analysis EV title")),
                                                                 br(),
                                                                 i18n$t("Sensitivity Analysis EV description"),
                                                                 br(),
                                                                 DT::renderDataTable({DT::datatable((sensitivity_analysis_values$output_EV))})
                         )}
                       })
                     }
                     })
                 
                 ## Pass output to UI ----
                 output$sensitivity_analysis_output <- renderUI(sensitivity_analysis_values$output)
                 output$sensitivity_analysis_output_error <- renderUI(sensitivity_analysis_values$output_error)
                 
                 })
}
