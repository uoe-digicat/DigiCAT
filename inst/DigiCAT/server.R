#load("data/zp_example.RData")

server <- function(input, output, session) {
  
  ## Source descriptions
  source(system.file("DigiCAT/desc_global.R", package = "DigiCAT"), local=TRUE)
  ####
  # App theme ----
  ####
  output$style <- renderUI({
    if (!is.null(input$style)){
      if (input$style) {
        includeCSS("./www/themes/dark.css")
      } else {
        includeCSS("./www/themes/light.css")
      }}
  })
  
  ####
  # TCs page ----
  ####
  
  ## T&Cs agreement: If 'Yes, I agree', continue to data upload page
  observeEvent(input$Btn_agree_TCs, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = 'analysis')
    updateTabsetPanel(session, inputId = "methods-tabs", selected = "upload")
    removeModal() ## remove modal
  })
  
  
  ####
  # Start Page ----
  ####
  
  DigiCAT:::home_server("home",
                        parent = session,
                        enableLocal = enable_local_data)
  
  ####
  # Data upload ----
  ####
  
  data_upload_output <- DigiCAT:::data_upload_server("data_upload",
                                                     parent = session,
                                                     enableLocal = enable_local_data)
  
  # Counterfactual Approach ----
  ####
  
  
  CF_approach_output <- DigiCAT:::CF_approach_server("CF_approach",
                                                     parent = session,
                                                     enableLocal = enable_local_data,
                                                     raw_data = reactive(data_upload_output$data),
                                                     categorical_variables = reactive(data_upload_output$categorical_vars),
                                                     outcome_variable = reactive(data_upload_output$outcome),
                                                     treatment_variable = reactive(data_upload_output$treatment),
                                                     matching_variables = reactive(data_upload_output$matchvars),
                                                     covariates = reactive(data_upload_output$covars),
                                                     survey_weight_var = reactive(data_upload_output$survey_weight_var),
                                                     cluster_var = reactive(data_upload_output$cluster_var),
                                                     stratification_var = reactive(data_upload_output$stratification_var),
                                                     validation_log = reactive(data_upload_output$validation_log),
                                                     descriptions = desc_global)
  
  ####
  # Balancing ----
  ####
  
  balancing_output <- DigiCAT:::balancing_server("balancing", 
                                                 parent = session,
                                                 raw_data = reactive(data_upload_output$data),
                                                 categorical_variables = reactive(data_upload_output$categorical_vars),
                                                 outcome_variable = reactive(data_upload_output$outcome),
                                                 treatment_variable = reactive(data_upload_output$treatment),
                                                 matching_variables = reactive(data_upload_output$matchvars),
                                                 covariates = reactive(data_upload_output$covars),
                                                 survey_weight_var = reactive(data_upload_output$survey_weight_var),
                                                 cluster_var = reactive(data_upload_output$cluster_var),
                                                 stratification_var = reactive(data_upload_output$stratification_var),
                                                 approach = reactive(CF_approach_output$CF_radio),
                                                 missingness = reactive(CF_approach_output$missingness),
                                                 balancing_model = reactive(CF_approach_output$balancing_model),
                                                 descriptions = desc_global)
  
  ####
  # Outcome Model ----
  ####
  
  outcome_output <- DigiCAT:::outcome_model_server("outcome_model",  
                                                   parent = session,
                                                   data_source = reactive(data_upload_output$data_source),
                                                   raw_data = reactive(data_upload_output$data),
                                                   file_path = reactive(data_upload_output$file_path),
                                                   categorical_variables = reactive(data_upload_output$categorical_vars),
                                                   treatment_variable = reactive(data_upload_output$treatment),
                                                   outcome_variable = reactive(data_upload_output$outcome),
                                                   matching_variables = reactive(data_upload_output$matchvars),
                                                   covariates = reactive(data_upload_output$covars),
                                                   survey_weight_var = reactive(data_upload_output$survey_weight_var),
                                                   cluster_var = reactive(data_upload_output$cluster_var),
                                                   stratification_var = reactive(data_upload_output$stratification_var),
                                                   validation_log = reactive(data_upload_output$validation_log),
                                                   approach = reactive(CF_approach_output$CF_radio),
                                                   missingness = reactive(CF_approach_output$missingness),
                                                   balancing_model = reactive(CF_approach_output$balancing_model),
                                                   matching_method = reactive(balancing_output$method_radio),
                                                   matching_ratio = reactive(balancing_output$ratio_radio),
                                                   estimation_stage_res = reactive(balancing_output$estimation_stage_res),
                                                   balancing_stage_res = reactive(balancing_output$balancing_stage_res),
                                                   common_support_plot = reactive(balancing_output$common_support_plot),
                                                   observation_table = reactive(balancing_output$observation_table),
                                                   love_plot = reactive(balancing_output$love_plot),
                                                   balance_table = reactive(balancing_output$balance_table),
                                                   descriptions = desc_global)
  
}

