library(shiny.i18n)

## File with translations
i18n <- Translator$new(translation_csvs_path = system.file("DigiCAT/translation/", package = "DigiCAT"),
                       translation_csv_config =system.file("DigiCAT/translation/config.yaml", package = "DigiCAT"))

## Increase maximum upload file size
options(shiny.maxRequestSize = 50*1024^2)

## If file path doesn't exist, default to home directory
if (!exists("file_path_global")){
  file_path_global <- fs::path_home()
}

server <- function(input, output, session) {
  
  
  ## Update language
  observeEvent(home_output$selected_language, {
    # Update language in session
    shiny.i18n::update_lang(home_output$selected_language)
  })

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
  
  DigiCAT:::TCs_server("TCs",
                       parent = session)
  
  ####
  # Start Page ----
  ####
  
  home_output <- DigiCAT:::home_server("home",
                        parent = session,
                        i18n = i18n)
  
  ####
  # Data upload ----
  ####
  
  data_upload_output <- DigiCAT:::data_upload_server("data_upload",
                                                     parent = session,
                                                     enableLocal = enable_local_data,
                                                     docker_version = docker_version_global,
                                                     filePath = file_path_global,
                                                     analysis_tab = reactive(input$`methods-tabs`),
                                                     i18n = i18n,
                                                     selected_language = reactive(input$selected_language))
  
  # Counterfactual Approach ----
  ####
  
  
  CF_approach_output <- DigiCAT:::CF_approach_server("CF_approach",
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
                                                     validation_log = reactive(data_upload_output$validation_log),
                                                     analysis_tab = reactive(input$`methods-tabs`),
                                                     i18n = i18n,
                                                     selected_language = reactive(input$selected_language))
  
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
                                                 validation_log = reactive(data_upload_output$validation_log),
                                                 approach = reactive(CF_approach_output$CF_radio),
                                                 missingness = reactive(CF_approach_output$missingness),
                                                 balancing_model = reactive(CF_approach_output$balancing_model),
                                                 approach_display = reactive(CF_approach_output$CF_radio_display),
                                                 missingness_display = reactive(CF_approach_output$missingness_display),
                                                 balancing_model_display = reactive(CF_approach_output$balancing_model_display),
                                                 analysis_tab = reactive(input$`methods-tabs`),
                                                 i18n = i18n,
                                                 selected_language = reactive(input$selected_language))
  
  ####
  # Outcome Model ----
  ####
  
  outcome_model_output <- DigiCAT:::outcome_model_server("outcome_model",  
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
                                                         approach_display = reactive(CF_approach_output$CF_radio_display),
                                                         missingness_display = reactive(CF_approach_output$missingness_display),
                                                         balancing_model_display = reactive(CF_approach_output$balancing_model_display),
                                                         matching_method = reactive(balancing_output$method_radio),
                                                         matching_method_display = reactive(balancing_output$method_radio_display),
                                                         matching_ratio = reactive(balancing_output$ratio),
                                                         estimation_stage_res = reactive(balancing_output$estimation_stage_res),
                                                         balancing_stage_res = reactive(balancing_output$balancing_stage_res),
                                                         common_support_plot = reactive(balancing_output$common_support_plot),
                                                         observation_table = reactive(balancing_output$observation_table),
                                                         love_plot = reactive(balancing_output$love_plot),
                                                         balance_table = reactive(balancing_output$balance_table),
                                                         analysis_tab = reactive(input$`methods-tabs`),
                                                         i18n = i18n,
                                                         selected_language = reactive(input$selected_language))
  
  ####
  # Sensitivity Analysis ----
  ####
  
  sensitivity_analysis_output <- DigiCAT:::sensitivity_analysis_server("sensitivity_analysis",  
                                                                       parent = session,
                                                                       treatment_variable = reactive(data_upload_output$treatment),
                                                                       outcome_variable = reactive(data_upload_output$outcome),
                                                                       approach = reactive(CF_approach_output$CF_radio),
                                                                       missingness = reactive(CF_approach_output$missingness),
                                                                       balancing_model = reactive(CF_approach_output$balancing_model),
                                                                       approach_display = reactive(CF_approach_output$CF_radio_display),
                                                                       missingness_display = reactive(CF_approach_output$missingness_display),
                                                                       balancing_model_display = reactive(CF_approach_output$balancing_model_display),
                                                                       matching_method = reactive(balancing_output$method_radio),
                                                                       matching_method_display = reactive(balancing_output$method_radio_display),
                                                                       matching_ratio = reactive(balancing_output$ratio_radio),
                                                                       outcome_model = reactive(outcome_model_output$outcome_formula),
                                                                       outcome_model_display = reactive(outcome_model_output$outcome_formula_display),
                                                                       analysis_tab = reactive(input$`methods-tabs`),
                                                                       i18n = i18n,
                                                                       selected_language = reactive(input$selected_language)
  )
  
  
  
}

