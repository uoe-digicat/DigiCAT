#load("data/zp_example.RData")

server <- function(input, output, session) {
  
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
              parent = session)
  
  ####
  # Data upload ----
  ####

  data_upload_res <- DigiCAT:::data_upload_server("data_upload",
                     parent = session)

  # Counterfactual Appraoch ----
  ####
  
  
  CF_approach <- DigiCAT:::CF_approach_server("CF_approach",
                                    parent = session,
                                    raw_data = reactive(data_upload_res$data),
                                    categorical_variables = reactive(data_upload_res$categorical_vars),
                                    outcome_variable = reactive(data_upload_res$outcome),
                                    treatment_variable = reactive(data_upload_res$treatment),
                                    matching_variables = reactive(data_upload_res$matchvars),
                                    covariates = reactive(data_upload_res$covars))
  
  ####
  # Balancing model ----
  ####
  
  balancing_model_res <- DigiCAT:::balancing_model_server("balancing_model", 
                         parent = session,
                         raw_data = reactive(data_upload_res$data),
                         approach = CF_approach,
                         outcome_variable = reactive(data_upload_res$outcome),
                         treatment_variable = reactive(data_upload_res$treatment),
                         matching_variables = reactive(data_upload_res$matchvars),
                         covariates = reactive(data_upload_res$covars))
  
  ####
  # Balancing ----
  ####
  
  balancing_res <- DigiCAT:::balancing_server("balancing", 
                   parent = session,
                   treatment_variable = reactive(data_upload_res$treatment),
                   matching_variables = reactive(data_upload_res$matchvars),
                   approach = CF_approach,
                   balancing_model_results = reactive(balancing_model_res$results))
  
  ####
  # Outcome Model ----
  ####
  
  DigiCAT:::outcome_model_server("outcome_model",  
                       parent = session,
                       treatment_variable = reactive(data_upload_res$treatment),
                       outcome_variable = reactive(data_upload_res$outcome),
                       matching_variables = reactive(data_upload_res$matchvars),
                       approach = CF_approach,
                       balancing_results = balancing_res)
  
  ####
  # Get Results ----
  ####
  
  DigiCAT:::get_results_server("get_results",
                     parent = session)

}
