#load("data/zp_example.RData")

server <- function(input, output, session) {
  
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
              parent = session)
  
  ####
  # Data upload ----
  ####

  data_upload_res <- DigiCAT:::data_upload_server("data_upload",
                                                  parent = session,
                                                  enableLocal = enable_local_data)

  # Counterfactual Appraoch ----
  ####
  
  
  CF_approach <- DigiCAT:::CF_approach_server("CF_approach",
                                    parent = session,
                                    raw_data = reactive(data_upload_res$data),
                                    categorical_variables = reactive(data_upload_res$categorical_vars),
                                    outcome_variable = reactive(data_upload_res$outcome),
                                    treatment_variable = reactive(data_upload_res$treatment),
                                    matching_variables = reactive(data_upload_res$matchvars),
                                    covariates = reactive(data_upload_res$covars),
                                    NRW_var = reactive(data_upload_res$NRW_var),
                                    descriptions = desc_global)
  
  ####
  # Balancing ----
  ####
  
  balancing_res <- DigiCAT:::balancing_server("balancing", 
                   parent = session,
                   raw_data = reactive(data_upload_res$data),
                   outcome_variable = reactive(data_upload_res$outcome),
                   treatment_variable = reactive(data_upload_res$treatment),
                   matching_variables = reactive(data_upload_res$matchvars),
                   covariates = reactive(data_upload_res$covars),
                   approach = reactive(CF_approach$CF_radio),
                   missingness = reactive(CF_approach$missingness),
                   balancing_model = reactive(CF_approach$balancing_model),
                   descriptions = desc_global)
  
  ####
  # Outcome Model ----
  ####
  
  outcome_res <- DigiCAT:::outcome_model_server("outcome_model",  
                       parent = session,
                       treatment_variable = reactive(data_upload_res$treatment),
                       outcome_variable = reactive(data_upload_res$outcome),
                       matching_variables = reactive(data_upload_res$matchvars),
                       approach = reactive(CF_approach$CF_radio),
                       missingness = reactive(CF_approach$missingness),
                       balancing_model = reactive(CF_approach$balancing_model),
                       balancing_method = reactive(balancing_res$method_radio),
                       balancing_ratio = reactive(balancing_res$ratio_radio),
                       balancing_model_res = reactive(balancing_res$balancing_model_res),
                       balancing_res = reactive(balancing_res$balancing_res),
                       descriptions = desc_global)
  
  ####
  # Get Results ----
  ####
  
  DigiCAT:::get_results_server("get_results",
                     parent = session)

}

