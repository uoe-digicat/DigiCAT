load("data/zp_example.RData")

source("source/modules/mod_home.R")
source("source/modules/mod_data_upload.R")
source("source/modules/mod_counterfactual_approach.R")
source("source/modules/mod_balancing_model.R")
source("source/modules/mod_balancing.R")
source("source/modules/mod_outcome_model.R")
source("source/modules/mod_get_results.R")
source("source/modules/mod_tutorial.R")

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
  
  home_server("home",
              parent = session)
  
  ####
  # Data upload ----
  ####

  data_upload_res <- data_upload_server("data_upload",
                     parent = session)

  # Counterfactual Appraoch ----
  ####
  
  
  CF_approach <- CF_approach_server("CF_approach",
                                    parent = session)
  
  ####
  # Balancing model ----
  ####
  
  balancing_model_res <- balancing_model_server("balancing_model", 
                         parent = session,
                         raw_data = reactive(data_upload_res$data),
                         treatment_variable = reactive(data_upload_res$treatment),
                         matching_variables = reactive(data_upload_res$matchvars))
  
  ####
  # Balancing ----
  ####
  
  balancing_res <-  balancing_server("balancing", 
                   parent = session,
                   treatment_variable = reactive(data_upload_res$treatment),
                   matching_variables = reactive(data_upload_res$matchvars),
                   approach = CF_approach,
                   balancing_model_results = balancing_model_res)
  
  ####
  # Outcome Model ----
  ####
  
  outcome_model_server("outcome_model",  
                       parent = session,
                       treatment_variable = reactive(data_upload_res$treatment),
                       outcome_variable = reactive(data_upload_res$outcome),
                       matching_variables = reactive(data_upload_res$matchvars),
                       approach = CF_approach,
                       balancing_results = balancing_res)
  
  ####
  # Get Results ----
  ####
  
  get_results_server("get_results",
                     parent = session)

}





