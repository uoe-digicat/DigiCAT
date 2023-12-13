#'@import rmarkdown
#'@import knitr

sensitivity_analysis_ui <- function(id) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for running sensitivity analysis
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           
           ## Navigation bar ----
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 8%; text-align: center;", h5("GET STARTED", style="color: white;")),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", p(h5("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))))),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", p(h5("APPROACH"), p(uiOutput(ns("prog_choiceCF"))))),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", p(h5("BALANCING"), p(uiOutput(ns("prog_choiceBM"))))),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", p(h5("OUTCOME"), p(uiOutput(ns("prog_choiceOM"))))),
               div(style="width: 8%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 8%; text-align: center;", h5("SENSITIVITY ANALYSIS", style="color: white; border-bottom: solid 2px white;"))
           ),
           
           ## Navigation ----
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button")
           ),
           br(),
           p("This is where sensitivity analysis will go."),
           br(),
           p("Once run download options will reappear? these will now include sensitivity abnalysis step(s). Can remove download options from previous page
             simultaneously.")
           
  )
}

sensitivity_analysis_server <- function(id, parent, outcome_variable, treatment_variable, approach, missingness, balancing_model, matching_method, matching_ratio, outcome_output) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0("Approach: ", approach()),br(),paste0("Missingness: ", missingness()),br(),paste0("Model: ", balancing_model()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceOM <- renderUI({p(paste0("Outcome Model: ", outcome_output()), style="width: 200%; margin-left: -50%")})
                 
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
                 
                 ## If outcome model changes, update navbar
                 observeEvent(outcome_output(), {
                   
                   if (outcome_output() == "linear_regression_w_mvars_interactions"){
                     output$prog_choiceOM <- renderUI({p("Outcome Model: LR (Y ~ T * Matching)", style="width: 200%; margin-left: -50%")})
                   }
                   if (outcome_output() == "linear_regression_w_mvars"){
                     output$prog_choiceOM <- renderUI({p("Outcome Model: LR (Y ~ T + Matching)", style="width: 200%; margin-left: -50%")})
                   }
                   if (outcome_output() == "linear_regression_wo_mvars"){
                     output$prog_choiceOM <- renderUI({p("Outcome Model: LR (Y ~ T)", style="width: 200%; margin-left: -50%")})
                   }
                 })
                 
                 
                 ## Navigation ----
                 ## When "Prev is selected", show and move to last tab
                 observeEvent(input$prev_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "outcome_model-tab")
                 })
                 
                 
                 
                 
               })
}
