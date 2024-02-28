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
               actionButton(NS(id, 'prev_outcome_model_btn'), 'Prev', class = "default_button")
           ),
           br(),
           p("This is where sensitivity analysis will go."),
           br(),
           p("Once run download options will reappear? these will now include sensitivity abnalysis step(s). Can remove download options from previous page
             simultaneously.")
           
  )
}

sensitivity_analysis_server <- function(id, parent, outcome_variable, treatment_variable, approach, missingness, balancing_model, approach_display, missingness_display, balancing_model_display, matching_method, matching_method_display, matching_ratio, outcome_model_display, outcome_model, analysis_tab, i18n, selected_language) {
  
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
                 
                 
                 
                 
               })
}
