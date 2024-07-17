# counterfactual approach module ----

CF_approach_ui <- function(id, i18n) {
  
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           useShinyjs(),
           br(),
           
           ## Navigation bar ----
           
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5(i18n$t("GET STARTED"))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5(i18n$t("DATA UPLOAD")), p(uiOutput(ns("prog_choiceDU"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("APPROACH"), style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("BALANCING"), style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("OUTCOME"), style="color: #607cc4;"))
           ),
           
           ## Navigation ----
           
           div(align="center",
               actionButton(NS(id,"prev_CF_btn"), i18n$t("Button Previous"), class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), i18n$t("Button Next"), class = "default_button")),
           br(),
           
           ## Approach selection ----
           
           div(style = "display: flex;",
               ## CF approach choices
               div(style = "width: 32.67%;",
                   class = "text_blocks",
                   uiOutput(ns("approach_selection")), ## Based on treatment variable type, display approach choices
                   uiOutput(ns("approach_missing_message")), ## If run is clicked before approach selected, issue warning 
                   uiOutput(ns("approach_rerun_message")), ## Give warning that rerun required upon re-selection
                   uiOutput(ns("approach_change_message")), ## Give warning that change will no require rerun
                   br(),
                   ## Description of general and selected counterfactual approach
                   uiOutput(ns("approach_description_general")), ## Add description of approach selected
                   br(),
                   uiOutput(ns("approach_description")) ## Add description of approach selected
                   
               ),
               ## Missingness choices
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   uiOutput(ns("missingness_selection")), ## Based on presence of complete non-response weights, display approach choices
                   uiOutput(ns("missingness_missing_message"), style = "color: grey;"), ## If run is clicked before missingness selected, issue warning 
                   uiOutput(ns("missingness_rerun_message")), ## Give warning that rerun required upon re-selection
                   uiOutput(ns("missingness_change_message")), ## Give warning that change will no require rerun
                   br(),
                   p(h5(i18n$t("Approach Description")),
                     p(i18n$t("Approach Missingness description")),
                     a(id = "link",i18n$t("Approach Missingness tutorial link"), href = "https://uoe-digicat.github.io/05_missing.html",  target="_blank")), ## Add general description
                   br(),
                   uiOutput(ns("missingness_description")) ## Add description of missingness selected
                   
               ),
               ## Balancing model choices
               div(style = "width: 32.67%; margin-left: 2%;",
                   class = "text_blocks",
                   uiOutput(ns("balancing_model_selection")), ## Based on missingness choices, display balancing model choices
                   br(),
                   uiOutput(ns("balancing_model_missing_message")), ## If run is clicked before model selected, issue warning 
                   uiOutput(ns("balancing_model_rerun_message")), ## Give warning that rerun required upon re-selection
                   uiOutput(ns("balancing_model_change_message")), ## Give warning that change will no require rerun
                   p(uiOutput(ns("balancing_model_description_general"))), ## Add general description
                   br(),
                   p(uiOutput(ns("balancing_model_description"))) ## Description of selected balancing model
               ))
  )
}

CF_approach_server <- function(id, parent, enableLocal, raw_data, outcome_variable, treatment_variable, matching_variables, categorical_variables, covariates, survey_weight_var, cluster_var, stratification_var, validation_log, analysis_tab, i18n, selected_language) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- NS(id)
                 
                 ## Navigation bar ----
                 
                 output$prog_choiceDU <- renderUI({p(paste0(i18n$t("Tabs DU outcome"), outcome_variable()),br(),paste0(i18n$t("Tabs DU treatment"), treatment_variable()), style="width: 200%; margin-left: -50%")})
                 
                 ## Define Reactives ----
                 
                 ## Create reactive value for elements on appraoch tab
                 CF_approach_values <- reactiveValues(
                   approach_description = NULL,
                   approach_rerun_message = NULL,
                   approach_change_message = NULL,
                   approach_missing_message = NULL,
                   missingness_description = NULL,
                   missingness_missing_message = NULL,
                   missingness_rerun_message = NULL,
                   missingness_change_message = NULL,
                   balancing_model_description = NULL,
                   balancing_model_missing_message = NULL,
                   balancing_model_rerun_message = NULL,
                   balancing_model_change_message = NULL,
                   page_complete = NULL,
                   approach_choice = NULL,
                   missingness_choice = NULL,
                   model_choice = NULL
                 )
                 
                 ## Navigation ----
                 
                 ## When "Prev is selected", go back to data upload page
                 observeEvent(input$prev_CF_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_CF_btn, {
                   ## Check required inputs have been selected
                   if(is.null(input$CF_radio)){
                     CF_approach_values$approach_missing_message <- p(i18n$t("Approach Warning no approach"), style = "color:red")
                   }
                   if(is.null(input$missingness_radio)){
                     CF_approach_values$missingness_missing_message <- p(i18n$t("Approach Warning no missingness"), style = "color:red")
                   }
                   if(is.null(input$balancing_model_radio) & !input$CF_radio == i18n$t("Approach CBPS")){
                     CF_approach_values$balancing_model_missing_message <- p(i18n$t("Approach Warning no model"), style = "color:red")
                   }
                   ## Only continue if all required input is given
                   if((!is.null(input$CF_radio) & !is.null(input$missingness_radio) & !is.null(input$balancing_model_radio)) | (!is.null(input$CF_radio) & !is.null(input$missingness_radio) & input$CF_radio == i18n$t("Approach CBPS"))){
                     ## Record page completion
                     CF_approach_values$page_complete <- 1
                     ## Add rerun warning to each parameter
                     CF_approach_values$approach_rerun_message <- p(i18n$t("Approach Warning rerun"), style = "color:grey")
                     CF_approach_values$missingness_rerun_message <- p(i18n$t("Approach Warning rerun"), style = "color:grey")
                     CF_approach_values$balancing_model_rerun_message <- p(i18n$t("Approach Warning rerun"), style = "color:grey")
                     ## Proceed to balancing page
                     updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing-tab")
                   }
                 })
                 
                 ## Reset page if counterfactual approach inputs have changed ----
                 
                 ## If data/variable selection has changed since previous approach selection, add question asking if current approach is still
                 ## appropriate to approach description and reset page
                 observeEvent(c(raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates(), survey_weight_var(), cluster_var(), stratification_var(), validation_log()), {
                   
                   ## Only run if validation has been carried out
                   if (!is.null(validation_log())){
                     
                     ## Remove approach choice specific descriptions
                     CF_approach_values$balancing_model_description <- NULL
                     CF_approach_values$missingness_description <- NULL
                     
                     ## Remove missing input warnings
                     CF_approach_values$approach_missing_message <- NULL
                     CF_approach_values$missingness_missing_message <- NULL
                     CF_approach_values$balancing_model_missing_message <- NULL

                     ## If page has already been completed, add message stating reselection is required
                     if (!is.null(CF_approach_values$page_complete)){
                       ## Remove rerun warning message
                       CF_approach_values$approach_rerun_message <- NULL
                       CF_approach_values$missingness_rerun_message <- NULL
                       CF_approach_values$balancing_model_rerun_message <- NULL
                       ## Have rerun message indicating data/variables have been changed
                       CF_approach_values$approach_change_message <- p(strong(i18n$t("Approach Warning change approach")))
                       CF_approach_values$missingness_change_message <- p(strong(i18n$t("Approach Warning change missingness")))
                       CF_approach_values$balancing_model_change_message <- p(strong(i18n$t("Approach Warning change model")))
                     }

                     ## Update approach based on treatment variable
                     ### Approach: binary treatment ----
                     if (length(unique(na.omit(raw_data()[,treatment_variable()]))) == 2){ ## Binary treatment approaches

                       CF_approach_values$approach_selection <- radioButtons(NS(id, "CF_radio"),
                                                                            label = h4(i18n$t("Approach Choose CA")),
                                                                            choices = list(i18n$t("Approach PSM"),
                                                                                           i18n$t("Approach IPTW"),
                                                                                           i18n$t("Approach CBPS")),
                                                                            selected = character(0)
                       )

                       CF_approach_values$approach_description_general <- p(h5(i18n$t("Approach Description")),
                                                                         p(i18n$t("Approach Binary description")),
                                                                         a(id = "link",i18n$t("Approach CA link"), href = "https://uoe-digicat.github.io/03_choosecf.html",  target="_blank")
                                                                       )
                     }
                     
                     ### Approach and PS model: continuous treatment ----
                     if (!any(treatment_variable() %in% categorical_variables())){ ## Continuous treatment approaches

                       CF_approach_values$approach_selection <- radioButtons(NS(id, "CF_radio"),
                                                                          label = h4(i18n$t("Approach Choose CA")),
                                                                          choices = i18n$t("Approach CBPS"),
                                                                          selected = character(0)
                       )

                       CF_approach_values$approach_description_general <- p(h5(i18n$t("Approach Description")),
                                                                         p(i18n$t("Approach Binary description")),
                                                                         a(id = "link",i18n$t("Approach CA link"), href = "https://uoe-digicat.github.io/03_choosecf.html",  target="_blank")
                       )
                       
                       CF_approach_values$balancing_model_selection <- p(i18n$t("Approach Model CBPS")
                                                                    )
                       CF_approach_values$balancing_model_change_message <- NULL


                     }
                     ### Approach and PS model: ordinal treatment ----
                     if (length(unique(na.omit(raw_data()[,treatment_variable()]))) > 2 & any(treatment_variable() %in% categorical_variables())){ ## Ordinal treatment approaches

                       CF_approach_values$approach_selection <- radioButtons(ns("CF_radio"),
                                                                            label = h4("1. Choose a Counterfactual Approach:"),
                                                                            choices = i18n$t("Approach NBP"),
                                                                            selected = character(0))

                       CF_approach_values$approach_description_general <- p(h5(i18n$t("Approach Description")),
                                                                         p(i18n$t("Approach NBP description")),
                                                                         a(id = "link",i18n$t("Approach NBP tutorial link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#nbp",  target="_blank")
                                                                       )
                       CF_approach_values$approach_description <- NULL

                      
                       ## OLR available as balancing model
                       CF_approach_values$balancing_model_selection <- p(
                         radioButtons(NS(id, "balancing_model_radio"), label = h4(i18n$t("Approach Choose model")),
                                      choices = list(
                                        i18n$t("Approach ORL")),
                                      selected = character(0)))
                       
                       CF_approach_values$balancing_model_description_general <- p(h5(i18n$t("Approach Description")), i18n$t("Approach Model description"))

                     }
                     
                     ## Add message stating missingness depends on appraoch and choices will be displayed after approach selection
                     CF_approach_values$missingness_selection <- p(i18n$t("Approach Missingness initial"))

                     if (length(unique(na.omit(raw_data()[,treatment_variable()]))) == 2){
                      
                       ## If binary treatment: add message stating balancing model depends on missingness and choices will be displayed after missingness selection
                       CF_approach_values$balancing_model_selection <- p(i18n$t("Approach Model initial"), br())
                       CF_approach_values$balancing_model_description_general <- p(h5(i18n$t("Approach Description")), i18n$t("Approach Model description"))
                       CF_approach_values$balancing_model_description
                       
                       }
                     }
                 })
                 
                 
                 ### Missingness ----
                 ## Update when approach is selected
                 observeEvent(input$CF_radio,{
                 ## Check presents of missing data and non-response variable, update missingness accordingly
                 ## If no missingness detected and no non repose weight, only offer complete case
                 if(validation_log()$no_missingness_no_non_response){
                   
                   CF_approach_values$missingness_selection <- p(
                     radioButtons(NS(id, "missingness_radio"),
                                  label = h4(p(i18n$t("Approach Choose missingness"))),
                                  choices = list(
                                    i18n$t("Approach Choose CC")),
                                  selected = character(0)),
                     br(),
                     p(i18n$t("Approach no missingness"), style = "color:gray;")
                   )
                 }
                 
                 ## If no missingness detected but non reponse weight, offer complete case and weighting
                 if(validation_log()$no_missingness_but_non_response){
                   
                   if(input$CF_radio == i18n$t("Approach NBP") | input$CF_radio == i18n$t("Approach CBPS")){ ## Don't show weighting option for NBP/CBPS
                     
                     CF_approach_values$missingness_selection <- p(
                       radioButtons(NS(id, "missingness_radio"),
                                    label = h4(p(i18n$t("Approach Choose missingness"))),
                                    choices = list(
                                      i18n$t("Approach Choose CC")),
                                    selected = character(0)),
                       br(),
                       p(i18n$t("Approach no missingness"), style = "color:gray;")
                     )
                   } else{
                     
                     CF_approach_values$missingness_selection <- p(
                       radioButtons(NS(id, "missingness_radio"),
                                    label = h4(p(i18n$t("Approach Choose missingness"))),
                                    choices = list(
                                      i18n$t("Approach Choose Weighting"),
                                      i18n$t("Approach Choose CC")),
                                    selected = character(0)),
                       br(),
                       p(i18n$t("Approach no missingness but weighting"), style = "color:gray;")
                     )
                   }
                 }
                   
                 
                 ## If there is data missingness and non response weights provided, include weighting as a missingness option
                 if (validation_log()$some_missingness_but_non_response){
                   
                   if(input$CF_radio == i18n$t("Approach NBP") | input$CF_radio == i18n$t("Approach CBPS")){ ## Don't show weighting option for NBP/CBPS
                     
                     CF_approach_values$missingness_selection <- p(
                       radioButtons(NS(id, "missingness_radio"),
                                    label = h4(p(i18n$t("Approach Choose missingness"))),
                                    choices = list(
                                      i18n$t("Approach Choose MI"),
                                      i18n$t("Approach Choose CC")),
                                    selected = character(0))
                     )
                   } else{
                     
                     CF_approach_values$missingness_selection <- p(
                       radioButtons(NS(id, "missingness_radio"),
                                    label = h4(p(i18n$t("Approach Choose missingness"))),
                                    choices = list(
                                      i18n$t("Approach Choose MI"),
                                      i18n$t("Approach Choose Weighting"),
                                      i18n$t("Approach Choose CC")),
                                    selected = character(0))
                     )
                   }
                 }
                 
                 ## If there is data missingness and no non response weight provided, don't add weighting
                 if (validation_log()$some_missingness_no_non_response){
                   CF_approach_values$missingness_selection <- p(
                     radioButtons(NS(id, "missingness_radio"),
                                  label = h4(p(i18n$t("Approach Choose missingness"))),
                                  choices = list(
                                    i18n$t("Approach Choose MI"),
                                    i18n$t("Approach Choose CC")),
                                  selected = character(0))
                   )
                 }
                 })
                 
                 
                 
                 
                 
                ### PS model ----
                 ## Update balancing model choice when approach/missingness changes ----
                 observeEvent(c(input$CF_radio, input$missingness_radio), {
                   
                   
                   ## If CPBS selected, update PS model selection to remove options
                   
                   if(!is.null(input$CF_radio)){
                     
                     if(input$CF_radio == i18n$t("Approach CBPS")){
                       CF_approach_values$balancing_model_selection <- p(i18n$t("Approach Model CBPS"))
                       CF_approach_values$balancing_model_change_message <- NULL
                       CF_approach_values$balancing_model_description <- NULL
                       CF_approach_values$balancing_model_description_general <- NULL
                     } else{
                       
                       if(!is.null(input$missingness_radio) & input$CF_radio != i18n$t("Approach NBP")){ ## If approach changed and missingness selected, ensure that general PS model description displayed
                         CF_approach_values$balancing_model_description_general <- p(h5(i18n$t("Approach Description")), i18n$t("Approach Model description"))
                       }
                       
                       if(is.null(input$missingness_radio) & input$CF_radio != i18n$t("Approach NBP")){ ## If approach changed and no missingness selected, set ps model to initial message, unless NBP or CBPS
                         CF_approach_values$balancing_model_description_general <- p(h5(i18n$t("Approach Description")), i18n$t("Approach Model description"))
                         CF_approach_values$balancing_model_selection <- p(i18n$t("Approach Model initial"), br())
                       }
                     }
                   }
                   
                   
                   ### Binary treatments ----
                   # If both appraoch and missingness have been selected and CBPS not selected
                   if(!is.null(input$CF_radio) & !is.null(input$missingness_radio)){

                     if (length(unique(na.omit(raw_data()[,treatment_variable()]))) == 2 & !input$CF_radio == i18n$t("Approach CBPS")){ ## PS model for binary treatments - don't apply when CBPS selected

                       ## If there are more than 50 or more rows in data, include GBM
                       if (!validation_log()$no_GBM){

                         CF_approach_values$balancing_model_selection <- p(
                           radioButtons(NS(id, "balancing_model_radio"), label = h4(i18n$t("Approach Choose model")),
                                        choices = list(
                                          i18n$t("Approach GBM"),
                                          i18n$t("Approach RF"),
                                          i18n$t("Approach GLM")),
                                        selected = character(0))
                         )
                       }
                       else{
                         CF_approach_values$balancing_model_selection <- p(
                           radioButtons(NS(id, "balancing_model_radio"), label = h4(i18n$t("Approach Choose model")),
                                        choices = list(
                                          i18n$t("Approach RF"),
                                          i18n$t("Approach GLM")),
                                        selected = character(0)),
                           br(),
                           p(i18n$t("Approach Too small GBM"), style = "color:gray;")
                         )
                       }

                       if (input$missingness_radio == i18n$t("Approach Choose Weighting")){
                         CF_approach_values$balancing_model_selection <- p(
                           radioButtons(NS(id, "balancing_model_radio"), label = h4(i18n$t("Approach Choose model")),
                                        choices = list(
                                          i18n$t("Approach GLM")),
                                        selected = character(0))
                         )
                         }
                     }
                   }
                 })
                 
                 
                 ## Update guide information based on choice of approach
                 observeEvent(input$CF_radio,{
                   if (input$CF_radio == i18n$t("Approach IPTW")){
                     CF_approach_values$approach_description <- p(h5(i18n$t("Approach IPTW")),
                                                                  p(i18n$t("Approach IPTW description")),
                                                                  a(id = "link",i18n$t("Approach IPTW link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#iptw",  target="_blank")
                                                                  )
                   }
                   if (input$CF_radio == i18n$t("Approach PSM")){
                     CF_approach_values$approach_description <- p(h5(i18n$t("Approach PSM")),
                                                                  p(i18n$t("Approach PSM description")),
                                                                  a(id = "link",i18n$t("Approach PSM link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#propensity-score-matching",  target="_blank")
                                                                  )
                   }
                   if (input$CF_radio == i18n$t("Approach NBP")){
                     CF_approach_values$approach_description <- NULL
                   }
                   if (input$CF_radio == i18n$t("Approach CBPS")){
                     CF_approach_values$approach_description <- p(h5(i18n$t("Approach CBPS")),
                                                                  p(i18n$t("Approach CBPS description"))
                                                                  
                     )
                   }
                   CF_approach_values$approach_missing_message <- NULL
                 })
                 
                 observeEvent(input$missingness_radio,{
                   
                   if (input$missingness_radio == i18n$t("Approach Choose MI")){
                     CF_approach_values$missingness_description <- p(h5(i18n$t("Approach Choose MI")),
                                                                     p(i18n$t("Approach MI description")),
                                                                     a(id = "link",i18n$t("Approach MI tutorial link"), href = "https://uoe-digicat.github.io/05_missing.html#multiple-imputation",  target="_blank")
                                                                     )
                   }
                   if (input$missingness_radio == i18n$t("Approach Choose Weighting")){
                     CF_approach_values$missingness_description <- p(h5(i18n$t("Approach Choose Weighting")),
                                                                     p(i18n$t("Approach Weighting description")),
                                                                     a(id = "link",i18n$t("Approach Weighting tutorial link"), href = "https://uoe-digicat.github.io/05_missing.html#non-response-weighting",  target="_blank")
                                                                     )
                   }
                   if (input$missingness_radio == i18n$t("Approach Choose CC")){
                     CF_approach_values$missingness_description <- p(h5(i18n$t("Approach Choose CC")),
                                                                     p(i18n$t("Approach CC description")),
                                                                     a(id = "link",i18n$t("Approach CC tutorial link"), href = "https://uoe-digicat.github.io/05_missing.html#complete-case-analysis",  target="_blank")
                                                                     )
                   }
                   ## Remove message with no missingness method selected error if present
                   CF_approach_values$missingness_missing_message <- NULL
                 })
                 
                 
                 
                 ## Update model description and parameters based on choice of approach
                 observeEvent(input$balancing_model_radio,{
                   
                   if (input$balancing_model_radio == i18n$t("Approach GLM")){
                     CF_approach_values$balancing_model_description <- p(h5(i18n$t("Approach GLM")),
                                                                         p(i18n$t("Approach Model GLM description")),
                                                                         a(id = "link",i18n$t("Approach Model GLM tutorial link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#propensity-score-specification-and-estimation",  target="_blank")
                                                                         )
                   }
                   
                   if (input$balancing_model_radio == i18n$t("Approach RF")){
                     CF_approach_values$balancing_model_description <- p(h5(i18n$t("Approach RF")),
                                                                         p(i18n$t("Approach Model RF description")))
                   }
                   
                   if (input$balancing_model_radio == i18n$t("Approach GBM")){
                     CF_approach_values$balancing_model_description <- p(h5(i18n$t("Approach GBM")),
                                                                         p(i18n$t("Approach Model GBM description")))
                   }
                   
                   if (input$balancing_model_radio == i18n$t("Approach ORL")){
                     CF_approach_values$balancing_model_description <- p(h5(i18n$t("Approach ORL")),
                                                                         p(i18n$t("Approach Model ORL description")),
                                                                         a(id = "link",i18n$t("Approach Model ORL tutorial link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#non-bipartite-nbp-methods",  target="_blank")
                     )
                     
                     CF_approach_values$balancing_model_description_general <- p(h5(i18n$t("Approach Description")), i18n$t("Approach Model description"))
                   }
                   
                   if (input$balancing_model_radio == i18n$t("Approach LR")){
                     CF_approach_values$balancing_model_description <- p("")
                   }
                   
                   ## Remove message with no balancing method selected error if present
                   CF_approach_values$balancing_model_missing_message <- NULL
                 })
                 
                 ## Save radio button input ----
                 ## Translation does not allow abbreviation of choices from radiobutton, therefor these must be assigned manually
                 observeEvent(input$CF_radio,{
                   if (input$CF_radio == i18n$t("Approach PSM")){
                     CF_approach_values$approach_choice <- "psm"
                   }
                   if (input$CF_radio == i18n$t("Approach IPTW")){
                     CF_approach_values$approach_choice <- "iptw"
                   }
                   if (input$CF_radio == i18n$t("Approach NBP")){
                     CF_approach_values$approach_choice <- "nbp"
                   }
                   if (input$CF_radio == i18n$t("Approach CBPS")){
                     CF_approach_values$approach_choice <- "cbps"
                   }
                 })
                 observeEvent(input$missingness_radio,{
                   if (input$missingness_radio == i18n$t("Approach Choose MI")){
                     CF_approach_values$missingness_choice <- "mi"
                   }
                   if (input$missingness_radio == i18n$t("Approach Choose CC")){
                     CF_approach_values$missingness_choice <- "complete"
                   }
                   if (input$missingness_radio == i18n$t("Approach Choose Weighting")){
                     CF_approach_values$missingness_choice <- "weighting"
                   }
                 })
                 
                 observeEvent(input$balancing_model_radio,{
                   if (input$balancing_model_radio == i18n$t("Approach GBM")){
                     CF_approach_values$model_choice <- "gbm"
                   }
                   if (input$balancing_model_radio == i18n$t("Approach RF")){
                     CF_approach_values$model_choice <- "randomforest"
                   }
                   if (input$balancing_model_radio == i18n$t("Approach GLM")){
                     CF_approach_values$model_choice <- "glm"
                   }
                   if (input$balancing_model_radio == i18n$t("Approach ORL")){
                     CF_approach_values$model_choice <- "poly"
                   }
                   if (input$balancing_model_radio == i18n$t("Approach LR")){
                     CF_approach_values$model_choice <- "lm"
                   }
                   if (input$CF_radio == i18n$t("Approach CBPS")){
                     ## Remove PS model if approach is CBPS
                     CF_approach_values$model_choice <- NULL
                   }
                 })
                 
                 ## Pass output to UI ----
                 
                 output$approach_description_general <- renderUI(CF_approach_values$approach_description_general)
                 output$approach_selection <- renderUI(CF_approach_values$approach_selection)
                 output$approach_description <- renderUI(CF_approach_values$approach_description)
                 output$approach_missing_message <- renderUI(CF_approach_values$approach_missing_message)
                 output$approach_rerun_message <- renderUI(CF_approach_values$approach_rerun_message)
                 output$approach_change_message <- renderUI(CF_approach_values$approach_change_message)
                 
                 output$missingness_selection <- renderUI(CF_approach_values$missingness_selection)
                 output$missingness_description <- renderUI(CF_approach_values$missingness_description)
                 output$missingness_rerun_message <- renderUI(CF_approach_values$missingness_rerun_message)
                 output$missingness_missing_message <- renderUI(CF_approach_values$missingness_missing_message)
                 output$missingness_change_message <- renderUI(CF_approach_values$missingness_change_message)

                 output$balancing_model_selection <- renderUI(CF_approach_values$balancing_model_selection)
                 output$balancing_model_description_general <- renderUI(CF_approach_values$balancing_model_description_general)
                 output$balancing_model_description <- renderUI(CF_approach_values$balancing_model_description)
                 output$balancing_model_rerun_message <- renderUI(CF_approach_values$balancing_model_rerun_message)
                 output$balancing_model_missing_message <- renderUI(CF_approach_values$balancing_model_missing_message)
                 output$balancing_model_change_message <- renderUI(CF_approach_values$balancing_model_change_message)
                 
                 ## Return counterfactual approach output to server ----
                 
                 CF_approach_output <- reactiveValues(missingness = NULL,
                                                      CF_radio = NULL,
                                                      balancing_model = NULL
                 )
                 
                 observe({
                   CF_approach_output$CF_radio <- CF_approach_values$approach_choice
                   CF_approach_output$CF_radio_display <- input$CF_radio
                   CF_approach_output$missingness <- CF_approach_values$missingness_choice
                   CF_approach_output$missingness_display <- input$missingness_radio
                   CF_approach_output$balancing_model <- CF_approach_values$model_choice
                   CF_approach_output$balancing_model_display <- input$balancing_model_radio
                 })
                 
                 return(CF_approach_output)
                 
               })
}
