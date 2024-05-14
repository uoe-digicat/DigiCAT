#'@import rmarkdown
#'@import knitr

outcome_model_ui <- function(id, i18n) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for choosing outcome model
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
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("OUTCOME"), style="color: white; border-bottom: solid 2px white;"))
           ),
           
           ## Navigation ----
           div(align="center",
               actionButton(NS(id, 'prev_outcome_model_btn'), i18n$t("Button Previous"), class = "default_button"),
               actionButton(NS(id, 'run_outcome_model_btn'), i18n$t("Outcome Button Run"), class = "default_button")
           ),
           br(),
           
           
           br(),
           
           ## Outcome model selection ----
           div(style = "display: flex;",
               div(style = "width: 65.5%;",
                   class = "text_blocks",
                   uiOutput(ns("outcome_model_selection")),
                   uiOutput(ns("outcome_model_missing_message"), style = "color: red;"), ## If no model selected when "Run" pressed, give warning
                   uiOutput(ns("outcome_model_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                   br(),
                   p(h5(i18n$t("Tabs Outcome model"))),
                     #i18n$t("Outcome LR description")),
                   uiOutput(ns('outcome_model_description')),
                   br(),
                   uiOutput(ns("outcome_model_description_method_selected"))
               ),
               
               ## Outcome model output ----
           
           mainPanel(wellPanel(id = "well_panel",
                               tabsetPanel(id = NS(id,"results_panel"),
                                           tabPanel(title = i18n$t("Outcome model output"),
                                                    value = NS(id,'outcome_model_results'),
                                                    ## Output of selected outcome_formula model
                                                    withSpinner(uiOutput(ns("outcome_model_output"))),
                                                    uiOutput(ns("outcome_model_output_initial")),
                                                    uiOutput(ns("outcome_model_output_change")),
                                                    uiOutput(ns("outcome_model_output_error"))
                                                    )
                                               )
                                   ))
           ),
               
               br(),
           
               ## Downloadable Output ----
               div(align="center",
                   uiOutput(ns("download_options"))
                   ),
           br(),
           div(align="center", uiOutput(ns("sensitivity_analysis_button")))
  )
}

outcome_model_server <- function(id, parent, data_source, file_path, raw_data, categorical_variables, treatment_variable, outcome_variable, matching_variables, covariates, survey_weight_var, cluster_var, stratification_var, validation_log, approach, missingness, balancing_model, approach_display, missingness_display, balancing_model_display, matching_method, matching_method_display, matching_ratio, estimation_stage_res, balancing_stage_res, observation_table, common_support_plot, love_plot, balance_table, descriptions, analysis_tab, i18n, selected_language) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----
                 output$prog_choiceDU <- renderUI({p(paste0(i18n$t("Tabs DU outcome"), outcome_variable()),br(),paste0(i18n$t("Tabs DU treatment"), treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0(i18n$t("Tabs Approach approach"), approach_display()),br(),paste0(i18n$t("Tabs Approach missingness"), missingness_display()),br(),paste0(i18n$t("Tabs Approach model"), balancing_model_display()), style="width: 200%; margin-left: -50%")})
                 
                 ## If approach, missingness or balancing model changes, update what is displayed as balancing stage choices
                 observeEvent(c(approach(), missingness(), balancing_model()), {
                   
                   if (!is.null(approach())){
                     ## If IPTW or CBPS selected, display nothing
                     if (approach() == "iptw" | approach() == "cbps"){
                       output$prog_choiceBM <- NULL
                     }
                     else{
                       ## If NBP or PSM selected, display matching method and ratio
                       output$prog_choiceBM <- renderUI({p(paste0(i18n$t("Balancing Matching method")," ", matching_method_display()), br(), paste0(i18n$t("Balancing Matching ratio"), "1:", matching_ratio()), style="width: 200%; margin-left: -50%")})
                     }}
                 })
                 
                 ## Define reactives ----
                 ## Create reactive value for approach description
                 outcome_model_values <- reactiveValues(
                   description_method_selected = NULL,
                   R_script = NULL,
                   report = NULL,
                   output_initial = p(h4(i18n$t("Balancing Tab output")),
                              p(i18n$t("Outcome model output initial"))),
                   output = NULL,
                   sensitivity_analysis_output = NULL
                 )
                 
                 ## Navigation ----
                 ## When "Prev is selected", show and move to last tab
                 observeEvent(input$prev_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "balancing-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_outcome_model_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "get_results-tab")
                 })
                 
                 
                 ## Language change ----
                 ## If language changes, reselect inputs - i18n resets radiobuttons
                 observeEvent(selected_language(), {
                   
                   outcome_type <- check_selected_outcome(raw_data(), outcome_variable())
                   output$outcome_model_description <- renderUI(
                     if (outcome_type == 'Continuous'){
                       p(i18n$t("Outcome LR description"))
                     } else if (outcome_type == 'Binary'){
                       p(i18n$t("Outcome LogReg description"))
                     })
                   
                   
                   if(!is.null(outcome_model_values$outcome_model_choice)){ ## Only run if approach has been selected, or if on 
                     if(!is.null(outcome_model_values$outcome_model_choice)){ ## Only run if approach has been selected, or if on 
                       if(outcome_model_values$outcome_model_choice == "marginal_effects"){
                         if (outcome_type == 'Continuous'){
                           updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LR w covar interaction"))
                           outcome_model_values$description_method_selected <- p(
                             h5(i18n$t("Outcome LR w covar interaction full"),
                                i18n$t("Outcome LR w covar interaction description"))
                           )
                         }else if (outcome_type == 'Binary'){
                           updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LogReg w covar interaction"))
                           outcome_model_values$description_method_selected <- p(
                             h5(i18n$t("Outcome LogReg w covar interaction full"),
                                i18n$t("Outcome LogReg w covar interaction description"))
                           )
                         }
                         
                       }
                       if(outcome_model_values$outcome_model_choice == "with_matching_variables"){
                         if (outcome_type == 'Continuous'){
                           updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LR w covar"))
                           outcome_model_values$description_method_selected <- p(
                             h5(i18n$t("Outcome LR w covar full"),
                                i18n$t("Outcome LR w covar description"))
                           )
                         } else if (outcome_type == 'Binary'){
                           updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LogReg w covar"))
                           outcome_model_values$description_method_selected <- p(
                             h5(i18n$t("Outcome LogReg w covar full"),
                                i18n$t("Outcome LogReg w covar description"))
                           )
                         }
                         
                       }
                       if(outcome_model_values$outcome_model_choice == "unadjusted"){
                         if (outcome_type == 'Continous'){
                           updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LR wo covar"))
                           outcome_model_values$description_method_selected <- p(
                             h5(i18n$t("Outcome LR wo covar full"),
                                i18n$t("Outcome LR wo covar description"))
                           )
                         } else if (outcome_type == 'Binary'){
                           updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LogReg wo covar"))
                           outcome_model_values$description_method_selected <- p(
                             h5(i18n$t("Outcome LogReg wo covar full"),
                                i18n$t("Outcome LogReg wo covar description"))
                           )
                         }
                         
                       }
                     }
                   }
                   
                   ## Replace output
                   if (!is.null(outcome_model_values$output) & !is.null(approach())){
                       if(approach() == "nbp"){
                         
                         if (outcome_type == 'Continuous'){
                         
                         outcome_model_values$output <- p(h4("Model Output"),
                                                          descriptions$estimate,
                                                          strong(p(paste0(i18n$t("Outcome model output estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                          br(),
                                                          descriptions$standard_error,
                                                          strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                          br(),
                                                          descriptions$p_value,
                                                          strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                          br(),
                                                          strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                         )
                         } else if (outcome_type == 'Binary'){
                           outcome_model_values$output <- p(h4("Model Output"),
                                                            descriptions$odds_ratio,
                                                            strong(p(paste0(i18n$t("Outcome model output odds ratio"), " ", round(exp(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"]), 4)))),
                                                            br(),
                                                            descriptions$estimate_bin,
                                                            strong(p(paste0(i18n$t("Outcome model output binary estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                            br(),
                                                            descriptions$standard_error,
                                                            strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                            br(),
                                                            descriptions$p_value,
                                                            strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                            br(),
                                                            strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                                                            
                           )
                         }
                         
                       }
                       if(approach() == "iptw" | approach() == "psm"){
                         
                         if (outcome_type == 'Continuous'){
                           outcome_model_values$output <- p(
                             h4("Model Output"),
                             i18n$t("Outcome model output estimate description"),
                             strong(p(paste0(i18n$t("Outcome model output estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                             br(),
                             i18n$t("Outcome model output SE description"),
                             strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                             br(),
                             i18n$t("Outcome model output P description"),
                             strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                             br(),
                             strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                           )
                         } else if (outcome_type == 'Binary'){
                           
                          
                           outcome_model_values$output <- p(
                             h4("Model Output"),
                             i18n$t("Outcome model output odds ratio description"),
                             strong(p(paste0(i18n$t("Outcome model output odds ratio"), " ", round(exp(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"]), 4)))),
                             br(),
                             i18n$t("Outcome model output binary estimate description"),
                             strong(p(paste0(i18n$t("Outcome model output binary estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                             br(),
                             i18n$t("Outcome model output SE description"),
                             strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                             br(),
                             i18n$t("Outcome model output P description"),
                             strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                             br(),
                             strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                           )
                         }
                         
                      
                       }
                   }
                   
                   ## Replace rerun warning
                   if (!is.null(outcome_model_values$model_rerun_message)){
                     outcome_model_values$model_rerun_message <- p(i18n$t("Outcome Warning rerun"))
                   }
                   
                   ## Replace output change warning
                   if (!is.null(outcome_model_values$output_rerun)){
                     outcome_model_values$output_rerun <- p(h4(i18n$t("Balancing Tab output")),
                                                            i18n$t("Outcome model output rerun"))
                   }
                 })
                 
                 ## Above only works when on appraoch tab, trigger same code when page if is revisted
                 observeEvent(analysis_tab(), {
                   
                   outcome_type <- check_selected_outcome(raw_data(), outcome_variable())
                   output$outcome_model_description <- renderUI(
                     if (outcome_type == 'Continuous'){
                       p(i18n$t("Outcome LR description"))
                     } else if (outcome_type == 'Binary'){
                       p(i18n$t("Outcome LogReg description"))
                     })
                   
                   if(analysis_tab() == "outcome_model-tab"){
                     if(!is.null(outcome_model_values$outcome_model_choice)){ ## Only run if approach has been selected, or if on 
                       if(!is.null(outcome_model_values$outcome_model_choice)){ ## Only run if approach has been selected, or if on 
                         if(outcome_model_values$outcome_model_choice == "marginal_effects"){
                           if (outcome_type == 'Continuous'){
                             updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LR w covar interaction"))
                             outcome_model_values$description_method_selected <- p(
                               h5(i18n$t("Outcome LR w covar interaction full"),
                                  i18n$t("Outcome LR w covar interaction description"))
                             )
                           } else if (outcome_type == 'Binary'){
                             updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LogReg w covar interaction"))
                             outcome_model_values$description_method_selected <- p(
                               h5(i18n$t("Outcome LogReg w covar interaction full"),
                                  i18n$t("Outcome LogReg w covar interaction description"))
                             )
                           }
                           
                         }
                         if(outcome_model_values$outcome_model_choice == "with_matching_variables"){
                           if (outcome_type == 'Continuous'){
                             updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LR w covar"))
                             outcome_model_values$description_method_selected <- p(
                               h5(i18n$t("Outcome LR w covar full"),
                                  i18n$t("Outcome LR w covar description"))
                             )
                           } else if (outcome_type == 'Binary'){
                             updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LogReg w covar"))
                             outcome_model_values$description_method_selected <- p(
                               h5(i18n$t("Outcome LogReg w covar full"),
                                  i18n$t("Outcome LogReg w covar description"))
                             )
                           }
                           
                         }
                         if(outcome_model_values$outcome_model_choice == "unadjusted"){
                           if (outcome_type == 'Continuous'){
                             updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LR wo covar"))
                             outcome_model_values$description_method_selected <- p(
                               h5(i18n$t("Outcome LR wo covar full"),
                                  i18n$t("Outcome LR wo covar description"))
                             )
                           } else if (outcome_type == 'Binary'){
                             updateRadioButtons(session, "outcome_model_radio", selected=i18n$t("Outcome LogReg wo covar"))
                             outcome_model_values$description_method_selected <- p(
                               h5(i18n$t("Outcome LogReg wo covar full"),
                                  i18n$t("Outcome LogReg wo covar description"))
                             )
                             
                           }
                          
                         }
                       }
                     }
                     
                     ## Replace output
                     if (!is.null(outcome_model_values$output) & !is.null(approach())){
                       if(approach() == "nbp"){
                         
                         if (outcome_type == 'Continuous'){
                           outcome_model_values$output <- p(h4("Model Output"),
                                                            descriptions$estimate,
                                                            strong(p(paste0(i18n$t("Outcome model output estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                            br(),
                                                            descriptions$standard_error,
                                                            strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                            br(),
                                                            descriptions$p_value,
                                                            strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                            br(),
                                                            strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                           )
                           
                         } else if (outcome_type == 'Binary'){
                           outcome_model_values$output <- p(h4("Model Output"),
                                                            i18n$t("Outcome model output odds ratio description"),
                                                            strong(p(paste0(i18n$t("Outcome model output odds ratio"), " ", round(exp(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"]), 4)))),
                                                            br(),
                                                            i18n$t("Outcome model output binary estimate description"),
                                                            strong(p(paste0(i18n$t("Outcome model output binary estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                            br(),
                                                            i18n$t("Outcome model output SE description"),
                                                            strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                            br(),
                                                            i18n$t("Outcome model output P description"),
                                                            strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                            br(),
                                                            strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                           )
                           
                         }
                         
                       }
                       if(approach() == "iptw" | approach() == "psm"){
                         
                         if (outcome_type == 'Continuous'){
                           outcome_model_values$output <- p(
                             h4("Model Output"),
                             i18n$t("Outcome model output estimate description"),
                             strong(p(paste0(i18n$t("Outcome model output estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                             br(),
                             i18n$t("Outcome model output SE description"),
                             strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                             br(),
                             i18n$t("Outcome model output P description"),
                             strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                             br(),
                             strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                           )
                           
                         } else if(outcome_type == 'Binary'){
                           outcome_model_values$output <- p(
                             h4("Model Output"),
                             i18n$t("Outcome model output odds ratio description"),
                             strong(p(paste0(i18n$t("Outcome model output odds ratio"), " ", round(exp(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"]), 4)))),
                             br(),
                             i18n$t("Outcome model output binary estimate description"),
                             strong(p(paste0(i18n$t("Outcome model output binary estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                             br(),
                             i18n$t("Outcome model output SE description"),
                             strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                             br(),
                             i18n$t("Outcome model output P description"),
                             strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                             br(),
                             strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                           )
                         }
                         
                       }
                     }
                     
                     ## Replace rerun warning
                     if (!is.null(outcome_model_values$model_rerun_message)){
                       outcome_model_values$model_rerun_message <- p(i18n$t("Outcome Warning rerun"))
                     }
                     
                     ## Replace output change warning
                     if (!is.null(outcome_model_values$output_rerun)){
                       outcome_model_values$output_rerun <- p(h4(i18n$t("Balancing Tab output")),
                                                              i18n$t("Outcome model output rerun"))
                     }
                   }
                 })

                   
                 ## Page setup ----
                 
                 ## Update choice of outcome model when approach is changed
                 observeEvent(approach(),{
                   
                   outcome_type <- check_selected_outcome(raw_data(), outcome_variable())
                   
                   output$outcome_model_description <- renderUI(
                     if (outcome_type == 'Continuous'){
                       p(i18n$t("Outcome LR description"))
                     } else if (outcome_type == 'Binary'){
                       p(i18n$t("Outcome LogReg description"))
                     })
                   
                   ## If NBP selected, remove choice with interaction
                   if(approach() == "nbp" | approach() == "cbps"){
                     
                     output$outcome_model_selection <- renderUI(
                       
                       if (outcome_type == 'Continuous'){
                         radioButtons(NS(id, "outcome_model_radio"), label = h4(i18n$t("Outcome Choose model")),
                                      choices = list(
                                        i18n$t("Outcome LR w covar interaction"),
                                        i18n$t("Outcome LR w covar"),
                                        i18n$t("Outcome LR wo covar")),
                                      selected = character(0),
                                      width = "200%")
                  
                       } else if (outcome_type == 'Binary'){
                         radioButtons(NS(id, "outcome_model_radio"), label = h4(i18n$t("Outcome Choose model")),
                                      choices = list(
                                        i18n$t("Outcome LogReg w covar interaction"),
                                        i18n$t("Outcome LogReg w covar"),
                                        i18n$t("Outcome LogReg wo covar")),
                                      selected = character(0),
                                      width = "200%")
        
                       }
                     )
                   }
                   ## Give choice with interaction for other approaches
                   if(approach() == "psm" | approach() == "iptw"){
                     output$outcome_model_selection <- renderUI(
                       
                       if (outcome_type == 'Continuous'){
                         radioButtons(NS(id, "outcome_model_radio"), label = h4(i18n$t("Outcome Choose model")),
                                      choices = list(
                                        i18n$t("Outcome LR w covar interaction"),
                                        i18n$t("Outcome LR w covar"),
                                        i18n$t("Outcome LR wo covar")),
                                      selected = character(0),
                                      width = "200%")
                       } else if (outcome_type == 'Binary'){
                         radioButtons(NS(id, "outcome_model_radio"), label = h4(i18n$t("Outcome Choose model")),
                                      choices = list(
                                        i18n$t("Outcome LogReg w covar interaction"),
                                        i18n$t("Outcome LogReg w covar"),
                                        i18n$t("Outcome LogReg wo covar")),
                                      selected = character(0),
                                      width = "200%")
                       }
                       
                     )
                   }
                 })
                 
                 ## Update descriptions and rerun message when input changes ----
                 observeEvent( outcome_model_values$outcome_model_choice,{
                   
                   outcome_type <- check_selected_outcome(raw_data(), outcome_variable())
                   output$outcome_model_description <- renderUI(
                     if (outcome_type == 'Continuous'){
                       p(i18n$t("Outcome LR description"))
                     } else if (outcome_type == 'Binary'){
                       p(i18n$t("Outcome LogReg description"))
                     })
                   
                   if( outcome_model_values$outcome_model_choice == "marginal_effects"){
                     if (outcome_type == 'Continuous'){
                       outcome_model_values$description_method_selected <- p(
                         h5(i18n$t("Outcome LR w covar interaction full"),
                            i18n$t("Outcome LR w covar interaction description"))
                       ) 
                     } else if (outcome_type == 'Binary'){
                       outcome_model_values$description_method_selected <- p(
                         h5(i18n$t("Outcome LogReg w covar interaction full"),
                            i18n$t("Outcome LogReg w covar interaction description"))
                       )
                     }
                     
                   }
                   
                   if( outcome_model_values$outcome_model_choice == "with_matching_variables"){
                     if (outcome_type == 'Continuous'){
                       outcome_model_values$description_method_selected <- p(
                         h5(i18n$t("Outcome LR w covar full"),
                            i18n$t("Outcome LR w covar description"))
                       )
                     } else if (outcome_type == 'Binary'){
                       
                       outcome_model_values$description_method_selected <- p(
                         h5(i18n$t("Outcome LogReg w covar full"),
                            i18n$t("Outcome LogReg w covar description"))
                       )
                     }

                   }
                   
                   if(outcome_model_values$outcome_model_choice == "unadjusted"){
                     if (outcome_type == 'Continuous'){
                       outcome_model_values$description_method_selected <- p(
                         h5(i18n$t("Outcome LR wo covar full"),
                            i18n$t("Outcome LR wo covar description"))
                       )
                     } else if (outcome_type == 'Binary'){
                       outcome_model_values$description_method_selected <- p(
                         h5(i18n$t("Outcome LogReg wo covar full"),
                            i18n$t("Outcome LogReg wo covar description"))
                       )
                     }
                     
                   }
                   
                   ## Remove missing parameter message if present
                   outcome_model_values$model_missing_message  <- NULL
                   

                   ## If outcome model has already been run, give informative message about rerun
                   if (!is.null(outcome_model_values$outcome_analysis_stage_res$standardised_format)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     ## Remove output message contents
                     outcome_model_values$output <- NULL
                     outcome_model_values$output_error <- NULL
                     outcome_model_values$output_rerun <- p(h4(i18n$t("Balancing Tab output")),
                                                            i18n$t("Outcome model output rerun"))
                                                      
                     
                   }
                 })
                 
                 
                 
                 ## Run outcome model ----
                 observeEvent(input$run_outcome_model_btn, {
                   
                   outcome_type <- check_selected_outcome(raw_data(), outcome_variable())
                   
                   ## If no outcome model has been selected, give error message
                   if(is.null(input$outcome_model_radio)){
                     outcome_model_values$model_missing_message <-  i18n$t("Outcome Warning no model")
                   }
                   
                   ## If outcome model has been selected, run model
                   if (!is.null(input$outcome_model_radio)){
                     
                     ## Remove output message contents
                     outcome_model_values$output <- NULL
                     outcome_model_values$output_initial <- NULL
                     outcome_model_values$output_rerun <- NULL
                     outcome_model_values$output_error <- NULL
                     outcome_model_values$output_change <- NULL
                     
                     ## Save potential error to check for running of code dependent on outcome model
                     error_check <- NA
                     error_check <- tryCatch({
                       
                       if(approach() == "psm" | approach() == "iptw" | approach() == "nbp" | approach() == "cbps"){
                         outcome_model_values$outcome_analysis_stage_res <- outcome_analysis_stage(
                           balanced_data = balancing_stage_res(),
                           counterfactual_method = approach(),
                           outcome_variable = outcome_variable(),
                           outcome_type = outcome_type,
                           treatment_variable = treatment_variable(),
                           matching_variable = matching_variables(),
                           covariates = covariates(),
                           psmodel_obj = estimation_stage_res(),
                           cluster_variable = cluster_var(),
                           missing_method = missingness(),
                           weighting_variable = survey_weight_var(),
                           outcome_formula = outcome_model_values$outcome_model_choice)
                       }
                       
                       # if(approach() == "nbp"){
                       #   outcome_model_values$outcome_analysis_stage_res <- outcome_analysis_stage(
                       #     balanced_data = balancing_stage_res(), 
                       #     counterfactual_method = approach(),
                       #     outcome_variable = outcome_variable(),
                       #     treatment_variable = treatment_variable(),
                       #     matching_variable = matching_variables(), 
                       #     covariates = covariates(),
                       #     psmodel_obj = estimation_stage_res(),
                       #     missing_method = missingness(),
                       #     outcome_formula = outcome_model_values$outcome_model_choice)
                       #   
                       #   
                       # }
                     },
                     
                     ## If outcome model does not run, return error message and enable run button 
                     error = function(cond) {
                       ## Enable "Run" button
                       shinyjs::enable("run_outcome_model_btn")
                       ## Output error message
                       outcome_model_values$output_error <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                     })
                     
                     
                     # Display output and show download button if no error in outcome model
                     if (all(!grepl("Error:", error_check))){
                       try({
                         ## Output estimate
                         
                         if(approach() == "psm" | approach() == "iptw" | approach() == "cbps"){
                           
                           if (outcome_type == 'Continuous'){
                             
                             outcome_model_values$output <- p(h4(i18n$t("Outcome model output")),
                                                              i18n$t("Outcome model output estimate description"),
                                                              strong(p(paste0(i18n$t("Outcome model output estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                              br(),
                                                              i18n$t("Outcome model output SE description"),
                                                              strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                              br(),
                                                              i18n$t("Outcome model output P description"),
                                                              strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                              br(),
                                                              strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                                                              
                             )
                             
                           } else if (outcome_type == 'Binary'){
                             outcome_model_values$output <- p(h4(i18n$t("Outcome model output")),
                                                              i18n$t("Outcome model output odds ratio description"),
                                                              strong(p(paste0(i18n$t("Outcome model output odds ratio"), " ", round(exp(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"]), 4)))),
                                                              br(),
                                                              i18n$t("Outcome model output binary estimate description"),
                                                              strong(p(paste0(i18n$t("Outcome model output binary estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                              br(),
                                                              i18n$t("Outcome model output SE description"),
                                                              strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                              br(),
                                                              i18n$t("Outcome model output P description"),
                                                              strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                              br(),
                                                              strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                                                              
                             )
                           }
                           
                           
                         }
                         
                         if(approach() == "nbp"){
                           
                           if (outcome_type == 'Continuous'){
                             outcome_model_values$output <- p(h4("Model Output"),
                                                              descriptions$estimate,
                                                              strong(p(paste0(i18n$t("Outcome model output estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                              br(),
                                                              descriptions$standard_error,
                                                              strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                              br(),
                                                              descriptions$p_value,
                                                              strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                              br(),
                                                              strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                             )
                             
                           } else if (outcome_type == 'Binary'){
                             
                             outcome_model_values$output <- p(h4("Model Output"),
                                                              i18n$t("Outcome model output odds ratio description"),
                                                              strong(p(paste0(i18n$t("Outcome model output odds ratio"), " ", round(exp(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"]), 4)))),
                                                              br(),
                                                              i18n$t("Outcome model output binary estimate description"),
                                                              strong(p(paste0(i18n$t("Outcome model output binary estimate"), " ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Coefficient Estimate"], 4)))),
                                                              br(),
                                                              descriptions$standard_error,
                                                              strong(p(paste0(i18n$t("Outcome model output SE")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Standard Error"], 4)))),
                                                              br(),
                                                              descriptions$p_value,
                                                              strong(p(paste0(i18n$t("Outcome model output P")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"P-value"], 4)))),
                                                              br(),
                                                              strong(p(paste0(i18n$t("Outcome model output CI")," ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Lower CI (2.5%)"], 4), " to ", round(outcome_model_values$outcome_analysis_stage_res$standardised_format[1,"Upper CI (97.5%)"], 4))))
                             )
                           }
                           
                           
                         }
                         
                         ## Add message noting that parameter reselection will require rerun
                         outcome_model_values$model_rerun_message <- p(i18n$t("Outcome Warning rerun"))
                         
                         ## Enable 'Run' button
                         shinyjs::enable("run_outcome_model_btn")
                         
                         ## Generate R script
                         # outcome_model_values$R_script <- get_R_script(
                         #   data_source = data_source(),
                         #   file_path = file_path(),
                         #   categorical_variables = categorical_variables(),
                         #   outcome_variable = outcome_variable(),
                         #   treatment_variable = treatment_variable(),
                         #   matching_variables = matching_variables(),
                         #   covariates = covariates(),
                         #   weighting_variable = survey_weight_var(),
                         #   cluster_variable = cluster_var(),
                         #   strata_variable = stratification_var(),
                         #   CF_approach = approach(),
                         #   missing_method = missingness(),
                         #   balancing_model = balancing_model(),
                         #   matching_method = matching_method(),
                         #   matching_ratio = matching_ratio(),
                         #   outcome_formula = input$outcome_model_radio,
                         #   DigiCAT_balanced_data = balancing_stage_res(),
                         #   DigiCAT_extracted_balanced_data = outcome_model_values$outcome_analysis_stage_res$extracted_balanced_data,
                         #   DigiCAT_fitted_model = outcome_model_values$outcome_analysis_stage_res$fitted_model,
                         #   DigiCAT_extracted_outcome_results = outcome_model_values$outcome_analysis_stage_res$extracted_outcome_results)

                         
                         ## Add download script button
                         output$download_options <- renderUI({
                           div(
                             #downloadButton(session$ns("download_script"), i18n$t("Outcome Button download script"), class = "default_button"),
                             downloadButton(session$ns("download_report"), i18n$t("Outcome Button download report"), class = "default_button"))
                         })
                         
                         ## Add sensitivity analysis option
                         # output$sensitivity_analysis_button <- renderUI({
                         #   div(
                         #     actionButton(session$ns("sensitivity_analysis_button"), i18n$t("Outcome Button sensitivity"), class = "default_button")
                         #     )
                         # })
                         
                         ## If file path is NULL (when example data used), create new variable to record this
                         if (is.null(file_path())){
                           outcome_model_values$file_path <- "DigiCAT Example Data"
                         }else{ ## If file path exists, get file name
                           outcome_model_values$file_path <- basename(file_path())
                         }
                       })
                     }
                   }
                 })
                 
                 ## Reset if outcome model input changes ----
                 ## Remove outcome model output and force rerun if previous steps have changed since previous run
                 observeEvent(c(estimation_stage_res(), balancing_stage_res(), outcome_model_values$outcome_model_choice), {
                   ## First check if outcome model has been run yet, if yes, print informative message in output
                   if (!is.null(outcome_model_values$outcome_analysis_stage_res$standardised_format)){
                     ## Replace output with explanation of why output has been deleted
                     ## Remove output message contents
                     outcome_model_values$output <- NULL
                     outcome_model_values$output_initial <- NULL
                     outcome_model_values$output_error <- NULL
                     outcome_model_values$output_change <- p(h4(i18n$t("Balancing Tab output")),
                                                      i18n$t("Outcome model output rerun"))
                     
                     ## Remove download button
                     output$download_options <- NULL
                     
                     ## Remove sensitivity analysis button
                     output$sensitivity_analysis_button <- NULL
                     
                     ## Hide sensitivity analysis tab and button
                     hideTab(session = parent, inputId = NS(id,"results_panel"), target = NS(id, "sensitivity_analysis"))
                     showTab(session = parent, inputId = NS(id,"results_panel"), target = NS(id, "outcome_model_results"), select = TRUE)
                     output$sensitivity_analysis_button <- NULL
                   }
                 })
                 
                 ## Sensitivity analysis ----
                 observeEvent(input$sensitivity_analysis_button, {
                   
                   ## Switch to sensitivity analysis tab
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "sensitivity_analysis-tab")

                 })
                 
                 ## Download output ----
                 output$download_options <- renderUI({
                   
                 })
                 
                 ## Download script when "download R script" clicked
                 output$download_script <- downloadHandler(
                   
                   filename = function() {
                     paste("DigiCAT.R", sep = "")
                   },
                   content = function(file) {
                     write.table(
                       isolate(outcome_model_values$R_script),
                       file, 
                       quote = FALSE,
                       row.names = FALSE, 
                       col.names = FALSE)
                   }
                 )
                 
                 ## Download report when "download report" clicked
                 output$download_report <- 
                   
                   downloadHandler(
                     filename = "DigiCAT_report.pdf",
                     content =
                       function(file) {
                         shinyjs::disable("download_report")
                         on.exit(shinyjs::enable("download_report"))
                         output <- render(
                           input = "report_template.Rmd",
                           output_format = "pdf_document",
                           params = list(n = 100,
                                         data_name = outcome_model_values$file_path,
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
                                         outcome_formula = outcome_model_values$outcome_model_choice,
                                         outcome_res = outcome_model_values$outcome_analysis_stage_res$standardised_format)
                         )
                         file.copy(output, file)
                       }
                   )
                 
                 ## Save radio button input ----
                 ## Translation does not allow abbreviation of choices from radiobutton, therefore these must be assigned manually
                 observeEvent(input$outcome_model_radio,{
                   if (input$outcome_model_radio %in% c(i18n$t("Outcome LR w covar interaction"),i18n$t("Outcome LogReg w covar interaction"))){
                     outcome_model_values$outcome_model_choice <- "marginal_effects"
                   }
                   if (input$outcome_model_radio %in% c(i18n$t("Outcome LR w covar"), i18n$t("Outcome LogReg w covar"))){
                     outcome_model_values$outcome_model_choice <- "with_matching_variables"
                   }
                   if (input$outcome_model_radio %in% c(i18n$t("Outcome LR wo covar"), i18n$t("Outcome LogReg wo covar"))){
                     outcome_model_values$outcome_model_choice <- "unadjusted"
                   }
                 })
                 
                 ## Pass output to UI ----
                 output$outcome_model_description_method_selected <- renderUI(outcome_model_values$description_method_selected)
                 output$outcome_model_missing_message <- renderUI(outcome_model_values$model_missing_message)
                 output$outcome_model_rerun_message <- renderUI(outcome_model_values$model_rerun_message)
                 output$outcome_model_parameters_method <- renderUI(outcome_model_values$parameters_method)
                 output$outcome_model_output <- renderUI(outcome_model_values$output)
                 output$outcome_model_output_initial <- renderUI(outcome_model_values$output_initial)
                 output$outcome_model_output_error <- renderUI(outcome_model_values$output_error)
                 output$outcome_model_output_change <- renderUI(outcome_model_values$output_change)
                 output$sensitivity_analysis_output <- renderUI(outcome_model_values$sensitivity_analysis_output)
                 
                 
                 ## Return outcome model output to server ----
                 
                 outcome_model_output <- reactiveValues()
                 
                 observe({
                   outcome_model_output$outcome_formula_display  <-  input$outcome_model_radio
                   outcome_model_output$outcome_formula <-  outcome_model_values$outcome_model_choice
                 })
                 
                 return(outcome_model_output)
                 
               })
}
