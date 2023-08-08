# balancing module ----
#'@import cobalt
#'@import shinycssloaders

balancing_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5("GET STARTED")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("DATA UPLOAD"), p(uiOutput(ns("prog_choiceDU"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", p(h5("APPROACH"), p(uiOutput(ns("prog_choiceCF"))))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: white; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("BALANCING", style="color: white; border-bottom: solid 2px white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("OUTCOME", style="color: #607cc4;"))
           ),
           div(align="center",
               actionButton(NS(id, 'prev_balancing_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_balancing_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_balancing_btn'), 'Next', class = "default_button")),
           br(),
             div(style = "display: flex;",
               ## Load balancing options and descriptions based on CF approach chosen
               div(style = "width: 49%;",
                 uiOutput(ns("balancing_options"))
                 ),
               ## Display balancing output
               div(style = "width: 49%; margin-left: 2%; border-color: transparent",
                   class = "text_blocks",
                   withSpinner(uiOutput(ns("balancing_output"))),
                 )
           )
           
  )
}

balancing_server <- function(id, parent, raw_data, outcome_variable, treatment_variable, matching_variables, covariates, survey_weight_var, cluster_var, stratification_var, approach, missingness, balancing_model, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----

                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0("Approach: ", approach()),br(),paste0("Missingness: ", missingness()),br(),paste0("Model: ", balancing_model()), style="width: 200%; margin-left: -50%")})
                 
                 ## Define Reactives ----
                 
                 ## Create reactive value for approach description
                 balancing_values <- reactiveValues(
                   description_method = NULL,
                   description_ratio = NULL,
                   estimation_stage_res = NULL,
                   balancing_stage_res = NULL,
                   output = NULL)
                 
                 ## Page Setup ----
                 
                 ## Render balancing analysis options based on approach chosen
                 observeEvent(approach(),{
                   
                   if (approach() == "psm"){
                     
                     output$balancing_options <- renderUI({
                       div(
                         div(class = "text_blocks",
                             style = "width: 100%;",
                             radioButtons(session$ns("method_radio"), label = h4("Choose a Matching Method:"),
                                          choices = c(
                                            "Optimal" = "optimal",
                                            "Nearest Neighbour (NN)" = "nearest"),
                                          selected = character(0)),
                             uiOutput(session$ns("balancing_method_missing_message"), style = "color: red;"), ## If no matching mehtod selected when "Run" pressed, give warning
                             uiOutput(session$ns("balancing_method_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             ## Description of selected balancing method and ratio
                             uiOutput(session$ns("balancing_description_method")),
                             p("For more information, visit our ", actionLink(session$ns("balancing_tab_tutorial_link"), "tutorial"), ".")
                         ),
                         br(),
                         div(style = "width: 100%",
                             class = "text_blocks",
                               radioButtons(session$ns("ratio_radio"), label = h4("Choose a Matching Ratio:"),
                                          choices = c(
                                            "1:1" = "one_to_one",
                                            "1:k" = "one_to_k"),
                                          selected = character(0)),
                                          uiOutput(session$ns("ratio_slider_output")), ## Only show ration slider if 1:K is selected
                             uiOutput(session$ns("balancing_ratio_missing_message"), style = "color: red;"), ## If no matching ratio selected when "Run" pressed, give warning
                             uiOutput(session$ns("balancing_ratio_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             uiOutput(session$ns("balancing_description_ratio"))
                         )
                       )
                     })
                     
                     ## Reactive value for approach description
                     balancing_values$description_method = descriptions$balancing_method
                     balancing_values$description_ratio = descriptions$balancing_ratio
                     balancing_values$ratio =  NULL
                     balancing_values$output = p(h4("Output:"),
                     p("Once you have selected your matching method and ratio, press
                       'Run' to get output."))
                   }
                   
                   if (approach() == "nbp"){
                     
                     output$balancing_options <- renderUI({
                       div(
                         div(class = "text_blocks",
                             style = "width: 100%;",
                             radioButtons(session$ns("method_radio"), label = h4("Choose a Matching Method:"),
                                          choices = c(
                                            "Optimal" = "optimal"),
                                          selected = character(0)),
                             uiOutput(session$ns("balancing_method_missing_message"), style = "color: red;"), ## If no matching mehtod selected when "Run" pressed, give warning
                             uiOutput(session$ns("balancing_method_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             ## Description of selected balancing method and ratio
                             uiOutput(session$ns("balancing_description_method")),
                             p("For more information, visit our ", actionLink(session$ns("balancing_tab_tutorial_link"), "tutorial"), ".")
                         ),
                         br(),
                         div(style = "width: 100%",
                             class = "text_blocks",
                             radioButtons(session$ns("ratio_radio"), label = h4("Choose a Matching Ratio:"),
                                          choices = c(
                                            "1:1" = "one_to_one"),
                                          selected = character(0)),
                             uiOutput(session$ns("ratio_slider_output")), ## Only show ration slider if 1:K is selected
                             uiOutput(session$ns("balancing_ratio_missing_message"), style = "color: red;"), ## If no matching ratio selected when "Run" pressed, give warning
                             uiOutput(session$ns("balancing_ratio_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             uiOutput(session$ns("balancing_description_ratio"))
                         )
                       )
                     })
                     
                     ## Reactive value for approach description
                     balancing_values$description_method = descriptions$balancing_method
                     balancing_values$description_ratio = descriptions$balancing_ratio
                     balancing_values$ratio =  NULL
                     balancing_values$output = p(h4("Output:"),
                                                 p("Once you have selected your matching method and ratio, press
                       'Run' to get output."))
                   }

                   if (approach() == "iptw"){
                     
                     output$balancing_options <- renderUI({
                       div(
                           div(style = "width: 100%;",
                               class = "text_blocks",
                               div(
                                 "Balancing options/extra info for weighting go here"
                               )
                           )
                       )
                     })
                     
                     ## Create reactive value for approach description
                     balancing_values$description_method = p(h4("Description:"),
                                                           p("Info about balancing with weighting"))
                     balancing_values$description_ratio = NULL ##no ratio for weighting
                     balancing_values$output = p(h4("Output:"),
                                  p("Once you have selected your matching method, press
                                'Run' to get output."))
                   }
                 })

                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("next_balancing_btn")
                 
                 ## Navigation ----
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_balancing_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "CF_approach-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_balancing_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "outcome_model-tab")
                 })
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$balancing_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "tutorial")
                 })
                 
                 ## Get Descriptions ----
                 
                 ## Update matching method description based on choice of approach
                 observeEvent(input$method_radio,{
                   
                   if(input$method_radio == "nearest"){
                     balancing_values$description_method <- descriptions$nearest
                   }
                   
                   if(input$method_radio == "optimal"){
                     balancing_values$description_method <- descriptions$optimal
                   }
                   
                   ## Remove missing parameter message if present
                   balancing_values$method_missing_message <- NULL

                   ## Check if balancing has already been run, if so, output informative message, hide balancing results and force rerun
                   if (!is.null(balancing_values$balancing_stage_res)){
                     ## Replace balancing output with explanation of why output has been deleted
                     balancing_values$output <- p(h4("Output:"),
                                                p(
                                                  strong("It looks like balancing will have to be rerun, this is because some of the required inputs have been changed since the 
                         previous run.")," Once you have selected your matching method and ratio, press
                         'Run' to get output."))
                     

                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                   }
                 })
                 
                 ## Update matching ratio description based on choice of ratio
                 observeEvent(input$ratio_radio,{
                   
                   if (input$ratio_radio == "one_to_one"){
                   
                       balancing_values$description_ratio <- descriptions$one_to_one

                      ## Update matching ratio to "1"
                       balancing_values$ratio <- 1
                       
                       ## Remove ratio slider if present
                        output$ratio_slider_output <- NULL
                     }
                     
                     if(input$ratio_radio == "one_to_k"){
                       balancing_values$description_ratio <- descriptions$one_to_K
                       
                       ## Add slider input for user to pick K in 1:k
                       output$ratio_slider_output <- renderUI(
                       sliderInput(session$ns("ratio_slider"), label = h4("Choose 'k':"),
                                     min = 2, max = 10, value = 2)
                       )
                       
                       ## Initiate "K" as 2
                       balancing_values$ratio <- 2
                       }
                   
                   ## Remove missing parameter message if present
                   balancing_values$ratio_missing_message <- NULL
                 })
                 
                 ## If K in matching ratio is changes, update reactive value indicating K
                 observeEvent(input$ratio_slider, {
                   balancing_values$ratio <- input$ratio_slider
                 })
                 
                 
                 ## If reactive value indicating K changes give informative message and force rerun
                 observeEvent(balancing_values$ratio, {
                   
                   ## Check if balancing has already been run, if so, output informative message, hide balancing results and force rerun
                   if (!is.null(balancing_values$balancing_stage_res)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_values$output <- p(h4("Output:"),
                                                  p(strong("It looks like balancing will have to be rerun, this is because some of the required inputs have been changed since the 
                         previous run.")," Once you have selected your matching method and ratio, press
                         'Run' to get output."))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                   }
                 })
                 
                 ## Run Balancing ----
                 observeEvent(input$run_balancing_btn, {
                   
                   ## If matching approach selected but no balancing method, give error message
                   if(approach() == "psm" & is.null(input$method_radio)){
                     balancing_values$method_missing_message <- p("Please select a matching method before proceeding", style = "color:red")
                   }
                   
                   ## If matching approach selected but no balancing ratio, give error message
                   ## Slider counter used as slider input cannot be initiated with NULL - counter value of greater than 0 indicates slider input has been selected
                   if(approach() == "psm" & is.null(balancing_values$ratio)){
                     balancing_values$ratio_missing_message <- p("Please select a matching ratio before proceeding", style = "color:red")
                   }
                   ## If all required input present, carry out balancing
                   if (approach() == "iptw" | ((approach() == "psm" & !is.null(input$method_radio) & !is.null(balancing_values$ratio))) ) {
                     
                     ## Disable 'Run' button
                     shinyjs::disable("run_balancing_btn")
  
                     ## Remove general output message
                     balancing_values$output  <- NULL
                     
                     ## Remove balancing result outputs
                     output$AUC <- NULL
                     output$love_plot  <- NULL
                     output$balance_table <- NULL
                     output$observations_table <- NULL

                     ## Save potential error to check for running of code dependent on balancing model
                     error_check <- NA
                     error_check <- tryCatch({
                       
                       # Get propensity scores
                       balancing_values$estimation_stage_res <- estimation_stage(
                         .data = raw_data(),
                         missing_method = missingness(),
                         model_type = balancing_model(),
                         treatment_variable = treatment_variable(),
                         matching_variable = matching_variables(),
                         weighting_variable = survey_weight_var(),
                         cluster_variable = cluster_var(),
                         strata_variable = stratification_var()
                         )

                       ## Balance dataset
                       if (approach() == "psm"){
                         
                         balancing_values$balancing_stage_res <- balance_data(
                           counterfactual_method = approach(),
                           treatment_variable = treatment_variable(),
                           matching_variable = matching_variables(),
                           PS_estimation_object = balancing_values$estimation_stage_res,
                           missing_method = missingness(),
                           ratio = balancing_values$ratio,
                           method = input$method_radio)

                       }

                       if (approach() == "iptw"){

                         balancing_values$balancing_stage_res <- balance_data(
                           counterfactual_method = approach(),
                           treatment_variable = treatment_variable(),
                           matching_variable = matching_variables(),
                           PS_estimation_object = balancing_values$estimation_stage_res,
                           missing_method = missingness())

                         }
                       },
                     
                     ## If balancing does not run, return error message and enable run button 
                     error = function(cond) {
                       ## Enable "Run" button
                       shinyjs::enable("run_balancing_btn")
                       ## Output error message
                       balancing_values$output <- p(p(paste0("Error: ", conditionMessage(cond)) , style = "color:red"))
                     })
  
                     
                     ## Output balance plots and tables if no error in balancing
                     if (all(!grepl("Error:", error_check))){
                       try({
                         
                         ## Get AUC
                         output$AUC <- renderUI(p(
                           h4("The Receiver Operating Characteristic (ROC) curve:"),
                           renderPlot(performance_plot(psmodel_obj = balancing_values$estimation_stage_res,
                                                       t_var = treatment_variable(),
                                                       treattype = "binary")),
                           p("The Receiver Operating Characteristic (ROC) curve is a plotting method used to assess
                                                       the performance of a binary classifier (such as a probit regression model) across
                                                       various discrimination thresholds. The curve plots the true positive rate (sensitivity)
                                                       against the false positive rate (1 - specificity) at different threshold values.
                                                       Examining the shape and steepness of the curve shows the classifier's ability
                                                       to distinguish between the two outcomes. The Area Under the Curve (AUC) summarizes
                                                       the overall performance, taking values between 0.5 and 1, with higher values indicating better
                                                       discrimination. A value of 0.5 suggests the classifier performs no better than random guessing,
                                                       and the corresponding curve would be a diagonal line from bottom-left to top-right.")
                         ))
                         # 
                         ## Get love plot
                         output$love_plot <- renderPlot(cobalt::love.plot(balancing_values$balancing_stage_res))
                         ## Get balance table
                         output$balance_table <- DT::renderDataTable({DT::datatable(as.data.frame(cobalt::bal.tab(balancing_values$balancing_stage_res)[[which(grepl("^Balance",names(cobalt::bal.tab(balancing_values$balancing_stage_res))))]]), rownames = TRUE, options = list(scrollX = TRUE))})
                         ## Get observation table
                         output$observations_table <- DT::renderDataTable({DT::datatable(as.data.frame(cobalt::bal.tab(balancing_values$balancing_stage_res)[["Observations"]]), rownames = TRUE, options = list(scrollX = TRUE))})

                         ## Add tabs to display output
                         balancing_values$output <- renderUI(
                           tabsetPanel(id = NS(id, "balancing_output_plots"),
                                     tabPanel(title = "Receiver Operating Characteristic (ROC) curve",
                                              value = NS(id, 'ROC_tab'),
                                              br(),
                                              withSpinner(uiOutput(session$ns("AUC")))),
                                     tabPanel(title = "Observation Table",
                                              value = NS(id, 'observations_table_tab'),
                                              br(),
                                              withSpinner(DT::dataTableOutput(session$ns("observations_table")))),
                                     tabPanel(title = "Love Plot",
                                              value = NS(id, 'love_plot_tab'),
                                              br(),
                                              withSpinner(plotOutput(session$ns("love_plot")))
                                     ),
                                     tabPanel(title = "Balance Table",
                                              value = NS(id, 'balance_table_tab'),
                                              br(),
                                              withSpinner(DT::dataTableOutput(session$ns("balance_table")))
                                     ))
                         )
        
                         ## Enable 'Run' and 'Next' buttons
                         shinyjs::enable("run_balancing_btn")
                         shinyjs::enable("next_balancing_btn")
                         
                         ## Add message noting that parameter reselection will require rerun
                         balancing_values$method_rerun_message <- p("Note: Changing this parameter will require balancing to be rerun along with all subsequent steps.")
                         balancing_values$ratio_rerun_message <- p("Note: Changing this parameter will require balancing to be rerun along with all subsequent steps.")
                       })
                     }
                     }
                 })
                 
                 ## Remove output if inputs have changed ----
                 
                 # Remove balancing output and force rerun if previous steps have changed since previous run
                 observeEvent(c(approach(), missingness(), balancing_model()), {
                   ## First check if balancing has been run yet, if yes, print informative message and force rerun
                   if (!is.null(balancing_values$balancing_stage_res)){

                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_values$output <- p(h4("Output:"),
                                                  strong("It looks like balancing will have to be rerun, this is because some of the required inputs have been changed since the
                       previous run.")," Once you have selected your matching method and ratio, press
                       'Run' to get output.")

                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")

                   }
                 })
                 
                 ## Return output ----

                 ## Display information for choosing counterfactual approach, relevent parameters and model output
                 output$balancing_description_method <- renderUI(balancing_values$description_method)
                 output$balancing_method_rerun_message <- renderUI(balancing_values$method_rerun_message)
                 output$balancing_method_missing_message <- renderUI(balancing_values$method_missing_message)
                 output$balancing_description_ratio <- renderUI(balancing_values$description_ratio)
                 output$balancing_ratio_rerun_message <- renderUI(balancing_values$ratio_rerun_message)
                 output$balancing_ratio_missing_message <- renderUI(balancing_values$ratio_missing_message)
                 output$balancing_output <- renderUI(balancing_values$output)
                 
                 ## Return balancing output
                 ## Return choices to server to pass to other tool pages
                 
                 ## Return choices to server to pass to other tool pages
                 Balancing_output <- reactiveValues(estimation_stage_res = NULL,
                                                    balancing_stage_res = NULL,
                                                    ratio_radio = NULL,
                                                    method_radio = NULL
                 )
                 
                 observe({
                   Balancing_output$estimation_stage_res <-  balancing_values$estimation_stage_res
                   Balancing_output$balancing_stage_res <- balancing_values$balancing_stage_res
                   Balancing_output$ratio_radio <- balancing_values$ratio
                   Balancing_output$method_radio <- input$method_radio
                 })
                 
                 return(Balancing_output)
               })
}
