# balancing module ----
#'@import cobalt
#'@import shinycssloaders

balancing_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing matching options and running balancing
  tabPanel(title = "",
           value = NS(id, 'tab'),
           br(),
           
           ## Navigation bar ----
           
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
           
           ## Navigation ----
           
           div(align="center",
               actionButton(NS(id, 'prev_balancing_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_balancing_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_balancing_btn'), 'Next', class = "default_button")),
           br(),
           
           ## Balancing choice selection ----
           
           div(style = "display: flex;",
               ## Load balancing options and descriptions based on CF approach chosen
               div(style = "width: 49%;",
                   uiOutput(ns("balancing_options"))
               ),
               
               ## Balancing output ----
               
               mainPanel(wellPanel(id = "well_panel",
                     tabsetPanel(id = NS(id,"results_panel"),
                                 tabPanel(title = "Matching Variable(s) Summary (Unbalanced)",
                                          value = NS(id, 'descriptive_stats'),
                                          br(),
                                          uiOutput(ns("descriptive_stats"))),
                                 tabPanel(title = "Output",
                                          value = NS(id, 'output'),
                                          br(),
                                          withSpinner(uiOutput(ns("balancing_output"))))
                     )
                   ))
           )
  )
}

balancing_server <- function(id, parent, raw_data, categorical_variables, outcome_variable, treatment_variable, matching_variables, covariates, survey_weight_var, cluster_var, stratification_var, approach, missingness, balancing_model, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----
                 
                 output$prog_choiceDU <- renderUI({p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0("Approach: ", approach()),br(),paste0("Missingness: ", missingness()),br(),paste0("Model: ", balancing_model()), style="width: 200%; margin-left: -50%")})
                 
                 ## Define Reactives ----
                 
                 ## Create reactive value for approach description
                 balancing_values <- reactiveValues(
                   matching_method_description = NULL,
                   matching_ratio_description = NULL,
                   estimation_stage_res = NULL,
                   balancing_stage_res = NULL,
                   matching_var_balance = NULL,
                   descriptive_stats = NULL,
                   output = p(h4("Output:"),
                              p("Once you have selected your matching method and ratio, press
                       'Run' to get output.")))
                 
                 ## Page Setup ----
                 
                 # Get descriptive statistics of unbalanced matching variables y treatment group
                 observeEvent(c(treatment_variable(), matching_variables()), {

                   ## Ensure data, matching variables and treatment variable have been seleceted
                   if (!(is.null(raw_data()) | is.null(treatment_variable()) | is.null(matching_variables()))){

                     ## Define summary function
                     my_summary <- function(v){
                       if(!any(is.na(v))){
                         res <- c(summary(v),"NA's"=0)
                       } else{
                         res <- summary(v)
                       }
                       return(res)
                     }

                     ## Get treatment groups
                     groups <- unique(isolate(raw_data())[,isolate(treatment_variable())])
                     groups <- sort(groups)

                     # Subset data by treatment group and get descriptives on each variable (by group)
                     unbalanced_summary <- NA

                     for (group in groups){
                       ## Get descriptives
                       ls <- lapply(as.data.frame(isolate(raw_data())[isolate(raw_data())[isolate(treatment_variable())] == group, isolate(matching_variables())]), my_summary)
                       df <- data.frame(matrix(unlist(ls), ncol = max(lengths(ls)), byrow = TRUE))
                       names(df) <- names(ls[[which(lengths(ls)>0)[1]]])
                       rownames(df) <- paste0(isolate(matching_variables()), " in ", isolate(treatment_variable()), " group ", group)
                       unbalanced_summary <- rbind(unbalanced_summary, df)
                     }
                     
                     unbalanced_summary <- unbalanced_summary[-1,]

                     ## Reorder table so matching variables are together
                     formatted_unbalanced_summary <- NA

                     for (matching_index in 1:length(isolate(matching_variables()))){
                       for (group in groups){
                         index <- matching_index + (length(isolate(matching_variables())) * (which(groups == group) - 1))
                         ## Get description of each matching variable in each group
                         formatted_unbalanced_summary_temp <- unbalanced_summary[index,]
                         formatted_unbalanced_summary <- rbind(formatted_unbalanced_summary,formatted_unbalanced_summary_temp)
                       }
                     }

                     balancing_values$descriptive_stats <- DT::renderDataTable({DT::datatable(round(formatted_unbalanced_summary[-1,], 2), rownames = TRUE, options = list(scrollX = TRUE))})
                   }
                 })
                 
                 
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
                             uiOutput(session$ns("matching_method_missing_message"), style = "color: red;"), ## If no matching mehtod selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_method_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             ## Description of selected balancing method and ratio
                             br(),
                             descriptions$matching_method,
                             br(),
                             uiOutput(session$ns("matching_method_description")),
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
                             uiOutput(session$ns("matching_ratio_missing_message"), style = "color: red;"), ## If no matching ratio selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_ratio_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             br(),
                             descriptions$matching_ratio,
                             br(),
                             uiOutput(session$ns("matching_ratio_description"))
                         )
                       )
                     })
                     
                     ## Reactive value for approach description
                     balancing_values$matching_method_description = NULL
                     balancing_values$matching_ratio_description = NULL
                     balancing_values$ratio =  NULL
                   }
                   
                   if (approach() == "nbp"){
                     
                     output$balancing_options <- renderUI({
                       div(
                         div(class = "text_blocks",
                             style = "width: 100%;",
                             radioButtons(session$ns("method_radio"), label = h4("Choose a Matching Method:"),
                                          choices = c(
                                            "Optimal" = "optimal"),
                                          selected = "optimal"),
                             uiOutput(session$ns("matching_method_missing_message"), style = "color: red;"), ## If no matching mehtod selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_method_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             ## Description of selected balancing method and ratio
                             br(),
                             descriptions$optimal_ordinal
                         ),
                         br(),
                         div(style = "width: 100%",
                             class = "text_blocks",
                             radioButtons(session$ns("ratio_radio"), label = h4("Choose a Matching Ratio:"),
                                          choices = c(
                                            "1:1" = "one_to_one"),
                                          selected = "one_to_one"),
                             uiOutput(session$ns("ratio_slider_output")), ## Only show ration slider if 1:K is selected
                             uiOutput(session$ns("matching_ratio_missing_message"), style = "color: red;"), ## If no matching ratio selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_ratio_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             br(),
                             descriptions$one_to_one_ordinal
                         )
                       )
                     })
                     
                     ## Reactive value for approach description
                     balancing_values$matching_method_description = NULL
                     balancing_values$matching_ratio_description = NULL
                     balancing_values$ratio =  NULL
                   }
                   
                   if (approach() == "iptw"){
                     
                     output$balancing_options <- renderUI({
                       div(
                         div(style = "width: 100%;",
                             class = "text_blocks",
                             div(
                               descriptions$IPTW_balancing
                             )
                         )
                       )
                     })
                     
                     ## Create reactive value for approach description
                     balancing_values$matching_method_description = p(h4("Description:"),
                                                                      p("Info about balancing with weighting"))
                     balancing_values$matching_ratio_description = NULL ##no ratio for weighting
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
                 
                 ## Update descriptions and rerun message when input changes ----
                 
                 ## Update matching method description based on choice of approach
                 observeEvent(input$method_radio,{
                   
                   if(input$method_radio == "nearest"){
                     balancing_values$matching_method_description <- descriptions$nearest
                   }
                   
                   if(input$method_radio == "optimal"){
                     balancing_values$matching_method_description <- descriptions$optimal_binary
                   }
                   
                   ## Remove missing parameter message if present
                   balancing_values$matching_method_missing_message <- NULL
                   
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
                     
                     balancing_values$matching_ratio_description <- descriptions$one_to_one_binary
                     
                     ## Update matching ratio to "1"
                     balancing_values$ratio <- 1
                     
                     ## Remove ratio slider if present
                     output$ratio_slider_output <- NULL
                   }
                   
                   if(input$ratio_radio == "one_to_k"){
                     balancing_values$matching_ratio_description <- descriptions$one_to_K
                     
                     ## Add slider input for user to pick K in 1:k
                     output$ratio_slider_output <- renderUI(
                       sliderInput(session$ns("ratio_slider"), label = h4("Choose 'k':"),
                                   min = 2, max = 10, value = 2)
                     )
                     
                     ## Initiate "K" as 2
                     balancing_values$ratio <- 2
                   }
                   
                   ## Remove missing parameter message if present
                   balancing_values$matching_ratio_missing_message <- NULL
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
                     balancing_values$matching_method_missing_message <- p("Please select a matching method before proceeding", style = "color:red")
                   }
                   ## If matching approach selected but no balancing ratio, give error message
                   ## Slider counter used as slider input cannot be initiated with NULL - counter value of greater than 0 indicates slider input has been selected
                   if(approach() == "psm" & is.null(balancing_values$ratio)){
                     balancing_values$matching_ratio_missing_message <- p("Please select a matching ratio before proceeding", style = "color:red")
                   }
                   ## If all required input present, carry out balancing
                   if (approach() == "iptw" | ((approach() == "psm" & !is.null(input$method_radio) & !is.null(balancing_values$ratio))) | ((approach() == "nbp"))  ) {

                     
                     ## Disable 'Run' button
                     shinyjs::disable("run_balancing_btn")
                     
                     ## Move to output tab
                     updateTabsetPanel(session = parent, inputId = "balancing-results_panel", selected = "balancing-output")
                     
                     ## Remove balancing output
                     balancing_values$estimation_stage_res <- NULL
                     balancing_values$balancing_stage_res <- NULL
                     
                     ## Remove general output message
                     balancing_values$output  <- NULL
                     
                     ## Remove balancing result outputs
                     output$AUC <- NULL
                     output$love_plot  <- NULL
                     output$balance_table <- NULL
                     output$observation_table <- NULL
                     
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
                       
                       if (approach() == "iptw" | approach() == "nbp"){
                         
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
                         
                         if(approach() == "psm" | approach() == "iptw"){
                           # Get common support graph
                           balancing_values$common_support_plot <- evaluate_propensity_stage(balancing_values$estimation_stage_res, evaluation_method = "support", missing_method = missingness())
                           output$common_support <- renderUI(p(
                             h4("Common Support Graph:"),
                             renderPlot(balancing_values$common_support_plot),
                             descriptions$common_support_graph
                           ))
                           #
                           ## Get love plot
                           balancing_values$love_plot <- cobalt::love.plot(balancing_values$balancing_stage_res)
                           output$love_plot <- renderPlot(balancing_values$love_plot)
                           ## Get balance table
                           balance_table <- as.data.frame(cobalt::bal.tab(balancing_values$balancing_stage_res)[[which(grepl("^Balance",names(cobalt::bal.tab(balancing_values$balancing_stage_res))))]])
                           ## Remove empty columns from balance table
                           balance_table <- balance_table[,colSums(is.na(balance_table))<nrow(balance_table)]
                           ## Round numbers in balance table to 4 decimals
                           names_temp <- row.names(balance_table)
                           balance_table <- data.frame(lapply(balance_table,function(x) if(is.numeric(x)) round(x, 3) else x))
                           row.names(balance_table) <- names_temp
                           ## Output balance table
                           balancing_values$balance_table <- balance_table
                           output$balance_table <- DT::renderDataTable({DT::datatable(balance_table, rownames = TRUE, options = list(scrollX = TRUE))})
                           
                           ## Get observation table
                           observation_table <- as.data.frame(cobalt::bal.tab(balancing_values$balancing_stage_res)[["Observations"]])
                           ## Round numbers in observation table to 4 decimals
                           names_temp <- row.names(observation_table)
                           observation_table <- data.frame(lapply(observation_table,function(x) if(is.numeric(x)) round(x, 3) else x))
                           row.names(observation_table) <- names_temp
                           ## Output observation table
                           balancing_values$observation_table <- observation_table
                           output$observation_table <- DT::renderDataTable({DT::datatable(observation_table, rownames = TRUE, options = list(scrollX = TRUE))})
                           
                         }
                         
                         
                         if(approach() == "nbp"){
                           
                           ## Get observation table, balancing table and love plot
                           balancing_values$NBP_balancing_output <- get_NBP_balancing_output(
                             estimation_model_object = balancing_values$estimation_stage_res,
                             balanced_data = balancing_values$balancing_stage_res,
                             treatment_variable = treatment_variable(),
                             matching_variables = matching_variables(),
                             missing_method = missingness())
                           
                           ## Get love plot
                           balancing_values$love_plot <- balancing_values$NBP_balancing_output$love_plot
                           output$love_plot <- renderPlot(balancing_values$love_plot)
                           ## Get balance table
                           balancing_values$balance_table <- as.data.frame(balancing_values$NBP_balancing_output$balance_table)
                           output$balance_table  <- DT::renderDataTable({DT::datatable(as.data.frame(balancing_values$NBP_balancing_output$balance_table), rownames = TRUE, options = list(scrollX = TRUE))})
                           ## Get observation table
                           balancing_values$observation_table <- as.data.frame(balancing_values$NBP_balancing_output$observation_table)
                           output$observation_table <- DT::renderDataTable({DT::datatable(as.data.frame(balancing_values$NBP_balancing_output$observation_table), rownames = TRUE, options = list(scrollX = TRUE))})
                           
                         }
                         
                         
                         ## Add tabs to display output
                         balancing_values$output <- renderUI(
                           tabsetPanel(id = "well_panel",
                                       ## Don't include common support graph as output if appraoch is NBP
                                       if(approach() != "nbp"){
                                         tabPanel(title = "Common Support Graph",
                                                  value = NS(id, 'common_support_graph_tab'),
                                                  br(),
                                                  withSpinner(uiOutput(session$ns("common_support"))))
                                       },
                                       tabPanel(title = "Observation Table",
                                                value = NS(id, 'observation_table_tab'),
                                                br(),
                                                withSpinner(DT::dataTableOutput(session$ns("observation_table"))),
                                                p("ESS = effective sample size")),
                                       tabPanel(title = "Love Plot",
                                                value = NS(id, 'love_plot_tab'),
                                                br(),
                                                withSpinner(plotOutput(session$ns("love_plot"))),
                                                if(missingness() == "mi"){
                                                  p("Dots indicate standardised mean differences in matching variables before and after adjusting.
                                                  Bars represent the range across imputations.")
                                                },
                                                if(missingness() == "complete"){
                                                  p("Dots indicate standardised mean differences in matching variables before and after adjusting.")
                                                }
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
                         balancing_values$matching_method_rerun_message <- p("Note: Changing this parameter will require balancing to be rerun along with all subsequent steps.")
                         balancing_values$matching_ratio_rerun_message <- p("Note: Changing this parameter will require balancing to be rerun along with all subsequent steps.")
                       })
                     }
                   }
                 })
                 
                 ## Reset if balancing inputs have changed ----
                 
                 # Remove balancing output and force rerun if previous steps have changed since previous run
                 observeEvent(c(approach(), missingness(), balancing_model(), raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates(), survey_weight_var(), cluster_var(), stratification_var()), {
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
                 
                 ## Pass output to UI ----
                 
                 output$matching_method_description <- renderUI(balancing_values$matching_method_description)
                 output$matching_method_rerun_message <- renderUI(balancing_values$matching_method_rerun_message)
                 output$matching_method_missing_message <- renderUI(balancing_values$matching_method_missing_message)
                 output$matching_ratio_description <- renderUI(balancing_values$matching_ratio_description)
                 output$matching_ratio_rerun_message <- renderUI(balancing_values$matching_ratio_rerun_message)
                 output$matching_ratio_missing_message <- renderUI(balancing_values$matching_ratio_missing_message)
                 output$descriptive_stats <- renderUI(balancing_values$descriptive_stats)
                 output$balancing_output <- renderUI(balancing_values$output)
                 
                 ## Return balancing output to server ----
                 
                 ## Return choices to server to pass to other tool pages
                 Balancing_output <- reactiveValues(estimation_stage_res = NULL,
                                                    balancing_stage_res = NULL,
                                                    ratio_radio = NULL,
                                                    method_radio = NULL,
                                                    common_support_plot = NULL,
                                                    observation_table = NULL,
                                                    love_plot = NULL,
                                                    balance_table = NULL
                 )
                 
                 observe({
                   Balancing_output$estimation_stage_res <-  balancing_values$estimation_stage_res
                   Balancing_output$balancing_stage_res <- balancing_values$balancing_stage_res
                   Balancing_output$ratio_radio <- balancing_values$ratio
                   Balancing_output$method_radio <- input$method_radio
                   Balancing_output$common_support_plot <- balancing_values$common_support_plot
                   Balancing_output$observation_table <- balancing_values$observation_table
                   Balancing_output$love_plot <- balancing_values$love_plot
                   Balancing_output$balance_table <- balancing_values$balance_table
                 })
                 
                 return(Balancing_output)
               })
}
