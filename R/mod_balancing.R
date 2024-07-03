# balancing module ----
#'@import cobalt
#'@import shinycssloaders
#'@import CBPS

balancing_ui <- function(id, i18n) {
  ns <- NS(id)
  
  ## Tab for choosing matching options and running balancing
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
               div(style="width: 12%; text-align: center;", h5(i18n$t("BALANCING"), style="color: white; border-bottom: solid 2px white;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("OUTCOME"), style="color: #607cc4;"))
           ),

           ## Navigation ----
           
           div(align="center",
               actionButton(NS(id, 'prev_balancing_btn'), i18n$t("Button Previous"), class = "default_button"),
               actionButton(NS(id, 'run_balancing_btn'), i18n$t("Outcome Button Run"), class = "default_button"),
               actionButton(NS(id, 'next_balancing_btn'), i18n$t("Button Next"), class = "default_button")),
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
                                               tabPanel(title = i18n$t("Balancing Tab summary"),
                                                        value = NS(id,'descriptive_stats'),
                                                        br(),
                                                        uiOutput(ns("descriptive_stats"))),
                                               tabPanel(title = i18n$t("Balancing Tab output"),
                                                        value = NS(id,"output"),
                                                        br(),
                                                        withSpinner(uiOutput(ns("balancing_output"))))
                                   ),
                                   
                                   br(),
                                   
                                   ## Downloadable Output ----
                                   div(align="center",
                                       uiOutput(ns("download_options"))
                                   )
               )),
              
           )
  )
}

balancing_server <- function(id, parent, raw_data, categorical_variables, outcome_variable, treatment_variable, matching_variables, covariates, survey_weight_var, cluster_var, stratification_var, validation_log, approach, missingness, balancing_model, approach_display, missingness_display, balancing_model_display, analysis_tab, i18n, selected_language) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Navigation bar ----
                 output$prog_choiceDU <- renderUI({p(paste0(i18n$t("Tabs DU outcome"), outcome_variable()),br(),paste0(i18n$t("Tabs DU treatment"), treatment_variable()), style="width: 200%; margin-left: -50%")})
                 output$prog_choiceCF <- renderUI({p(paste0(i18n$t("Tabs Approach approach"), approach_display()),br(),paste0(i18n$t("Tabs Approach missingness"), missingness_display()),br(),paste0(i18n$t("Tabs Approach model"), balancing_model_display()), style="width: 200%; margin-left: -50%")})
                 
                 ## Define Reactives ----
                 
                 ## Create reactive value for approach description
                 balancing_values <- reactiveValues(
                   method_choice = NULL,
                   ratio_choice = NULL,
                   ratio = NULL,
                   matching_method_description = NULL,
                   matching_ratio_description = NULL,
                   estimation_stage_res = NULL,
                   balancing_stage_res = NULL,
                   matching_var_balance = NULL,
                   descriptive_stats = NULL,
                   run_balancing = FALSE,
                   output = p(h4(i18n$t("Balancing Tab output")),
                              p(i18n$t("Balancing Tab output description"))))
                 
                 ## Page Setup ----
                 
                 # Get descriptive statistics of unbalanced matching variables in each treatment group
                 observeEvent(c(validation_log()), {
                   
                   ## Ensure data, matching variables and treatment variable have been selected
                   if (!(is.null(raw_data()) | is.null(treatment_variable()) | is.null(matching_variables()))){

                       if (any(treatment_variable() %in% categorical_variables())){ ## If treatment is categorical

                         ## Get mean and SD of each matching variable per treatment group
                         df <- isolate(raw_data())
                         df <- aggregate(. ~ get(treatment_variable()), df[,c(treatment_variable(), matching_variables())], function(x) summary = paste0(round(mean(as.numeric(as.character(x))), 2), " (", round(sd(as.numeric(as.character(x))), 2), ")"))
                         df <- as.data.frame(t(df))
                         names(df) <-paste0(treatment_variable(), " ", df[1,], " Mean (Standard Deviation)")
                         formatted_unbalanced_summary <- df[-c(1,2),]

                       } else{ ## If treatment is continuous, categories into groups

                         df <- isolate(raw_data())
                         ## Slit continuous treatment into quartiles
                         df$treatment_quartiles <- ntile(df[,treatment_variable()], 4)
                         ## Get mean and SD of each matching variable per treatment group
                         df <- aggregate(. ~ treatment_quartiles, df[,c("treatment_quartiles", matching_variables())], function(x) summary = paste0(round(mean(as.numeric(as.character(x))), 2), " (", round(sd(as.numeric(as.character(x))), 2), ")"))
                         df <- as.data.frame(t(df))
                         names(df) <- paste0(treatment_variable(), " Q", df[1,], " Mean (Standard Deviation)")
                         formatted_unbalanced_summary <- df[-1,]
                       }

                     ## Render unmatched balancing table
                     balancing_values$descriptive_stats <- DT::renderDataTable({DT::datatable(formatted_unbalanced_summary, rownames = TRUE, options = list(scrollX = TRUE))})
                   }
                 })
                 
                 ## Render balancing analysis options based on approach chosen
                 observeEvent(approach(),{
                   if (approach() == "psm"){
                     output$balancing_options <- renderUI({
                       div(
                         div(class = "text_blocks",
                             style = "width: 100%;",
                             radioButtons(session$ns("method_radio"), label = h4(i18n$t("Balancing Choose Method")),
                                          choices = c(
                                            i18n$t("Balancing Optimal"),
                                            i18n$t("Balancing NN")),
                                          selected = character(0)),
                             uiOutput(session$ns("matching_method_missing_message"), style = "color: red;"), ## If no matching mehtod selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_method_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             ## Description of selected balancing method and ratio
                             br(),
                             p( balancing_values$counter),
                             p(
                               h5(i18n$t("Balancing Matching method")),
                               p(i18n$t("Balancing Matching method description PSM")),
                               a(id = "link",i18n$t("Balancing Matching method tutorial link PSM"), href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score",
                                 target="_blank")
                             ),
                             br(),
                             uiOutput(session$ns("matching_method_description")),
                         ),
                         br(),
                         div(style = "width: 100%",
                             class = "text_blocks",
                             h4(i18n$t("Balancing Choose Ratio")),
                             radioButtons(session$ns("ratio_radio"), label = "",
                                          choices = c(
                                            "1:1" = "one_to_one",
                                            "1:k" = "one_to_k"),
                                          selected = character(0)),
                             uiOutput(session$ns("ratio_slider_output")), ## Only show ration slider if 1:K is selected
                             uiOutput(session$ns("matching_ratio_missing_message"), style = "color: red;"), ## If no matching ratio selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_ratio_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             br(),
                             p(
                               h5(i18n$t("Balancing Matching ratio")),
                               p(i18n$t("Balancing Matching ratio description"))
                             ),
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
                             radioButtons(session$ns("method_radio"), label = h4(i18n$t("Balancing Choose Method")),
                                          choices = c(
                                            i18n$t("Balancing Optimal")),
                                          selected = i18n$t("Balancing Optimal")),
                             uiOutput(session$ns("matching_method_missing_message"), style = "color: red;"), ## If no matching mehtod selected when "Run" pressed, give warning
                             uiOutput(session$ns("matching_method_rerun_message"), style = "color: grey;"), ## Give warning that rerun required upon re-selection
                             ## Description of selected balancing method and ratio
                             br(),
                             p(h5(paste0(i18n$t("Balancing Matching method"), " ", i18n$t("Balancing Optimal"))), 
                               p(i18n$t("Balancing Optimal ordinal description")),
                               
                             )
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
                             p(
                               h5(paste0(i18n$t("Balancing Matching ratio"), ": 1:1")),
                               p(i18n$t("Balancing Ratio ordinal description"))
                             )
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
                               p(
                                 h5(i18n$t("Approach IPTW")),
                                 p(i18n$t("Balancing Matching method description IPTW")),
                                 a(id = "link",i18n$t("Balancing Matching method tutorial link IPTW"), href = "https://uoe-digicat.github.io/04_cfmethod.html#iptw",
                                   target="_blank")
                               )
                             )
                         )
                       )
                     })
                   }
                   
                   if (approach() == "cbps"){
                     
                     output$balancing_options <- renderUI({
                       div(
                         div(style = "width: 100%;",
                             class = "text_blocks",
                             div(
                               p(
                                 h5(i18n$t("Approach CBPS")),
                                 p(i18n$t("Balancing Matching method description CBPS"))
                               )
                             )
                         )
                       )
                     })
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
                   
                   if (input$method_radio == i18n$t("Balancing NN")){
                     balancing_values$matching_method_description <- p(
                       h5(i18n$t("Balancing NN")),
                       p(i18n$t("Balancing NN description")),
                       a(id = "link", i18n$t("Balancing NN tutorial link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")
                     )
                   }
                   
                   if (input$method_radio == i18n$t("Balancing Optimal")){
                     balancing_values$matching_method_description <- p(
                       h5(i18n$t("Balancing Optimal")),
                       p(i18n$t("Balancing Optimal description")),
                       a(id = "link", i18n$t("Balancing Optimal tutorial link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")
                     )
                   }
                   
                   ## Remove missing parameter message if present
                   balancing_values$matching_method_missing_message <- NULL
                   
                   ## Check if balancing has already been run, if so, output informative message, hide balancing results and force rerun
                   if (!is.null(balancing_values$balancing_stage_res)){
                     ## Replace balancing output with explanation of why output has been deleted
                     balancing_values$output <- p(h4(i18n$t("Balancing Tab output")),
                                                  p(i18n$t("Balancing Warning output rerun")))
                     
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                     
                     ## Remove Ddwnload button
                     output$download_options <- NULL
                   }
                 })
                 
                 ## Update matching ratio description based on choice of ratio
                 observeEvent(input$ratio_radio,{
                   
                   if (input$ratio_radio == "one_to_one"){
                     
                     balancing_values$matching_ratio_description <- p(
                       h5("1:1:"),
                       p(i18n$t("Balancing 1to1 ratio description")),
                       a(id =  "link", i18n$t("Balancing 1to1 ratio tutorial link"), href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")
                     )
                     
                     ## Update matching ratio to "1"
                     balancing_values$ratio <- 1
                     
                     ## Remove ratio slider if present
                     output$ratio_slider_output <- NULL
                     
                     
                     ## Remove missing parameter message if present
                     balancing_values$matching_ratio_missing_message <- NULL
                   }
                   
                   if (input$ratio_radio == "one_to_k"){
                     balancing_values$matching_ratio_description <- p(
                       h5("1:K:"),
                       p(i18n$t("Balancing 1tok ratio description")),
                     )
                     
                     ## Add slider input for user to pick K in 1:k
                     output$ratio_slider_output <- renderUI(
                       sliderInput(session$ns("ratio_slider"), label = h4("Choose 'k':"),
                                   min = 2, max = 10, value = 2)
                     )
                     
                     ## Initiate "K" as 2
                     balancing_values$ratio <- 2
                     
                     ## Remove missing parameter message if present
                     balancing_values$matching_ratio_missing_message <- NULL
                   }
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
                     balancing_values$output <- p(h4(i18n$t("Balancing Tab output")),
                                                  p(i18n$t("Balancing Warning output rerun")))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                     
                     ## Remove Ddwnload button
                     output$download_options <- NULL
                   }
                 })
                 
                 ## Run Balancing ----
                 
                 ## Ensure balancing can be run 
                 observeEvent(input$run_balancing_btn, {
                   
                   ## If matching approach selected but no balancing method, give error message
                   if(approach() == "psm" & is.null(input$method_radio)){
                     balancing_values$matching_method_missing_message <- p(i18n$t("Balancing Warning no method"), style = "color:red")
                   }
                   ## If matching approach selected but no balancing ratio, give error message
                   ## Slider counter used as slider input cannot be initiated with NULL - counter value of greater than 0 indicates slider input has been selected
                   if(approach() == "psm" & is.null(balancing_values$ratio)){
                     balancing_values$matching_ratio_missing_message <- p(i18n$t("Balancing Warning no ratio"), style = "color:red")
                   }
                   ## If all required input present, carry out balancing
                   if (approach() == "iptw" | approach() == "cbps" | ((approach() == "psm" & !is.null(input$method_radio) & !is.null(balancing_values$ratio))) | ((approach() == "nbp"))  ) {
                     
                     ## Disable 'Run' button
                     shinyjs::disable("run_balancing_btn")
                     
                     ## Show output tab - temporarily switch to "descriptive_stats" so unput updates if already on output tabs
                     updateTabsetPanel(
                       session = parent, inputId = NS(id,"results_panel"), selected = NS(id, "descriptive_stats")
                     )
                     updateTabsetPanel(
                       session = parent, inputId = NS(id,"results_panel"), selected = NS(id, "output")
                     )
                     
                     ## Update reactive value in order to run balancing
                     balancing_values$run_balancing <- TRUE
                     
                   }
                 })
                 
                 ## Run balancing
                 observeEvent(input$results_panel, {
                   
                   ## Only run if output tab is selected and balancing can be run 
                   if(balancing_values$run_balancing){

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
                         model_type = balancing_model(),
                         treatment_variable = treatment_variable(),
                         matching_variable = matching_variables(),
                         PS_estimation_object = balancing_values$estimation_stage_res,
                         missing_method = missingness(),
                         ratio = balancing_values$ratio,
                         method = balancing_values$method_choice)
                       
                     }
                     
                     if (approach() == "iptw" | approach() == "nbp" | approach() == "cbps"){
                       
                       balancing_values$balancing_stage_res <- balance_data(
                         counterfactual_method = approach(),
                         model_type = balancing_model(),
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
                     balancing_values$output <- p(p(paste0(conditionMessage(cond)) , style = "color:red"))
                   })
                   
                   
                   ## Output balance plots and tables and enable downloadables if no error in balancing
                   if (all(!grepl("Error:", error_check))){
                     try({
                       
                       if(approach() == "psm" | approach() == "iptw"){
                         
                         if((balancing_model() == "glm" & missingness() == "complete") | (balancing_model() == "glm" & missingness() == "mi")){
                           # Get common support graph - only works with GLM currently (MI and CC)
                           balancing_values$common_support_plot <- evaluate_propensity_stage(balancing_values$estimation_stage_res, evaluation_method = "support", missing_method = missingness())
                           output$common_support <- renderUI(p(
                             h4("Common Support Graph:"),
                             renderPlot(balancing_values$common_support_plot),
                             p(
                               h5(i18n$t("Balancing Common support graph")),
                               p(i18n$t("Balancing Common support graph description"))
                             )
                           ))
                         }
                       }
                       
                       if(approach() == "psm" | approach() == "iptw" | approach() == "cbps"){
                         
                         ## Get love plot
                         balancing_values$love_plot <- cobalt::love.plot(balancing_values$balancing_stage_res)
                         output$love_plot <- renderPlot(balancing_values$love_plot)
                         
                         ## Get balance table
                         balance_table <- as.data.frame(cobalt::bal.tab(balancing_values$balancing_stage_res,  un = TRUE)[[which(grepl("^Balance",names(cobalt::bal.tab(balancing_values$balancing_stage_res, un = TRUE))))]])
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
                                     ## Don't include common support graph if propensity model other than GLM used or CBPS approach had been taken
                                     if(balancing_model() == "glm" & !approach() == "cbps"){
                                       tabPanel(title = i18n$t("Balancing Common support graph"),
                                                value = NS(id, 'common_support_graph_tab'),
                                                br(),
                                                withSpinner(uiOutput(session$ns("common_support"))))
                                     },
                                     tabPanel(title = i18n$t("Balancing Observation table"),
                                              value = NS(id, 'observation_table_tab'),
                                              br(),
                                              withSpinner(DT::dataTableOutput(session$ns("observation_table"))),
                                              p(paste("ESS = ", i18n$t("Balancing ESS")))),
                                     tabPanel(title =i18n$t("Balancing Love plot"),
                                              value = NS(id, 'love_plot_tab'),
                                              br(),
                                              withSpinner(plotOutput(session$ns("love_plot"))),
                                              if(missingness() == "mi"){
                                                p(i18n$t("Balancing Balance plot dots MI"))
                                              },
                                              if(missingness() == "complete"){
                                                p(i18n$t("Balancing Balance plot dots"))
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
                       balancing_values$matching_method_rerun_message <- p(i18n$t("Balancing Warning change rerun"))
                       balancing_values$matching_ratio_rerun_message <- p(i18n$t("Balancing Warning change rerun"))
                       
                       ### Add download buttons ----
                       output$download_options <- renderUI({
                         div(
                           downloadButton(session$ns("download_balanced_rdata"), i18n$t("Balancing Button download balanced RData"), class = "default_button"),
                         downloadButton(session$ns("download_balanced_csv"), i18n$t("Balancing Button download balanced CSV"), class = "default_button"))
                       })
                     })
                   }
                   ## Update reactive value so balancing only runs when "Run" clicked
                   balancing_values$run_balancing <- FALSE
                 }
                 }, ignoreInit = T)
                 
                 ## Reset if balancing inputs have changed ----
                 
                 # Remove balancing output and force rerun if previous steps have changed since previous run
                 observeEvent(c(approach(), missingness(), balancing_model(), raw_data(), treatment_variable(), outcome_variable(), matching_variables(), categorical_variables(), covariates(), survey_weight_var(), cluster_var(), stratification_var()), {
                   ## First check if balancing has been run yet, if yes, print informative message and force rerun
                   if (!is.null(balancing_values$balancing_stage_res)){
                     
                     ## Switch back to matching vars summary
                     showTab(session = parent, inputId = NS(id,"results_panel"), target = NS(id, "descriptive_stats"), select = TRUE)
                     
                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_values$output <- p(h4(i18n$t("Balancing Tab output")),
                                                  i18n$t("Balancing Warning output rerun"))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                     
                     ## Remove Ddwnload button
                     output$download_options <- NULL
                   }
                 })
                 
                 ## Save radio button input ----
                 ## Translation does not allow abbreviation of choices from radiobutton, therefore these must be assigned manually
                 observeEvent(input$method_radio,{
                   if (input$method_radio == i18n$t("Balancing Optimal")){
                     balancing_values$method_choice <- "optimal"
                   }
                   if (input$method_radio == i18n$t("Balancing NN")){
                     balancing_values$method_choice <- "nearest"
                   }
                 })
                 
                 observeEvent(input$ratio_radio,{
                   if (input$ratio_radio == "one_to_one"){
                     balancing_values$ratio_choice <- "one_to_one"
                   }
                   if (input$ratio_radio == "one_to_k"){
                     balancing_values$ratio_choice <- "one_to_k"
                   }
                 })
                 
                 ## Download balanced data when "download balanced data" clicked
                 downloadable_balanced <- reactiveValues()
                 observe({
                   if(!is.null(balancing_values$balancing_stage_res))
                     isolate(
                       downloadable_balanced <<- balancing_values$balancing_stage_res
                     )
                 })
                 
                 output$download_balanced_rdata <- 
                   
                   downloadHandler(
                     filename = "DigiCAT_balanced.RData",
                     content =
                       function(file) {
                         shinyjs::disable("download_balanced")
                         on.exit(shinyjs::enable("download_balanced"))
                         save(downloadable_balanced, file = file)
                       }
                   )
                 
                 output$download_balanced_csv <- downloadHandler(
                   filename = "DigiCAT_balanced.zip",
                   content = 
                     function(file){
                     #go to a temp dir to avoid permission issues
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     files <- NULL;
                     
                     if (class(downloadable_balanced) == "matchit" | class(downloadable_balanced) == "weightit"){
                       
                       data_fileName <- "matched_data.csv"
                       write.table(match.data(downloadable_balanced),
                                   data_fileName,sep = ';', row.names = F, col.names = T)
                       
                       weights_fileName <- "weights.csv"
                       write.table(as.data.frame(downloadable_balanced$weights),
                                   weights_fileName,sep = ';', row.names = T, col.names = T)
                       
                       files <- c(data_fileName, weights_fileName)
                       
                     } else{  ## If MI, iterate through imputations
                       
                       for (i in 1:length(downloadable_balanced)){
                         
                         data_fileName <- paste0("matched_data_", i, ".csv")
                         write.table(complete(downloadable_balanced, i),
                                     data_fileName,sep = ';', row.names = F, col.names = T)
                         
                         ## Weights included in balanced dataframe
                         files <- c(data_fileName, files)
                       }
                     }
                     #create the zip file
                     zip(file,files)
                   }
                 )
                 
                 
                 
                 
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
                 Balancing_output <- reactiveValues()
                 
                 observe({
                   Balancing_output$estimation_stage_res <-  balancing_values$estimation_stage_res
                   Balancing_output$balancing_stage_res <- balancing_values$balancing_stage_res
                   Balancing_output$ratio_radio_display <- balancing_values$ratio_radio
                   Balancing_output$ratio <- balancing_values$ratio
                   Balancing_output$ratio_radio <- input$ratio_radio
                   Balancing_output$method_radio <- input$method_choice
                   Balancing_output$method_radio_display <- input$method_radio
                   Balancing_output$common_support_plot <- balancing_values$common_support_plot
                   Balancing_output$observation_table <- balancing_values$observation_table
                   Balancing_output$love_plot <- balancing_values$love_plot
                   Balancing_output$balance_table <- balancing_values$balance_table
                 })
                 
                 return(Balancing_output)
               })
}
