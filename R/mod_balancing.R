# balancing module ----
#'@import cobalt
#'@import shinycssloaders

balancing_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/balancing.png" width="1000px"></center>'),
           div(align="center",
               actionButton(NS(id, 'prev_balancing_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_balancing_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_balancing_btn'), 'Next', class = "default_button")),
           br(),
           
           ## matching method
           uiOutput(ns("balancing_options")), ## Load balancing options and descriptions based on CF approach chosen
           br(), br(),
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   ## Parameters of selected balancing model
                   uiOutput(ns("balancing_parameters_method")),
                   br(),br(),
                   uiOutput(ns("balancing_parameters_ratio"))),
               
               div(style = "width: 49%; margin-left: 2%; border-color: transparent",
                   class = "text_blocks",
                   
                   ## Show initial output message
                   withSpinner(uiOutput(ns("balancing_output"))),
               )
           )
           
  )
}

balancing_server <- function(id, parent, outcome_variable, treatment_variable, matching_variables, balancing_model_results, approach, balancing_model, descriptions) {
  
  moduleServer(id,
               function(input, output, session) {

                 # output$prog_choiceDU <- renderUI({
                 #   p(paste0("Outcome: ", outcome_variable()),br(),paste0("Treatment: ", treatment_variable()))
                 # })
                 # output$prog_choiceCF <- renderUI({
                 #   paste0(approach())
                 # })
                 # output$prog_choiceBM <- renderUI({
                 #   paste0(balancing_model$balancing_model, ", ", balancing_model$missingness)
                 # })
                 
                 ## Create reactive value for approach description
                 balancing_values <- reactiveValues(
                   description_method = NULL,
                   description_ratio = NULL,
                   parameters_method = NULL,
                   parameters_ratio = NULL,
                   output = NULL)
                 
                 ## Render balancing analysis options based on approach chosen
                 observeEvent(approach$cfapproach_radio(),{
                   
                   if (approach$cfapproach_radio() == "matching"){
                     
                     output$balancing_options <- renderUI({
                       div(style = "display: flex;",
                         div(style = "width: 49%;",
                             class = "text_blocks",
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
                         div(style = "width: 49%; margin-left: 2%;",
                             class = "text_blocks",
                               radioButtons(session$ns("ratio_radio"), label = h4("Choose a Matching Method:"),
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
                     balancing_values$description_method = p(h4("Matching Method:"),
                     p("In order to balance covariates between treatment groups, propensity score matching involves matching 
                     individuals based on their propensity scores, which represent their likelihood of being treated based on 
                     observed characteristics. The goal is to create a pseudo-randomized comparison between the treatment and 
                       control groups by matching individuals who have similar or close propensity scores."))
                     balancing_values$description_ratio = p(h4("Matching Ratio:"),
                     p("Matching ratios in propensity score matching refer to the number of control/untreated individuals that are matched 
                     to each treated individual. In DigiCAT, matching ratios can be specified to control the trade-off between 
                                           achieving better balance between treatment groups and maintaining an adequate sample size."))
                     balancing_values$parameters_method = p(h4("Matching Parameters:"),
                                                          p("Once you have selected a matching method and ratio, we will show you the parameters in use here."))
                     balancing_values$parameters_ratio = NULL
                     balancing_values$ratio =  NULL
                     balancing_values$output = p(h4("Output:"),
                     p("Once you have selected your matching method and ratio, press
                       'Run' to get output."))
                   }

                   if (approach$cfapproach_radio() == "weighting"){
                     
                     output$balancing_options <- renderUI({
                       div(style = "display: flex;",
                           div(style = "width: 49%;",
                               class = "text_blocks",
                               div(
                                 "Balancing options/extra info for weighting go here"
                               )
                           ),
                           div(style = "width: 49%; margin-left: 2%;",
                               class = "text_blocks",
                               
                               ## Description of selected balancing method and ratio
                               uiOutput(session$ns("balancing_description_method")),
                               br(),br(),
                               uiOutput(session$ns("balancing_description_ratio")),
                               br(),br(),
                               p("For more information, visit our ", actionLink(session$ns("balancing_tab_tutorial_link"), "tutorial"), ".")
                           )
                       )
                     })
                     
                     ## Create reactive value for approach description
                     balancing_values$description_method = p(h4("Description:"),
                                                           p("Info about balancing with weighting"))
                     balancing_values$description_ratio = NULL ##no ratio for weighting
                     balancing_values$parameters_method = p(h4("Matching Parameters:"),
                                             p("Once you have selected a matching method and ratio, we will show you the parameters in use here."))
                     balancing_values$parameters_ratio = NULL
                     balancing_values$output = p(h4("Output:"),
                                  p("Once you have selected your matching method, press
                                'Run' to get output."))
                   }
                 })

                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("next_balancing_btn")
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_balancing_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing_model-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_balancing_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "outcome_model-tab")
                 })
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$balancing_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "tutorial")
                 })
                 
                 
                 ## Update matching method description and parameters based on choice of appraoch
                 observeEvent(input$method_radio,{
                   
                   if(input$method_radio == "nearest"){
                     balancing_values$description_method <- p(h4("Matching Method: Nearest Neighbour (NN)"),
                                                            br(),
                                                            p("Nearest neighbour matching is used in counterfactual analysis as a method for pairing treated 
                                                            and control cases with similar propensity scores. In DigiCat, nearest neighbour greedy matching 
                                                            is implemented whereby the most similar treated and control cases are matched first. Then, from 
                                                            those left the remaining most similar treated and control cases are paired and so on and so forth 
                                                            until all viable matches have been made. See the nearest neighbour matching tutorial for more 
                                                            details."))
                     
                     balancing_values$parameters_method <- p(h4("Matching Method Parameters: Nearest Neighbour (NN)"),
                                                           br(),
                                                           p("Information on parameters in use."))
                   }
                   
                   if(input$method_radio == "optimal"){
                     balancing_values$description_method <- p(h4("Matching Method: Optimal"),
                                                            br(),
                                                            p("You've choosen optimal matching, this is why is may/may not be a good choice."))
                     
                     balancing_values$parameters_method <- p(h4("Matching Method Parameters: Optimal"),
                                                           br(),
                                                           p("Information on parameters in use."))
                   }
                   
                   ## Remove missing parameter message if present
                   balancing_values$method_missing_message <- NULL

                   ## Check if balancing has already been run, if so, output informative message, hide balancing results and force rerun
                   if (!is.null(balancing_values$balancing_res)){
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
                 
                 ## Update matching ratio description and parameters based on choice of ratio
                 observeEvent(input$ratio_radio,{
                   
                   if (input$ratio_radio == "one_to_one"){
                   
                       balancing_values$description_ratio <- p(h4("Matching Ratio: 1:1"),
                                                             br(),
                                                             p("1:1 is the most commonly used matching ratio, although this ratio can be changed, for example, 
                                                         if there are many more members belonging to the control group than members belonging to the 
                                                         treatment group. In 1:1 matching, we match one unit belonging to the intervention group with one 
                                                         unit belonging to the control group that has the closest propensity score. The default matching 
                                                         strategy in DigiCAT is a nearest neighbour (‘greedy’) matching algorithm, with a 1:1 matching 
                                                         ratio. As our default is matching without replacement, the selected units will be taken out of 
                                                         the donor pool once matched. If you think this may not be best for you, see our tutorial pages 
                                                         for more detailed guidance on choosing an appropriate matching ratio. "))
                       
                       balancing_values$parameters_ratio <- p(h4("Matching Ratio Parameters: 1:1"),
                                                            br(),
                                                            p("Information on parameters in use."))

                      ## Update matching ratio to "1"
                       balancing_values$ratio <- 1
                       
                       ## Remove ratio slider if present
                        output$ratio_slider_output <- NULL
                     }
                     
                     if(input$ratio_radio == "one_to_k"){
                       balancing_values$description_ratio <- p(h4("Matching ratio: 1:K"),
                                                             br(),
                                                             p("You've choosen a 1:K matching ratio, this is why is may/may not be a good choice."))
                       
                       balancing_values$parameters_ratio <- p(h4("Matching Ratio Parameters: 1:K"),
                                                            br(),
                                                            p("Information on parameters in use."))
                       
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
                   if (!is.null(balancing_values$balancing_res)){
                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_values$output <- p(h4("Output:"),
                                                  p(
                                                    strong("It looks like balancing will have to be rerun, this is because some of the required inputs have been changed since the 
                         previous run.")," Once you have selected your matching method and ratio, press
                         'Run' to get output."))
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                   }
                 })

                 ## Run balancing
                 observeEvent(input$run_balancing_btn, {
                   
                   ## If matching approach selected but no balancing method, give error message
                   if(approach$cfapproach_radio() == "matching" & is.null(input$method_radio)){
                     balancing_values$method_missing_message <- p("Please select a matching method before proceeding", style = "color:red")
                   }
                   
                   ## If matching approach selected but no balancing ratio, give error message
                   ## Slider counter used as slider input cannot be initiated with NULL - counter value of greater than 0 indicates slider input has been selected
                   if(approach$cfapproach_radio() == "matching" & is.null(balancing_values$ratio)){
                     balancing_values$ratio_missing_message <- p("Please select a matching ratio before proceeding", style = "color:red")
                   }
                   ## If all required input present, carry out balancing
                   if (approach$cfapproach_radio() == "weighting" | (approach$cfapproach_radio() == "matching" & !is.null(input$method_radio) & !is.null(balancing_values$ratio))) {
                     
                     ## Disable 'Run' button
                     shinyjs::disable("run_balancing_btn")
  
                     ## Remove general output message
                     balancing_values$output  <- NULL
                     
                     ## Remove balancing result outputs
                     output$love_plot  <- NULL
                     output$balance_table <- NULL
                     output$observations_table <- NULL

                     ## Save potential error to check for running of code dependent on balancing model
                     error_check <- NA
                     error_check <- tryCatch({
                       ## Balance dataset
                       if (approach$cfapproach_radio() == "matching"){
                       balancing_values$balancing_res <- balancing(
                         cf_method = approach$cfapproach_radio(),
                         t_var = treatment_variable(),
                         m_vars = matching_variables(),
                         psmodel_obj = balancing_model_results(),
                         ratio = balancing_values$ratio,
                         method = input$method_radio,
                       )}
                       
                       if (approach$cfapproach_radio() == "weighting"){
                         balancing_values$balancing_res <- DigiCAT::balancing(
                           cf_method = approach$cfapproach_radio(),
                           t_var = treatment_variable(),
                           m_vars = matching_variables(),
                           psmodel_obj = balancing_model_results()
                         )}
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
                         output$love_plot <- renderPlot(cobalt::love.plot(balancing_values$balancing_res))
                         output$balance_table <- DT::renderDataTable({DT::datatable(as.data.frame(cobalt::bal.tab(balancing_values$balancing_res)[[which(grepl("^Balance",names(cobalt::bal.tab(balancing_values$balancing_res))))]]), rownames = TRUE, options = list(scrollX = TRUE))})
                         output$observations_table <- DT::renderDataTable({DT::datatable(as.data.frame(cobalt::bal.tab(balancing_values$balancing_res)[["Observations"]]), rownames = TRUE, options = list(scrollX = TRUE))})
                         
                         ## Add tabs to display output
                         balancing_values$output <- renderUI(
                           tabsetPanel(id = NS(id, "balancing_output_plots"),
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
                         
                         ## Add message noting that parameter reselction will require rerun
                         balancing_values$method_rerun_message <- p("Note: Changing this parameter will require balancing to be rerun along with all subsequent steps.")
                         balancing_values$ratio_rerun_message <- p("Note: Changing this parameter will require balancing to be rerun along with all subsequent steps.")
                       })
                     }
                     }
                 })
                 
                 
                 ## Remove balancing output and force rerun if previous steps have changed since previous run
                 observeEvent(balancing_model_results(), {
                   ## First check if balancing has been run yet, if yes, print informative message and force rerun
                   if (!is.null(balancing_values$balancing_res)){
                     
                     ## Replace balancing model output with explanation of why output has been deleted
                     balancing_values$output <- p(h4("Output:"),
                                                  strong("It looks like balancing will have to be rerun, this is because some of the required inputs have been changed since the 
                       previous run.")," Once you have selected your matching method and ratio, press
                       'Run' to get output.")
                     
                     ## Disable "Next" button to force a rerun before proceeding to next step
                     shinyjs::disable("next_balancing_btn")
                     
                   }
                 })

                 ## Display information for choosing counterfactual approach, relevent parameters and model output
                 output$balancing_description_method <- renderUI(balancing_values$description_method)
                 output$balancing_method_rerun_message <- renderUI(balancing_values$method_rerun_message)
                 output$balancing_method_missing_message <- renderUI(balancing_values$method_missing_message)
                 output$balancing_description_ratio <- renderUI(balancing_values$description_ratio)
                 output$balancing_ratio_rerun_message <- renderUI(balancing_values$ratio_rerun_message)
                 output$balancing_ratio_missing_message <- renderUI(balancing_values$ratio_missing_message)
                 output$balancing_parameters_method <- renderUI(balancing_values$parameters_method)
                 output$balancing_parameters_ratio <- renderUI(balancing_values$parameters_ratio)
                 output$balancing_output <- renderUI(balancing_values$output)
                 
                 ## Return balancing output
                 return(reactive({balancing_values$balancing_res}))
               })
}
