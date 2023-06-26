# balancing module ----

balancing_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'balancing_tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/balancing.png" width="1000"></center>'),
           
           br(), br(),
           
           ## matching method
           div(style = "display: flex;",
               div(style = "width: 23.5%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "matching_method_radio"), label = h4("Choose a Matching Method:"),
                                choices = c("Nearest Neighbour (NN)" = "NN", 
                                               "Optimal" = "optimal"),
                                selected = character(0))
               ),
               div(style = "width: 23.5%; margin-left: 2%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "matching_ratio_radio"), label = h4("Choose a Ratio Matching:"),
                                choices = list("1:1" = "one_to_one", 
                                               "1:K" = "one_to_K"),
                                selected = character(0))
               ),
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   ## Description of selected balancing model
                   uiOutput(ns("balancing_description_method")),
                   br(),br(),
                   uiOutput(ns("balancing_description_ratio")),
                   br(),br(),
                   p("For more information, visit our ", actionLink(ns("balancing_tab_tutorial_link"), "tutorial"), ".")
                   
                   
               )),
           
           br(), br(),
           
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   ## Parameters of selected balancing model
                   uiOutput(ns("balancing_parameters_method")),
                   br(),br(),
                   uiOutput(ns("balancing_parameters_ratio"))),
               
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   ## Show initial output message
                   uiOutput(ns("balancing_output")),
                   
                   ## Add tabs to display output
                   tabsetPanel(id = NS(id, "balancing_output_plots"),
                     tabPanel(title = "Observation Table",
                              value = NS(id, 'observations_table_tab'),
                              br(),
                              DT::dataTableOutput(ns("observations_table"))),
                     tabPanel(title = "Love Plot", 
                              value = NS(id, 'love_plot_tab'),
                              br(),
                              plotOutput(ns("love_plot"))
                              ),
                     tabPanel(title = "Balance Table", 
                              value = NS(id, 'balance_table_tab'),
                              br(),
                              DT::dataTableOutput(ns("balance_table"))
                              )
                     )
           )
           ),
           
           
           br(), br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_balancing_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_balancing_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_balancing_btn'), 'Next', class = "default_button"))
           
  )
}

balancing_server <- function(id, parent, treatment_variable, matching_variables, balancing_model_results, approach) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_balancing_btn, {
                   updateTabsetPanel(session = session, inputId = 'tabs', selected = NS(id, 'balancing_model_tab'))
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_balancing_btn, {
                   updateTabsetPanel(session = session, inputId = 'tabs', selected = NS(id, 'outcome_model_tab'))
                 })
                 
                 ## Create reactive value for approach description
                 mathingBalance <- reactiveValues(
                   description_method = p(h4("Matching Method:"),
                                          p("Here is some guidance on selecting an appropriate matching method.")),
                   description_ratio = p(h4("Matching Ratio:"),
                                         p("Here is some guidance on selecting an appropriate matching ratio")),
                   parameters_method = p(h4("Matching Parameters:"),
                                         p("Once you have selected a matching method and model, we will show you the parameters in use here.")),
                   parameters_ratio = NULL,
                   balancing_res = NULL,
                   output = p(h4("Output:"),
                              p("Once you have selected your matching method and ratio, press
                                'Run' to get output."))
                   )
                 
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$balancing_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "tutorial")
                 })
                 
                 
                 ## Update matching method description and parameters based on choice of appraoch
                 observeEvent(input$matching_method_radio,{
                   
                   if(input$matching_method_radio == "NN"){
                     mathingBalance$description_method <- p(h4("Matching Method: Nearest Neighbour (NN)"),
                                                          br(),
                                                          p("Nearest neighbour matching is used in counterfactual analysis as a method for pairing treated 
                                                            and control cases with similar propensity scores. In DigiCat, nearest neighbour greedy matching 
                                                            is implemented whereby the most similar treated and control cases are matched first. Then, from 
                                                            those left the remaining most similar treated and control cases are paired and so on and so forth 
                                                            until all viable matches have been made. See the nearest neighbour matching tutorial for more 
                                                            details."))
                     
                     mathingBalance$parameters_method <- p(h4("Matching Method Parameters: Nearest Neighbour (NN)"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                   
                   if(input$matching_method_radio == "optimal"){
                     mathingBalance$description_method <- p(h4("Matching Method: Optimal"),
                                                          br(),
                                                          p("You've choosen optimal matching, this is why is may/may not be a good choice."))
                     
                     mathingBalance$parameters_method <- p(h4("Matching Method Parameters: Optimal"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                   
                 })
                 
                 ## Update matching ratio description and parameters based on choice of appraoch
                 observeEvent(input$matching_ratio_radio,{
                   
                   if(input$matching_ratio_radio == "one_to_one"){
                     mathingBalance$description_ratio <- p(h4("Matching Ratio: 1:1"),
                                                     br(),
                                                     p("You've choosen a 1:1 matching ratio, this is why is may/may not be a good choice."))
                     
                     mathingBalance$parameters_ratio <- p(h4("Matching Ratio Parameters: 1:1"),
                                                    br(),
                                                    p("Information on parameters in use."))
                   }
                   
                   if(input$matching_ratio_radio == "one_to_K"){
                     mathingBalance$description_ratio <- p(h4("Matching ratio: 1:K"),
                                                     br(),
                                                     p("You've choosen a 1:K matching ratio, this is why is may/may not be a good choice."))
                     
                     mathingBalance$parameters_ratio <- p(h4("Matching Ratio Parameters: 1:K"),
                                                    br(),
                                                    p("Information on parameters in use."))
                   }
                   
                 })
                 
                 
                 ## Run balancing 
                 source("source/func/balancing.R")
                 require("cobalt")
                 
                 ## Hide output plots initially
                 hideTab(session = parent, inputId = NS(id, "balancing_output_plots"), target = NS(id, 'observations_table_tab'))
                 hideTab(session = parent, inputId = NS(id, "balancing_output_plots"), target = NS(id, 'love_plot_tab'))
                 hideTab(session = parent, inputId = NS(id, "balancing_output_plots"), target = NS(id, 'balance_table_tab'))
                 
                 observeEvent(input$run_balancing_btn, {
  
                   mathingBalance$balancing_res <- balancing(
                     cf_method = "matching",
                     t_var = treatment_variable,
                     m_vars = matching_variables,
                     psmodel_obj = balancing_model_results
                   )
                   
                   ## Remove general output message
                   mathingBalance$output <- NULL
                   
                   ## Output balance plots and tables
                   output$love_plot <- renderPlot(cobalt::love.plot(mathingBalance$balancing_res))
                   output$balance_table <- DT::renderDataTable({DT::datatable(cobalt::bal.tab(mathingBalance$balancing_res)$Balance.Across.Imputations, rownames = TRUE, options = list(scrollX = TRUE))})
                   output$observations_table <- DT::renderDataTable({DT::datatable(cobalt::bal.tab(mathingBalance$balancing_res)$Observations, rownames = TRUE, options = list(scrollX = TRUE))})
                   
                   ## Show output plots
                   showTab(session = parent, inputId = NS(id, "balancing_output_plots"), target = NS(id, 'observations_table_tab'))
                   showTab(session = parent, inputId = NS(id, "balancing_output_plots"), target = NS(id, 'love_plot_tab'))
                   showTab(session = parent, inputId = NS(id, "balancing_output_plots"), target = NS(id, 'balance_table_tab'))
                 })
                 
                 
                 ## Display information for choosing counterfactual approach, relevent parameters and model output
                 output$balancing_description_method <- renderUI(mathingBalance$description_method)
                 output$balancing_description_ratio <- renderUI(mathingBalance$description_ratio)
                 output$balancing_parameters_method <- renderUI(mathingBalance$parameters_method)
                 output$balancing_parameters_ratio <- renderUI(mathingBalance$parameters_ratio)
                 output$balancing_output <- renderUI(mathingBalance$output)
                 
                 ## Return balancing output
                 reactive(mathingBalance$balancing_res)
               })
}
