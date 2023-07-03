# balancing model module ----

balancing_model_ui <- function(id) {
  ns <- NS(id)
  
  require(shinycssloaders)
  
  ## Tab for balancing model
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/balancing_model.png" width="1000"></center>'),
           
           br(), br(),
           
           ## balancing model choices
           div(style = "display: flex;",
               div(style = "width: 23.5%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "balancing_model_radio"), label = h4("Choose a Balancing Model:"),
                                choices = list(
                                  #"Gradient Boosting Machine (GBM)" = "gbm",
                                  #"Random Forest" = "rforest"),
                                "Probit Regression" = "glm"),
                                selected = character(0))
               ),
               div(style = "width: 23.5%; margin-left: 2%;",
                   class = "text_blocks",
                   radioButtons(NS(id, "balancing_model_missingness_radio"), label = h4("Model Missingess:"),
                                choices = list(
                                  #"Full Information Maximum Likelihood (FIML)" = "fiml", 
                                  "Multiple Imputation" = "mi",
                                  #"Weighting" = "weighting",
                                  "Complete Case" = "complete"),
                                selected = character(0))
               ),
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   ## Description of selected balancing model
                   uiOutput(ns("balancing_model_description")),
                   br(),
                   ## Description of selected model missingness
                   uiOutput(ns("balancing_missingness_description")),
                   br(),
                   p("For more information, visit our ", actionLink(ns("balancing_model_tab_tutorial_link"), "tutorial"), ".")
               )),
           
           br(), br(),
           
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   ## Parameters of selected balancing model and missingness
                   uiOutput(ns("balancing_model_parameters")),
                   br(),
                   uiOutput(ns("balancing_missingness_parameters"))),
               
               div(style = "width: 49%; margin-left: 2%; overflow-y: scroll;",
                   class = "text_blocks",
                   ## Output of selected balancing model
                   withSpinner(uiOutput(ns("balancing_model_output"))))
           ),
               
           
           br(), br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_balancing_model_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'run_balancing_model_btn'), 'Run', class = "default_button"),
               actionButton(NS(id, 'next_balancing_model_btn'), 'Next', class = "default_button"))
           
  )
}

balancing_model_server <- function(id, parent, raw_data, treatment_variable, matching_variables) {
  
  #v <- reactiveValues(treatment_variable = treatment_variable)
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Disable 'Next' button initially
                 shinyjs::disable("next_balancing_model_btn")
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_balancing_model_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "CF_approach-tab")
                 })

                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_balancing_model_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing-tab")
                 })

                 ## Create reactive value for approach description
                 methodsBalanceModel <- reactiveValues(
                   model_description = p(h4("Balancing Model:"),
                                   p("In order to balance our dataset, we must first calculate the probability each individual has of being in our treatment
                                   group, based on their observed characteristics, i.e., their propensity score. On this page, you will select a balancing model
                                   to train on your dataset to predict the likelihood of an individual being treated. Different modelling approaches can be used 
                                   for this step and the selection of specific model depends on the characteristics of the data and the research question at hand. 
                                   Please visit our tutorial if you would like more guidance on choosing a balancing model.")
                                   ),
                   missingness_description = p(h4("Model Missingness:"),
                                         p("Missing data can introduce bias, affecting the validity of analyses and the reliability of conclusions drawn from 
                                           the data. In order to minimise the impact of missingness in our data, DigiCAT offers several approaches to handle
                                           missing values.")),
                   model_parameters = p(h4("Balancing Model Parameters:"),
                                  p("Once you have selected a balancing model, we will show you the parameters in use here.")),
                   missingness_parameters = p(h4("Balancing Model Parameters:"),
                                        p("Once you have selected a method of dealing with missingness, we will show you the parameters in use here.")),
                   output = p(h4("Output:"),
                              p("Once you have selected your balancing model and method of dealing with missingness, press
                                'Run' to get results.")),
                   result = NULL)
                 
                 ## If tutorial link clicked, go to tutorial page
                 observeEvent(input$balancing_model_tab_tutorial_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "tutorial")
                 })

                 ## Update model description and parameters based on choice of appraoch
                 observeEvent(input$balancing_model_radio,{

                   if(input$balancing_model_radio == "glm"){
                     methodsBalanceModel$model_description <- p(h4("Balancing Model: Probit Regression"),
                                                      br(),
                                                      p("Probit regression is a method of modelling binary outcomes (such as whether or not someone is 
                                                        in the treatment group). This assumes that the probability of the outcome follows a cumulative 
                                                        distribution function of a normal distribution. This 'probit' function links the linear 
                                                        combination of independent variables into the probability space between 0 and 1."))
                     
                     methodsBalanceModel$model_parameters <- p(h4("Balancing Model Parameters: Probit Regression"),
                                                          br(),
                                                          p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_radio == "gbm"){
                     methodsBalanceModel$model_description <- p(h4("Balancing Model: Gradient Boosting Machine (GBM)"),
                                                          br(),
                                                          p("You've choosen GBM, this is why is may/may not be a good choice."))
                     
                     methodsBalanceModel$model_parameters <- p(h4("Balancing Model Parameters: Gradient Boosting Machine (GBM)"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_radio == "rforest"){
                     methodsBalanceModel$model_description <- p(h4("Balancing Model: Random Forest"),
                                                          br(),
                                                          p("You've choosen random forest, this is why is may/may not be a good choice."))
                     
                     methodsBalanceModel$model_parameters <- p(h4("Balancing Model Parameters: Random Forest"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                 })
                 
                                  ## Update missingness description and parameters based on choice of appraoch
                 observeEvent(input$balancing_model_missingness_radio,{

                   if(input$balancing_model_missingness_radio == "fiml"){
                     methodsBalanceModel$missingness_description <- p(h4("Model Missingness: Full Information Maximum Likelihood (FIML)"),
                                                      br(),
                                                      p("You've choosen FIML, this is why is may/may not be a good choice."))
                     
                     methodsBalanceModel$missingness_parameters <- p(h4("Model Missingness Parameters: Full Information Maximum Likelihood (FIML)"),
                                                          br(),
                                                          p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_missingness_radio == "mi"){
                     methodsBalanceModel$missingness_description <- p(h4("Model Missingness: Multiple Imputation"),
                                                          br(),
                                                          p("The fundamental idea behind multiple imputation is to create several (M) completed datasets by 
                                                          predicting what the missing values would have been if we could observe them. These datasets are then analysed 
                                                          using software for completely observed datasets, and the results of each of these datasets are combined, or 
                                                          ‘pooled’, together. The variability across datasets allows the standard errors to take account of the uncertainty 
                                                          due to the fact that some of the data are predicted rather than observed."),
                                                          br(),
                                                          p("However,don’t forget that this statistical method relies on assumptions, which can be difficult to test. 
                                                          If these assumptions are violated, your inferences may be invalid. Please see our tutorial pages for a detailed 
                                                            discussion of which missingness handling method may be best for you. Note: if you set m = 1, you will 
                                                            be performing single imputation. The default in DigiCAT is m = 5. "))
                     
                     methodsBalanceModel$missingness_parameters <- p(h4("Model Missingness Parameters: Multiple Imputation"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_missingness_radio == "weighting"){
                     methodsBalanceModel$missingness_description <- p(h4("Model Missingness: Weighting"),
                                                          br(),
                                                          p("You've choosen weighting, this is why is may/may not be a good choice."))
                     
                     methodsBalanceModel$missingness_parameters <- p(h4("Model Missingness Parameters: Weighting"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                   
                   if(input$balancing_model_missingness_radio == "complete"){
                     methodsBalanceModel$missingness_description <- p(h4("Model Missingness: Complete Cases"),
                                                                br(),
                                                                p("Complete case analysis (CCA) (also sometimes known as ‘listwise deletion’), will only analyse the 
                                                                  completely observed cases. This analysis will allow valid inferences of your data if 
                                                                  the missing data are missing completely at random (MCAR), because the observed values 
                                                                  will be a random sample of the complete dataset. If the data are missing at random (MAR) 
                                                                  or missing not at random (MNAR), the inferences may be invalid. Please see our tutorial 
                                                                  pages for a detailed discussion of which missingness handling method may be best for you."))
                     
                     methodsBalanceModel$missingness_parameters <- p(h4("Model Missingness Parameters: Complete Cases"),
                                                         br(),
                                                         p("Information on parameters in use."))
                   }
                 })
                 
                 
                 ## Run balancing model 
                 source("source/func/get_score.R")
                 source("source/func/performance_plot.R")
                 
                 observeEvent(input$run_balancing_model_btn, {
                   
                   ## Disable 'Run' button
                   shinyjs::disable("run_balancing_model_btn")
                   
                   ## Remove general output message
                   methodsBalanceModel$output <- NULL
                   
                   methodsBalanceModel$result <-
                     get_score(
                       psmodel = input$balancing_model_radio,
                       .data = raw_data(),
                       t_var = treatment_variable(),
                       m_vars = matching_variables(),
                       missing = input$balancing_model_missingness_radio
                     )
                   
                   methodsBalanceModel$output <- p(
                     h4("The Receiver Operating Characteristic (ROC) curve:"),
                     renderPlot(performance_plot(psmodel_obj = methodsBalanceModel$result,
                                                 t_var =treatment_variable(),
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
                                                   )
                   ## Enable 'Run' and 'Next' buttons 
                   shinyjs::enable("run_balancing_model_btn")
                   shinyjs::enable("next_balancing_model_btn")
                   
                   })
                 
                 ## Display information for choosing counterfactual approach, relevent parameters and model output
                 output$balancing_model_description <- renderUI(methodsBalanceModel$model_description)
                 output$balancing_missingness_description <- renderUI(methodsBalanceModel$missingness_description)
                 output$balancing_model_parameters <- renderUI(methodsBalanceModel$model_parameters)
                 output$balancing_missingness_parameters <- renderUI(methodsBalanceModel$missingness_parameters)
                 output$balancing_model_output <- renderUI(methodsBalanceModel$output)
                 
                 ## Return output of balancing model
                 
                 return(reactive({methodsBalanceModel$result}))
               })
}
               
               
               
