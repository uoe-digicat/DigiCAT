# counterfactual approach module ----

CF_approach_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/CF_approach.png" width="1000"></center>'),
           
           br(), br(),
           
           ## CF approach choices
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   
                   radioButtons(NS(id, "CF_radio"), label = h4("Choose a Counterfactual Approach:"),
                                br(),
                                choices = list("Propensity Matching" = "matching", " Inverse probability of treatment weighting (IPTW)" = "weighting"))
               ),
               div(style = "width: 49%; margin-left: 2%;",
                   class = "text_blocks",
                   
                   ## Description of selected counterfactual approach
                   uiOutput(ns("approach_description"))
                   
               )),
           
           br(), br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_CF_btn'), 'Prev', class = "default_button"),
               actionButton(NS(id, 'next_CF_btn'), 'Next', class = "default_button"))
           
  )
}

CF_approach_server <- function(id, parent) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_CF_btn, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                 })
                 
                 ## When "Next is selected", show and move to new tab
                 observeEvent(input$next_CF_btn, {
                   updateTabsetPanel(session = parent, inputId = 'methods-tabs', selected = "balancing_model-tab")
                 })
                 
                 ## Create reactive value for approach description
                 methodsApproach <- reactiveValues(description = div(
                   h4("Description:"),
                   p("The aim of counterfactual analysis is to estimate the causal effects of 'exposures' or 'treatments' by comparing what 
                     actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual 
                     outcomes). Because we can never directly observe counterfactual outcomes, we compare groups who differ in their treatment. 
                     However, in observational settings where random allocation into different treatments is not possible, researchers may employ methods that such as 'matching' or 'weighting' of participants to ensure that the different treatment groups 
                     being compared are 'balanced' with respect to other characteristics."),
                   br(),
                   p("Please click on the approach you would like to take. We will give you more information about each approach as you select them.")))
                 
                 ## Update guide information based on choice of appraoch
                 observeEvent(input$CF_radio,{
                   
                   if(input$CF_radio == "matching"){
                     methodsApproach$description <- p(h4("Propensity Matching:"), 
                                                      br(),
                                                      p("You've chosen propensity matching. This approach involves creating balanced comparison
                                                        groups by matching treated individuals with similar untreated individuals based on their propensity 
                                                        scores (probability of someone having a specific treatment based on observed characteristics). 
                                                        This aims to ensure that the groups are comparable in terms of potential confounding variables. 
                                                        The treatment effect is then estimated by comparing outcomes between the matched groups."))
                   }
                   if(input$CF_radio == "weighting"){
                     methodsApproach$description <- p(h4("Inverse probability of treatment weighting (IPTW):"), 
                                                      br(),
                                                      p("You've choosen IPTW, this is why is may/may not be a good choice."))
                   }
                 })
                 
                 ## Display information for choosing counterfactual approach
                 output$approach_description <- renderUI(methodsApproach$description)
                 
                 return(reactive({input$CF_radio}))
                 
               })
}
