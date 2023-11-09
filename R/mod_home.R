# home module ----

home_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab displaying tool summary and start button
  tabPanel(title = "",
           value = NS(id, "tab"),
           br(),
           
           ## Navigation bar ----
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5("GET STARTED")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("DATA UPLOAD", style="color: #607cc4;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("APPROACH", style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("BALANCING", style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5("OUTCOME", style="color: #607cc4;"))
           ),
           br(),br(),
           
           ## Tool overview ----
           h2("How to use this tool", style = "text-align: center;"),
           br(),br(),
           ## Add 
           div(style = "display: flex;",
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4("1. Data Upload:", style = "text-align: left;"),
                   p("Upload your own data or use our sample data. More infromation on data requirements and validation will be available on the
                 data upload page.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4("2. Counterfactual Approach:", style = "text-align: left;"),
                   p("Select which counterfactual approach, method of dealing with missing data and balancing model you would like to use based 
                     on you data and research question.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4("3. Balancing:", style = "text-align: left;"),
                   p("Use propensity scores to balance your sample across treatment groups.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4("4. Outcome model:", style = "text-align: left;"),
                   p("Run your outcome model to estimate the effect of the treatment on the outcome."))
           ),
           br(),br(),br(),
           
           ## Navigation ----
           div(style="text-align: center;",
               actionButton(ns("start_btn"),label="Get Started!", class = "default_button")),
           br(),br(),br(),
           
           ## Tool description ----
           fluidRow(
             column(12,
                    div(style = "display: flex; text-align: left;", 
                        div(class = "text_blocks",
                            style = "width: 49%;",
                            h3("Counterfactual Analysis"),
                            h5("The aim of counterfactual analysis is to estimate the causal effects of interventions or 
                  treatments, by comparing what actually happened (observed outcomes) with what would have happened 
                  if a different action had been taken (counterfactual outcomes). In observational settings, where 
                  random allocation into different treatments is not possible, researchers often employ methods 
                  involving 'propensity scores' (the estimated probability of receiving the treatment/intervention, 
                  based on a set of observed covariates). These propensity scores can then be used in an analysis 
                  to balance the characteristics of treatment vs non-treated groups, reducing bias and enabling a 
                  more accurate estimation of the causal effect of receiving the treatment. For more info, see our ", 
                               a(id = "link","tutorial", href = "https://uoe-digicat.github.io/#overview-of-tutorials"), ".")),
                        div(class = "text_blocks",
                            style = "width: 49%; margin-left: 2%",
                            h3("DigiCAT"),
                            h5("DigiCAT stands for ‘Digital counterfactual analysis tool’. With DigiCAT, you can leverage propensity 
                               score methods in your own data or using our sample dataset to conduct counterfactual analyses and gain 
                               insights into the causal effects of specific interventions or treatments. DigiCAT aims to provide researchers, 
                               regardless of their statistical background, with a user-friendly platform that removes barriers and enables them 
                               to utilize these methods effectively. Please visit our ", a(id = "link", "tutorial", href = "https://uoe-digicat.github.io/#overview-of-tutorials"),
                               " for more info on using DigiCAT."))
                    )
             ),
             
             column(12,
                    br(),
                    br(),
                    div(class = "text_blocks",
                        style = "text-align: left;", 
                        h3("Our Approach:"),
                        h5("DigiCAT was developed with the help of a lived experience expert groupleft. It took a user-centred approach to design and we 
                           continue to welcome feedback from users at:", a(id = "link",href='mailto:uoe_digicat-group@uoe.onmicrosoft.com', "uoe_digicat-group@uoe.onmicrosoft.com"), "It was designed with FAIR principles 
                           in mind and you can find all code for the tool ", a(id = "link",href='https://github.com/uoe-digicat/DigiCAT', "here."))))
           ))
  
}

home_server <- function(id, parent) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Start and terms of use ----
                 ## When "Get Started!" selected on home page check if user has agreed to T&Cs, if so, proceed, if not, ask again 
                 observeEvent(input$start_btn,{
                   
                   ## If user has already agreed to T&Cs, proceed to upload page
                   if (isTruthy(input$Btn_agree) | isTruthy(input$Btn_agree_TCs)){
                     updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                     addClass(selector = "body", class = "sidebar-collapse") ## Collapse NavBar
                   } else{ ## If they have not yet agreed, ask (again)
                     
                     ## Pop up agreement
                     showModal(modalDialog(
                       HTML("<center>"),
                       h4("Before you get started:"),
                       br(),
                       tags$div("Have you read and agree to the terms of the", actionLink(NS(id,"TCs_link"), "DigiCAT User Agreement"), "?"),
                       footer=tagList(
                         div(style = "text-align:center",
                             actionButton(NS(id,"Btn_dont_agree"), "No, I don't agree", style="color: white; background: #4f78dc"),
                             actionButton(NS(id,"Btn_agree"), 'Yes, I agree', style="color: white; background: green"))),
                       HTML("<center>")))
                   }
                 })
                 
                 ## T&Cs agreement: If terms and conditions link clicked, close modal and switch to T&Cs tab
                 observeEvent(input$TCs_link, {
                   updateTabsetPanel(session = parent, inputId = 'main_tabs', selected = "TC")
                   removeModal() ## remove modal
                 })
                 
                 ## T&Cs agreement: If 'No, I don't agree', remove modal and remain in start page
                 observeEvent(input$Btn_dont_agree, {
                   removeModal() ## remove modal
                 })
                 
                 ## T&Cs agreement: If 'Yes, I agree', continue to data upload page
                 observeEvent(input$Btn_agree, {
                   updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                   addClass(selector = "body", class = "sidebar-collapse") ## Collapse NavBar
                   removeModal() ## remove modal
                 })
               })
}
