# home module ----

home_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "",
           value = NS(id, "tab"),
           HTML("<center>"),
           div(style="display: flex; align: center; width: '1000px'; margin:auto",
               div(style="width: 160px; text-align: center;", p("GET STARTED", style="border-bottom: solid 5px red;")),
               div(style="width: 160px; text-align: center;", p("DATA UPLOAD")),
               div(style="width: 160px; text-align: center;", p("APPROACH")),
               div(style="width: 160px; text-align: center;", p("BALANCING MOD")),
               div(style="width: 160px; text-align: center;", p("BALANCING")),
               div(style="width: 160px; text-align: center;", p("OUTCOME"))
           ),
           br(),br(),
           h2("How to use this tool"),
           br(),br(),
           ## Add 
           div(style = "display: flex;",
               div(style = "width: 16%; text-align: justify;",
                   class = "text_blocks",
               h4("1. Data Upload:", style = "text-align: left;"),
               p("Upload your own data or use our sample data. More infromation on data requirements and validation will be available on the
                 data upload page.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 16%; text-align: justify;",
                   class = "text_blocks",
                   h4("2. Counterfactual Approach:", style = "text-align: left;"),
                   p("Select which counterfactual approach you would like to take based on you data and research question.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 16%; text-align: justify;",
                   class = "text_blocks",
                   h4("3. Balancing Model:", style = "text-align: left;"),
                   p("Choose a balancing model and method of dealing with missing data to calculate propensity scores in your sample.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 16%; text-align: justify;",
                   class = "text_blocks",
                   h4("4. Balancing:", style = "text-align: left;"),
                   p("Use propensity scores to balance your sample across treatment groups.")),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 16%; text-align: justify;",
                   class = "text_blocks",
                   h4("5. Outcome model:", style = "text-align: left;"),
                   p("Run your outcome model to estimate the effect of the treatment on the outcome."))
           ),
           br(),br(),br(),
           div(actionButton(ns("start_btn"),label="Get Started!", class = "default_button")),
           br(),br(),br(),
           fluidRow(
             column(12,
                    div(style = "display: flex; text-align: justify;", 
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
                               actionLink(ns("tutorial_link_home_1"), "tutorial"), ".")),
                        div(class = "text_blocks",
                            style = "width: 49%; margin-left: 2%",
                            h3("Our App"),
                            h5("With the DigiCAT app, you can upload your own data and leverage propensity score methods to
                  conduct counterfactual analyses, gaining insights into the causal effects of specific 
                  interventions or treatments. The primary objective of the DigiCAT app is to provide researchers, 
                  regardless of their statistical background, with a user-friendly platform that removes barriers 
                  and enables them to utilize these methods effectively. Please visit our ", actionLink(ns("tutorial_link_home_2"), "tutorial"),
                               " for more info on using DigiCAT."))
                    )
             ),
             column(12,
                    br(),
                    br(),
                    div(class = "text_blocks",
                        style = "text-align: justify;", 
                        h4("Counterfactual Analysis Methods:"),
                        h5(a(href="https://www.stat.cmu.edu/~ryantibs/journalclub/rosenbaum_1983.pdf", 
                             "The Central Role of the Propensity Score in Observational Studies for Causal Effects", target="_blank")),
                        h4("Counterfactual Analysis Studies:"),
                        h5(a(href="https://psyarxiv.com/dsbec/", 
                             "Is reading for pleasure in adolescence good for mental health? A counterfactual and within-person 
                            analysis in a large longitudinal study", target="_blank"))))
           ))
  
  
  
}

home_server <- function(id, parent) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 
                 ## When "Get Started!" selected on home page check if user has agreed to T&Cs, if so, proceed, if not, ask again 
                 observeEvent(input$start_btn,{
                   
                   ## If user has already agreed to T&Cs, proceed to upload page
                   if (isTruthy(input$Btn_agree) | isTruthy(input$Btn_agree_TCs)){
                     updateTabsetPanel(session = parent, inputId = "methods-tabs", selected = "data_upload-tab")
                   } else{ ## If they have not yet agreed, ask (again)
                     
                     ## Pop up agreement
                     showModal(modalDialog(
                       HTML("<center>"),
                       h4("Before you get started:"),
                       br(),
                       tags$div("Have you read and agree to the terms of the", actionLink(NS(id,"TCs_link"), "DigiCAT Customer Agreement"), "?"),
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
                   removeModal() ## remove modal
                 })
                 
                 ## If tutorial link clicked, switch to tutorial page
                 observeEvent(c(input$tutorial_link_home_1, input$tutorial_link_home_2), {
                   updateTabsetPanel(session = parent, inputId = "main_tabs", selected = 'tutorial')
                   removeModal() ## remove modal
                 }, ignoreInit = TRUE)
                 
                 
               })
}
