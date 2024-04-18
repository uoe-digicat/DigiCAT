#' @import tidyverse
# home module ----

home_ui <- function(id, i18n) {
  ns <- NS(id)
  ## Tab displaying tool summary and start button
  tabPanel(title = "",
           value = NS(id, "tab"),
           br(),
           
           ## Navigation bar ----
           div(style="display: flex; align: center; width: '100%'; margin:auto",
               div(style="width: 12%; text-align: center;", h5(i18n$t("GET STARTED"))),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("DATA UPLOAD"), style="color: #607cc4;")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("APPROACH"), style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("BALANCING"), style="color: #607cc4")),
               div(style="width: 12%; text-align: center; height: 1px; background-color: #607cc4; margin:18px;"),
               div(style="width: 12%; text-align: center;", h5(i18n$t("OUTCOME"), style="color: #607cc4;"))
           ),
           br(),br(),
           
           ## Tool overview ----
           h2(i18n$t("Home How to use"), style = "text-align: center;"),
           br(),br(),
           ## Add 
           div(style = "display: flex;",
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4(i18n$t("Home Data Upload"), style = "text-align: left;"),
                   p(i18n$t("Home Data Upload description"))),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4(i18n$t("Home Counterfactual Approach"), style = "text-align: left;"),
                   p(i18n$t("Home Counterfactual Approach description"))),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4(i18n$t("Home Balancing"), style = "text-align: left;"),
                   p(i18n$t("Home Balancing description"))),
               div(style = "width: 5%;position: relative; top: 50%; transform: translateY(40%);",
                   HTML('<center><img src="graphics/chevron_home_page.png" width="25"></center>')),
               div(style = "width: 21%; text-align: left;",
                   class = "text_blocks",
                   h4(i18n$t("Home Outcome Model"), style = "text-align: left;"),
                   p(i18n$t("Home Outcome Model description")))
           ),
           br(),br(),
           
           ## Select Language ----
           
           div(style = "text-align: center; position: relative; top: 50%; transform: translateX(35%); z-index:1002;",
               selectInput(NS(id,"selected_language"),
                                              i18n$t("Language"),
                                              choices = i18n$get_languages()[2],
                                              #choices = "en",
                                              selected = i18n$get_key_translation()
           )),
           ## Navigation ----
           div(style="text-align: center; position: relative;",
               uiOutput(ns("warning")),
               actionButton(ns("start_btn"),label=i18n$t("Home Get Started"), class = "default_button")),
           br(),br(),
           

           
           ## Tool description ----
           fluidRow(
             column(12,
                    div(style = "display: flex; text-align: left;", 
                        div(class = "text_blocks",
                            style = "width: 49%;",
                            h3(i18n$t("Home Counterfactual Analysis")),
                            h5(p(i18n$t("Home CA description")),
                               a(id = "link",i18n$t("Home CA tutorial link"), href = "https://uoe-digicat.github.io/#overview-of-tutorials"))),
                        div(class = "text_blocks",
                            style = "width: 49%; margin-left: 2%",
                            h3("DigiCAT"),
                            h5(p(i18n$t("Home DigiCAT description")),
                               a(id = "link",i18n$t("Home CA tutorial link"), href = "https://uoe-digicat.github.io/#overview-of-tutorials")))
                    )
             ),
             
             column(12,
                    br(),
                    br(),
                    div(class = "text_blocks",
                        style = "text-align: left;", 
                        h3(i18n$t("Home Our Approach")),
                        h5(p(i18n$t("Home Our Approach description")),
                           a(id = "link",i18n$t("Home CA git link"), href = "https://github.com/uoe-digicat/DigiCAT"))))
           ))
  
}

home_server <- function(id, parent, enableLocal, i18n) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## Create reactive value for elements on home tab
                 home_values <- reactiveValues(
                   select_language = NULL
                 )
                 
                 ## If data upload is enabled, give warning about current developemnt
                 if(enableLocal==TRUE){
                   output$warning = renderUI({
                     h3("Caution: This tool is under development; outputs may be incomplete or inaccurate. Non-bipartite matching (NBP) coming soon.", style = "color:red")
                   })
                 }
                 
                 ## State and terms of use ----
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
                       h4(i18n$t("TC popup heading")),
                       br(),
                       tags$div(
                         p(i18n$t("TC popup description")),
                         actionLink(NS(id,"TCs_link"), i18n$t("TC link"))),
                       footer=tagList(
                         div(style = "text-align:center",
                             actionButton(NS(id,"Btn_dont_agree"), i18n$t("TC disagree"), style="color: white; background: #4f78dc"),
                             actionButton(NS(id,"Btn_agree"), i18n$t("TC agree"), style="color: white; background: green"))),
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
                 
                 ## Disable select language ----
                 observeEvent(input$Btn_agree, {
                   disable(id = "selected_language")
                 })
                 
                 ## Return home output to server ----
                 
                 home_output <- reactiveValues(selected_language = NULL
                 )
                 
                 observe({
                   home_output$selected_language <- input$selected_language
                 })
                 
                 return(home_output)

               })
}
