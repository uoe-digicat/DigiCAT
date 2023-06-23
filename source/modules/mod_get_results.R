# outcome model module ----

get_results_ui <- function(id) {
  ns <- NS(id)
  
  ## Tab for choosing counterfactual analysis approach
  tabPanel(title = "",
           value = NS(id, 'get_results_tab'),
           ## Add navbar image
           HTML('<center><img src="progress_bar/new/get_results.png" width="1000"></center>'),
           
           br(), br(), br(),
           
           div(style = "display: flex;",
               div(style = "width: 49%;",
                   class = "text_blocks",
                   p("Download the R script used to carry out your counterfactual analysis. 
                     This will allow you to document your analysis. "),
                   br(),
                   div(align="center",
                       actionButton(NS(id, 'get_code_btn'), 'Download R Script', class = "default_button")
                       ),
                   br()),
               div(style = "width: 49%; margin-left: 2%",
                   class = "text_blocks",
                   p("Download a full report of you counterfactual analysis. This report will detail the methods choosen
                     for the analysis and the results obtained."),
                   br(),
                   div(align="center",
                       actionButton(NS(id, 'get_report_btn'), 'Download Report', class = "default_button")
                       ),
                   br())),
           
           br(),br(),
           
           div(align="center",
               actionButton(NS(id, 'prev_get_results_btn'), 'Prev', class = "default_button"))
           
  )
}

get_results_server <- function(id) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 ## When "Prev is selected", show and move to new tab
                 observeEvent(input$prev_get_results_btn, {
                   updateTabsetPanel(session = session, inputId = 'tabs', selected = NS(id, 'outcome_model_tab'))
                 })
                 
                 
               })
}
