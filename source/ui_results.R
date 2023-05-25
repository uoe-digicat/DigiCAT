results_page <- 
  div(id="results",
      HTML('<center><img src="progress_bar/get_results.png" width="800"></center>'),
      br(),br(),br(),
      sidebarLayout(
        sidebarPanel(id="sidebarPanel",
          tags$h2(style="text-align: center; ","Outcome Model"),
          br(),
          HTML("<center>"),
          div(class = "buttonagency",
          actionBttn(inputId='resshow_btn', label= 'Go!', color="default", style = "simple", size="sm"),
          br(),br(),
          actionBttn(inputId='resprint_btn', label= 'Print results', color="default", style = "simple", size="sm"),
          actionBttn(inputId='resemail_btn', label= 'Email results', color="default", style = "simple", size="sm"),
          br(),br(),
          actionBttn(inputId='resPrev_btn', label= 'Prev', color="default", style = "simple", size="sm")),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("residual checks", plotOutput("outcome_resid")),
            tabPanel("plot", plotOutput("outcome_plot")),
            tabPanel("table", tableOutput("outcome_table"))
          )
        )
      )
  )