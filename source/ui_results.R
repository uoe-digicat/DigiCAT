results_page <- 
  div(id="results",
      sidebarLayout(
        sidebarPanel(
          tags$h2(style="text-align: center; ","Outcome Model"),
          br(),
          HTML("<center>"),
          actionBttn(inputId='resshow_btn', label= 'Go!', color = "success"),
          br(),br(),
          actionBttn(inputId='resprint_btn', label= 'Print results', color = "default"),
          actionBttn(inputId='resemail_btn', label= 'Email results', color = "default")
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