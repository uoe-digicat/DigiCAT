results_page <- 
  div(id="results",
      HTML('<center><img src="progress_bar/get_results.png" width="800"></center>'),
      br(),br(),br(),
      sidebarLayout(
        sidebarPanel(id="sidebarPanel",
          tags$h2(style="text-align: center; ","Outcome Model"),
          br(),
          HTML("<center>"),
          actionButton("resshow_btn", "Go!", class = "default_button"),
          br(),br(),
          actionButton("resprint_btn", "Print results", class = "default_button"),
          actionButton("resemail_btn", "Email results", class = "default_button"),
          br(),br(),
          actionButton("resPrev_btn", "Prev", class = "default_button"),
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