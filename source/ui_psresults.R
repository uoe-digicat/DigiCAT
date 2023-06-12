psres_page <- 
  div(id="psresults",
      HTML('<center><img src="progress_bar/model_results.png" width="800"></center>'),
      br(),br(),br(),
      sidebarLayout(
        sidebarPanel(id="sidebarPanel",
          
          tags$h2(style="text-align: center; ","Propensity Model Results & Diagnostics"),
          br(),
          
          pickerInput(
            inputId = "psresult_metric", 
            label = "What would you like to see?", 
            choices = c("Covariate Balance Plot","Covariate Balance Table","AUC", "...", "...")
          ),
          
          br(),
          div(class = "buttonagency",
            style="text-align:right", 
            actionButton("prevPSR_btn", "Prev", class = "default_button"),
            actionButton("nextPSR_btn", "Next", class = "default_button")
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("baltab",verbatimTextOutput("PSmodel_baltab")),
            tabPanel("balplot",plotOutput("PSmodel_balplot")),
            tabPanel("loveplot",plotOutput("PSmodel_loveplot"))
          )
        ) 
      )
      
  )