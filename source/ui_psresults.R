psres_page <- 
  div(id="psresults",
      sidebarLayout(
        sidebarPanel(
          
          tags$h2(style="text-align: center; ","Propensity Model Results & Diagnostics"),
          br(),
          
          pickerInput(
            inputId = "psresult_metric", 
            label = "What would you like to see?", 
            choices = c("Covariate Balance Plot","Covariate Balance Table","AUC", "...", "...")
          ),
          
          br(),
          div(style="text-align:right", 
            actionBttn("prevPSR_btn", "Prev", color="success"),
            actionBttn("nextPSR_btn", "Next", color="success")
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