psmodel_page <- 
  div(id="psmodel",
      
      sidebarLayout(
        
        sidebarPanel(
          tags$h2(style="text-align: center; ","Propensity Model"),
          br(),
          
          awesomeCheckboxGroup(
            inputId="psm", 
            label='Select your propensity model',
            choices=c('None','Regression','CART', 'Random forest','Gradient boosted model with post-calibration')
          ),
          
          br(),
          div(style="text-align:right", 
            actionBttn("prevBtn_3", "Prev", color="success"),
            actionBttn("nextBtn_3", "Next", color="success")
          )
        ),
        mainPanel(
          p("options & guidance")
        ) 
      )
      
  )