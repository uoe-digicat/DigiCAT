cfmethod_page <- 
  div(id="cfmethod",
      
      # side bar page, sidebar for choosing method, main panel includes further options plus guidance. 
      sidebarLayout(
        sidebarPanel(
          tags$h2(style="text-align: center; ","Counterfactual Method"),
          br(),
          
          awesomeCheckboxGroup(
            inputId="counterfactual", 
            label='Select your counterfactual method',
            choices=c('1:1 PSM','k:1 PSM', 'CEM', 'IPTW','Non-bipartite optimal matching')
          ),
          
          br(),
          div(style="text-align:right", 
            actionBttn("prevBtn_5", "Prev", color="success"),
            actionBttn("nextBtn_5", "Next", color="success")
          )
        ),
        mainPanel(
          h3("explanations, more options & guidance"),
          p("..."),br(),p("..."),br(),p("...")
        ) 
      )
      
  )