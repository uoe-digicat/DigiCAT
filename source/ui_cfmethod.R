method_page <- 
  div(id="method",
      HTML('<center><img src="progress_bar/model_config.png" width="800"></center>'),
      br(), br(), br(),
      div(class = "text_blocks",
          tags$h2(style="text-align: center; ","CHOICES"),
      br(), br(), br(),
      fluidRow(
          column(title="Missingness", width=4,
                 awesomeRadio(
                   inputId="missingmethod", 
                   label='How would you like to address missingness in your data?',
                   choices=c('Method default','Weights','Multiple imputation','Full information maximum likelihood')
                 )
          ),
          column(title="Method", width=4,
              awesomeRadio(
                inputId="counterfactual", 
                label='Select your counterfactual method',
                choices=c('1:1 PSM','K:1 PSM', 'CEM', 'IPTW','Non-bipartite optimal matching')
              ),
              switchInput(
                inputId = "drobust",
                label = "Doubly Robust?", 
                value = TRUE
              )
          ),
          column(title="Propensity Score Model", width=4,
                 awesomeRadio(
                   inputId="psm", 
                   label='Select your propensity model',
                   choices=c('None','Regression','CART', 'Random forest','Gradient boosted model')
                 )
          )
        ),
      fluidRow(
        column(12, align="center",
               div(class = "buttonagency",
                 actionBttn("prevCM_btn", "Prev", color="default", style = "simple", size="sm"),
               actionBttn("nextCM_btn", "BUILD!", color="default", style = "simple", size="sm"))
        )
      ))
      
  )