home_page <- 
  div(id="home",
      # text summary
      HTML("<center>"),
      HTML('<center><img src="progress_bar/get_started.png" width="800"></center>'),
      br(),br(),br(),
      h2("How to use this tool"),
      br(),
      plot(1:100),
      h5('Step 1: Attach dataset and specify your outcome, ‘treatment’ and other variables'),
      br(),
      h5('Step 2: Select your propensity model(s)'),
      br(),
      h5('Step 3: Select your counterfactual method(s) and click Run to see the results'),
      br(),
      div(class = "buttonagency", actionBttn("start_btn",label="Get Started!", size="sm", color="default", style = "simple")),
      br(),br(),br(),
      (""),
      br(),br(),
      fluidRow(
        
        column(12,
               div(style = "display: flex;", 
                   div(class = "text_blocks",
                     style = "width: 48%;",
                       h3("Counterfactual Analysis"),
                       h5("The aim of counterfactual analysis is to estimate the causal effects of interventions or 
            treatments, by comparing what actually happened (observed outcomes) with what would have happened 
            if a different action had been taken (counterfactual outcomes). In observational settings, where 
            random allocation into different treatments is not possible, researchers often employ methods 
            involving 'propensity scores' (the estimated probability of receiving the treatment/intervention, 
            based on a set of observed covariates). These propensity scores can then be used in an analysis 
            to balance the characteristics of treatment vs non-treated groups, reducing bias and enabling a 
            more accurate estimation of the causal effect of receiving the treatment. For more info, see our ", 
                          actionLink("tutorial_link", "tutorial"), ".")),
                   div(class = "text_blocks",
                     style = "width: 48%; margin-left: 4%",
                       h3("Our App"),
                       h5("With the DigiCAT app, you can upload your own data and leverage propensity score methods to
            conduct counterfactual analyses, gaining insights into the causal effects of specific 
            interventions or treatments. The primary objective of the DigiCAT app is to provide researchers, 
            regardless of their statistical background, with a user-friendly platform that removes barriers 
            and enables them to utilize these methods effectively. Please visit our ", actionLink("tutorial_link", "tutorial"),
                          " for more info on using DigiCAT."))
               )
        ),
      column(12,
             br(),
             br(),
             div(class = "text_blocks",
                 h4("Counterfactual Analysis Methods:"),
                 h5(a(href="https://www.stat.cmu.edu/~ryantibs/journalclub/rosenbaum_1983.pdf", 
                      "The Central Role of the Propensity Score in Observational Studies for Causal Effects", target="_blank")),
                 h4("Counterfactual Analysis Studies:"),
                 h5(a(href="https://psyarxiv.com/dsbec/", 
                           "Is reading for pleasure in adolescence good for mental health? A counterfactual and within-person 
                      analysis in a large longitudinal study", target="_blank"))))
  ))
