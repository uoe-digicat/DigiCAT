home_page <- 
  div(id="home",
      # text summary
      HTML("<center>"),
      h2("How to use this tool"),
      p('Step 1: Attach dataset and specify your outcome, ‘treatment’ and other variables'),
      
      p('Step 2: Select your propensity model(s)'),
      
      p('Step 3: Select your counterfactual method(s) and click Run to see the results'),
      
      br(),
      actionBttn("tutorial",label="Tutorial"),
      actionBttn("start_btn",label="Get Started!", color="success"),
      HTML("</center>")
  )