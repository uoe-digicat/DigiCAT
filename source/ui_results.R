results_page <- 
  div(id="results",
      HTML("<center>"),
      
      # we could try and render some generic summary text based on the inputs chosen. 
      # can sometimes be a bit messy, but definitely possible
      p("Add summary text here"),
      htmlOutput("finresult"),
      br(),
      actionBttn(inputId='printres', label= 'Print results', color = "success"),
      actionBttn(inputId='emailres', label= 'Email results', color = "success"),
      br(),
      p(""),
      br(),
      actionBttn(inputId='codedownload', label= 'Show me the code'),
      HTML("</center>")
  )