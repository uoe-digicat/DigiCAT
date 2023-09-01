#'@import shiny

TCs_page <- 
  div(class = "text_blocks",
      id="TCs",
      h3("Terms and Conditions for DigiCAT App Usage (Non-Anonymized Data Restrictions)"),
      br(),
      h4("Purpose and Data Restrictions:"),
       br(),
      h5("Users must not upload non-anonymized data and must adhere to the relevant all applicable end user and legal restrictions in using 
                their own data with DigiCAT. Note that a version of DigiCAT that can be used on a local computer is available. User must not upload 
                content that violates laws, infringes intellectual property rights, or is harmful."),
      br(),
      h4("User Responsibilities:"),
      h5("Users are responsible for complying with laws and regulations when uploading or sharing content."),
      br(),
      h4("Intellectual Property:"),
      h5("App-related intellectual property belongs to The University of Edinburgh and may not be used without consent."),
      br(),
      h4("Limitation of Liability:"),
      h5("The App is provided 'as is' and we are not liable for damages or interruptions in its use."),
      br(),
      h4("Indemnification:"),
      h5("Users agree to indemnify The University of Edinburgh against claims or liabilities resulting from their use of the App."),
      br(),
      h4("Modifications and Termination:"),
      h5("We may modify or terminate the App or these terms without prior notice."),
      br(),
      div(style = "text-align:center",
          actionButton('Btn_agree_TCs', 'I agree, get started'))

  )