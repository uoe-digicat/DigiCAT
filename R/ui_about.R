#'@import shiny

about_page <- 
  fluidRow(
    column(12,
           div(style = "display: flex; text-align: justify;", 
               div(class = "text_blocks",
                   style = "width: 49%;",
                   h3("About the DigiCAT Team:"),
                   h5("DigiCAT was developed to help make counterfactual analysis more accessible and through providing an easy-to-use tool and accessible 
             tutorial materials. It was developed as part of the Wellcome Trust Mental Health Data Prize by a multi-disciplinary team of researchers 
             at the University of Edinburgh. The tool development team includes",  a(id = "link","Aja Murray", href = "https://www.ed.ac.uk/profile/aja-murray", target = "_blank"),
                      ", ",  a(id = "link","Josiah King", href = "https://www.ed.ac.uk/profile/josiah-pj-king", target = "_blank"), ", Helen Wright and Hannah Casey with lived 
             experience input leads ",  a(id = "link","Marie Allitt", href = "https://www.ed.ac.uk/profile/dr-marie-allitt", target = "_blank")," and ", 
                       a(id = "link","Ingrid Obsuth", href = "https://www.ed.ac.uk/profile/ingrid-obsuth", target = "_blank")," and user input leads ", 
                       a(id = "link","Dan Mirman",href = "https://www.ed.ac.uk/profile/dr-daniel-mirman", target = "_blank")," and ", 
                       a(id = "link","Patrick Errington.", href = "https://www.ed.ac.uk/profile/dr-patrick-errington", target = "_blank")),
               ),
               div(class = "text_blocks",
                   style = "width: 49%; margin-left: 2%",
                   h3("Get in touch:"),
                   br(),
                   h5("Email us:",  a(id = "link",href='mailto:uoe_digicat-group@uoe.onmicrosoft.com', "uoe_digicat-group@uoe.onmicrosoft.com")),
                   br(),
                   h5("Open an issue on ",  a(id = "link",href='https://github.com/uoe-digicat/DigiCAT/issues', "the DigiCAT GitHub page", target = "_blank"))
           )
    )),
    
    column(12,
           br(),
           br(),
           div(class = "text_blocks",
               style = "text-align: justify;", 
               h3("Acknowledgements:"),
               h5("We are grateful to the Wellcome Trust for funding the Discovery and Prototyping phases of the development of DigiCAT. Our thanks also go to the many
               who contributed to DigiCAT in various ways. We are grateful to the young person advisory groups (YPAGs) who provided invaluable insights and help guide 
               the direction of DigiCAT, to the users who responded to our user survey and to the group members who provided feedback on iterations of the tool.

")))
  )



