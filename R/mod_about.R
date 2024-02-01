#'@import shiny

about_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    column(12,
           div(style = "display: flex; text-align: justify;", 
               div(class = "text_blocks",
                   style = "width: 49%;",
                   h3(i18n$t("About Team title")),
                   h5(i18n$t("About Team description")),
               ),
               div(class = "text_blocks",
                   style = "width: 49%; margin-left: 2%",
                   h3(i18n$t("About Contact title")),
                   br(),
                   h5(a(id = "link",href='mailto:uoe_digicat-group@uoe.onmicrosoft.com', i18n$t("About Contact email"))),
                   br(),
                   h5(a(id = "link",href='https://github.com/uoe-digicat/DigiCAT/issues', i18n$t("About Contact git"), target = "_blank"))
           )
    )),
    
    column(12,
           br(),
           br(),
           div(class = "text_blocks",
               style = "text-align: justify;", 
               h3(i18n$t("About Acknowledgements title")),
               h5(i18n$t("About Acknowledgements description"))))
  )
}



