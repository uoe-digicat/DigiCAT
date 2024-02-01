
TCs_ui <- function(id, i18n) {
    ns <- NS(id)
    div(class = "text_blocks",
      id="TCs",
      h3(i18n$t("TC title")),
      br(),
      h4(i18n$t("TC heading 1")),
      h5(i18n$t("TC body 1")),
      br(),
      h4(i18n$t("TC heading 2")),
      h5(i18n$t("TC body 2")),
      br(),
      h4(i18n$t("TC heading 3")),
      h5(i18n$t("TC body 3")),
      br(),
      h4(i18n$t("TC heading 4")),
      h5(i18n$t("TC body 4")),
      br(),
      h4(i18n$t("TC heading 5")),
      h5(i18n$t("TC body 5")),
      br(),
      h4(i18n$t("TC heading 6")),
      h5(i18n$t("TC body 6")))
}

TCs_server <- function(id, parent) {
  

}