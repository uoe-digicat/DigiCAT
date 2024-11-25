library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(bsplus)
library(DT)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(optmatch)
library(marginaleffects)
library(performance)
library(see)
library(patchwork)
library(shiny.i18n)
library(svyVGAM)

# favicon (icon in browser tab)
HTML('<link rel="icon" type="www/favicon.ico" href="www/favicon.ico"/>')

## File with translations
i18n <- Translator$new(translation_csvs_path = system.file("DigiCAT/translation/", package = "DigiCAT"),
                       translation_csv_config =system.file("DigiCAT/translation/config.yaml", package = "DigiCAT"))

## Set default language to EN
i18n$set_translation_language("en") 

ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  title = "DigiCAT",
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  useShinyjs(),
  includeCSS("./www/themes/light.css"),
  uiOutput("style"),
  
  # Dashboard page layout
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title="DigiCAT",
      titleWidth = 300,
      tags$li(class = "dropdown",a(id = "link", i18n$t("GH feedback"),  href = "https://github.com/uoe-digicat/DigiCAT/issues", target = "_blank"))),
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       id = "main_tabs",
                       br(), br(),
                       img(src = "logos/DigiCAT/logo.png", width = 300),
                       br(), br(),
                       menuItem(i18n$t("Analysis"), tabName = "analysis", icon = icon("home")),
                       menuItem(i18n$t("T and Cs"), tabName = "TC", icon = icon("square-check")),
                       menuItem(i18n$t("About"), tabName = "about", icon = icon("circle-info")),
                       checkboxInput("style", i18n$t("Dark Mode")),
                       HTML("<br><br><br><br><br><br><br><br><br><br>"),
                       HTML(paste0(
                         "<p style = 'text-align: left; padding-left: 20px; font-size: 12px;'><a href='mailto:uoe_digicat-group@uoe.onmicrosoft.com' target='_blank'> uoe_digicat-group@uoe.onmicrosoft.com</a>",
                         "<br><br><br>",
                         "<a href='https://wellcome.org' target='_blank'><img src='logos/WT.png' height = '60'>",
                         "&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                         "<a href='https://www.ed.ac.uk' target='_blank'><img src='logos/UoE.png' height = '60'>"),
                         "&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                         "<a href='https://github.com/uoe-digicat/DigiCAT' target='_blank'><img src='logos/github-mark.png' height = '60'>")
                       )
                     ),
    
    body = dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(tabName = "analysis", tabsetPanel(
          id = "methods-tabs",
          DigiCAT:::home_ui("home", i18n = i18n),  ## Load home tab
          DigiCAT:::data_upload_ui("data_upload", i18n = i18n),  ## Load home tab
          DigiCAT:::CF_approach_ui("CF_approach", i18n = i18n), ## Load CF approach tab
          DigiCAT:::balancing_ui("balancing", i18n = i18n), ## Load balancing tab
          DigiCAT:::outcome_model_ui("outcome_model", i18n = i18n), ## Load outcome model tab
          DigiCAT:::sensitivity_analysis_ui("sensitivity_analysis",  i18n = i18n) ## Load outcome model tab
        )),
        tabItem(tabName = "TC", DigiCAT:::TCs_ui("TCs", i18n = i18n)),
        tabItem(tabName = "about", DigiCAT:::about_ui("TCs", i18n = i18n)))
    )# end dashboardBody
  )
)

