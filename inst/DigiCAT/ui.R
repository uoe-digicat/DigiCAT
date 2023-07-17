library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
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

# favicon (icon in browser tab)
HTML('<link rel="icon" type="www/favicon.ico" href="www/favicon.ico"/>')

ui <- fluidPage(
  title = "DigiCAT",
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  useShinyjs(),
  includeCSS("./www/themes/light.css"),
  uiOutput("style"),
  
  # Dashboard page layout
  dashboardPage(
    skin = "blue",
    dashboardHeader(title="DigiCAT", titleWidth = 300),
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       id = "main_tabs",
                       br(), br(),
                       img(src = "logos/DigiCAT/logo.png", width = 300),
                       br(), br(),
                       menuItem("Analysis", tabName = "analysis", icon = icon("home")),
                       menuItem("Tutorial", tabName = "tutorial", icon = icon("arrow-pointer")),
                       menuItem("Terms & Conditions", tabName = "TC", icon = icon("square-check")),
                       menuItem("About", tabName = "about", icon = icon("circle-info")),
                       checkboxInput("style", "Dark Mode"),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br><br><br><br>",
                         "<p style = 'text-align: left; padding-left: 20px;'><small><a href='mailto:uoe_digicat-group@uoe.onmicrosoft.com' target='_blank'>uoe_digicat-group@uoe.onmicrosoft.com</a>",
                         "<br><br><br>",
                         "<img src='logos/WT.png' height = '60'>",
                         "&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                         "<img src='logos/UoE.png' height = '60'>")
                         )
                     )
                     
    ),
    
    dashboardBody(
      tabItems(
      tabItem(tabName = "analysis", tabsetPanel(
        id = "methods-tabs",
        DigiCAT:::home_ui("home"),  ## Load home tab
        DigiCAT:::data_upload_ui("data_upload"),  ## Load home tab
        DigiCAT:::CF_approach_ui("CF_approach"), ## Load CF approach tab
        DigiCAT:::balancing_model_ui("balancing_model"), ## Load balancing model tab
        DigiCAT:::balancing_ui("balancing"), ## Load balancing tab
        DigiCAT:::outcome_model_ui("outcome_model"), ## Load outcome model tab
        DigiCAT:::get_results_ui("get_results") ## Load get results tab
      )),
      DigiCAT:::tutorial_ui("methods"),
      tabItem(tabName = "TC", DigiCAT:::TCs_page),
      tabItem(tabName = "about", ""))
      )# end dashboardBody
  )
)

