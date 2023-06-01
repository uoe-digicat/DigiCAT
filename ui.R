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
library(shinydashboard)


# source individual pages
source("source/ui_home.R")
source("source/ui_dataupload.R")
source("source/ui_psresults.R")
source("source/ui_cfmethod.R")
source("source/ui_results.R")


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
    dashboardHeader(title="DigiCAT"),
    dashboardSidebar(width = 250,
                     sidebarMenu(
                       id = "main_tabs",
                       br(), br(),
                       img(src = "logos/DigiCAT/logo.png", width = 250),
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
                         "<img src='logos/WT.png' height = '40'>",
                         "&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                         "<img src='logos/UoE.png' height = '40'>")
                         )
                     )
                     
    ),
    
    dashboardBody(
      tabItems(
      tabItem(tabName = "analysis", tabsetPanel(
        id = "Tab_analysis",
        tabPanel(title = "Home", value = "home", home_page), 
        tabPanel(title = "Upload", value = "upload", dataupload_page),
        tabPanel("methods", value = "methods", method_page),
        tabPanel("psres", value = "psres", psres_page),
        tabPanel("results", value = "results", results_page)
      )),
      tabItem(tabName = "tutorial", ""),
      tabItem(tabName = "TC", ""),
      tabItem(tabName = "about", ""))
      )# end dashboardBody
  )
)

