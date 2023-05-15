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
  # include any CSS
  includeCSS("./www/stuff.css"),

  # MAIN HEADER BANNER 
  # outside of nav pages
  div(id="header-content",
      tags$h2(id = "header-text", 
              #a(target="_blank",href="",HTML("DigiCAT<br><h4>A digital tool to facilitate counterfactual analysis</h4>")),
              tags$img(style="max-height: 100px; padding-left: 100px; padding-right: 100px", src="digicat6b.png"))
  ),
  
  
  navbarPage(title="",
             id="mynavlist",
             collapsible=TRUE,
             tabPanel("Home", value="home", icon = icon('home',lib="glyphicon"),
                      home_page
             ),
             tabPanel("Data Upload", value="dataupload", icon = icon('table'),
                      dataupload_page
             ),
             tabPanel("Choose Things", value="method", icon = icon('certificate',lib="glyphicon"),
                      method_page
             ),
             tabPanel("Propensity Model Results", value="psresults", icon = icon('tree'),
                      psres_page
             ),
             tabPanel("Get Results", value="results", icon = icon('save',lib="glyphicon"),
                      results_page
             )
  ),
  
  div(id="footer", tags$img(style="max-width: 150px; margin-left: 5px", src="uoelogo.svg"))
)

