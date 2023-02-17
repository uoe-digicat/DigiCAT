library(shiny)
library(DT)

server <- function(input, output, session) {
  
  ###
  # NAVIGATION
  ###
  tab <- reactiveValues(page = 1, min = 1, max = 7)
  
  observe({
    toggleState(id = "prevBtn_1", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_2", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_3", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_4", condition = tab$page > tab$min)
    toggleState(id = "prevBtn_5", condition = tab$page > tab$min)
    toggleState(id = "nextBtn_1", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_2", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_3", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_4", condition = tab$page < tab$max)
    toggleState(id = "nextBtn_5", condition = tab$page < tab$max)
  })
  
  observe({
    if (input$mynavlist == "home") tab$page = 1
    if (input$mynavlist == "dataupload") tab$page = 2
    if (input$mynavlist == "missing") tab$page = 3
    if (input$mynavlist == "psmodel") tab$page = 4
    if (input$mynavlist == "psresults") tab$page = 5
    if (input$mynavlist == "cfmethod") tab$page = 6
    if (input$mynavlist == "results") tab$page = 7
  })
  
  navPage <- function(direction) {
    if("None" %in% input$psm & (tab$page == 4 & direction>0)|(tab$page == 6 & direction<0)){ 
      tab$page <- tab$page + 2*direction 
    } else {
      tab$page <- tab$page + direction
    }
  }
  
  ###
  # BUTTONS
  ###
  observeEvent(input$prevBtn_1 |  input$prevBtn_2 |  input$prevBtn_3 | input$prevBtn_4 | input$prevBtn_5, {
    navPage(-1)
    updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
  })
  observeEvent(input$start | input$nextBtn_1 |  input$nextBtn_2 |  input$nextBtn_3 | input$nextBtn_4 | input$nextBtn_5, {
    navPage(1)
    updateTabsetPanel(session, "mynavlist", tab.names[tab$page])
  })
  
  # Source servser side 
  source("source/server_home.R",local=T)
  source("source/server_dataupload.R",local=T)
  
  
  
}