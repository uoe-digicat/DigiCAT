dataupload_page <- 
  div(useShinyFeedback(), # include shinyFeedback
      sidebarLayout(
        sidebarPanel(id="sidebarPanel",
          tags$h2(style="text-align: centre; margin-bottom:20px","Upload data:"),
          
          ## Give instructions to get data
          tags$h4(style="text-align: left;", "Choose CSV File or Use Sample Data"),
          
          ## Add button to load sample data
          div(style="max-width:40%; float:right;", ## position to right
              actionBttn("Btn_sampledata", "Load Sample Data",size="sm", color="danger", style = "simple")),
          
          ## Add file input for user to upload own data
          div(style=";max-width:55%; float:left;", ## position to left
              fileInput('file1', label = NULL,
                        accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
              )),
          
          ## Give instructions to select/check variable class
          br(),
          br(),
          br(),
          br(),
          
          pickerInput(inputId= 'categorical_vars', label ='Select categorical variables *', multiple = TRUE, 
                      choices=NULL, selected=NULL
          )  %>% ## add info about variables being classed automatically
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title = "Any variables with 5 or fewer unique values are automatically classed as categorical, please alter where necessary", placement = "right")
          ),
          
          
          pickerInput(inputId= 'outcome', label ='Select your outcome variable *',
                      choices=NULL, selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1)) %>% ## Multiple choices allowed but max set to 1 so input can be reset to NULL
                        shinyInput_label_embed(
                          shiny_iconlink() %>% ## Add information about outcome having to be a continuous variable
                            bs_embed_popover(title = "Please select your outcome. Please note you can only select a continuous variable (i.e. not catagorical)", placement = "right")
         ),
          
          pickerInput(inputId= 'treatment', label ='Select your treatment variable *',
                      choices=NULL,selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1) ## Multiple choices allowed but max set to 1 so input can be reset to NULL
          ),
          pickerInput(inputId= 'matchvars', label ='Select your matching variables *',
                      choices=NULL,selected=NULL,multiple=TRUE
          ),
          pickerInput(inputId= 'covars', label ='Select any additional covariates',
                               choices=NULL,selected=NULL,multiple=TRUE
          ) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title = "covariates are characteristics (excluding the treatment) of the participants, that may also affect the outcome", placement = "right")
            ),
          br(),
          div(style="text-align:right", 
            actionBttn("prevBtn_1", "Prev", color="danger", style = "simple"),
            actionBttn("nextBtn_1", "Next", color="danger", style = "simple")
          )
        ),
        mainPanel(
          DT::dataTableOutput('contents', width=750)
        ) 
      )
      
  )

