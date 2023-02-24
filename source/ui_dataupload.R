dataupload_page <- 
  div(id="dataupload",
      # text summary
      sidebarLayout(
        sidebarPanel(
          tags$h2(style="text-align: center; ","Feed me data!"),
          
          div(style="text-align:right;margin-bottom:5px",
              actionBttn("Btn_sampledata", "Load Sample Data",size="sm")
          ),
          fileInput('file1', 'Choose CSV File',
                      accept=c('text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv')
          ),
          

          pickerInput(inputId= 'outcome', label ='Select your outcome variable',
                      choices=NULL,selected=NULL
          ),
          pickerInput(inputId= 'treatment', label ='Select your treatment variable',
                      choices=NULL,selected=NULL
          ),
          pickerInput(inputId= 'matchvars', label ='Select your matching variables',
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
            actionBttn("prevBtn_1", "Prev", color="success"),
            actionBttn("nextBtn_1", "Next", color="success")
          )
        ),
        mainPanel(
          DT::dataTableOutput('contents', width=750)
        ) 
      )
      
  )
