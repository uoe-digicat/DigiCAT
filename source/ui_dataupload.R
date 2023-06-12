library(shinyFeedback)
library(shinycssloaders)

dataupload_page <- 
  div(id = "dataupload",
    useShinyFeedback(), # include shinyFeedback
      HTML('<center><img src="progress_bar/data_upload.png" width="800"></center>'),
      br(), br(), br(),
      sidebarLayout(
        sidebarPanel(id="sidebarPanel",
                     
                     ## Add space to give no data warning
                     tags$h2(style="text-align: centre; margin-bottom:20px","Upload data:"),
                     uiOutput("no_data_warning"), ## Give "no data" warning
                     ## Give instructions to get data
                     tags$h4(style="text-align: left;", "Choose CSV File or Use Sample Data"),
                     
                     ## Add button to load sample data
                     div(class = "buttonagency",
                       style="max-width:40%; float:right;", ## position to right
                       actionButton("Btn_sampledata", "Load Sample Data")),
                     
                     ## Add file input for user to upload own data
                     div(style=";max-width:55%; float:left;", ## position to left
                         fileInput('file1', label = NULL, placeholder = "No file selected",
                                   accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                         )),
                     
                     ## Give instructions to select/check variable class
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
                                 choices=NULL, selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F)) %>% ## Multiple choices allowed but max set to 1 so input can be reset to NULL
                       shinyInput_label_embed(
                         shiny_iconlink() %>% ## Add information about outcome having to be a continuous variable
                           bs_embed_popover(title = "Please select your outcome. Please note you can only select a continuous variable (i.e. not catagorical)", placement = "right")
                       ),
                     
                     pickerInput(inputId= 'treatment', label ='Select your treatment variable *',
                                 choices=NULL,selected=NULL, multiple=TRUE, options = pickerOptions(maxOptions = 1, dropupAuto = F) ## Multiple choices allowed but max set to 1 so input can be reset to NULL
                     ),
                     pickerInput(inputId= 'matchvars', label ='Select your matching variables *',
                                 choices=NULL,selected=NULL,multiple=TRUE, options = pickerOptions(dropupAuto = F)
                     ),
                     pickerInput(inputId= 'covars', label ='Select any additional covariates',
                                 choices=NULL,selected=NULL,multiple=TRUE,  options = pickerOptions(dropupAuto = F)
                     ) %>%
                       shinyInput_label_embed(
                         shiny_iconlink() %>%
                           bs_embed_popover(title = "covariates are characteristics (excluding the treatment) of the participants, that may also affect the outcome", placement = "right")
                       ),
                     br(),
                     
                     div(align="center",
                         actionButton("clear_btn", "Clear Data", class = "default_button")),
                     
                     br(),
                     
                     div(align="center",
                         actionButton("prevDU_btn", "Prev", class = "default_button"),
                         actionButton("validate_btn", "Validate Data", class = "progress_button")
                     )
        ),
        mainPanel(wellPanel(id = "well_panel",
          tabsetPanel(id = "Tab_data",
                      tabPanel(title = "Requirements", value = "data_requirements",
                               br(),
                               h4(strong("Data requirements:")),
                               h5(strong("File type:"), "CSV"),
                               h5(strong("Maximum file size:"), "5MB"),
                               h5(strong("Number of rows:")," 10-10,000"),
                               h5(strong("Number of columns:")," 2-100"),
                               h5(strong("Variable type:"), " All data must be numeric, categorical data should be coded as index variables"),
                               h5(strong("Outcome Variable:"), " Outcome variable must be continuous"),
                               h5(strong("Missing data:"), " Missing values should be coded as 'NA'"),
                               br(),
                               h4(strong("Input requirements:")),
                               h5(strong("Required input:"), "Data, categorical variables, outcome, treatment, matching variables"),
                               br(),
                               h4(strong("Required actions:")),
                               h5(strong("1. Upload data: "), "Upload your own data or use our sample data"),
                               h5(strong("2. Select input variables ")),
                               h5(strong("3. Validate input:"), "Ensure inputed data and variables meet requirments"),
                               h5(strong("4. Process to model configuration"))
                               ),
                      tabPanel(title = "Data", value = "raw_data", br(), withSpinner(DT::dataTableOutput('contents'))),
                      tabPanel(title = "Validation", value = "data_validation", br(), div(style='width:auto;height:570px;overflow-y: scroll;',
                                                                                          withSpinner(uiOutput("data_validation"))))))
        ) 
      )
  )

