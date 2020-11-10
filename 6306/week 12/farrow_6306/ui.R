library(shiny)

plots <- c("boxplot", "histogram")
yes <- c("yes", "no")

ui <- shinyUI(fluidPage(
    titlePanel("Exploring Beer Data"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Histogram",
                 pageWithSidebar(
                     
                     headerPanel('Distribution'),
                     
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded
                         selectInput('xcol', 'What is the x-variable?', ""),
                         radioButtons("plot", "What Type of Plot?", plots),
                         
                         sliderInput(
                             inputId = "bins",
                             label = "Number of bins:(only applicable for Histograms)",
                             min = 1,
                             max = 50,
                             value = 30
                         )
                         
                     ),
                     
                     mainPanel(plotOutput('Hist'))
                     
                 )), 
        
        tabPanel("Scatter Plot",
                 pageWithSidebar(
                     headerPanel('Scatter Plot'),
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded
                         selectInput('scatter_x', 
                                     label = 'What is the x-variable?',
                                     choices = c("ABV", "IBU", "Ounces"),
                                     # selected = "All",
                                     # multiple = TRUE,
                                     # selectize = TRUE,
                                     # width = NULL,
                                     # size = NULL
                                     ),
                         
                         selectInput('scatter_y', 
                                     label = 'What is the y-variable?',
                                     choices = c("ABV", "IBU", "Ounces"),
                                     # selected = "All",
                                     # multiple = TRUE,
                                     # selectize = TRUE,
                                     # width = NULL,
                                     # size = NULL
                                     ),
                         
                         radioButtons("add.line", "Add Line?", yes)
                         
                     ),
                     
                     mainPanel(
                         plotOutput('Scatter')
                         
                     )
                 )
        )
        
    )
)
)