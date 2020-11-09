#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Let the user read in the data (the Beer Data.)

# Let the user make a histogram of the IBU

# Let the user make a histogram of the ABV  (separate histogram)

# Low the user switch back and forth between histograms and boxplots to
# represent this information.  You could do this with a radio button that
# switches the plot from histogram to boxplot … the implementation however is up
# to you.

# Allow the user to filter the histograms / boxplots above by state.

# In addition to the histograms, add a scatter plot of IBU v. ABV

# Add the ability to add or take off the simple linear regression line.

# Allow the user to filter the data on the scatter plot by state.

# Add at least one additional plot from your analysis and make sure to clearly
# label and/or describe the information it is conveying.

# Add the link to the tab you created for Case Study 1.

library(shiny)

ui <- fluidPage(
  titlePanel("Uploading Files"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
        multiple = TRUE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),

      tags$hr(),

      checkboxInput("header", "Header", TRUE),

      radioButtons("sep", "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ","
      ),

      radioButtons("quote", "Quote",
        choices = c(
          None = "",
          "Double Quote" = '"',
          "Single Quote" = "'"
        ),
        selected = '"'
      ),

      tags$hr(),

      radioButtons("disp", "Display",
        choices = c(
          Head = "head",
          All = "all"
        ),
        selected = "head"
      )
    ),

    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file1)

    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )

    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
}

shinyApp(ui, server)