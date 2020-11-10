library(shiny)
library(tidyverse)

server <- shinyServer(function(input, output, session) {
    
    data <- reactive({
        
        req(input$file1) 
        
        inFile <- input$file1 
        
        df <- read.csv(inFile$datapath, header = input$header)
        
        updateSelectInput(
            session,
            inputId = 'xcol',
            label = 'Variable',
            choices = c("ABV", "IBU", "Ounces"),
            selected = names(df)
        )
        
        updateSelectInput(
            session,
            inputId = 'xcol2',
            label = 'X Variable',
            choices = c("ABV", "IBU", "Ounces"),
            selected = names(df)
        )
        
        updateSelectInput(
            session,
            inputId = 'ycol',
            label = 'Y Variable',
            choices = c("ABV", "IBU", "Ounces"),
            selected = names(df)
        )
        
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$Hist <- renderPlot({
        plot1    <- data()[, input$xcol]
        dist <-
            switch(input$plot,
                   histogram = hist(plot1, breaks = input$bins),
                   boxplot = boxplot(plot1))
    })
    
    output$Scatter <- renderPlot({
        
        scatter <- 
            ggplot(data(), aes_string(input$scatter_x, input$scatter_y)) + 
            geom_jitter(alpha = 0.5) +
            theme_minimal()
        
        withline <- scatter + stat_smooth(method = "lm")
        
        switch(input$add.line, yes = withline , no = scatter)
        
    })

})