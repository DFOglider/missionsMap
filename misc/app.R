## ## A sample app to try and figure out reactive data loading
library(shiny)

new_data <- function() {
    d <- rnorm(100)
    write.csv(d, file='data.csv')
}
new_data()

server <- function(input, output, session) {
    data <- reactiveFileReader(1000,
                                         session,
                                         'data.csv',
                                         read.csv)

    observeEvent(input$newdata, {
        new_data()
    })
    
    output$plot1 <- renderPlot({
        hist(data()[[2]])
    })
}

ui <- fluidPage(    
    
    ## Give the page a title
    titlePanel("Title"),
    
    ## Generate a row with a sidebar
    sidebarLayout(      
        
        ## Define the sidebar with one input
        sidebarPanel(
            actionButton(inputId = 'newdata',
                         label = 'Generate new data')
        ),
        
        ## Create a spot for the barplot
        mainPanel(
            plotOutput("plot1")  
        )
        
    )
)

shinyApp(ui = ui, server = server)

