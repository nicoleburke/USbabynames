#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  titlePanel("US Babynames"),
  fluidRow(
    column(4, wellPanel(
      textInput("text", "Type name in quotes here:", "'name'"),
      
      br(),
      actionButton("goButton", "Go!")
    )),
    column(8,
           h4("summary"),
           textOutput("summary")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary <- renderText({
      # Simply accessing input$goButton here makes this reactive
      # object take a dependency on it. That means when
      # input$goButton changes, this code will re-execute.
      input$goButton

      paste0(name_popularyear(babynames, input$text))
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

