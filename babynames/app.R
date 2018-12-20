#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(babynames)
name_popularyear <- function(babynames, name_in_quotes) {
  desired_name <- subset(babynames, name == name_in_quotes)
  # create an empty dataframe
  newdata <- matrix(nrow = 1, ncol = 2)
  newdata <- data.frame(newdata)
  row.names(newdata) <- name_in_quotes
  colnames(newdata) <- c("Year", "#ofpeople")
  # for loop
  for (i in 1:nrow(desired_name)) {
    maxnumber <- max(desired_name$n)
    if (desired_name$n[i] == maxnumber) {
      # Add year to dataframe
      newdata[1,1] <- desired_name$year[i]
      # Add the number of people 
      newdata[1,2] <- maxnumber
    }
  }
  return(newdata)
}

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
           dataTableOutput("summary")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary <- renderDataTable({
      input$goButton

      name_popularyear(babynames, input$text)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

