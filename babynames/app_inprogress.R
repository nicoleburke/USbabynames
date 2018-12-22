#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(babynames)
#### given name, what year was it most popular?
name_popularyear <- function(babynames, name_in_quotes) {
  # subset datasets by name
  desired_name <- subset(babynames, name == name_in_quotes)
  # re-order the dataset by most popular 
  desired_name <- desired_name[order(desired_name$n, decreasing = TRUE),]
  # get the top 10
  desired_name <- desired_name[c(1:10),]
  # create an empty dataframe
  newdata <- matrix(nrow = 1, ncol = 2)
  newdata <- data.frame(newdata)
  row.names(newdata) <- name_in_quotes
  colnames(newdata) <- c("Year", "#ofpeople")
  # for loop
  for (i in 1:nrow(desired_name)) {
    # just get the loop to go through so the function will work! 
    if (desired_name$n[i] > 0) {
      # Add year to dataframe
      newdata[i,1] <- desired_name$year[i]
      # Add the number of people 
      newdata[i,2] <- desired_name$n[i]
    }
  }
  return(newdata)
}

## UserInterface 

ui <- tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = "flatly",
    "US Babynames: 1800-2015",
    tabPanel("Names",
             sidebarPanel(
               textInput("text", "Type name here:", "'name'"),
               br(),
               actionButton("goButton", "Go!")
             ),
             mainPanel(
               h4("Top 10 years name is most popular"),
               dataTableOutput("table")
             )# close mainPanel
    ),# close tabPanel
    tabPanel("Year", "This panel is intentionally left blank")
  ) # close navbarPage
)# close tagList

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- renderDataTable({
      input$goButton

      name_popularyear(babynames, input$text)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

