#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#load("demo.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Demonstration Shiny Page"),
  sidebarPanel(
    selectInput("County",label=NULL, choices=c("Chicago"="Cook",
                                                 "Ludington"="Mason"))),
  mainPanel(plotOutput("Plot")))
   
   # dateRangeInput("Event_Date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
    #           min = "2007-01-01", max = "2017-07-31")

demo_data<-demo

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Plot<-renderPlot({
    data<-switch(input$County,
                 "Cook"=demo_data[which(demo_data$County=='Cook'),],
                 "Mason"=demo_data[which(demo_data$County=='Mason'),])
    
    ggplot(data,aes(x=numVolunteers,y=Weight))+
             geom_point()+geom_smooth(method="lm")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

