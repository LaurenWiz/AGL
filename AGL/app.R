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
    selectInput("Beach_Name",label=NULL, choices=beach_names)),
  mainPanel(plotOutput("Plot"))
  )
   
   # dateRangeInput("Event_Date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
    #           min = "2007-01-01", max = "2017-07-31")

demo_data<-tidy_debris

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Plot<-renderPlot({
    data<-switch(input$Beach_Name,
                 demo_data[which(demo_data$SiteName==paste(input$Beach_Name)),])
    
    ggplot(data,aes(x=Year,y=ActualParticipantCount))+
             geom_point()+
             geom_boxplot()+
             theme_classic()+
             labs(title=paste(input$Beach_Name),x="Year",y="Number of Participants")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

