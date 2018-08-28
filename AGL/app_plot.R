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
library(readxl)
library(DT)
library(lubridate)
library(leaflet)
library(tidyverse)
library(htmlTable)
#load("demo.rds")
# Define UI for application that draws a histogram
demo_data<-readRDS("tidy_debris.Rds")
beach_names<-unique(demo_data$SiteName)
state<-unique(demo_data$StateName)

ui <- fluidPage(
  headerPanel("Demonstration Shiny Page"),
  conditionalPanel(
    condition="",
    selectizeInput("State",label=NULL, choices=state,options=list(create=TRUE)),
    selectInput("Beach_Name",label=NULL, choices=beach_names)),
  mainPanel(plotOutput("Plot"),tableOutput("Table"))
  )
   


server <- function(input, output) {

  output$Plot<-renderPlot({
    data<-switch(input$Beach_Name,
                 demo_data[which(demo_data$SiteName==paste(input$Beach_Name)),])
    
    ggplot(data,aes(x=Year,y=ActualParticipantCount),group)+
             geom_point()+
             geom_smooth()+
             theme_classic()+
             labs(title=paste(input$Beach_Name),x="Year",y="Number of Participants")
    
  
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

