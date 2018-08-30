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
#demo_data<-readRDS("tidy_debris.Rds")
#demo_data<-readRDS("tidy_debris.Rdata")
demo_data<-beachdata
beach_names<-unique(demo_data$SiteName)
state<-unique(demo_data$StateName)
date_start<-min(demo_data$Year)
date_end<-max(demo_data$Year)


#if (interactive()){
ui <- fluidPage(
  sidebarPanel(selectInput("State_Name",label=NULL, choices=state),
               selectInput("Beach_Name",label=NULL, choices=beach_names),
               dateRangeInput("Year",label=NULL, 
                                    start=date_start, 
                                    end=date_end,
                                    startview =)
               ),
  tabsetPanel(
    tabPanel("Scatterplots",plotOutput("Plot")),
    tabPanel("Searchable Data",DT::dataTableOutput("Table")))
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  output$Table<-renderTable({
    
    
    renderDataTable(input$Beach_Name,
                    demo_data[which(demo_data$SiteName==paste(input$Beach_Name)),],
                    colnames=c("Year","Month","Day","type","num"),
              filter='top',
              extensions='Buttons',
              options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
    output$Plot<-renderPlot({
    data<-switch(input$Beach_Name,
                 demo_data[which(demo_data$SiteName==paste(input$Beach_Name)),])

    ggplot(data,aes(x=EventDate,y=ActualParticipantCount))+
      geom_point()+
      geom_smooth()+
      theme_classic()+
      labs(title=paste(input$Beach_Name),x="Year",y="Number of Participants")
  })
      
  
}
#}
# Run the application 
shinyApp(ui = ui, server = server)

