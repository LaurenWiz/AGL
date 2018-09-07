#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shiny)
#library(shinydashboard)

tidy_debris<-

ui <- (
  fluidPage(
    fluidRow(
      column(4,wellPanel(
        selectInput("StateName","State",choices=unique(tidy_debris$StateName))
      )),
      column(4,wellPanel(
        uiOutput("county")
      )),
      column(4,wellPanel(
        uiOutput("beach")
      )),
      column(4,wellPanel(
        checkboxGroupInput("col","Columns",choices=colnames(tidy_debris))))
    ),
    mainPanel(
      DT::dataTableOutput("tab")
      )
  )
)

server <- function(input, output,session) {
  output$county<-renderUI({
   selectInput("county","County",choices=unique(tidy_debris[tidy_debris$StateName==input$StateName,"CountyName"]))
    })
  
  output$beach<-renderUI({
    selectInput("beach","Beach",choices=unique(tidy_debris[tidy_debris$CountyName==input$county,"SiteName"]))
  })

  output$tab<-DT::renderDataTable({
    DT::datatable(tidy_debris[tidy_debris$SiteName==input$beach,input$col])
  })
}

shinyApp(ui, server)


