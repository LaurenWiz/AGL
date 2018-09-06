#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(DT)
library(shinydashboard)

#tidy_debris<-

ui <- (
  fluidPage(
    fluidRow(
      column(3,wellPanel(
        selectInput("StateName","State",choices=unique(tidy_debris$StateName))
      )),
      column(3,wellPanel(
        uiOutput("county")
      )),
      column(3,wellPanel(
        uiOutput("beach")
      )),
      column(3,wellPanel(
        selectizeInput("col","Columns",choices=colnames(tidy_debris),multiple=TRUE)
      ))
     
      
    ),
    mainPanel(
      DT::dataTableOutput("tab")
      )
  )
)

server <- function(input, output,session) {
    #updateSelectizeInput(session,"col",choices=colnames(tidy_debris))
  
    output$county<-renderUI({
      selectInput("county","County",choices=unique(tidy_debris[tidy_debris$StateName==input$StateName,"CountyName"]))
    })
    
    output$beach<-renderUI({
      selectInput("beach","Beach",choices=unique(tidy_debris[tidy_debris$CountyName==input$county,"SiteName"]))
    })
  

    output$tab<-DT::renderDataTable({
      DT::datatable(tidy_debris[tidy_debris$SiteName==input$beach,input$col],
                    filter='top',
                    extensions='Buttons',
                    options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })
}

shinyApp(ui, server)


