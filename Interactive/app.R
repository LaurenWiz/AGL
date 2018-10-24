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

cols_to_display<-c("EventDate",
        "Year",
        "Month",
        "Date",
        "CityName",
        "WaterbodyName",
        "type",
        "num",
        "totalNumPersonHours",
        "percentAbundance")


ui <- (
  fluidPage(
    titlePanel("Beach Litter Data Exploration"),
    sidebarLayout(
      sidebarPanel(
      
        selectInput("StateName","State",choices=unique(tidy_debris$StateName))
      ,
      
        uiOutput("county")
      ,
      
        uiOutput("beach")
      ,
      
        uiOutput("EventID")
    ),
      mainPanel(
        h1("Using this table"),
        p("Use the inputs to select a State, Coutny, and Beach to get data from.",
          "Lastly, select as many Event IDs as you want to pull up data from different dates.",
          "The buttons above the table will export the visible selection to the format of your choice!"),
        DT::dataTableOutput("tab")
      )
    )
  )
)

server <- function(input, output,session) {
  output$county<-renderUI({
   selectInput("county",
               "County",
               choices=unique(tidy_debris[tidy_debris$StateName==input$StateName,
                                          "CountyName"]))
    })
  
  output$beach<-renderUI({
    selectInput("beach",
                "Beach",
                choices=unique(tidy_debris[tidy_debris$CountyName==input$county,
                                           "SiteName"]))
  })
  
  output$EventID<-renderUI({
    selectInput("EventID",
                "EventID",
                choices=unique(tidy_debris[tidy_debris$SiteName==input$beach,
                                           "EventID"]),
                multiple=TRUE)
  })

  output$tab<-DT::renderDataTable({
    DT::datatable(tidy_debris[tidy_debris$EventID==input$EventID,cols_to_display],
                  filter='top',
                  extensions=c("Buttons",'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print'))
                  )
  })
}

shinyApp(ui, server)


