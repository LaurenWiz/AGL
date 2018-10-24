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
tidy_debris<-readRDS("tidy_debris.Rdata")


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
        p("Use the inputs to select a State, County, and Beach to get data from.",
          "Lastly, select as many Event IDs as you want to pull up data from different dates.  You can use the slider in the EventDate column to refine dates as you want.",
          "The buttons above the table will export all the data to the format of your choice!  This table was produced by Sam Dunn, PhD for use by Loyola Students.  Please direct any issues to samuel.t.dunn@gmail.com"),
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

  output$tab<-DT::renderDataTable(server=FALSE,{
    DT::datatable(tidy_debris[tidy_debris$EventID==input$EventID,cols_to_display],
                  filter='top',
                  extensions=c("Buttons",'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c(  'csv','excel'))
                  )
  })
}

shinyApp(ui, server)


