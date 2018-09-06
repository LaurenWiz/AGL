## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Beach Litter Data"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",tabName="dashboard",icon=icon("dashboard")
    ),
    menuItem(
      "Widgets",tabname="widgets",icon=icon("th")
    )
  )),
  dashboardBody(
    tabItems(
      tabItem(tabname="dashboard",
              fluidRow(box(title="Controls",
                  selectInput("StateName","State",choices=unique(tidy_debris$StateName)),
                  uiOutput("county"),
                  uiOutput("beach"),
                  checkboxGroupInput("col","Columns",choices=colnames(tidy_debris))
              ))),
      tabItem(tabname="widgets",
              fluidRow(DT::dataTableOutput("tab")))
      
      
    )
  )
)

server <- function(input, output) { 
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