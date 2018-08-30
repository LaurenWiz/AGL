#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
beachdata<-bl_df
# Choices for drop-downs
vars <- c(
  "Cleanup Time" = "ActualCleanupHours",
  "Number of Pieces" = "num",
  "Weight of Trash"="TrashWeight"

)


ui <- fluidPage(
  navbarPage("Beach Litter", id="nav",
             
             tabPanel("Interactive map",
                      div(class="outer",
                          
                          #tags$head(
                            # Include our custom CSS
                         #   includeCSS("styles.css"),
                        #    includeScript("gomap.js")
                          #),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map"),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Map Explorer"),
                                        
                                        selectInput("color", "Color", vars),
                                        selectInput("size", "Size", vars, selected = "num"),
                                        conditionalPanel("input.color == 'ActualCleanupHours' || input.size == 'weight'"
                                        ),
                                        
                                        plotOutput("histNum", height = 200),
                                        plotOutput("scatterWeightDate", height = 250)
                          )
                      )
             ),
             
             tabPanel("Data explorer",
                      fluidRow(
                        column(3,
                               selectInput("StateName", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                        ),
                        column(3,
                               conditionalPanel("input.states",
                                                selectInput("CityName", "Cities", c("All cities"=""), multiple=TRUE)
                               )
                        ),
                        column(3,
                               conditionalPanel("input.states",
                                                selectInput("SiteName", "Beach Name", c("All beaches"=""), multiple=TRUE)
                               )
                        )
                      ),
                      
                      hr(),
                      DT::dataTableOutput("beachtable")
             ),
             
             conditionalPanel("false", icon("crosshair"))
  )  
)

# Define server logic required to draw a histogram
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
server <- function(input, output,session) {
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(beachdata) %>%
      addTiles(
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  beachesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(beachdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(beachdata,
           LatitudeCenter >= latRng[1] & LatitudeCenter <= latRng[2] &
             LongitudeCenter >= lngRng[1] & LongitudeCenter <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  numBreaks <- hist(plot = FALSE, beachdata$num, breaks = 20)$breaks
  
  output$histNum <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(beachesInBounds()) == 0)
      return(NULL)
    
    hist(beachesInBounds()$num,
         breaks = numBreaks,
         main = "SuperZIP score (visible zips)",
         xlab = "Percentile",
         xlim = range(beachdata$num),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterWeightDate <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(beachesInBounds()) == 0)
      return(NULL)
    
    print(xyplot(Weight ~ EventDate, data = beachesInBounds(), xlim = range(beachdata$EventDate), ylim = range(beachdata$Weight)))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
   
    
    leafletProxy("map", data = beachdata) %>%
      clearShapes() %>%
      addCircles(~LongitudeCenter, ~LatitudeCenter, radius=radius, layerId=~CountyName,
                 stroke=FALSE, fillOpacity=0.4 ) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  #Popup <- function(zipcode, lat, lng) {
 #   selectedBeach <- beachdata[beachdata$SiteName == SiteName,]
#    content <- as.character(tagList(
 #     tags$h4("Score:", as.integer(selectedBeach$centile)),
 #     tags$strong(HTML(sprintf("%s, %s %s",
#                               selectedBeach$city.x, selectedBeach$state.x, selectedBeach$zipcode
#      ))), tags$br(),
 #     sprintf("Median household income: %s", dollar(selectedBeach$income * 1000)), tags$br(),
 #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedBeach$college)), tags$br(),
#      sprintf("Adult population: %s", selectedBeach$adultpop)
#    ))
#    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
#  }
  
  # When map is clicked, show a popup with city info
#  observe({
#    leafletProxy("map") %>% clearPopups()
#    event <- input$map_shape_click
##    if (is.null(event))
 #     return()
    
#    isolate({
#      showZipcodePopup(event$id, event$lat, event$lng)
#    })
#  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    SiteName <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('SiteNames') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$SiteName[input$SiteName %in% SiteName])
    updateSelectInput(session, "SiteName", choices = SiteName,
                      selected = stillSelected)
  })
  
 # observe({
 #   if (is.null(input$goto))
  #    return()
 #   isolate({
 #     map <- leafletProxy("map")
 #     map %>% clearPopups()
 #     dist <- 0.5
 #     zip <- input$goto$zip
 #     lat <- input$goto$lat
 #     lng <- input$goto$lng
 #     showZipcodePopup(zip, lat, lng)
 #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
 #   })
 # })
  
  output$beachtable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$SiteName) | SiteName %in% input$SiteName
      )# %>%
     # mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
   # action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

