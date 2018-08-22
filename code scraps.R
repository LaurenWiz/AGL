# Subset data
selected_date <- reactive({
  req(input$Event_Date)
  validate(need(!is.na(input$Event_Date[1]) & !is.na(input$Event_Date[2]), "Error: Please provide both a start and an end date."))
  validate(need(input$Event_Date[1] < input$Event_Date[2], "Error: Start date should be earlier than end date."))
  trend_data %>%
    filter(
      type == input$type,
      Event_Date > as.POSIXct(input$Event_Date[1]) & Event_Date < as.POSIXct(input$Event_Date[2]
      ))
})