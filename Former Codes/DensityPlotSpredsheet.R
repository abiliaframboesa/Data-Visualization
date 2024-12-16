# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(lubridate)

# UI Definition
ui <- fluidPage(
  titlePanel("NYC 311 Complaints Dashboard"),
  
  # Layout with sidebar and main panel
  sidebarLayout(
    # Sidebar with controls
    sidebarPanel(
      # Complaint type checkboxes
      checkboxGroupInput("complaintTypes",
                         "Select Complaint Types:",
                         choices = c("Homeless Situation" = "Homeless Person Assistance",
                                     "Street Lights" = "Street Light Condition",
                                     "Unsanitary Conditions" = "UNSANITARY CONDITION")),
      
      # Days of week checkboxes
      checkboxGroupInput("daysOfWeek",
                         "Select Days of Week:",
                         choices = c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday", "Sunday"),
                         selected = c("Monday", "Tuesday", "Wednesday",
                                      "Thursday", "Friday", "Saturday", "Sunday"))
    ),
    
    # Main panel with plots
    mainPanel(
      # Time series plot
      plotOutput("timePlot", height = "300px"),
      
      # Map
      leafletOutput("map", height = "400px"),
      
      # Time slider
      sliderInput("timeSlider",
                  "Select Date Range:",
                  min = as.Date("2016-09-01"),
                  max = as.Date("2016-09-30"),
                  value = c(as.Date("2016-09-01"), as.Date("2024-09-30")),
                  timeFormat = "%Y-%m-%d")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    data %>%
      filter(
        ComplaintType %in% input$complaintTypes,
        weekday %in% input$daysOfWeek,
        IssuedDate >= input$timeSlider[1],
        IssuedDate <= input$timeSlider[2]
      )
  })
  
  # Time series plot
  output$timePlot <- renderPlot({
    req(filtered_data())
    
    ggplot(filtered_data(), aes(x = IssuedDate, y = after_stat(count), color = ComplaintType)) +
      geom_line(stat = "count") +
      labs(title = "Complaints Over Time",
           x = "Date",
           y = "Number of Complaints") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Map
  output$map <- renderLeaflet({
    req(filtered_data())
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = -73.9665, lat = 40.7812, zoom = 11) %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = 3,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste("Type:", ComplaintType, "<br>",
                       "Date:", IssuedDate)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
