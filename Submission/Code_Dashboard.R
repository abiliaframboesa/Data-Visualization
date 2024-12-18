library(data.table)
library(dplyr)
library(ggplot2)
library(osmdata)
library(sf)
library(leaflet)
library(shiny)
library(igraph)
library(dplyr)
library(visNetwork)
library(RColorBrewer)
library(shinydashboard)
library(visNetwork)
library(tidyverse)
library(tidygraph)
library(visNetwork)  # Pacote para grafos interativos
library(httr)
library(jsonlite)
library(lubridate)

data <- fread(choose.files(),sep = ";")
# Reading a local file
#data <- fread("C:/Users/dcssi/OneDrive/Ambiente de Trabalho/2ºano_Mestrado/VD/NYC_311_Data_20241009.csv")
#data <- fread("C:/Users/Ana Luísa/Desktop/Mestrado em Bioinformática e Biologia Computacional/2 ano/Visualização de Dados/Projeto/NYC_311_Data_20241009.csv")


# ------------------------------------- Pre-processing ------------------------------------------

################
# Data Cleaning
################

# Identify missing values, including empty strings and whitespace-only strings
count_missing <- function(x) {
  if (inherits(x, "POSIXt")) {
    # For datetime objects, only check for NA
    sum(is.na(x))
  } else {
    # For character or other types, check for NA, empty strings, and whitespace-only strings
    sum(is.na(x) | x == "N/A" | x == "" | grepl("^\\s*$", x))
  }
}

# Check for missing values summary
missing_summary <- sapply(data, count_missing)
print(missing_summary)

# Setting a threshold: drop columns with more than 60% missing values
threshold <- 0.60 * nrow(data)
dataTrimmed <- data %>% select_if(~ count_missing(.) < threshold)

# If both 'Incident Address' and 'Latitude'/'Longitude' are missing or empty, remove the row
dataTrimmed <- dataTrimmed %>% filter(
  !( (is.na(`Incident Address`) | `Incident Address` == "" | grepl("^\\s*$", `Incident Address`)) & 
       (is.na(Latitude) | Latitude == "" | grepl("^\\s*$", Latitude)) & 
       (is.na(Longitude) | Longitude == "" | grepl("^\\s*$", Longitude)) )
)


# Removal of columns that have already inherent info on the dataset, or that aren't really useful
dataTrimmed <- dataTrimmed %>%
  select(-c('Cross Street 1', 'Cross Street 2', 'Address Type', 
            'X Coordinate (State Plane)', 'Y Coordinate (State Plane)', 
            'Park Facility Name', 'Park Borough', 'Location')
  )

# Re-establish the data variable as the main one
data <- dataTrimmed
rm(dataTrimmed)

# Missing Values Posterior Analysis
missing_summary <- sapply(data, count_missing)
print(missing_summary)


# Rename the "Complaint Type" column to "ComplaintType"
data <- data %>%
  rename(ComplaintType = 'Complaint Type')

# str(data)
# summary(data)
# View(data)

#############
# Date Setup
#############

# Convert the Date columns to POSIXct acceptable format
data$IssuedDate <- as.POSIXct(strptime(data$'Created Date', format="%m/%d/%Y %I:%M:%S %p"))
data$ResolutionDate <- as.POSIXct(strptime(data$'Closed Date', format="%m/%d/%Y %I:%M:%S %p"))
data$DueDate <- as.POSIXct(strptime(data$'Due Date', format="%m/%d/%Y %I:%M:%S %p"))
data$ResUpdateDate <- as.POSIXct(strptime(data$'Resolution Action Updated Date', format="%m/%d/%Y %I:%M:%S %p"))

# Remove the previous date columns that are no longer needed
data <- data %>%
  select(-'Created Date', -'Closed Date', -'Due Date', -'Resolution Action Updated Date')

# Update DueDate to get the latest date between DueDate and ResUpdateDate for each instance
data <- data %>%
  mutate(
    DueDate = pmax(DueDate, ResUpdateDate, na.rm = TRUE)
  )


# Remove ResUpdateDate since it won't be needed anymore
data[ ,':='(ResUpdateDate=NULL)] 

missing_stats <- data %>%
  summarise(
    IssueDate_Missing = sum(is.na(IssuedDate)),
    ResolutionDate_Missing = sum(is.na(ResolutionDate)),
    DueDate_Missing = sum(is.na(DueDate)),
    
    IssueDate_Percent = mean(is.na(IssuedDate)) * 100,
    ResolutionDate_Percent = mean(is.na(ResolutionDate)) * 100,
    DueDate_Percent = mean(is.na(DueDate)) * 100
  )

print(missing_stats)


# IssueDuration: Calculate the duration between IssuedDate and ResolutionDate in seconds
data$IssueDuration <- difftime(data$ResolutionDate, data$IssuedDate, units = "secs")
# IssueBalance: Calculate the balance between DueDate and ResolutionDate in seconds
data$IssueBalance <- difftime(data$DueDate, data$ResolutionDate, units = "secs")


# Create a column for the days of the week
Sys.setlocale("LC_TIME", "C")
data$weekday <- weekdays(data$IssuedDate)
#View(data)

#############
# Missing Values in ResolutionDate
###########

sum(is.na(data$ResolutionDate))

# Resolution date without dates because some issues were not solved yet (status not closed) or not inputted. 

table(data$Status)
# Not all issues were fixed. Some are still pending, so we created a new column
# called IsResolved, which indicates that if they are closed and have a finished date, they are considered resolved

# Create the "IsResolved" column for unresolved complaints based on the Status and ResolutionDate
data$IsResolved <- case_when(
  data$Status == "Closed" & !is.na(data$ResolutionDate) ~ "Resolved", 
  data$Status == "Closed - Testing" & !is.na(data$ResolutionDate) ~ "Resolved", 
  data$Status == "Started" & !is.na(data$ResolutionDate) ~ "Resolved", 
  data$Status %in% c("Assigned", "Open", "Pending") ~ "Not Resolved", 
  is.na(data$ResolutionDate) ~ "Not Resolved", 
  TRUE ~ "Indeterminate"
)

# Check the results of the "IsResolved" column
table(data$IsResolved)

# Now that we know which issues were resolved and which weren't,
# let's calculate how long they took to resolve

# ResolutionExpDurationDays: Calculate the expected duration between IssuedDate and DueDate in days
data$ResolutionExpDurationDays <- round(as.numeric(difftime(data$DueDate, data$IssuedDate, units = "days")), 1)

# ResolutionObvDurationDays: Calculate the duration between IssuedDate and ResolutionDate in days
data$ResolutionObvDurationDays <- round(as.numeric(difftime(data$ResolutionDate, data$IssuedDate, units = "days")), 1)

# Create the column "ResolutionDurationCategory" to categorize ComplaintTypes by their resolution duration
data <- data %>%
  mutate(
    ResolutionDurationCategory = case_when(
      ResolutionObvDurationDays < 1 ~ "<1 day",
      ResolutionObvDurationDays >= 1 & ResolutionObvDurationDays < 3 ~ "1-3 days",
      ResolutionObvDurationDays >= 3 & ResolutionObvDurationDays < 7 ~ "3-7 days",
      ResolutionObvDurationDays >= 7 & ResolutionObvDurationDays < 14 ~ "1-2 weeks",
      ResolutionObvDurationDays >= 14 & ResolutionObvDurationDays < 30 ~ "2 weeks-1 month",
      ResolutionObvDurationDays >= 30 & ResolutionObvDurationDays < 180 ~ "2-6 months",
      ResolutionObvDurationDays >= 180 ~ ">6 months",
      TRUE ~ "Indeterminate"
    )
  )

# ------------------------------- Top 100 complaints in 10 categories by Borough -------------------------

# Assign detailed categories to complaint types
data <- data %>%
  mutate(Category = case_when(
    ComplaintType %in% c("Noise", "Noise - Residential", "Noise - Commercial", "Noise - Vehicle", 
                         "Noise - Street/Sidewalk", "Noise - Park", "Noise - House of Worship", 
                         "Noise - Helicopter") ~ "Noise Complaints",
    
    ComplaintType %in% c("Blocked Driveway", "Illegal Parking", "Street Condition", "Street Light Condition",
                         "Street Sign - Missing", "Street Sign - Damaged", "Street Sign - Dangling", 
                         "Highway Condition", "Bridge Condition", "Tunnel Condition",
                         "Traffic Signal Condition", "Sidewalk Condition", "Bike Rack Condition") ~ "Traffic & Streets",
    
    ComplaintType %in% c("Rodent", "Standing Water", "Sanitation Condition", "Food Poisoning",
                         "Unsanitary Pigeon Condition", "Unsanitary Animal Pvt Property",
                         "Dirty Conditions", "Lead", "Air Quality", "Recycling Enforcement",
                         "Water System", "Water Quality", "Sewer", "Litter Basket / Request",
                         "Indoor Air Quality", "Hazardous Materials", "Overflowing Recycling Baskets",
                         "Sanitation Condition", "Overflowing Litter Baskets", "Missed Collection (All Materials)",
                         "Asbestos", "Mold", "Industrial Waste", "UNSANITARY CONDITION", "Mosquitoes", 
                         "Unsanitary Animal Pvt Property") ~ "Health & Sanitation",
    
    ComplaintType %in% c("PLUMBING", "WATER LEAK", "HEAT/HOT WATER", "Drinking Water", "Water Quality", 
                         "Water Conservation", "Sewer", "Water System", "Standing Water", "Indoor Sewage") ~ "Water & Plumbing Issues",
    
    ComplaintType %in% c("Building/Use", "Elevator", "Heating/Hot Water", "Building Marshals office", 
                         "ELECTRIC", "GENERAL", "FLOORING/STAIRS", "Illegal Animal Kept as Pet", "Asbestos", 
                         "Boilers", "DOOR/WINDOW", "General Construction/Plumbing") ~ "Building & Housing Issues",
    
    ComplaintType %in% c("Sidewalk Condition", "Curb Condition", "Tree Condition", "Bus Stop Shelter Complaint", 
                         "Damaged Tree", "New Tree Request", "Overgrown Tree/Branches", "Graffiti", 
                         "Posting Advertisement", "Dead/Dying Tree") ~ "Public Space Conditions",
    
    ComplaintType %in% c("Consumer Complaint", "Taxi Complaint", "For Hire Vehicle Complaint", "Food Establishment") ~ "Consumer Issues",
    
    ComplaintType %in% c("Non-Emergency Police Matter", "Homeless Person Assistance", "Homeless Encampment", 
                         "Panhandling", "Traffic", "Other Enforcement", "SAFETY") ~ "Social Issues",
    
    ComplaintType %in% c("Derelict Vehicle", "Derelict Vehicles", "Broken Muni Meter", 
                         "Street Sign - Dangling", "Street Sign - Missing", "Street Sign - Damaged", 
                         "Root/Sewer/Sidewalk Condition", "Traffic Signal Condition", "Vacant Lot") ~ "Urban Infrastructure",
    TRUE ~ "Other"
  ))

View(data)



#########################################################################################################################
# -----------------------------------------------------  DASHBOARD  ---------------------------------------
##########################################################################################################################

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)

# duplicar a coluna de IssuedDate com o nome IssuedHour
data$IssuedHour <- data$IssuedDate

#Converter para o formato de hora a coluna IssuedHour
data$Hour <- format(data$IssuedHour, "%H")  # Extrai apenas a hora --> importante para o heatmap plot

#Converter para o formato de data a coluna de IssuedDate --> importante para o timeseries plot
data$IssuedDate <- as.Date(data$IssuedDate)

garagelots <- read.csv("Issued_Licenses_bn.csv")
zip_locations <- read.csv("zip_locations.csv")


# UI
ui <- navbarPage("Dashboard with NYC Complaints",
                 tabPanel("Dashboard",  # Aba principal com os gráficos existentes
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              sliderInput("threshold", "Minimum Complaint Threshold:",
                                          min = 1, max = 100, value = 50
                              ),
                              checkboxGroupInput("day_of_week", "Select Days of the Week:",
                                                 choices = unique(data$weekday), selected = unique(data$weekday)
                              ),
                              sliderInput("timeSlider", "Select Date Range:",
                                          min = as.Date("2016-09-01"),
                                          max = as.Date("2016-09-30"),
                                          value = c(as.Date("2016-09-01"), as.Date("2016-09-30")),
                                          timeFormat = "%Y-%m-%d"
                              ),
                              checkboxGroupInput("boroughs", "Select Borough:",
                                                 choices = c("MANHATTAN", "BROOKLYN","STATEN ISLAND","QUEENS","BRONX"), selected = c("MANHATTAN", "BROOKLYN","STATEN ISLAND","QUEENS","BRONX")
                              ),
                              checkboxGroupInput("complaint_type", "Select Complaint Type:",
                                                 choices = c("Noise - Residential", "Illegal Parking", "Blocked Driveway", "Noise - Street/Sidewalk", "UNSANITARY CONDITION", "Homeless Person Assistance", "Homeless Encampment", "Street Condition", "Street Light Condition", "Sanitation Condition", "General Construction/Plumbing"), selected = NULL
                              )
                            ),
                            mainPanel(
                              fluidRow(
                                column(7, plotlyOutput("complaints_by_borough")),
                                column(5, plotlyOutput("complaints_over_time"))
                              ),
                              fluidRow(
                                column(12, leafletOutput("map_dashboard", height = "600px")),  # Mapa para a primeira aba
                                checkboxInput("showMap", "Show Garage and Parking Lots 🟠", value = FALSE)
                              )
                            )
                          )
                 ),
                 tabPanel("Heat Map",  # Nova aba "Additional Info"
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              sliderInput("threshold_additional", "Minimum Complaint Threshold (Additional Info):",
                                          min = 1, max = 100, value = 50
                              ),
                              checkboxGroupInput("day_of_week_additional", "Select Days of the Week (Additional Info):",
                                                 choices = unique(data$weekday), selected = unique(data$weekday)
                              ),
                              sliderInput("timeSlider_additional", "Select Date Range (Additional Info):",
                                          min = as.Date("2016-09-01"),
                                          max = as.Date("2016-09-30"),
                                          value = c(as.Date("2016-09-01"), as.Date("2016-09-30")),
                                          timeFormat = "%Y-%m-%d"
                              ),
                              checkboxGroupInput("boroughs_additional", "Select Borough (Additional Info):",
                                                 choices = c("MANHATTAN", "BROOKLYN","STATEN ISLAND","QUEENS","BRONX"), selected = c("MANHATTAN", "BROOKLYN","STATEN ISLAND","QUEENS","BRONX")
                              ),
                              checkboxGroupInput("complaint_type_additional", "Select Complaint Type (Additional Info):",
                                                 choices = c("Noise - Residential", "Illegal Parking", "Blocked Driveway", "Noise - Street/Sidewalk", "UNSANITARY CONDITION", "Homeless Person Assistance", "Homeless Encampment", "Street Condition", "Street Light Condition", "Sanitation Condition", "General Construction/Plumbing"), selected = NULL
                              )
                            ),
                            mainPanel(
                              fluidRow(column(12, plotlyOutput("additional_plot"))),
                              fluidRow(
                                column(12, leafletOutput("map_additional")),  # Mapa para a segunda aba
                                checkboxInput("showMap_additional", "Show Garage and Parking Lots 🟠", value = FALSE)
                              )
                            )
                          )
                 ),
                 tabPanel("Network Plot",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              sliderInput("threshold", "Minimum Complaint Threshold:",
                                          min = 1, max = 100, value = 49
                              ),
                              checkboxGroupInput("day_of_week_Network", "Select Days of the Week:",
                                                 choices = unique(data$weekday), selected = unique(data$weekday)
                              ),
                              sliderInput("timeSlider_Network", "Select Date Range:",
                                          min = as.Date("2016-09-01"),
                                          max = as.Date("2016-09-30"),
                                          value = c(as.Date("2016-09-01"), as.Date("2016-09-30")),
                                          timeFormat = "%Y-%m-%d"
                              ),
                              checkboxGroupInput("boroughs_Netwotkplot", "Select Borough:",
                                                 choices = c("MANHATTAN", "BROOKLYN","STATEN ISLAND","QUEENS","BRONX"), selected = c("MANHATTAN", "BROOKLYN","STATEN ISLAND","QUEENS","BRONX")
                              ),
                              checkboxGroupInput("complaint_type_Networkplot", "Select Complaint Type:",
                                                 c("Noise - Residential", "Illegal Parking", "Blocked Driveway", "Noise - Street/Sidewalk", "UNSANITARY CONDITION", "Homeless Person Assistance", "Homeless Encampment", "Street Condition", "Street Light Condition", "Sanitation Condition", "General Construction/Plumbing"), selected = NULL
                              )
                            ),
                            mainPanel(
                              fluidRow(
                                column(width = 12, visNetworkOutput("networkPlot", height = "600px"))
                              ),
                              fluidRow(
                                column(width = 12, leafletOutput("mapPlot", height = "400px"))
                              ),
                              
                              # Adiciona a caixa Borough no canto superior direito
                              absolutePanel(
                                top = 10, right = 10, width = 250,
                                div(
                                  style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
                                  h4("Borough:"),
                                  tags$div(style = "margin-bottom: 5px;",
                                           tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #8DD3C7; margin-right: 10px; border-radius: 50%;"), 
                                           "Manhattan"),
                                  tags$div(style = "margin-bottom: 5px;",
                                           tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #FFFFB3; margin-right: 10px; border-radius: 50%;"), 
                                           "Brooklyn"),
                                  tags$div(style = "margin-bottom: 5px;",
                                           tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #C7C4DE; margin-right: 10px; border-radius: 50%;"), 
                                           "Staten Island"),
                                  tags$div(style = "margin-bottom: 5px;",
                                           tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #FB8072; margin-right: 10px; border-radius: 50%;"), 
                                           "Queens"),
                                  tags$div(style = "margin-bottom: 5px;",
                                           tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #80B1D3; margin-right: 10px; border-radius: 50%;"), 
                                           "Bronx")
                                ),
                                absolutePanel(
                                  top = 250, right = 10, width = 250,
                                  div(
                                    style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
                                    h4("Complaint Type:"),
                                    tags$div(style = "margin-bottom: 5px;",
                                             tags$span(style = "display: inline-block; width: 20px; height: 4px; background-color: #1b9e77; margin-right: 10px;"), 
                                             "Homeless Person Assistance"),
                                    tags$div(style = "margin-bottom: 5px;",
                                             tags$span(style = "display: inline-block; width: 20px; height: 4px; background-color: #666666; margin-right: 10px;"), 
                                             "Homeless Encampment"),
                                    tags$div(style = "margin-bottom: 5px;",
                                             tags$span(style = "display: inline-block; width: 20px; height: 4px; background-color: #e6ab02; margin-right: 10px;"), 
                                             "Street Condition"),
                                    tags$div(style = "margin-bottom: 5px;",
                                             tags$span(style = "display: inline-block; width: 20px; height: 4px; background-color: #a6761d; margin-right: 10px;"), 
                                             "Street Light Condition"),
                                    tags$div(style = "margin-bottom: 5px;",
                                             tags$span(style = "display: inline-block; width: 20px; height: 4px; background-color: #e7298a; margin-right: 10px;"), 
                                             "Sanitation Condition"),
                                    tags$div(style = "margin-bottom: 5px;",
                                             tags$span(style = "display: inline-block; width: 20px; height: 4px; background-color: #d95f02; margin-right: 10px;"), 
                                             "General Construction/Plumbing")
                                  )
                                )
                              )
                              
                              
                            )
                            
                          )
                 ))
# Server logic
server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    data %>%
      filter(
        ComplaintType %in% input$complaint_type,
        weekday %in% input$day_of_week,
        IssuedDate >= input$timeSlider[1],
        IssuedDate <= input$timeSlider[2],
        Borough %in% input$boroughs
      )
  })
  
  # Complaints over time plot
  output$complaints_over_time <- renderPlotly({
    req(nrow(filtered_data()) > 0)  # Garante que há dados para exibir
    p <- ggplot(filtered_data(), aes(x = IssuedDate, color = ComplaintType)) +
      geom_line(stat = "count") +
      labs(title = "Complaints Over Time",
           x = "Date", y = "Number of Complaints") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p)  # Transformando o gráfico ggplot em um gráfico interativo
  })
  
  # Complaints by borough plot
  output$complaints_by_borough <- renderPlotly({
    req(nrow(filtered_data()) > 0)  # Garante que há dados para exibir
    p <- ggplot(filtered_data(), aes(x = Borough, fill = ComplaintType)) +
      geom_bar() +
      labs(title = "Complaints by Borough",
           x = "Borough", y = "Number of Complaints") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)  # Transformando o gráfico ggplot em um gráfico interativo
  })
  
  # Mapa para a primeira aba (Dashboard)
  output$map_dashboard <- renderLeaflet({
    req(data, garagelots)
    
    # Criando uma paleta de cores baseada nos tipos de reclamação
    complaint_types <- unique(filtered_data()$ComplaintType)
    color_palette <- RColorBrewer::brewer.pal(min(12, length(complaint_types)), "Set1")
    color_map <- colorFactor(palette = color_palette, domain = complaint_types)
    
    # Criando o mapa inicial
    map <- leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = -73.9665, lat = 40.7812, zoom = 11) %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = 5,  # Ajuste o raio conforme necessário
        color = ~color_map(ComplaintType),  # Cor baseada no tipo de reclamação
        fillColor = ~color_map(ComplaintType),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste0("<strong>Tipo de Reclamação:</strong> ", ComplaintType,
                        "<br><strong>Borough:</strong> ", Borough,
                        "<br><strong>Data:</strong> ", IssuedDate)
      ) %>%
      addLegend(
        "bottomright", 
        pal = color_map, 
        values = ~ComplaintType,
        title = "Complaint Types",
        opacity = 1
      )
    
    # Adiciona os marcadores de garagens se a checkbox estiver marcada
    if (input$showMap) {
      map <- map %>%
        addCircleMarkers(
          data = garagelots,
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~paste(
            "<strong>Borough:</strong>", Borough, "<br>",
            "<strong>Business:</strong>", Business.Name
          ),
          radius = 5,
          color = "orange",
          fillOpacity = 0.7
        )
    }
    
    map
  })
  
  # Gráfico adicional na nova aba
  # Reactive filtered dataset for additional info tab
  filtered_data_additional <- reactive({
    data %>%
      filter(
        ComplaintType %in% input$complaint_type_additional,
        weekday %in% input$day_of_week_additional,
        IssuedDate >= input$timeSlider_additional[1],
        IssuedDate <= input$timeSlider_additional[2],
        Borough %in% input$boroughs_additional
      )
  })
  
  # Gráfico de heatmap na aba "Additional Info"
  output$additional_plot <- renderPlotly({
    req(nrow(filtered_data_additional()) > 0)  # Garante que há dados para exibir
    
    # Criando uma nova variável com a coluna 'hour' adicionada (não modificando diretamente o reativo)
    data_with_hour <- filtered_data_additional() %>%
      mutate(hour = Hour)  # Criando a nova coluna 'hour' com a informação da coluna 'Hour'
    
    # Agrupando os dados por dia da semana e hora
    heatmap_data <- data_with_hour %>%
      count(weekday, hour)  # Contando o número de reclamações por dia da semana e hora
    
    # Gerando o gráfico de heatmap com ggplot
    p <- ggplot(heatmap_data, aes(x = hour, y = weekday, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +  # Gradiente de cores
      labs(title = "Complaints Distribution by Hour and Day of Week",
           x = "Hour of Day",
           y = "Day of Week",
           fill = "Number of Complaints") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            legend.position = "right")
    
    ggplotly(p)  # Transformando o gráfico ggplot em um gráfico interativo
  })
  
  # Mapa para a segunda aba (Additional Info)
  output$map_additional <- renderLeaflet({
    req(nrow(filtered_data_additional()) > 0)  # Garante que há dados para exibir
    
    # Criando uma paleta de cores baseada nos tipos de reclamação
    complaint_types <- unique(filtered_data_additional()$ComplaintType)
    color_palette <- RColorBrewer::brewer.pal(min(12, length(complaint_types)), "Set1")
    color_map <- colorFactor(palette = color_palette, domain = complaint_types)
    
    # Criando o mapa inicial para a segunda aba
    map <- leaflet(filtered_data_additional()) %>%
      addTiles() %>%
      setView(lng = -73.9665, lat = 40.7812, zoom = 11) %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = 5,  # Ajuste o raio conforme necessário
        color = ~color_map(ComplaintType),  # Cor baseada no tipo de reclamação
        fillColor = ~color_map(ComplaintType),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste0("<strong>Tipo de Reclamação:</strong> ", ComplaintType,
                        "<br><strong>Borough:</strong> ", Borough,
                        "<br><strong>Data:</strong> ", IssuedDate)
      ) %>%
      addLegend(
        "bottomright", 
        pal = color_map, 
        values = ~ComplaintType,
        title = "Complaint Types",
        opacity = 1
      )
    
    # Adiciona os marcadores de garagens se a checkbox estiver marcada
    if (input$showMap_additional) {
      map <- map %>%
        addCircleMarkers(
          data = garagelots,
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~paste(
            "<strong>Borough:</strong>", Borough, "<br>",
            "<strong>Business:</strong>", Business.Name
          ),
          radius = 5,
          color = "orange",
          fillOpacity = 0.7
        )
    }
    
    map
  })
  filtered_zip <- reactive({
    req(input$boroughs_additional, input$complaint_type_additional)
    data %>%
      filter(
        Borough %in% input$boroughs_additional,
        ComplaintType %in% input$complaint_type_additional,
        Date >= input$timeSlider_additional[1],
        Date <= input$timeSlider_additional[2]
      )
  })
  # Borough colors
  borough_colors <- reactive({
    boroughs <- unique(nodes$Borough)
    colors <- RColorBrewer::brewer.pal(min(length(boroughs), 8), "Set3")
    setNames(colors, boroughs)
  })
  
  # Complaint type colors
  complaint_colors <- reactive({
    types <- unique(edges_filtered_with_count$type)
    num_types <- length(types)
    colors <- RColorBrewer::brewer.pal(max(3, min(num_types, 12)), "Dark2")
    if (num_types > length(colors)) {
      colors <- rep(colors, length.out = num_types)
    }
    setNames(colors, types)
  })
  
  
  filtered_edges <- reactive({
    edges_filtered_with_count %>%
      filter(type %in% input$complaint_type_Networkplot, n >= input$threshold)
  })
  
  
  filtered_nodes <- reactive({
    nodes %>%
      filter(id %in% c(filtered_edges()$from, filtered_edges()$to)) %>%
      filter(Borough %in% input$boroughs_Netwotkplot) %>%
      distinct(id, .keep_all = TRUE)
  })
  
  
  output$networkPlot <- renderVisNetwork({
    req(filtered_edges())
    req(filtered_nodes())
    
    color_map <- borough_colors()
    
    nodes <- filtered_nodes() %>%
      mutate(color = color_map[Borough],
             label = id, size = 15)
    
    edges <- filtered_edges() %>%
      filter(from %in% nodes$id & to %in% nodes$id) %>%
      mutate(color = sapply(type, function(x) complaint_colors()[[x]]),
             width = 3)  # Define a largura padrão para as linhas)
    
    visNetwork(nodes, edges) %>%
      visNodes(color = list(background = nodes$color),
               font = list(size = 17)) %>%  # Define o tamanho da fonte) %>%
      visEdges(color = list(color = edges$color)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(dragNodes = TRUE, dragView = TRUE)
  })
  
  
  output$mapPlot <- renderLeaflet({
    req(filtered_nodes())
    req(all(c("Latitude", "Longitude") %in% names(filtered_nodes())))
    
    map_nodes <- filtered_nodes() %>%
      filter(!is.na(Latitude) & !is.na(Longitude))
    
    color_map <- borough_colors()
    
    leaflet(data = map_nodes) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        label = ~paste("ZIP:", id, "<br>Borough:", Borough),  # Rótulo que ficará visível ao passar o mouse
        labelOptions = labelOptions(noHide = TRUE, direction = 'top'),  # Garante que o label fique sempre visível
        radius = 5,
        color = ~color_map[Borough],
        fillColor = ~color_map[Borough],
        fillOpacity = 0.8
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

