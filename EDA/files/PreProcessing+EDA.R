# Work Objectives:
# - Identify the best and worst agencies and their locations.
# - Analyze the most frequent and severe complaints in each borough; assess the time taken to resolve them.
# - Investigate where the complaints are located and what factors might influence their occurrence, such as location and type of building.

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


# Reading a local file
data <- fread("C:/Users/dcssi/OneDrive/Ambiente de Trabalho/2ºano_Mestrado/VD/NYC_311_Data_20241009.csv")
data <- fread("C:/Users/Ana Luísa/Desktop/Mestrado em Bioinformática e Biologia Computacional/2 ano/Visualização de Dados/Projeto/NYC_311_Data_20241009.csv")


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
View(data)

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



# --------------------------------- EDA - Visualization plots -----------------------------------------------


# ------------------------- 1. Evolution of Number of Problems by Borough ------------------------------

# Filter to exclude rows where Borough is "Unspecified" and convert IssuedDate to <Date> format
date_format <- data %>%
  filter(Borough != "Unspecified") %>%  # Exclui as linhas com Borough "Unspecified"
  mutate(IssuedDate = as.Date(IssuedDate))  # Converte a coluna IssuedDate para o formato Date


# Calculate the number of complaints by date and borough and group by date and borough
daily_counts <- date_format %>%
  group_by(IssuedDate, Borough) %>%
  summarise(Complaints = n()) %>%
  ungroup()


# Create a line plot with ggplot2
ggplot(daily_counts, aes(x = IssuedDate, y = Complaints, color = Borough)) +
  geom_line(linewidth = 1.5) +  # Increase line width for clearer lines
  labs(title = "Evolution of Number of Complaints by Borough",
       x = "Date",
       y = "Number of Complaints") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +  # Show all dates with a format
  theme_minimal() +
  theme(
    legend.position = "right",  # Move the legend to the right
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate the x-axis labels vertically
    legend.text = element_text(size = 14),  # Increase the size of the legend text
    legend.title = element_text(size = 18),  # Increase the size of the legend title
    plot.title = element_text(size = 20, face = "bold"),  # Increase the plot title size and make it bold
    axis.title.x = element_text(size = 20),  # Increase the x-axis title size
    axis.title.y = element_text(size = 20)   # Increase the y-axis title size
  )


# Conclusions: Brooklyn has the most complaints, while Staten Island has the fewest complaints



# --------------------------- 2. Density of Complaints by Borough ---------------------------------------

# Ensure latitude and longitude are in numeric form and drop rows with missing values
date_format <- data %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(across(c(Latitude, Longitude), as.numeric))


# Convert to spatial data frame
complaints_sf <- st_as_sf(date_format, coords = c("Longitude", "Latitude"), crs = 4326)


# Load borough boundaries
boroughs <- st_read("geo_export_bca88cbd-0aab-46db-891b-2acf63e33536.shp")


# Spatial join complaints to boroughs
complaints_by_borough <- st_join(complaints_sf, boroughs)


# Aggregate complaint counts by borough
borough_complaint_counts <- complaints_by_borough %>%
  group_by(Borough) %>%
  summarise(Complaints = n()) %>%
  st_as_sf() # Convert to spatial object


# Perform the spatial join using st_join()
boroughs_with_complaints <- st_join(boroughs, borough_complaint_counts, join = st_intersects)


# Check the result
head(boroughs_with_complaints)


# Calculate centroids and retain borough names, suppressing warning
boroughs_centroids <- suppressWarnings(
  boroughs %>%
    st_transform(crs = 4326) %>%      # Transform to a common CRS (WGS 84)
    st_centroid() %>%                 # Get centroids
    st_coordinates() %>%              # Get coordinates
    as.data.frame() %>%               # Convert to a data frame
    bind_cols(boroughs %>% st_drop_geometry())  # Retain borough names
)

boroughs_centroids


# Plot the borough boundaries with complaint density and centroids for labeling
ggplot() +
  # Plot borough boundaries with thicker lines
  geom_sf(data = boroughs, fill = "lightgray", color = "black", alpha = 0.5, size = 5) +
  
  # Overlay density of complaints based on lat/long
  stat_density_2d(data = st_drop_geometry(complaints_sf), 
                  aes(x = st_coordinates(complaints_sf)[,1], 
                      y = st_coordinates(complaints_sf)[,2], 
                      fill = after_stat(level)),
                  geom = "polygon", 
                  contour = TRUE,
                  alpha = 0.6) +
  
  # Color scale for density
  scale_fill_viridis_c(option = "plasma", name = "Complaint Density") +
  
  # Add borough names using computed centroids
  geom_text(data = boroughs_centroids, 
            aes(x = X, y = Y, label = boro_name),  # Use X, Y from centroid coordinates and boro_name for label
            color = "black", 
            fontface = "bold", 
            size = 5) +  # Adjust font size as needed
  
  # Title, subtitle, and axis labels
  labs(title = "NYC Complaints Density by Borough",
       x = "Longitude",  # Label for the x-axis
       y = "Latitude") +  # Label for the y-axis
  
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())




# ---------------------- ANA LU ------------------------
# DIAS DA SEMANA

# Ensure the weekdays are in English for consistency
Sys.setlocale("LC_TIME", "C")

# Extract the hour of the day from the IssuedDate
data$hour <- format(data$IssuedDate, "%H")

# Count complaints by weekday and hour
complaint_counts <- data %>%
  group_by(weekday, hour) %>%
  summarise(count = n())

# Convert weekday to an ordered factor to keep days in order
complaint_counts$weekday <- factor(complaint_counts$weekday, 
                                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Plot the heatmap
heatmap_24h <- ggplot(complaint_counts, aes(x = hour, y = weekday, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Number of Complaints per Hour and Day of the Week",
       x = "Hour of the Day",
       y = "Day of the Week",
       fill = "Complaint Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(heatmap_24h)
ggsave("C:/Users/Ana Luísa/Desktop/graficos/heatmap_24h.png", plot = heatmap_24h, width = 6, height = 4, dpi = 300)


# ----- DE 3 EM 3 HORAS 

# Ensure the weekdays are in English for consistency
Sys.setlocale("LC_TIME", "C")

# Extract the hour of the day from the IssuedDate
data$hour <- as.numeric(format(data$IssuedDate, "%H"))

# Define the hour ranges and create a new column for time periods
data$hour_group <- cut(data$hour, 
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), 
                       labels = c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21", "21-24"),
                       include.lowest = TRUE,
                       right = FALSE)

# Count complaints by weekday and hour group
complaint_counts <- data %>%
  group_by(weekday, hour_group) %>%
  summarise(count = n())

# Convert weekday to an ordered factor to keep days in order
complaint_counts$weekday <- factor(complaint_counts$weekday, 
                                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Plot the heatmap
heatmap_3_3 <- ggplot(complaint_counts, aes(x = hour_group, y = weekday, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Number of Complaints per Time Period and Day of the Week",
       x = "Time Period",
       y = "Day of the Week",
       fill = "Complaint Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap_3_3)
ggsave("C:/Users/Ana Luísa/Desktop/graficos/heatmap_3_3.png", plot = heatmap_3_3, width = 6, height = 4, dpi = 300)


#  ------------------- PIEPLOT OF COMPLAINT TYPES 


# Summarize the data into a data frame
complaint_summary <- as.data.frame(table(data$ComplaintType))
colnames(complaint_summary) <- c("ComplaintType", "Count")

# Sort by count and select the top 10 complaint types
top10_complaints <- complaint_summary[order(-complaint_summary$Count), ][1:10, ]


# Calculate percentages for each ComplaintType
top10_complaints$Percentage <- round(top10_complaints$Count / sum(top10_complaints$Count) * 100, 1)

# Reorder the ComplaintType factor by Count (in descending order)
top10_complaints$ComplaintType <- factor(top10_complaints$ComplaintType, 
                                         levels = top10_complaints$ComplaintType[order(-top10_complaints$Count)])

# Create the pie chart
ggplot(top10_complaints, aes(x = "", y = Count, fill = ComplaintType)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Complaint Types") +
  theme_void()

# ------- PIE PLOT DE COMPLAINTS POR CATEGORIAS FALTA POR POR ORDEM

# Summarize the data by Category to count the number of complaints
category_summary <- data %>%
  group_by(Category) %>%
  summarise(Count = n(), .groups = "drop")

# Calculate percentages
category_summary <- category_summary %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the pie chart with percentages
ggplot(category_summary, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Convert to pie chart
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  # Add percentages
  labs(title = "Complaints by Category") +
  theme_void() +  # Remove axis labels and background
  theme(legend.title = element_blank())  # Optional: Remove the legend title


# ---------- PIE PLOT DOS DESCRIPTORS PARA NOISE RESIDENTIAL

# Filtrar os dados onde "Complaint Type" é "Noise - Residential"
filtered_data <- data[data$ComplaintType == "Noise - Residential", ]

# Criar uma tabela de frequência para "Descriptor"
descriptor_counts <- as.data.frame(table(filtered_data$Descriptor))
colnames(descriptor_counts) <- c("Descriptor", "Count")


# Criar o gráfico circular com percentagens
ggplot(descriptor_counts, aes(x = "", y = Count, fill = Descriptor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Quantidade de Cada Descriptor para Noise - Residential") +
  theme_void() + # Remove elementos desnecessários
  theme(axis.text.x = element_blank())  # Remove os rótulos do eixo X


# ---------- HEAT MAP DAS HORAS E DIAS DA SEMANA DO NOISE RESIDENTIAL - LOUD MUSIC/PARTY 

# Ensure the weekdays are in English for consistency
Sys.setlocale("LC_TIME", "C")


# Filtrar os dados onde ComplaintType é "Noise - Residential" e Descriptor é "Loud Music/Party"
filtered_data <- data %>%
  filter(ComplaintType == "Noise - Residential", Descriptor == "Loud Music/Party")


# Extract the hour of the day from the IssuedDate
filtered_data$hour <- format(filtered_data$IssuedDate, "%H")


# Count complaints by weekday and hour
complaint_counts <- filtered_data %>%
  group_by(weekday, hour) %>%
  summarise(count = n())

# Convert weekday to an ordered factor to keep days in order
complaint_counts$weekday <- factor(complaint_counts$weekday, 
                                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Plot the heatmap
heatmap_24h <- ggplot(complaint_counts, aes(x = hour, y = weekday, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Number of Complaints per Hour and Day of the Week",
       x = "Hour of the Day",
       y = "Day of the Week",
       fill = "Complaint Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Print the plot
print(heatmap_24h)


# ----- SPATIAL HEATMAP 

# Filtrar os dados onde ComplaintType é "Noise - Residential" e Descriptor é "Loud Music/Party"
filtered_data <- data %>%
  filter(ComplaintType == "Noise - Residential", Descriptor == "Loud Music/Party")

cleaned_data <- filtered_data %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# Check if there are still missing values in Latitude or Longitude
sum(is.na(cleaned_data$Latitude))  # Should return 0
sum(is.na(cleaned_data$Longitude)) # Should return 0

#View(cleaned_data)

# Aggregate the data by latitude and longitude to count the number of complaints per location
heatmap_data <- cleaned_data %>%
  group_by(Latitude, Longitude) %>%
  summarise(Count = n(), .groups = "drop")

# Load New York City shapefile (you need to download this shapefile beforehand)
nyc_shapefile <- st_read("geo_export_bca88cbd-0aab-46db-891b-2acf63e33536.shp")


# Convert cleaned data to sf object for spatial mapping
heatmap_sf <- st_as_sf(heatmap_data, coords = c("Longitude", "Latitude"), crs = 4326)


ggplot() +
  geom_sf(data = nyc_shapefile, fill = "lightgray") +  # NYC boundary shapefile
  stat_density_2d(
    data = heatmap_data,  # Use the original data frame with lat/lon
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "blue", high = "red") +  # Density color gradient
  scale_alpha(range = c(0.1, 0.4), guide = "none") +  # Adjust transparency
  labs(title = "Density Map: Loud Music/Party Complaints in NYC",
       x = "Longitude", y = "Latitude", fill = "Density") +
  theme_minimal()



# ----------------- TIME SERIES COM DAY OF THE MONTH

library(dplyr)
library(ggplot2)

# Ensure IssuedDate is in Date format
data$IssuedDate <- as.Date(data$IssuedDate)

# Aggregate data by IssuedDate and ComplaintType
time_series_data <- data %>%
  group_by(IssuedDate, ComplaintType) %>%
  summarise(Count = n(), .groups = "drop")

# Optional: Filter for top 5 complaint types by total count
top_complaints <- time_series_data %>%
  group_by(ComplaintType) %>%
  summarise(Total = sum(Count)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 5) %>%
  pull(ComplaintType)

filtered_time_series <- time_series_data %>%
  filter(ComplaintType %in% top_complaints)

# Plot the time series
ggplot(filtered_time_series, aes(x = IssuedDate, y = Count, color = ComplaintType)) +
  geom_line() +
  labs(
    title = "Time Series of Complaints by Complaint Type",
    x = "Day of the month",
    y = "Number of Complaints",
    color = "Complaint Type"
  ) +
  scale_x_date(
    date_breaks = "1 day",     # Show every day as a tick mark
    date_labels = "%d"  # Format dates as YYYY-MM-DD
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate for better readability

# -------- COM DATAS E WEEKDAYS

library(dplyr)
library(ggplot2)

# Add a weekday column to your time series data
filtered_time_series <- filtered_time_series %>%
  mutate(Weekday = weekdays(IssuedDate))  # Get the weekday name from IssuedDate

# Plot the time series with weekdays on the x-axis
ggplot(filtered_time_series, aes(x = IssuedDate, y = Count, color = ComplaintType)) +
  geom_line() +
  labs(
    title = "Time Series of Complaints by Complaint Type",
    x = "Date (Weekday)",
    y = "Number of Complaints",
    color = "Complaint Type"
  ) +
  scale_x_date(
    date_breaks = "1 day",     # Show every day as a tick mark
    date_labels = "%d"   # Format dates as YYYY-MM-DD
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate for better readability
  scale_x_date(
    breaks = "1 day",           # Breaks by each day
    labels = function(x) paste(weekdays(x), "\n", format(x, "%Y-%m-%d"))  # Display weekday + date
  )

# ----------- COM Blocked Driveway e Illegal Parking A BOLD


library(dplyr)
library(ggplot2)

# Adicionar a coluna Weekday ao seu conjunto de dados
filtered_time_series <- filtered_time_series %>%
  mutate(Weekday = weekdays(IssuedDate))  # Obter o nome do dia da semana a partir de IssuedDate

# Definir os complaint types que você quer destacar
highlight_complaints <- c("Blocked Driveway", "Illegal Parking")

# Plotar a série temporal
ggplot(filtered_time_series, aes(x = IssuedDate, y = Count, color = ComplaintType)) +
  geom_line(data = filtered_time_series %>% filter(ComplaintType %in% highlight_complaints), 
            aes(color = ComplaintType), size = 1.5) +  # Destacar com linha mais grossa
  geom_line(data = filtered_time_series %>% filter(!ComplaintType %in% highlight_complaints), 
            aes(color = ComplaintType), size = 0.8) +  # Linhas mais finas para os outros complaint types
  labs(
    title = "Time Series of Complaints by Complaint Type",
    x = "Date (Weekday)",
    y = "Number of Complaints",
    color = "Complaint Type"
  ) +
  scale_x_date(
    date_breaks = "1 day",     # Mostrar cada dia como um marco
    date_labels = "%d"         # Mostrar apenas o dia do mês
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotacionar para melhor legibilidade
  scale_x_date(
    breaks = "1 day",           # Breaks by each day
    labels = function(x) paste(weekdays(x), "\n", format(x, "%Y-%m-%d"))  # Display weekday + date
  )

# ------ SÓ COM OS NOISES E O UNSANITARY CONDITIONS

# Filter the data to include only the desired complaint types
filtered_complaints <- filtered_time_series %>%
  filter(ComplaintType %in% c("Noise - Residential", "Noise - Street/Sidewalk", "UNSANITARY CONDITION")) %>%
  mutate(Weekday = weekdays(IssuedDate))  # Add weekday column if not already added

# Plot the time series with custom colors for complaint types
ggplot(filtered_complaints, aes(x = IssuedDate, y = Count, color = ComplaintType)) +
  geom_line(size = 1) +  # Set line size for better visibility
  labs(
    title = "Time Series of Selected Complaints by Complaint Type",
    x = "Date (Weekday)",
    y = "Number of Complaints",
    color = "Complaint Type"
  ) +
  scale_color_manual(
    values = c(
      "Noise - Residential" = "green",
      "Noise - Street/Sidewalk" = "lightblue",
      "UNSANITARY CONDITION" = "pink"
    )
  ) +
  scale_x_date(
    date_breaks = "1 day",     # Show every day as a tick mark
    date_labels = "%d"         # Format dates to show only the day of the month
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate text for better readability
  scale_x_date(
    breaks = "1 day",          # Breaks by each day
    labels = function(x) paste(weekdays(x), "\n", format(x, "%Y-%m-%d"))  # Show weekday and date
  )


# --------------------------------------------------------- Dashboard Diana -------------------------------------------------------------------



# Preparar dados de nodes e edges (sem as funções extras)
nodes <- data %>%
  filter(!is.na(`Incident Zip`) & `Incident Zip` != "") %>%
  distinct(`Incident Zip`) %>%
  rename(id = `Incident Zip`)

nodes

edges <- data %>%
  filter(!is.na(`Incident Zip`) & `Incident Zip` != "") %>%
  group_by(ComplaintType) %>%
  filter(n_distinct(`Incident Zip`) > 1) %>%
  reframe(pairs = list(combn(unique(`Incident Zip`), 2, simplify = FALSE))) %>%
  filter(lengths(pairs) > 0) %>%
  unnest(pairs) %>%
  transmute(from = map_chr(pairs, 1),
            to = map_chr(pairs, 2),
            type = ComplaintType)

edges_count <- edges %>%
  count(from, to) %>%
  filter(n >= 50)

edges_filtered_with_count <- edges %>%
  left_join(edges_count, by = c("from", "to")) %>%
  filter(!is.na(n))

top_complaints <- data %>%
  count(ComplaintType) %>%
  arrange(desc(n)) %>%
  head(5)

top_complaints_types <- top_complaints$ComplaintType

edges_filtered_top_complaints <- edges_filtered_with_count %>%
  filter(type %in% top_complaints_types)


# Carregar os dados de localização dos ZIP Codes
zip_locations <- read.csv("zip_locations.csv")


# Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Rede de Reclamações por ZIP Code"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold",
                  "Threshold mínimo de reclamações:",
                  min = 1, max = 100, value = 50),
      
      checkboxGroupInput("weekday_filter",
                         "Selecione os dias da semana:",
                         choices = unique(data$weekday),
                         selected = unique(data$weekday)),
      
      sliderInput("hour_filter",
                  "Selecione a faixa de horas do dia:",
                  min = 0, max = 23, value = c(0, 23), step = 1, 
                  ticks = TRUE, animate = TRUE),
      
      
      
      checkboxGroupInput("complaint_filter",
                         "Selecione os tipos de reclamação:",
                         choices = unique(data$ComplaintType),
                         selected = top_complaints_types),
      
      actionButton("update", "Atualizar Gráficos"),
      width = 3
    ),
    
    
    mainPanel(
      # Linha superior: Grafo e Gráfico de Barras
      fluidRow(
        column(width = 7, 
               h4("Rede de Reclamações por ZIP Code", align = "center"),
               visNetworkOutput("networkPlot", height = "400px")),
        column(width = 5, 
               plotOutput("barPlot", height = "400px"))
      ),
      
      # Linha do meio: Gráfico de Evolução Temporal e Reclamações ao longo do dia
      fluidRow(
        column(width = 6, 
               plotOutput("timeSeriesPlot", height = "400px")),
        column(width = 6, 
               plotOutput("hourlyPlot", height = "400px"))
      ),
      
      # Linha inferior: Mapa
      fluidRow(
        column(width = 12, 
               h4("Mapa de Reclamações por ZIP Code", align = "center"),
               leafletOutput("mapPlot", height = "400px"))
      )
    )
    
  )
)

# Funções do Servidor (Server)
server <- function(input, output, session) {
  
  # Filtrar reclamações por dia da semana
  filtered_data_weekday <- reactive({
    data %>%
      filter(weekday %in% input$weekday_filter)
  })
  
  # Filtrar reclamações por faixa de horas
  filtered_data_by_hour <- reactive({
    filtered_data_weekday() %>%
      mutate(hour = hour(IssuedDate)) %>%
      filter(hour >= input$hour_filter[1], hour <= input$hour_filter[2])
  })
  
  # Filtrar arestas reativamente
  filtered_edges <- reactive({
    edges_filtered_with_count %>%
      filter(type %in% input$complaint_filter, n >= input$threshold)
  })
  
  # Filtrar nós reativamente
  filtered_nodes <- reactive({
    nodes %>%
      filter(id %in% c(filtered_edges()$from, filtered_edges()$to))
  })
  
  # Filtrar reclamações por tipo e hora
  filtered_data <- reactive({
    filtered_data_by_hour() %>%
      filter(ComplaintType %in% input$complaint_filter)
  })
  
  # Filtrar reclamações por ZIP Code selecionado e tipo de reclamação
  filtered_complaints_by_zip <- reactive({
    selected_zip_codes <- input$networkPlot_selected
    if (is.null(selected_zip_codes) || length(selected_zip_codes) == 0) {
      return(NULL)
    }
    filtered_data() %>%
      filter(`Incident Zip` %in% selected_zip_codes) %>%
      count(`Incident Zip`, ComplaintType) %>%
      arrange(desc(n))
  })
  
  # Renderizar o gráfico de série temporal
  output$timeSeriesPlot <- renderPlot({
    req(filtered_data_weekday())
    
    filtered_data_weekday() %>%
      mutate(date = as.Date(IssuedDate)) %>%
      count(date) %>%
      ggplot(aes(x = date, y = n)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Evolução Temporal das Reclamações",
           x = "Data",
           y = "Número de Reclamações") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20)
      )
  })
  
  # Renderizar o grafo interativo
  output$networkPlot <- renderVisNetwork({
    req(filtered_edges())
    
    complaint_types <- unique(filtered_edges()$type)
    complaint_colors <- RColorBrewer::brewer.pal(length(complaint_types), "Set3")
    
    edges <- filtered_edges() %>%
      mutate(color = sapply(type, function(x) {
        complaint_colors[which(complaint_types == x)]
      })) %>%
      select(from, to, color)
    
    nodes <- filtered_nodes() %>%
      mutate(label = id, size = 10)
    
    visNetwork(nodes, edges) %>%
      visEdges(color = list(color = edges$color)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(dragNodes = TRUE, dragView = TRUE) %>%
      visEvents(select = "function(nodes) {
        Shiny.setInputValue('networkPlot_selected', nodes.nodes);
      }")
  })
  
  # Renderizar o gráfico de barras reativo para frequências por ZIP Code
  output$barPlot <- renderPlot({
    req(filtered_complaints_by_zip())
    
    # Filtrar as reclamações por ZIP Code selecionado
    filtered_data <- filtered_complaints_by_zip()
    
    # Criar o gráfico de barras com várias barras por ComplaintType
    filtered_data %>%
      ggplot(aes(x = reorder(ComplaintType, -n), y = n, fill = ComplaintType)) +
      geom_bar(stat = "identity", show.legend = FALSE) +  # Remove a legenda para clareza
      labs(title = paste("Frequência de Reclamações no ZIP Code:", input$networkPlot_selected), 
           x = "Tipo de Reclamação", 
           y = "Frequência") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),   # Ajusta o texto do eixo X
        axis.text.y = element_text(hjust = 1, size = 14), 
        axis.title.x = element_text(size = 16),  # Aumenta o título do eixo X
        axis.title.y = element_text(size = 16),  # Aumenta o título do eixo Y
        plot.title = element_text(size = 20),    # Aumenta o título do gráfico
        legend.position = "none"                 # Esconde a legenda (opcional)
      )
  })
  
  
  # Renderizar o gráfico de horas do dia
  output$hourlyPlot <- renderPlot({
    req(filtered_data())
    
    filtered_data() %>%
      mutate(hour = hour(IssuedDate)) %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Reclamações ao Longo do Dia", 
           x = "Hora do Dia", 
           y = "Número de Reclamações") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20)
      )
  })
  
  # Renderizar o mapa
  output$mapPlot <- renderLeaflet({
    req(filtered_nodes())
    
    nodes_with_coords <- zip_locations %>%
      filter(id %in% filtered_nodes()$id)
    
    leaflet(data = nodes_with_coords) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        label = ~id,
        radius = 5,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.6,
        popup = ~paste("ZIP Code:", id)
      )
  })
}

# Rodar a aplicação Shiny
shinyApp(ui = ui, server = server)

# ------------------------------------------------ Dashboard Tomás --------------------------------------------------


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











# ------------------------------------------------ Dashboard Ana Luisa --------------------------------------------------


# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("NYC Complaints: Heatmap and Time Series"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "complaint_type",
        label = "Select Complaint Type(s):",
        choices = NULL # Dynamically populated in server
      ),
      uiOutput("dynamic_descriptor_inputs")  # Dynamic UI for Descriptors
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("heatmap")),  # Heatmap on the left
        column(6, plotOutput("time_series")) # Time series on the right
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  

  # Extract the hour from IssuedDate and convert IssuedDate to Date format
  data$hour <- format(data$IssuedDate, "%H")
  data$IssuedDate <- as.Date(data$IssuedDate)
  
  # Populate ComplaintType checkboxGroupInput options
  updateCheckboxGroupInput(session, "complaint_type", choices = unique(data$ComplaintType))
  
  # Dynamic UI for Descriptor checkboxes
  output$dynamic_descriptor_inputs <- renderUI({
    req(input$complaint_type)  # Ensure at least one ComplaintType is selected
    
    # Generate a checkboxGroupInput for each selected ComplaintType
    lapply(input$complaint_type, function(type) {
      descriptors <- data %>%
        filter(ComplaintType == type) %>%
        pull(Descriptor) %>%
        unique()
      
      checkboxGroupInput(
        inputId = paste0("descriptor_", gsub(" ", "_", type)), # Unique ID for each ComplaintType
        label = paste("Select Descriptors for", type, ":"),
        choices = descriptors
      )
    })
  })
  
  # Helper to get selected descriptors
  selected_descriptors <- reactive({
    req(input$complaint_type)  # Ensure at least one ComplaintType is selected
    
    # Collect all selected descriptors from dynamic inputs
    lapply(input$complaint_type, function(type) {
      input[[paste0("descriptor_", gsub(" ", "_", type))]]  # Access dynamic input by ID
    }) %>%
      unlist(use.names = FALSE)  # Flatten into a single vector
  })
  
  # Render the heatmap
  output$heatmap <- renderPlot({
    req(input$complaint_type)  # Ensure at least one ComplaintType is selected
    req(selected_descriptors())  # Ensure at least one Descriptor is selected
    
    # Filter data by selected ComplaintType(s) and Descriptors
    filtered_data <- data %>%
      filter(ComplaintType %in% input$complaint_type, Descriptor %in% selected_descriptors())
    
    # Count complaints by weekday and hour
    complaint_counts <- filtered_data %>%
      group_by(weekday, hour) %>%
      summarise(count = n(), .groups = "drop")
    
    # Convert weekday to an ordered factor
    complaint_counts$weekday <- factor(complaint_counts$weekday, 
                                       levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    
    # Plot the heatmap
    ggplot(complaint_counts, aes(x = hour, y = weekday, fill = count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Heatmap for", paste(input$complaint_type, collapse = ", ")),
           x = "Hour of the Day",
           y = "Day of the Week",
           fill = "Complaint Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the time series plot
  output$time_series <- renderPlot({
    req(input$complaint_type)  # Ensure at least one ComplaintType is selected
    req(selected_descriptors())  # Ensure at least one Descriptor is selected
    
    # Filter data by selected ComplaintType(s) and Descriptors
    filtered_data <- data %>%
      filter(ComplaintType %in% input$complaint_type, Descriptor %in% selected_descriptors())
    
    # Aggregate complaints by IssuedDate and ComplaintType
    time_series_data <- filtered_data %>%
      group_by(IssuedDate, ComplaintType) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Plot the time series with a line for each ComplaintType
    ggplot(time_series_data, aes(x = IssuedDate, y = Count, color = ComplaintType)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Time Series for", paste(input$complaint_type, collapse = ", ")),
           x = "Date",
           y = "Number of Complaints",
           color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)








#----------------------------------------------- Ana Luisa final? --------------

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# Define the UI
ui <- fluidPage(
  titlePanel("NYC Complaints: Heatmap, Time Series, and Map"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "complaint_type",
        label = "Select Complaint Type(s):",
        choices = NULL # Dynamically populated in server
      ),
      uiOutput("dynamic_descriptor_inputs"),  # Dynamic UI for Descriptors
      sliderInput(
        inputId = "time_range",
        label = "Select Date Range:",
        min = as.Date("2016-09-01"),  # Adjust as per your data
        max = as.Date("2024-09-30"),  # Adjust as per your data
        value = c(as.Date("2016-09-01"), as.Date("2024-09-30")),
        timeFormat = "%Y-%m-%d"
      )
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("heatmap")),  # Heatmap on the left
        column(6, plotOutput("time_series")) # Time series on the right
      ),
      leafletOutput("map", height = "400px")  # Map below the plots
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Ensure IssuedDate is in Date format and extract the hour
  data$IssuedDate <- as.POSIXct(data$IssuedDate)  # If IssuedDate is in datetime format
  data$hour <- format(data$IssuedDate, "%H")
  
  # Populate ComplaintType checkboxGroupInput options
  updateCheckboxGroupInput(session, "complaint_type", choices = unique(data$ComplaintType))
  
  # Dynamic UI for Descriptor checkboxes
  output$dynamic_descriptor_inputs <- renderUI({
    req(input$complaint_type)  # Ensure at least one ComplaintType is selected
    
    # Generate a checkboxGroupInput for each selected ComplaintType
    lapply(input$complaint_type, function(type) {
      descriptors <- data %>%
        filter(ComplaintType == type) %>%
        pull(Descriptor) %>%
        unique()
      
      checkboxGroupInput(
        inputId = paste0("descriptor_", gsub(" ", "_", type)), # Unique ID for each ComplaintType
        label = paste("Select Descriptors for", type, ":"),
        choices = descriptors
      )
    })
  })
  
  # Helper to get selected descriptors
  selected_descriptors <- reactive({
    req(input$complaint_type)  # Ensure at least one ComplaintType is selected
    
    # Collect all selected descriptors from dynamic inputs
    lapply(input$complaint_type, function(type) {
      input[[paste0("descriptor_", gsub(" ", "_", type))]]  # Access dynamic input by ID
    }) %>%
      unlist(use.names = FALSE)  # Flatten into a single vector
  })
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    req(input$complaint_type, selected_descriptors(), input$time_range)
    
    data %>%
      filter(
        ComplaintType %in% input$complaint_type,
        Descriptor %in% selected_descriptors(),
        IssuedDate >= input$time_range[1],
        IssuedDate <= input$time_range[2]
      )
  })
  
  # Render the heatmap
  output$heatmap <- renderPlot({
    req(filtered_data())
    
    # Count complaints by weekday and hour
    complaint_counts <- filtered_data() %>%
      group_by(weekday, hour) %>%
      summarise(count = n(), .groups = "drop")
    
    # Convert weekday to an ordered factor
    complaint_counts$weekday <- factor(complaint_counts$weekday, 
                                       levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    
    # Plot the heatmap
    ggplot(complaint_counts, aes(x = hour, y = weekday, fill = count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Heatmap for", paste(input$complaint_type, collapse = ", ")),
           x = "Hour of the Day",
           y = "Day of the Week",
           fill = "Complaint Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the time series plot
  output$time_series <- renderPlot({
    req(filtered_data())
    
    # Aggregate complaints by IssuedDate and ComplaintType
    time_series_data <- filtered_data() %>%
      group_by(IssuedDate, ComplaintType) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Plot the time series with a line for each ComplaintType
    ggplot(time_series_data, aes(x = IssuedDate, y = Count, color = ComplaintType)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Time Series for", paste(input$complaint_type, collapse = ", ")),
           x = "Date",
           y = "Number of Complaints",
           color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the map
  output$map <- renderLeaflet({
    req(filtered_data())
    
    # Generate a palette of colors for the selected complaint types
    complaint_types <- unique(filtered_data()$ComplaintType)
    color_palette <- colorFactor(
      palette = "Set1",  # Choose a color palette
      domain = complaint_types
    )
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = -73.9665, lat = 40.7812, zoom = 11) %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = 3,
        color = ~color_palette(ComplaintType),  # Assign color based on ComplaintType
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste("Type:", ComplaintType, "<br>",
                       "Descriptor:", Descriptor, "<br>",
                       "Date:", IssuedDate)
      ) %>%
      addLegend(
        "bottomright", 
        pal = color_palette, 
        values = ~ComplaintType,
        title = "Complaint Type",
        opacity = 0.7
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)



