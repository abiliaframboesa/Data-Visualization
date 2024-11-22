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
#dataTrimmed <- dataTrimmed %>% filter(
#  !( (is.na(`Incident Address`) | `Incident Address` == "" | grepl("^\\s*$", `Incident Address`)) & 
#       (is.na(Latitude) | Latitude == "" | grepl("^\\s*$", Latitude)) & 
#       (is.na(Longitude) | Longitude == "" | grepl("^\\s*$", Longitude)) )
#)


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

