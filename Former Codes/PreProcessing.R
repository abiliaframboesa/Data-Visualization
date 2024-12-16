# Work Objectives:
# - Identify the best and worst agencies and their locations.
# - Analyze the most frequent and severe complaints in each borough; assess the time taken to resolve them.
# - Investigate where the complaints are located and what factors might influence their occurrence, such as location and type of building.

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)

# Reading a local file
data <- fread("C:/Users/dcssi/OneDrive/Ambiente de Trabalho/2ºano_Mestrado/VD/NYC_311_Data_20241009.csv")

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

##############
# Categories
##############
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

