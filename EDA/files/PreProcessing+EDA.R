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


# --------------------- tentativa ploty ----------------------------

library(plotly)
boroughs
# Criação do gráfico com ggplot
ggplot_plot <- ggplot() +
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

# Transforming the ggplot into an interactive plotly plot with zoom functionality
interactive_plot <- ggplotly(ggplot_plot)

# Show the interactive plot
interactive_plot









#install.packages("ggiraph")
library(ggiraph)

# Criação do gráfico com ggplot (semelhante ao código anterior)
ggplot_plot <- ggplot() +
  geom_sf(data = boroughs, fill = "lightgray", color = "black", alpha = 0.5, size = 5) +
  stat_density_2d(data = st_drop_geometry(complaints_sf), 
                  aes(x = st_coordinates(complaints_sf)[,1], 
                      y = st_coordinates(complaints_sf)[,2], 
                      fill = after_stat(level)),
                  geom = "polygon", 
                  contour = TRUE,
                  alpha = 0.6) +
  scale_fill_viridis_c(option = "plasma", name = "Complaint Density") +
  geom_text(data = boroughs_centroids, 
            aes(x = X, y = Y, label = boro_name), 
            color = "black", 
            fontface = "bold", 
            size = 5) +
  labs(title = "NYC Complaints Density by Borough",
       x = "Longitude", 
       y = "Latitude") + 
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Criando o gráfico interativo
interactive_plot <- girafe(ggobj = ggplot_plot)

# Mostrar o gráfico interativo com zoom
interactive_plot









library(leaflet)
library(dplyr)
library(sf)

# Load boroughs shapefile
boroughs <- st_read("geo_export_bca88cbd-0aab-46db-891b-2acf63e33536.shp")

# Ensure latitude and longitude are numeric and remove rows with missing values
date_format <- data %>% 
  filter(!is.na(Latitude), !is.na(Longitude)) %>% 
  mutate(across(c(Latitude, Longitude), as.numeric))

# Convert to spatial data frame
complaints_sf <- st_as_sf(date_format, coords = c("Longitude", "Latitude"), crs = 4326)

# Spatial join complaints to boroughs
complaints_by_borough <- st_join(complaints_sf, boroughs)

# Aggregate complaint counts by borough
borough_complaint_counts <- complaints_by_borough %>%
  group_by(Borough) %>%
  summarise(Complaints = n(), .groups = 'drop') %>%
  st_as_sf()

# Create a color palette for complaint density
pal <- colorNumeric(palette = "YlOrRd", domain = borough_complaint_counts$Complaints)

# Create the leaflet map
leaflet() %>%
  # Add borough boundaries
  addPolygons(data = boroughs, 
              fillColor = ~pal(Complaints),  # Color by complaint density
              color = "black", 
              weight = 1, 
              fillOpacity = 0.5, 
              popup = ~boro_name) %>%
  # Add points for individual complaints
  addCircleMarkers(data = complaints_sf, 
                   radius = 3, 
                   color = "red", 
                   fillOpacity = 0.7, 
                   popup = ~paste("Complaint ID:", row_number())) %>%
  # Add a legend for complaint density
  addLegend(pal = pal, 
            values = borough_complaint_counts$Complaints, 
            opacity = 0.7, 
            title = "Complaint Density", 
            position = "bottomright") %>%
  # Set map view (zoom level and center)
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordinates for NYC


class(complaints_sf)  # Deve retornar "sf" (Simple Feature)
st_crs(boroughs)  # Verificar o CRS dos bairros
st_crs(complaints_sf)  # Verificar o CRS das queixas


# Verificar a estrutura do objeto boroughs
str(boroughs)

# Verifique o conteúdo das geometrias para garantir que os polígonos estão presentes
boroughs$geometry
library(leaflet)

# Criar o mapa inicial com as fronteiras dos bairros
leaflet(boroughs) %>%
  addPolygons(
    fillColor = "lightgray", 
    color = "black", 
    weight = 1, 
    fillOpacity = 0.5, 
    popup = ~boro_name
  ) %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordenadas de NYC


# Forçar a renderização no Viewer
m



# Verificar o bounding box dos bairros para garantir que as coordenadas estão corretas
st_bbox(boroughs)


# Criar o mapa inicial com as fronteiras dos bairros
leaflet() %>%
  # Adicionar os bairros com polígonos
  addPolygons(data = boroughs, 
              fillColor = "lightgray", 
              color = "black", 
              weight = 1, 
              fillOpacity = 0.5, 
              popup = ~boro_name) %>%
  
  # Definir a visualização inicial do mapa
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordenadas de NYC



# ----------------- tentativa leaflet -----------
# Calcular a densidade de reclamações de ruído
# Como queremos um mapa de densidade de pontos, podemos criar uma variável de densidade com base na contagem de reclamações
complaints_count <- complaints_sf %>%
  st_transform(crs = 4326) %>%  # Assegura que a projeção está correta
  st_coordinates() %>%          # Extrair coordenadas de latitude e longitude
  as.data.frame() %>%
  mutate(density = 1)           # Assumimos que cada ponto é uma unidade de densidade

# Definir a paleta de cores para a densidade
pal <- colorNumeric(palette = "plasma", domain = complaints_count$density)

# Criar o mapa interativo com leaflet
leaflet() %>%
  # Adicionar os limites dos boroughs com cores
  addPolygons(data = boroughs, 
              fillColor = "lightgray", 
              color = "black", 
              weight = 1, 
              fillOpacity = 0.5, 
              popup = ~boro_name) %>%
  
  # Adicionar os pontos de reclamação de ruído
  addCircleMarkers(data = complaints_sf, 
                   radius = 3, 
                   color = ~pal(density),  # Colorir os pontos de acordo com a densidade
                   fillOpacity = 0.7, 
                   popup = ~paste("Reclamação ID:", row_number())) %>%
  
  # Adicionar legenda para densidade
  addLegend(pal = pal, values = complaints_count$density, opacity = 0.7, title = "Densidade de Reclamações", position = "bottomright") %>%
  
  # Definir a posição inicial e o zoom
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordenadas de NYC












# Realizando uma junção espacial entre os dados de reclamações e os bairros
complaints_by_borough <- st_join(complaints_sf, boroughs)

complaints_by_borough

# Contagem das reclamações por borough
borough_complaints <- complaints_by_borough %>%
  group_by(boro_name) %>%
  summarise(Complaints = n()) %>%
  st_as_sf()


borough_complaints

# Definir a paleta de cores para a densidade
pal <- colorNumeric(palette = "plasma", domain = borough_complaints$Complaints)

pal

# Criar o mapa interativo com leaflet
leaflet() %>%
  # Adicionar os limites dos boroughs com cores
  addPolygons(data = boroughs, 
              fillColor = ~pal(Complaints),  # Colorir os bairros de acordo com o número de reclamações
              color = "black", 
              weight = 1, 
              fillOpacity = 0.5, 
              popup = ~boro_name) %>%
  
  # Adicionar os pontos de reclamação de ruído
  addCircleMarkers(data = complaints_sf, 
                   radius = 3, 
                   color = "red",  # Definir a cor dos pontos de reclamação
                   fillOpacity = 0.7, 
                   popup = ~paste("Reclamação ID:", row_number())) %>%
  
  # Adicionar legenda para densidade
  addLegend(pal = pal, values = borough_complaints$Complaints, opacity = 0.7, title = "Densidade de Reclamações", position = "bottomright") %>%
  
  # Definir a posição inicial e o zoom
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordenad










# Agregar o número de reclamações por bairro
borough_complaints <- complaints_by_borough %>%
  group_by(boro_name) %>%
  summarise(Complaints = n()) %>%
  st_as_sf()  # Garantir que ainda seja um objeto espacial

# Verifique os dados agregados de reclamações
print(borough_complaints)

# Agora, associamos as reclamações aos bairros, e criamos uma paleta para a visualização
pal <- colorNumeric(palette = "plasma", domain = borough_complaints$Complaints)

# Criar o mapa interativo com o leaflet
leaflet() %>%
  # Adicionar os bairros ao mapa
  addPolygons(data = boroughs, 
              fillColor = ~pal(Complaints),  # Colorir os bairros de acordo com o número de reclamações
              color = "black", 
              weight = 1, 
              fillOpacity = 0.5, 
              popup = ~boro_name) %>%
  
  # Adicionar pontos de reclamações (opcional, caso queira visualizar a localização exata)
  addCircleMarkers(data = complaints_sf, 
                   radius = 3, 
                   color = "red",  # Cor dos pontos
                   fillOpacity = 0.7, 
                   popup = ~paste("Reclamação ID:", row_number())) %>%
  
  # Adicionar legenda para a visualização da densidade de reclamações
  addLegend(pal = pal, values = borough_complaints$Complaints, opacity = 0.7, title = "Densidade de Reclamações", position = "bottomright") %>%
  
  # Definir a posição inicial do mapa e o zoom
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordenadas de NYC










# ---------------------- ZOOM noise complaints ------------------------

# Preparar os dados de reclamações de ruído e converter para formato espacial
noise_complaints <- date_format %>%
  filter(Category == "Noise Complaints") %>%  # Filtrar apenas reclamações de ruído
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Converter para sf com coordenadas

# Realizar o join espacial para adicionar o nome do borough a cada reclamação de ruído
noise_complaints_with_borough <- st_join(noise_complaints, boroughs, join = st_intersects)


noise_complaints_with_borough


# Gerar o mapa com Leaflet
leaflet() %>%
  # Adicionar mapa base
  addTiles() %>%
  
  # Adicionar os limites dos boroughs
  addPolygons(data = boroughs, color = "blue", fillOpacity = 0.2, weight = 2,
              popup = ~paste("Borough:", boro_name)) %>%  # Nome do borough no popup
  
  # Adicionar os pontos de reclamação de ruído
  addCircleMarkers(data = noise_complaints_with_borough, 
                   radius = 3, color = "red", fillOpacity = 0.7,
                   popup = ~paste("Reclamação de Ruído no Borough:", boro_name)) %>%
  
  # Definir a posição inicial e o nível de zoom
  setView(lng = -74.0060, lat = 40.7128, zoom = 11)



# -----------------------
# Criar um mapa com os limites dos boroughs e os pontos
leaflet() %>%
  addTiles() %>%  # Adicionar mapa base
  
  # Adicionar limites dos boroughs
  addPolygons(data = boroughs, color = "blue", fillOpacity = 0.2, weight = 1,
              popup = ~paste("Borough:", boro_name)) %>%  # Exibir nome do borough no popup
  
  # Adicionar os pontos (reclamações de ruído) da tabela `dados`
  addCircleMarkers(
    data = noise_complaints_with_borough,
    ~longitude, ~latitude,
    radius = 3,
    color = "red",
    fillOpacity = 0.7,
    popup = ~paste("Local:", nome)
  ) %>%
  setView(lng = -74.0060, lat = 40.7128, zoom = 11)  # Centralizar o mapa

# ------------------------------------------------- API ---------------------------

# Buscar os limites dos boroughs de Nova Iorque
nyc_boroughs <- opq(bbox = "New York City, USA") %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "10") %>%  # Nível de administração dos boroughs
  osmdata_sf()

# Extrair o contorno dos boroughs
boroughs_sf <- nyc_boroughs$osm_multipolygons


# Filtrar e preparar dados de ruído
noise_data <- date_format %>%
  filter(Category == "Noise Complaints") %>%  # Apenas linhas onde a categoria é "Noise Complaints"
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Converter para sf com coordenadas

View(noise_data)




# Criar o mapa interativo com Leaflet
leaflet() %>%
  # Adicionar o mapa de fundo da OpenStreetMap
  addTiles() %>%
  
  # Adicionar os limites dos boroughs carregados do shapefile
  addPolygons(data = boroughs_sf, color = "blue", fillOpacity = 0.2, weight = 2) %>%
  
  # Adicionar os pontos de reclamação de ruído
  addCircleMarkers(data = noise_data, radius = 3, color = "red", fillOpacity = 0.7,
                   popup = ~paste("Reclamação de Ruído")) %>%
  
  # Definir a posição inicial e o nível de zoom para Nova Iorque
  setView(lng = -74.0060, lat = 40.7128, zoom = 11) %>%
  
  # Adicionar controles de zoom para mais interatividade
  addEasyButton(easyButton(
    icon="fa-globe", title="Reset Zoom",
    onClick=JS("function(btn, map){ map.setView([40.7128, -74.0060], 11); }")
  )) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Zoom to Noise Data",
    onClick=JS("function(btn, map){ map.fitBounds(leafletPip.bbox(noise_data)); }")
  ))


















# Criar o mapa interativo com leaflet
leaflet() %>%
  # Adicionar o mapa de fundo da OpenStreetMap
  addTiles() %>%
  
  # Adicionar os limites dos boroughs
  addPolygons(data = boroughs_sf, color = "blue", fillOpacity = 0.2, weight = 2) %>%
  
  # Adicionar os pontos de reclamação de ruído
  addCircleMarkers(data = noise_data, radius = 3, color = "red", fillOpacity = 0.7,
                   popup = ~paste("Reclamação de Ruído")) %>%
  
  # Definir a posição inicial e o nível de zoom para NYC
  setView(lng = -74.0060, lat = 40.7128, zoom = 11)



noise_data
View(noise_data)
View(date_format)




library(dplyr)
library(leaflet)
library(osmdata)
library(sf)


##

#tentativa leaflet

# Verificar e garantir que a coluna correta será utilizada
community_boards <- community_boards %>%
  mutate(
    community_board_id = as.numeric(gsub("Community Board ", "", community_board))
  )
# Definir uma paleta de cores para os Boroughs
library(RColorBrewer)
borough_colors <- brewer.pal(n = length(unique(boroughs$boro_name)), name = "Set3")

# Criar um mapa interativo com o leaflet
leaflet() %>%
  # Adicionar os limites dos Boroughs com cores
  addPolygons(data = boroughs, 
              fillColor = ~borough_colors[as.factor(boro_name)], 
              color = "black", 
              weight = 1, 
              fillOpacity = 0.5, 
              popup = ~paste("Borough:", boro_name)) %>%
  
  # Adicionar os limites dos Community Boards sem preenchimento (apenas bordas)
  addPolygons(data = community_boards, 
              fillColor = "transparent", 
              color = "white", 
              weight = 0.5) %>%
  
  # Adicionar etiquetas com os números dos Community Boards no centro de cada área
  addLabelOnlyMarkers(data = community_boards,
                      label = ~community_board_id,  # Usando o ID dos Community Boards
                      labelOptions = labelOptions(noHide = TRUE, direction = "center", textsize = "15px", fontweight = "bold", col = "black")) %>%
  
  # Adicionar o mapa base
  addTiles() %>%
  
  # Definir a posição inicial e o nível de zoom
  setView(lng = -73.935242, lat = 40.730610, zoom = 11)  # Coordenadas de NYC, ajuste conforme necessário



