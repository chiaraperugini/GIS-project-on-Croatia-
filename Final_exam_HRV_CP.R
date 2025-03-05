################################################################################
###############                 CHIARA PERUGINI                  ###############
###############                   ID: 1130360                    ###############
###############           Alma Mater Studiorum - LMEC^2          ###############
###############                  Subject : GIS                  ###############
################################################################################

# Set directory
path <- "/Users/chiaraperugini/Library/CloudStorage/OneDrive-AlmaMaterStudiorumUniversitàdiBologna/2nd year/GIS/Exam_HRV"
setwd(path)
getwd()

# Install Packages 
# install.packages(c("sf", "spData", "tidyverse", "ggplot2", "osmdata", 
#                   "rnaturalearth", "rnaturalearthdata", "grid", "readxl", 
#                   "dplyr", "terra", "raster", "exactextractr", 
#                   "spdep", "spatialreg"))


# Load libraries
library(sf)
library(spData)
library(tidyverse) 
library(ggplot2)
library(osmdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(readxl)
library(dplyr)
library(terra)
library(raster)
library(exactextractr)
library(spdep)
library(spatialreg)
library(conflicted)
library(lwgeom)

conflict_prefer("extract", "terra")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# ----------------------------- Final Exam Structure ------------------------- #

# Section A: 
# Research Proposal

# Section B: 
# Mandatory tasks (task_1 : task_19)
# Not Mandatory tasks: 
# First not mandatory task
# Second not mandatory task
# Third not mandatory task


# -------------------------------- Section B: -------------------------------- #

# ******************************** Mandatory Tasks *************************** #

##################################  Task 1  ####################################
# Load & Plot Croatia's NUTS 3 Regions with Bounding Box

# Load Croatia's boundaries (world map)
shp <- ne_download(scale = 10, type = "countries", category = "cultural", returnclass="sf") %>%
  filter(SOVEREIGNT == "Croatia")

# Load NUTS3 data & filter Croatia
shp3 <- st_read("data/Natural_Earth/NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp") %>%
  filter(CNTR_CODE == "HR")
# Create a bounding box with a margin
box_sf <- st_as_sfc(st_bbox(shp) + c(-0.5, -0.5, 0.5, 0.5))

# Plot Croatia, NUTS3 regions, and bounding box
ggplot() +
  geom_sf(data = shp, fill = "lightblue", color = "grey40", size = 0.2) +
  geom_sf(data = shp3, fill = "palegreen", color = "grey30", size = 0.2) + 
  geom_sf(data = box_sf, fill = NA, color = "red", linetype = "dashed", size = 0.8) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
  ) +
  labs(title = "NUTS 3 Croatia", caption = "Reference: Natural Earth")


##################################  Task 2  ####################################

# Load & Crop SPEI Data (Climate Data)

# Load SPEI raster data
r.spei <- rast("data/Climate_data/spei01.nc")

# Crop the SPEI data using Croatia's bounding box
cropped_spei <- crop(r.spei, st_as_sf(box_sf))

# Keep the last 6 layers (corresponding to years of interest)
reduced_spei <- cropped_spei[[1435:1440]]


##################################  Task 3  ####################################

# Apply Random Weights to SPEI Layers

set.seed(360)  # Ensure reproducibility
random_vector <- runif(6)  # Generate 6 random values

# Multiply each layer by its corresponding random value
reduced_spei <- lapply(1:6, function(i) reduced_spei[[i]] * random_vector[i]) |> rast()

##################################  Task 4  ####################################

# Convert raster layers to points
points_sf <- as.points(reduced_spei) |> st_as_sf()

# Plot grid points over Croatian NUTS3 regions
ggplot() +
  geom_sf(data = shp3, fill = "palegreen", color = "black") +
  geom_sf(data = points_sf, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Grid Points of the SPEI Raster Data in Croatian NUTS 3")

# Plot grid points over Croatia boundary
ggplot() +
  geom_sf(data = shp, fill = "lightblue", color = "black") +
  geom_sf(data = points_sf, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Grid Points of the SPEI Raster Data in Croatia")

##################################  Task 5  ####################################

#Load PRE data: Climatic Research Unit Time series
pre_path <- "data/Climate_data/cru_ts4.08.2011.2020.pre.dat.nc"
r.pre <- rast(pre_path)
r.pre 

# Extract dates associated with raster levels
time_vals <- time(r.pre)

# Filter only years between 2015 and 2020
indices <- which(format(time_vals, "%Y") %in% as.character(2015:2020))

# Select raster with filtered levels 
r.pre_2015_2020 <- r.pre[[indices]]

# Create a variable with the corresponding years 
years <- format(time_vals[indices], "%Y")

# Compute annual mean using `tapp()` to aggregate for every year
r.pre_mean_annual <- tapp(r.pre_2015_2020, index = years, fun = mean)

# Print the result 
print(r.pre_mean_annual)

names(r.pre_mean_annual)
names(r.pre_mean_annual) <- unique(format(time_vals[indices], "%Y"))

#Crop the SPEI data around the boundary box of Croatia
cropped_pre <- crop(r.pre_mean_annual, box_sf)
plot(cropped_pre)

##################################  Task 6  ####################################

# Load Aqueduct 4.0 shapefile
aqueduct_shapefile <- st_read("data/Aqueduct_data/Aqueduct_baseline.shp")

##################################  Task 7  ####################################

set.seed(360)

#Check the number of elements in the column 'bws_raw' (382)
length(aqueduct_shapefile$bws_raw)

#Assume 'bws_raw' is the water stress index of each watershed in 2015
water_stress_2015 <- aqueduct_shapefile$bws_raw 

#Add this vector to the Aqueduct shapefile
aqueduct_shapefile$water_stress_2015 <- water_stress_2015

# Generate a matrix of random multipliers for 5 years (2016 to 2020) for 382 elements
multipliers <- matrix(rnorm(5 * 382, mean = 1, sd = 1), nrow = 382, ncol = 5)

# Calculate the annual values by multiplying the 2015 values by the random multipliers
water_stress_ts <- water_stress_2015 * multipliers

# Add those attributes to the Aqueduct shapefile
aqueduct_shapefile$water_stress_2016 <- water_stress_ts[, 1] # Values for 2016
aqueduct_shapefile$water_stress_2017 <- water_stress_ts[, 2] # Values for 2017
aqueduct_shapefile$water_stress_2018 <- water_stress_ts[, 3] # Values for 2018
aqueduct_shapefile$water_stress_2019 <- water_stress_ts[, 4] # Values for 2019
aqueduct_shapefile$water_stress_2020 <- water_stress_ts[, 5] # Values for 2020

##################################  Task 8  ####################################

# Define bounding box and years list
bbox_extent <- ext(vect(box_sf))
years <- 2015:2020
rasters <- vector("list", length(years))
names(rasters) <- as.character(years)

# Loop to create and rasterize each year's data
for (year in years) {
  field_name <- paste0("water_stress_", year)
  raster <- rasterize(vect(aqueduct_shapefile), rast(bbox_extent, ncol = 250, nrow = 250), field = field_name)
  rasters[[as.character(year)]] <- mask(crop(raster, vect(shp)), vect(shp))  # Crop and mask for Croatia
}

# Plot raster maps for each year without printing extra lines in the console
par(mfrow = c(2, 3))
invisible(lapply(years, function(y) plot(rasters[[as.character(y)]], main = paste("Water Stress in Croatia -", y))))
par(mfrow = c(1, 1))

# Extract raster values for croatia and croatian NUTS3 regions in 2020
water_stress_2020_croatia <- exact_extract(rasters[["2020"]], shp, 'mean')
water_stress_2020_nuts3 <- exact_extract(rasters[["2020"]], shp3, 'mean')

# Compute average water stress levels
avg_water_stress_2020_croatia <- mean(water_stress_2020_croatia, na.rm = TRUE)
avg_water_stress_2020_nuts3 <- mean(water_stress_2020_nuts3, na.rm = TRUE)
avg_water_stress_2020_croatia
avg_water_stress_2020_nuts3


#The two averages are slightly different, and this divergence may occur because the 
#simple average does not account for the size of each region. In contrast, the average for 
#all of Croatia considers the entire country’s area. As a result, larger regions with higher 
#or lower water stress values have a greater impact on Croatia’s overall average, whereas 
#the simple average treats each NUTS 3 region equally, regardless of its size.

# Extract raster values for croatian NUTS3 regions (2015-2020)
water_stress_matrix <- do.call(cbind, lapply(years, function(y) exact_extract(rasters[[as.character(y)]], shp3, 'mean')))
colnames(water_stress_matrix) <- paste0("water_stress_", years)

# Initialize an empty list to store water stress values for each year
years <- 2015:2020
water_stress_list <- list()

# Loop through each year to calculate mean water stress and store in the list
for (i in seq_along(years)) {
  water_stress_list[[i]] <- exact_extract(rasters[[i]], shp3, 'mean')
}

##################################  Task 9  ####################################

# Load population density data for 2015
pop_path <- "data/Population_data/gpw_v4_population_density_rev11_2015_15_min.asc"
r.pop <- rast(pop_path)

# Crop to Croatia's bounding box
cropped_pop <- crop(r.pop, box_sf)

# Calculate average population density in Croatia before resampling
avg_pop_density_before <- mean(exact_extract(cropped_pop, shp, 'mean'), na.rm = TRUE)

# Resample to match SPEI resolution using bilinear interpolation
resampled_pop <- resample(cropped_pop, reduced_spei[[1]], method = "bilinear")

# Calculate average population density after resampling
avg_pop_density_after <- mean(exact_extract(resampled_pop, shp, 'mean'), na.rm = TRUE)

# Plot original and resampled population density data
par(mfrow = c(1, 2))
plot(cropped_pop, main = "Population Density in Croatia - 2015 (Original)")
plot(shp3$geometry, add = TRUE)
plot(resampled_pop, main = "Population Density in Croatia - 2015 (Resampled)")
plot(shp3$geometry, add = TRUE)
par(mfrow = c(1, 1))

# Summary of changes
cat("Before resampling, the average population density in Croatia was:", avg_pop_density_before, "\n")
cat("After resampling, the average population density in Croatia is:", avg_pop_density_after, "\n")
cat("Resampling slightly altered the average population density due to bilinear interpolation, which adjusts values based on neighboring cells. This minor change occurs because bilinear interpolation smooths transitions between grid cells. Such adjustments are expected and indicate that resampling had minimal impact on the overall population density pattern.")

# ******************************** First Not Mandatory Tasks *******************

# create a population-weighted version of SPEI and water stress data using the 
# gridded data of population density.

# Calculate the average population density in croatia
pop_density_Croatia <- exact_extract(resampled_pop, shp, 'mean')
avg_pop_density_Croatia <- mean(pop_density_Croatia, na.rm = TRUE)

# Create the population density ratio for each grid point
pop_density_ratio <- resampled_pop / avg_pop_density_Croatia

# Initialize a list to store population-weighted SPEI values and plot them all together
years <- 2015:2020
pop_weighted_spei <- list()

# Loop through each year to calculate and plot the population-weighted SPEI
par(mfrow = c(2, 3))
for (i in seq_along(years)) {
  pop_weighted_spei[[i]] <- reduced_spei[[i]] * pop_density_ratio
  plot(pop_weighted_spei[[i]], main = paste("Population-weighted SPEI for", years[i]))
}
par(mfrow = c(1, 1))

# Same for water stress index
water_stress_weighted_list <- list()
par(mfrow = c(2, 3))  # Set up 2x3 plot layout

for (i in seq_along(years)) {
  # Ensure alignment: resample pop_density_ratio to match the current raster
  aligned_pop_density <- resample(pop_density_ratio, rasters[[i]])
  
  # Multiply the current raster with the population density ratio
  water_stress_weighted_list[[i]] <- rasters[[i]] * aligned_pop_density
  
  # Plot each year's population-weighted water stress
  plot(water_stress_weighted_list[[i]], main = paste("Population-weighted Water Stress for", years[i]))
}
# Reset plot layout
par(mfrow = c(1, 1))

# Same for precipitation data
precipitation_weigthed_list <- list()
par(mfrow = c(2, 3))
for (i in seq_along(years)) {
  precipitation_weigthed_list[[i]] <- cropped_pre[[i]] * pop_density_ratio
  plot(precipitation_weigthed_list[[i]], main = paste("Population-weighted precipitation for", years[i]))
}
par(mfrow = c(1, 1))


##################################  Task 10  ###################################
#Using population-weighted climate variables can be relevant when analyzing GDP
#data, but its relevance depends on the sector in question. For overall GDP 
#and Construction GVA, population-weighted climate data may be useful since
#economic activity is often concentrated in populated areas where infrastructure
#and investment are higher. However, for Agricultural GVA, population weighting may 
#not be ideal because agricultural production is often spread across rural areas where 
#population density is low. Similarly, Industrial GVA might be better analyzed using 
#sector-specific weighting (e.g., industrial activity maps) rather than population 
#weighting, as many industries operate in sparsely populated zones. In geospatial 
#analysis, sector-specific spatial weighting (e.g., land use or production-based weighting)
#is often more relevant than purely population-based approaches.


##################################  Task 11  ###################################
#It is preferable to downscale SPEI to match the higher-resolution population 
#density (0.25° × 0.25°) before creating a population-weighted SPEI index. 
#This ensures that climate data aligns with finer population distribution, 
#preserving spatial variability and improving accuracy. Aggregating population 
#density to match the lower-resolution SPEI (1° × 1°) would lose critical details, 
#leading to a less precise weighting of climate impacts on populated areas.

##################################  Task 12  ###################################

# Load Croatian Hours Worked Data (Sector F)
Hours_worked_path <- "data/Urban_data/Hours_Worked.csv"
Croatian_Hours_worked <- read_csv(Hours_worked_path, show_col_types = FALSE) %>%
  filter(SECTOR == "F") %>%
  select(TERRITORY_ID, SECTOR, `2015`:`2020`) %>%
  rename(NUTS_ID = TERRITORY_ID) %>%
  rename_with(~ paste0("HW_", .), starts_with("20"))

# Load Croatian GVA Data (Sheet 2)
Croatiangva_path <- "data/Urban_data/ARDECO_SUVGZ.csv"
Croatian_GVA <- read_csv(Croatiangva_path, show_col_types = FALSE) %>%
  filter(SECTOR == "F") %>%
  filter(UNIT == "Million EUR" ) %>%
  select(TERRITORY_ID, `2015`:`2020`) %>%
  rename(NUTS_ID = TERRITORY_ID) %>%
  rename_with(~ paste0("cnst_gva_", .), starts_with("20"))

#There are no Croatian NUTS ending with "ZZZ"

# Rename ISO to match the current nomenclature

Croatian_GVA <- Croatian_GVA %>% 
  mutate(NUTS_ID = str_replace_all(NUTS_ID, c("HR047" = "HR021", 
                                              "HR048" = "HR022",
                                              "HR049" = "HR023", 
                                              "HR04A" = "HR024", 
                                              "HR04B" = "HR025",
                                              "HR04C" = "HR026", 
                                              "HR04D" = "HR027", 
                                              "HR04E" = "HR028", 
                                              "HR041" = "HR050", 
                                              "HR046" = "HR061", 
                                              "HR044" = "HR062", 
                                              "HR045" = "HR063", 
                                              "HR043" = "HR064", 
                                              "HR042" = "HR065")))

# Merge shp3 with Croatian Hours Worked & GVA Data
shp3_merged <- shp3 %>%
  left_join(Croatian_Hours_worked, by = "NUTS_ID") %>%
  left_join(Croatian_GVA, by = "NUTS_ID")

##################################  Task 13  ###################################

# Extract SPEI, precipitation and water stress data for croatia and regions
#Extract the average value of SPEI for each NUTS in each year
spei_avg_nuts3 <- shp3 %>%
  mutate(
    spei_2015 = exact_extract(pop_weighted_spei[[1]], shp3, 'mean'),
    spei_2016 = exact_extract(pop_weighted_spei[[2]], shp3, 'mean'),
    spei_2017 = exact_extract(pop_weighted_spei[[3]], shp3, 'mean'),
    spei_2018 = exact_extract(pop_weighted_spei[[4]], shp3, 'mean'),
    spei_2019 = exact_extract(pop_weighted_spei[[5]], shp3, 'mean'),
    spei_2020 = exact_extract(pop_weighted_spei[[6]], shp3, 'mean')
  )

#Extract the average water stress for each NUTS in each year
water_stress_avg_nuts3 <- spei_avg_nuts3 %>%
  mutate(
    water_stress_2015 = exact_extract(water_stress_weighted_list[[1]], shp3, 'mean'),
    water_stress_2016 = exact_extract(water_stress_weighted_list[[2]], shp3, 'mean'),
    water_stress_2017 = exact_extract(water_stress_weighted_list[[3]], shp3, 'mean'),
    water_stress_2018 = exact_extract(water_stress_weighted_list[[4]], shp3, 'mean'),
    water_stress_2019 = exact_extract(water_stress_weighted_list[[5]], shp3, 'mean'),
    water_stress_2020 = exact_extract(water_stress_weighted_list[[6]], shp3, 'mean')
  )

#Extract the average precipitation for each NUTS in each year
precipitation_avg_nuts3 <- water_stress_avg_nuts3 %>%
  mutate(
    prec_2015 = exact_extract(precipitation_weigthed_list[[1]], shp3, 'mean'),
    prec_2016 = exact_extract(precipitation_weigthed_list[[2]], shp3, 'mean'),
    prec_2017 = exact_extract(precipitation_weigthed_list[[3]], shp3, 'mean'),
    prec_2018 = exact_extract(precipitation_weigthed_list[[4]], shp3, 'mean'),
    prec_2019 = exact_extract(precipitation_weigthed_list[[5]], shp3, 'mean'),
    prec_2020 = exact_extract(precipitation_weigthed_list[[6]], shp3, 'mean')
  )

precipitation_avg_nuts3_df <- st_drop_geometry(precipitation_avg_nuts3)  # Remove geometry

final_sf <- shp3_merged %>%
  select(NUTS_ID, starts_with("cnst_gva"), geometry) %>%
  left_join(precipitation_avg_nuts3_df, by = "NUTS_ID")  


# Compute averages for all numeric columns
final_sf_avg <- final_sf %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

##################################  Task 14  ###################################

# Reshape the data for SPEI and Water Stress
final_SPEI_sf_long_avg <- final_sf_avg %>%
  pivot_longer(cols = starts_with("spei_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("spei_", "", year)), variable = "SPEI")

final_water_sf_long_avg <- final_sf_avg %>%
  pivot_longer(cols = starts_with("water_stress_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("water_stress_", "", year)), variable = "Water Stress")

final_pre_sf_long_avg <- final_sf_avg %>%
  pivot_longer(cols = starts_with("prec_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("prec_", "", year)), variable = "Precipitation")

# Combine the two datasets into one
final_combined_long_avg <- bind_rows(final_SPEI_sf_long_avg, final_water_sf_long_avg, final_pre_sf_long_avg)

# Generate the combined plot
ggplot(final_combined_long_avg, aes(x = year, y = value, color = variable)) +
  geom_line() +     
  geom_point() +      
  labs(title = "Time Series of SPEI, Water Stress and Precipitation (2015-2020)", x = "Year", y = "Value") +
  theme_minimal()

##################################  Task 15  ###################################

# Reshape the data for SPEI, water stress, and Construction GVA
final_SPEI_sf_long <- final_sf %>%
  select(starts_with("spei_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("spei_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("spei_", "", year)), variable = "SPEI")

final_water_sf_long <- final_sf %>%
  select(starts_with("water_stress_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("water_stress_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("water_stress_", "", year)), variable = "Water Stress")

final_pre_sf_long <- final_sf %>%
  select(starts_with("prec_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("prec_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("prec_", "", year)), variable = "Precipitation")

final_cnst_sf_long <- final_sf %>%
  select(starts_with("cnst_gva_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("cnst_gva_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("cnst_gva_", "", year)), variable = "Construction GVA")

# Combine the datasets into one
final_combined_long <- bind_rows(final_SPEI_sf_long, final_water_sf_long, 
                                 final_cnst_sf_long, final_pre_sf_long) %>%
  arrange(NUTS_ID)

# Generate the combined plot
ggplot(final_combined_long, aes(x = year, y = value, color = variable)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ NUTS_ID, scales = "free_y") + 
  labs(title = "Time Series for each NUTS_ID", x = "Year", y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

##################################  Task 16  ###################################

# Calculate the growth rate of SPEI, Water Stress and precipitation
final_sf <- final_sf %>%
  st_transform(crs = 4326) %>%
  mutate(
    speigr = (spei_2020 - spei_2015) / spei_2015, 
    wsgr = (water_stress_2020 - water_stress_2015) / water_stress_2015,
    pregr = (prec_2020 - prec_2015) / prec_2015
  )

# Plot SPEI Growth Rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = speigr), color = "black") +  
  scale_fill_viridis_c(option = "magma", name = "SPEI Growth Rate") + 
  labs(
    title = "Growth Rate of SPEI from 2015 to 2020 in croatian NUTS 3 Regions",
    subtitle = "Growth Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Plot water stress Growth Rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = wsgr), color = "black") +  
  scale_fill_viridis_c(option = "viridis", name = "water stress Growth Rate") +  
  labs(title = "Growth Rate of water stress from 2015 to 2020 in croatian NUTS 3 Regions",
       subtitle = "Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")


##################################  Task 17  ###################################
# Calculate the growth rate of SPEI, Water Stress and precipitation
final_sf <- final_sf %>%
  st_transform(crs = 32634) %>%
  mutate(
    speigr = (spei_2020 - spei_2015) / spei_2015, 
    wsgr = (water_stress_2020 - water_stress_2015) / water_stress_2015,
    pregr = (prec_2020 - prec_2015) / prec_2015
  )


# Plot Precipitation Growth Rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = pregr), color = "black") +  
  scale_fill_viridis_c(option = "rocket", name = "Precipitation Growth Rate") + 
  labs(
    title = "Growth Rate of Precipitation from 2015 to 2020 in Croatian NUTS 3 Regions",
    subtitle = "Growth Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

##################################  Task 18  ###################################

# Plot a map showing the growth rate of Construction GVA from 2015 to 2020 of each croatian NUTS 3
# region, in CRS 3035. 

final_sf_3035 <- final_sf %>%
  st_transform(crs = 3035) %>%  # Imposta il CRS a 3035
  mutate(cnstgvcnst = ( cnst_gva_2020 - cnst_gva_2015) / cnst_gva_2015) 

ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = cnstgvcnst), color = "black") +  
  scale_fill_viridis_c(option = "viridis", name = "Construction GVA Growth Rate") + 
  labs(title = "Growth Rate of Construction GVA from 2015 to 2020 in croatian NUTS 3 Regions",
       subtitle = "Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")

#  plot the map again using blue for regions experiencing a negative growth, lightblue
#  for those having a growth rate between 0% and 5%, and white for regions with a growth rate >5%.

# divide growth rates in new categories 
final_sf_3035 <- final_sf_3035 %>%
  mutate(cnstvcnst_category = case_when(
    cnstgvcnst < 0 ~ "Negative Growth",
    cnstgvcnst >= 0 & cnstgvcnst <= 0.05 ~ "0-5% Growth",
    cnstgvcnst > 0.05 ~ ">5% Growth"
  ))

# generate the same grapth with the bin colours setted above
ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = cnstvcnst_category), color = "black") +  # Colora in base alla categoria
  scale_fill_manual(values = c("Negative Growth" = "blue", 
                               "0-5% Growth" = "lightblue", 
                               ">5% Growth" = "white"),
                    name = "Construction GVA Growth Rate") +
  labs(title = "Growth Rate of Construction GVA from 2015 to 2020 in croatian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")  

# divide growth rates in new categories for visual purposes
final_sf_3035 <- final_sf_3035 %>%
  mutate(cnstvcnst_category = case_when(
    cnstgvcnst < 0.25 ~ "0-25%",
    cnstgvcnst >= 0.25 & cnstgvcnst <= 0.50 ~ "25-50% Growth",
    cnstgvcnst > 0.50 ~ ">50% Growth"
  ))

# generate the same grapth with the bin colours setted above
ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = cnstvcnst_category), color = "black") +  # Colora in base alla categoria
  scale_fill_manual(values = c("0-25%" = "blue", 
                               "25-50% Growth" = "lightblue", 
                               ">50% Growth" = "white"),
                    name = "Construction GVA Growth Rate") +
  labs(title = "Growth Rate of Construction GVA from 2015 to 2020 in croatian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")  

##################################  Task 19  ###################################

#Yes, GDP per capita in a given region can be influenced by lagged investment 
#in neighboring regions. This phenomenon, known as the spatial spillover effect,
#occurs when economic activities or outcomes in one area impact adjacent areas. 
#Specifically, lagged investment from neighboring regions may affect GDP per capita by 
#stimulating trade, generating employment, enhancing infrastructure, or strengthening 
#economic connectivity. To assess whether there is spatial correlation, spatial 
#autocorrelation tests such as Moran’s I test can be applied. More generally, a spatial 
#econometric approach that accounts for potential spatial dependencies over time can provide 
#a comprehensive analysis of these lagged effects.

# ******************************** Second Not Mandatory Tasks *******************

# Using the “queen” criterion

final_sf <- st_make_valid(final_sf) # Useful to correct geometries!
final_sf <- st_transform(final_sf, crs = 4326)

# Spatial Lags
k <- poly2nb(final_sf, row.names = final_sf$NUTS_ID)
k_weights <- nb2listw(k, style = "W")

print(summary(k))

# create centroids
coords <- st_coordinates(st_centroid(st_geometry(final_sf))) 
p <- plot(k, coords)
# compute the Moran’s I Test for SPEI in 2015
index_lag <- lag.listw(k_weights, final_sf$spei_2015)

# Moran's I Test
moran1_SPEI2015 <- moran.test(final_sf$spei_2015,k_weights)
print(moran1_SPEI2015) #Given the very small p-value, we reject the null hypothesis, and we can notice that there is a strong and positive spatial autocorrelation (0.59) in SPEI data

# create a spatial autocorrelation scatterplot for the above-mentioned variables

# Precompute scaled values
final_sf <- final_sf %>%
  mutate(SPEI_2015_scaled = scale(spei_2015),
         index_lag_scaled = scale(index_lag))

# Compute mean values for horizontal and vertical lines
mean_SPEI_2015_scaled <- mean(final_sf$SPEI_2015_scaled, na.rm = TRUE)
mean_index_lag_scaled <- mean(final_sf$index_lag_scaled, na.rm = TRUE)

# Create the plot
ggplot <- ggplot(final_sf) +
  geom_point(aes(x = SPEI_2015_scaled, y = index_lag_scaled, fill = NUTS_ID), shape = 22, size = 3, alpha = 0.8) +
  geom_hline(yintercept = mean_index_lag_scaled, linetype = "dashed") +
  geom_vline(xintercept = mean_SPEI_2015_scaled, linetype = "dashed") +
  geom_smooth(aes(x = SPEI_2015_scaled, y = index_lag_scaled), method = "lm", color = "black") +
  labs(x = "SPEI_2015 (scaled)", y = "Lagged Index (scaled)", fill = "Country") +
  theme_classic()
ggplot

# compute the Moran’s I Test for Construction in 2015
# create Spatial Lags variable
index_lag <- lag.listw(k_weights, final_sf$cnst_gva_2015)

# Moran's I Test
moran1_cnst2015 <- moran.test(final_sf$cnst_gva_2015,k_weights)
print(moran1_cnst2015) #Given the high p-value, we fail to reject the null hypothesis, indicating that there is no significant spatial autocorrelation in the data

# create a spatial autocorrelation scatterplot for the above-mentioned variables

# Precompute scaled values
final_sf <- final_sf %>%
  mutate(cnst_gva_2015_scaled = scale(cnst_gva_2015),
         index_lag_scaled = scale(index_lag))

# Compute mean values for horizontal and vertical lines
mean_cnst_gva_2015_scaled <- mean(final_sf$cnst_gva_2015_scaled, na.rm = TRUE)
mean_index_lag_scaled <- mean(final_sf$index_lag_scaled, na.rm = TRUE)

# Create the plot
ggplot <- ggplot(final_sf) +
  geom_point(aes(x = cnst_gva_2015_scaled, y = index_lag_scaled, fill = NUTS_ID), shape = 22, size = 3, alpha = 0.8) +
  geom_hline(yintercept = mean_index_lag_scaled, linetype = "dashed") +
  geom_vline(xintercept = mean_cnst_gva_2015_scaled, linetype = "dashed") +
  geom_smooth(aes(x = cnst_gva_2015_scaled, y = index_lag_scaled), method = "lm", color = "black") +
  labs(x = "cnst_gva_2015 (scaled)", y = "Lagged Index (scaled)", fill = "Country") +
  theme_classic()
ggplot

# Moran’s I doesn’t directly test the causal impact of a lagged variable in neighboring regions .
# To explore the effect of lagged investment in neighboring regions on GDP, a spatial lag model (SLM) 
# or a spatial Durbin model (SDM) would be more appropriate. These models incorporate
# spatially lagged independent variables, allowing for analysis of the potential 
# impact of investment at t−1 in region j on GDP at t in region i.

# ******************************** Third Not Mandatory Task: *******************

#Load the .gdb file
gdb_path <- "data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb"

#List layers in the GDB file
gdb_layers <- st_layers(gdb_path)
print(gdb_layers)

#Read the desired layer from the GDB file (replace 'desired_layer' with the correct layer name)
# Assuming 'desired_layer' contains water risk data and "gid_0" represents countries
aqua_sf <- st_read(gdb_path, layer = "baseline_annual")

#Filter the data for Croatia using 'gid_0' column (assuming Croatia is labeled as "HRV" in 'gid_0')
Croatia_aqua_sf <- aqua_sf %>%
  filter(gid_0 == "HRV")  # Adjust "HRV" if Croatia has a different code in the dataset

#Remove any rows with missing data to reduce computational burden
Croatia_aqua_sf <- Croatia_aqua_sf %>%
  drop_na()

#Crop the Aqueduct data to Croatia's NUTS 3 shapefile shp3
Croatia_aqua_sf <- st_make_valid(Croatia_aqua_sf)
Croatia_aqua_sf <- st_cast(Croatia_aqua_sf, "MULTIPOLYGON")  # Ensure proper geometry
shp3 <- st_make_valid(shp3)

# Check the geometry type of the features in your sf object
st_geometry_type(Croatia_aqua_sf)

# Now perform the intersection
shp3 <- st_make_valid(shp3)
Croatia_aqua_cropped <- st_intersection(Croatia_aqua_sf, shp3)

#Plot data
plot(st_geometry(Croatia_aqua_sf)) 

plot(st_geometry(Croatia_aqua_cropped))
#Save the cropped data to a shapefile
st_write(Croatia_aqua_cropped, "Croatia_aqueduct_data.shp", delete_layer = TRUE)