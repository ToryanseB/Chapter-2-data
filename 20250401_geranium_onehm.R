#####################################
### Geranium 100m site selection ###
#####################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025")

#load libraries
set.seed(123)
#load libraries
{library(tidyverse)
  library(sf)
  library(sp)
  library(ggplot2)}


# Load Filtered Plant Dataset ---------------------------------------------

ab_dat<-read.csv("ab_dat.csv")

#filter to only include observations made after 2019 and that are identified to species level
ab_dat <- ab_dat %>% 
  filter(year >= 2019, !is.na(species), nzchar(species))

# Only include Flowering Plants -------------------------------------------

#load taxonomy data
taxonomy<-read.csv("plant_taxonomy.csv")

#flowering plants are in subphylum Angiospermae, which is only the class Magnoliopsida and Liliopsida
flowering<-taxonomy %>% 
  filter(class %in% c("Magnoliopsida", "Liliopsida"))

#filter to include only flowering plants

ab_dat_flowering<-ab_dat %>% 
  filter(year>= 2019) %>% 
  filter(class %in% flowering$class)

write.csv(ab_dat_flowering, "ab_dat_flowering.csv", row.names = FALSE)


# Create Buffers around Plant Observations --------------------------------

oneh_dat <- ab_dat_flowering %>%
  mutate(
    original_long = decimalLongitude, 
    original_lat = decimalLatitude
  ) %>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>% 
  st_transform(3400) %>%   
  st_buffer(dist = 100) %>%  
  st_transform(4326)

# Convert occurrence data (ab_dat_flowering) to sf and ensure same CRS
ab_points <- ab_dat_flowering %>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
  st_transform(4326) 

# Use st_intersects() instead of st_within() for better point-in-buffer counting
oneh_dat$obs_count <- lengths(st_intersects(oneh_dat, ab_points))

#check it worked
unique(oneh_dat$obs_count)


# Determine Geranium Presence ---------------------------------------------

#make a column that includes a list of all the plants observed at that site

# Find intersections between buffered areas and occurrence points
intersections <- st_intersects(oneh_dat, ab_points)

# Extract species names for each buffer
oneh_dat$listed_species <- sapply(intersections, function(index) {
  unique(ab_dat_flowering$species[index])  # Extract unique species names
})

# Convert to a comma-separated string
oneh_dat$listed_species <- sapply(oneh_dat$listed_species, function(species_list) {
  paste(species_list, collapse = ", ")
})

# Create a column to indicate presence of "Geranium viscosissimum"
oneh_dat$geranium_present <- sapply(intersections, function(index) {
  if ("Geranium viscosissimum" %in% ab_dat_flowering$species[index]) {
    return(1)  # Present
  } else {
    return(0)  # Not present
  }
})

# Check results
oneh_dat %>% 
  select(id, species, geometry, obs_count, listed_species, geranium_present) %>% 
  View()

# Explore the buffer data -------------------------------------------------

# Compute summary statistics
max(oneh_dat$obs_count, na.rm = TRUE)
min(oneh_dat$obs_count, na.rm = TRUE)
as.numeric(names(sort(table(oneh_dat$obs_count), decreasing = TRUE)[1]))
mean(oneh_dat$obs_count, na.rm = TRUE)

#see the distribution of number of observations per buffer
ggplot(oneh_dat, aes(x = obs_count)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Frequency Distribution of Observation Counts",
       x = "Number of Observations in Buffer",
       y = "Frequency") +
  theme_minimal()

#count the number of buffers that contain geranium

sum(oneh_dat$geranium_present == 1)#2572
sum(oneh_dat$geranium_present == 0)#72090

# Find out maximum observations at plant walk sites -----------------------

#find the boundary area for each plant walk by copying canvas extent in QGis
#eagle tipi ring site coordinates are -113.41137820,49.22444387,-113.40884286,49.22581415
#naapi garden site corrdinates are -113.45200106,49.24421532,-113.43171470,49.25517188
#lethbridge indian battle coordinates are -112.86195195,49.69920096,-112.85937740,49.70058288
#mini thini is -114.84459687,51.15043405,-114.83939618,51.15311309

# Define coordinates for each site
sites <- list(
  "eagle_tipi" = c(-113.41137820, 49.22444387, -113.40884286, 49.22581415),
  "naapi_garden" = c(-113.45200106, 49.24421532, -113.43171470, 49.25517188),
  "lethbridge_battle" = c(-112.86476870,49.69820279,-112.85716571,49.70228375),
  "mini_thni" = c(-114.84459687, 51.15043405, -114.83939618, 51.15311309)
)

# Function to create a bounding box as an sf polygon
create_bbox <- function(coords) {
  bbox_matrix <- matrix(
    c(coords[1], coords[2],  # bottom-left
      coords[3], coords[2],  # bottom-right
      coords[3], coords[4],  # top-right
      coords[1], coords[4],  # top-left
      coords[1], coords[2]), # closing the polygon
    ncol = 2, byrow = TRUE
  )
  
  st_polygon(list(bbox_matrix))
}

# Create sf objects for each site
site_polygons <- lapply(sites, create_bbox)

# Convert list of polygons into an sf object with site names
plantwalk_sites <- st_sf(site = names(sites), geometry = st_sfc(site_polygons, crs = 4326))

#write for mapping purposes
st_write(plantwalk_sites, "plantwalk_sites.shp")

# Perform spatial intersection to check which plant walk area each observation falls into
oneh_dat$plantwalk <- sapply(st_intersects(oneh_dat, plantwalk_sites), function(x) {
  if (length(x) > 0) {
    plantwalk_sites$site[x]  # Assign the site name if there is an intersection
  } else {
    "none"  # Assign "none" if no intersection
  }
})

unique(oneh_dat$plantwalk)

names(oneh_dat)

oneh_dat %>% 
  select(id,gbifID,species, day, month, year, recordedBy, obs_count, listed_species, geranium_present, plantwalk, original_long, original_lat) %>% 
  filter(plantwalk=="naapi_garden") %>% 
  View()

oneh_dat %>% 
  select(id, gbifID, species, day, month, year, recordedBy, obs_count, listed_species, 
         geranium_present, plantwalk, original_long, original_lat) %>% 
  filter(plantwalk == "mini_thni") %>% 
  group_by(original_long, original_lat) %>% 
  filter(n() > 1) %>%  # Keep only duplicates
  ungroup() %>% 
  View()

#look at the number of observations for those buffers

#lethbridge
lethbridge_battle_summary<-oneh_dat %>% 
  select(id,species,geometry, obs_count, listed_species, geranium_present,plantwalk) %>%
  filter(plantwalk == "lethbridge_battle")

max(lethbridge_battle_summary$obs_count, na.rm = TRUE)#44
min(lethbridge_battle_summary$obs_count, na.rm = TRUE)#3
mean(lethbridge_battle_summary$obs_count, na.rm = TRUE)#23
as.numeric(names(sort(table(lethbridge_battle_summary$obs_count), decreasing = TRUE)[1]))#24

#naapi garden
naapi_garden_summary<-oneh_dat %>% 
  select(id,species,geometry, obs_count, listed_species, geranium_present,plantwalk) %>%
  filter(plantwalk == "naapi_garden")

max(naapi_garden_summary$obs_count, na.rm = TRUE)#84
min(naapi_garden_summary$obs_count, na.rm = TRUE)#1
mean(naapi_garden_summary$obs_count, na.rm = TRUE)#55
as.numeric(names(sort(table(naapi_garden_summary$obs_count), decreasing = TRUE)[1]))#80

#eagle tipi
eagle_tipi_summary<-oneh_dat %>% 
  select(id,species,geometry, obs_count, listed_species, geranium_present,plantwalk) %>%
  filter(plantwalk == "eagle_tipi")

max(eagle_tipi_summary$obs_count, na.rm = TRUE)#60
min(eagle_tipi_summary$obs_count, na.rm = TRUE)#60
mean(eagle_tipi_summary$obs_count, na.rm = TRUE)#60
as.numeric(names(sort(table(eagle_tipi_summary$obs_count), decreasing = TRUE)[1]))#60

#mini thni
mini_thni_summary<-oneh_dat %>% 
  select(id,species,geometry, obs_count, listed_species, geranium_present,plantwalk) %>%
  filter(plantwalk == "mini_thni")

max(mini_thni_summary$obs_count, na.rm = TRUE)#106
min(mini_thni_summary$obs_count, na.rm = TRUE)#33
mean(mini_thni_summary$obs_count, na.rm = TRUE)#94
as.numeric(names(sort(table(mini_thni_summary$obs_count), decreasing = TRUE)[1]))#104


# so the lowest maximum out of all 4 plant walk locations is 44. That is your threshold for 100m. 



#find the maximum amount, and that is your threshold for the next filter

# Filter your buffers -----------------------------------------------------

# Filter the occurrence points for "Geranium viscosissimum"
geranium_points <- ab_points %>%
  filter(species == "Geranium viscosissimum")

max(st_coordinates(geranium_points)[, 2], na.rm = TRUE)# the maximum latitude that geranium exists at is 52.87396

#filter to only include those points that have more than 44 observations
onehdat_filtered <- oneh_dat %>%
  select(id, obs_count, occurrenceID, phylum, class, order,family, genus, species, coordinateUncertaintyInMeters, eventDate, day, month, year, basisOfRecord, institutionCode, identifiedBy, listed_species, geranium_present, geometry, plantwalk) %>%
  filter(obs_count >= 44, obs_count<=106) %>% 
  mutate(
    centroid_lat = st_coordinates(st_centroid(geometry))[, 2]  # Extract latitude of centroid
  ) %>%
  filter(centroid_lat <= 52.87396)  # Keep only buffers with latitude <= 52.87396

sum(onehdat_filtered$geranium_present == 1)#618
sum(onehdat_filtered$geranium_present == 0)#2810


# Randomly select buffers that don't have rare species --------------------
# Filter out buffers that contain "Geranium viscosissimum"
non_geranium_buffers <- onehdat_filtered %>%
  filter(geranium_present == 0)

# Randomly sample 618 points from the remaining data
set.seed(123)  # Set seed for reproducibility
random_non_geranium_buffers <- non_geranium_buffers %>%
  sample_n(618)

# Filter for buffers that contain "Geranium viscosissimum"
geranium_buffers <- onehdat_filtered %>%
  filter(geranium_present == 1)

# The previously selected buffers that do not contain Geranium viscosissimum
# (Already stored in random_non_geranium_buffers)
# Combine the two datasets: buffers that contain Geranium viscosissimum and the random non-Geranium buffers
onehdat_buffers <- bind_rows(
  geranium_buffers,  # Buffers with Geranium viscosissimum
  random_non_geranium_buffers  # Randomly selected buffers without Geranium viscosissimum
)

View(onehdat_buffers)


#thus far here are your two important datasets:
onehdat#all of the plant observations in your boundary (that fit criteria of 10m accuracy and year greater than 2019)
onehdat_buffers #the filtered buffers, containing geranium or not

# Remove Overlapping Layers -----------------------------------------------

# Compute centroids
onehdat_buffers <- onehdat_buffers %>%
  mutate(centroid = st_centroid(geometry))

# Remove exact centroid duplicates
onehdat_buffers <- onehdat_buffers %>%
  group_by(centroid = st_as_text(centroid)) %>%
  slice(1) %>% #keep only the first occurrence in each group
  ungroup()

onehdat_buffers_reduced <- onehdat_buffers
keepGoing <- TRUE

while(keepGoing){
  siteOver <- st_overlaps(onehdat_buffers_reduced,sparse = TRUE) #Gets overlap indices between sites
  ovlpNum <- sapply(siteOver,length) #How many sites does each site overlap?
  if(!any(ovlpNum>0)){ #If all are zero
    keepGoing <- FALSE #Stops the while loop
  } else { #otherwise
    chooseMe <- which.max(ovlpNum) #Chooses the first site that has maximum overlap with other sites
    onehdat_buffers_reduced <- onehdat_buffers_reduced[-chooseMe,] #Removes that site from the dataset
  }
}
ggplot(onehdat_buffers_reduced)+geom_sf()


#see how many sites contain geranium and dont
sum(onehdat_buffers_reduced$geranium_present == 1)#12
sum(onehdat_buffers_reduced$geranium_present == 0)#70


#write for mapping purposes
onehdat_buffers_reduced_map<-onehdat_buffers_reduced %>% 
  select(id)
st_write(onehdat_buffers_reduced_map, "onehdat_buffers_reduced_map.shp")
View(onehdat_buffers_reduced)

onehdat_final<-onehdat_buffers_reduced

# Re-even buffers ---------------------------------------------------------

# Filter out buffers that contain "Geranium viscosissimum"
non_geranium_buffers_two <- onehdat_buffers_reduced %>%
  filter(geranium_present == 0)

# Randomly sample 42 points from the remaining data
set.seed(123)  # Set seed for reproducibility
random_non_geranium_buffers_two <- non_geranium_buffers_two %>%
  sample_n(42)
# Filter for buffers that contain "Geranium viscosissimum"
geranium_buffers_two <- onehdat_buffers_reduced %>%
  filter(geranium_present == 1)

# Combine datasets: buffers with Geranium viscosissimum, random non-Geranium buffers
onehdat_final <- bind_rows(
  geranium_buffers_two,  
  random_non_geranium_buffers_two
)

#check the ratios are correct
sum(onehdat_final$geranium_present == 1)
sum(onehdat_final$geranium_present == 0)

ggplot(onehdat_final)+geom_sf()

onehdat_final#your final buffers

#for external mapping purposes
onehdat_final_buffer<-onehdat_final %>% 
  select(id)
st_write(onehdat_final_buffer, "onehdat_final_buffer.shp")

#sites for easy-to-see mapping

#extract centroid lat and lon from geometry

onehdat_seeable <- onehdat_final %>%
  mutate(
    centroid = st_centroid(geometry),  
    centroid_lat = st_coordinates(centroid)[,2],  
    centroid_lon = st_coordinates(centroid)[,1]   
  ) %>%
  select(id, centroid_lat, centroid_lon, geranium_present) %>%  # Keep geranium_present
  st_drop_geometry()  

# Save to CSV
write.csv(onehdat_seeable, "onehdat_seeable.csv", row.names = FALSE)

# Assign Occurrence Data to Buffers ---------------------------------------
ab_dat<-read.csv("ab_dat.csv")#all the observations, without geometries

abdat_observations<-ab_dat %>% 
  filter(year>=2019) %>% #make sure the observations are filtered by year
  filter(class %in% flowering$class) %>% 
  select(-id)#remove the id column, because you don't need it


# Convert occurrence data to sf
ab_observations <- abdat_observations %>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
  st_transform(st_crs(onehdat_final))  # Ensure same CRS


# Ensure onehdat_final is an sf object
onehdat_final <- st_as_sf(onehdat_final)

# Spatial join: Assign polygon ID to overlapping observations
onehdat_overlap <- st_join(ab_observations, onehdat_final["id"], left = FALSE)

# Create Presence-Absence Matrix ------------------------------------------
# Rename the ID column to "site", and select only what you'll need for the presence-absence matrix
onehdat_pa <- onehdat_overlap %>%
  rename(site = id) %>%
  select(species, site) %>%
  filter(species != "" & !is.na(species)) %>%  # Remove blank or NA species
  mutate(site = paste0("site", site)) %>%  # Add "site" prefix to site numbers
  st_drop_geometry()  # Removes the geometry and converts it into a standard dataframe

# Transform into correct Presence-Absence for Analysis --------------------

# Count the occurrences of each species at each site
onehdat_pa_counts <- onehdat_pa %>%
  count(species, site)

# Transform the 'n' column: set values to 1 if greater than 0
onehdat_pa_counts <- onehdat_pa_counts %>%
  mutate(n = ifelse(n > 0, 1, 0))  # Change n to 1 if greater than 0, else 0

# Reshape the data to have species as rows and sites as columns
onehdat_matrix <- onehdat_pa_counts %>%
  pivot_wider(names_from = site, values_from = n, values_fill = 0)

View(onehdat_matrix)

glimpse(onehdat_matrix)

unique(onehdat_matrix$species)
#print onehdat_matrix so that you have a copy of a csv

write.csv(onehdat_matrix, "onehdat_geranium.csv", row.names = FALSE)


# Rarefy the data ---------------------------------------------------------
# Count the occurrences of each species at each site
onehdat_pa_counts <- onehdat_pa %>%
  count(species, site)

onehdat_wide<-onehdat_pa_counts %>% 
  pivot_wider(names_from="species", values_from="n", values_fill = 0) %>% 
  as.data.frame()

rownames(onehdat_wide) <- onehdat_wide$site #because tibbles do not allow row names, make the group name the row name
onehdat_wide <- onehdat_wide[,-1] #now remove the group name from the dataframe

View(onehdat_wide)

library(vegan)

#find the smallest sized sample
min_n_seqs <- onehdat_pa_counts %>%
  group_by(site) %>% #groups are like sites and value is like the number of observations
  summarize(n_seqs = sum(n)) %>%
  summarize(min = min(n_seqs)) %>%
  pull(min)

min_n_seqs #44

vegans <- rarefy(onehdat_wide, min_n_seqs) %>% #going to rarefy to the smallest sized sample. a vector of values that are rarefied to a specific observation
  as_tibble(rownames="site")
  select(site, vegan=value)
  
View(vegans)
