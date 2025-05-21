###################################
### Milkweed 10m Site Selection ###
###################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site Download May 2025")

#load libraries
set.seed(123)
#load libraries
{library(tidyverse)
  library(sf)
  library(sp)
  library(ggplot2)}

# Load Filtered Plant Dataset ---------------------------------------------

ab_dat<-read.csv("C:/Users/torya/OneDrive/Documents/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025/ab_dat.csv")

#filter to only include observations made after 2019 and that are identified to species level
ab_dat <- ab_dat %>% 
  filter(year >= 2019, !is.na(species), nzchar(species))

# Only include Flowering Plants -------------------------------------------

#load taxonomy data
taxonomy<-read.csv("C:/Users/torya/OneDrive/Documents/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025/plant_taxonomy.csv")

#flowering plants are in subphylum Angiospermae, which is only the class Magnoliopsida and Liliopsida
flowering<-taxonomy %>% 
  filter(class %in% c("Magnoliopsida", "Liliopsida"))

#find the max latitude for Asclepias viridiflora
max_latitude <- ab_dat %>%
  filter(year>= 2019) %>% 
  filter(class %in% flowering$class) %>% 
  filter(species == "Asclepias viridiflora") %>%
  summarise(max_lat = max(decimalLatitude, na.rm = TRUE))


max_latitude #the max latitude is 49.08557

#filter to include only flowering plants, and to include only sites in boundary (where you made observations)
ab_dat_flowering<-ab_dat %>% 
  filter(year>= 2019) %>% 
  filter(class %in% flowering$class) %>% 
  filter(decimalLatitude<= 49.1518590) %>% 
  filter(decimalLongitude>=-112.0866766)

write.csv(ab_dat_flowering, "ab_dat_flowering_m.csv", row.names = FALSE)

# Create Buffers around Plant Observations --------------------------------
tenm_dat <- ab_dat_flowering %>%
  mutate(
    original_long = decimalLongitude, 
    original_lat = decimalLatitude
  ) %>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>% 
  st_transform(3400) %>%   
  st_buffer(dist = 10) %>%  #10m radius around each occurrence
  st_transform(4326)

# Convert occurrence data (ab_dat_flowering) to sf and ensure same CRS
ab_points <- ab_dat_flowering %>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
  st_transform(4326) 

# Use st_intersects() instead of st_within() for better point-in-buffer counting
tenm_dat$obs_count <- lengths(st_intersects(tenm_dat, ab_points))

#check it worked
unique(tenm_dat$obs_count)


# Determine asclepias Presence ---------------------------------------------

#make a column that includes a list of all the plants observed at that site

# Find intersections between buffered areas and occurrence points
intersections <- st_intersects(tenm_dat, ab_points)

# Extract species names for each buffer
tenm_dat$listed_species <- sapply(intersections, function(index) {
  unique(ab_dat_flowering$species[index])  # Extract unique species names
})

# Convert to a comma-separated string
tenm_dat$listed_species <- sapply(tenm_dat$listed_species, function(species_list) {
  paste(species_list, collapse = ", ")
})

# Create a column to indicate presence of "Asclepias viridiflora"
tenm_dat$asclepias_present <- sapply(intersections, function(index) {
  if ("Asclepias viridiflora" %in% ab_dat_flowering$species[index]) {
    return(1)  # Present
  } else {
    return(0)  # Not present
  }
})

# Check results
tenm_dat %>% 
  select(id, species, geometry, obs_count, listed_species, asclepias_present) %>% 
  View()

# Explore the buffer data -------------------------------------------------

# Compute summary statistics
max(tenm_dat$obs_count, na.rm = TRUE)
min(tenm_dat$obs_count, na.rm = TRUE)
as.numeric(names(sort(table(tenm_dat$obs_count), decreasing = TRUE)[1]))
mean(tenm_dat$obs_count, na.rm = TRUE)

#see the distribution of number of observations per buffer
ggplot(tenm_dat, aes(x = obs_count)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Frequency Distribution of Observation Counts",
       x = "Number of Observations in Buffer",
       y = "Frequency") +
  theme_minimal()

#count the number of buffers that contain asclepias

sum(tenm_dat$asclepias_present == 1)#344
sum(tenm_dat$asclepias_present == 0)#907

# Filter your buffers -----------------------------------------------------

#filter to only include those points that have more than 5 observations
tenmdat_filtered <- tenm_dat %>%
  select(id, obs_count, occurrenceID, phylum, class, order,family, genus, species, coordinateUncertaintyInMeters, eventDate, day, month, year, basisOfRecord, institutionCode, identifiedBy, listed_species, asclepias_present, geometry) %>%
  filter(obs_count >= 5) %>% 
  mutate(
    centroid_lat = st_coordinates(st_centroid(geometry))[, 2]  # Extract latitude of centroid
  )

sum(tenmdat_filtered$asclepias_present == 1)#310
sum(tenmdat_filtered$asclepias_present == 0)#193


#thus far here are your two important datasets:
tenmdat#all of the plant observations in your boundary (that fit criteria of 10m accuracy and year greater than 2019)
tenmdat_buffers #the filtered buffers, containing asclepias or not

# Remove Overlapping Layers -----------------------------------------------

# Compute centroids
tenmdat_buffers <- tenmdat_filtered %>%
  mutate(centroid = st_centroid(geometry))

# Remove exact centroid duplicates
tenmdat_buffers <- tenmdat_buffers %>%
  group_by(centroid = st_as_text(centroid)) %>%
  slice(1) %>% #keep only the first occurrence in each group
  ungroup()

tenmdat_buffers_reduced <- tenmdat_buffers
keepGoing <- TRUE

while(keepGoing){
  siteOver <- st_overlaps(tenmdat_buffers_reduced,sparse = TRUE) #Gets overlap indices between sites
  ovlpNum <- sapply(siteOver,length) #How many sites does each site overlap?
  if(!any(ovlpNum>0)){ #If all are zero
    keepGoing <- FALSE #Stops the while loop
  } else { #otherwise
    chooseMe <- which.max(ovlpNum) #Chooses the first site that has maximum overlap with other sites
    tenmdat_buffers_reduced <- tenmdat_buffers_reduced[-chooseMe,] #Removes that site from the dataset
  }
}
ggplot(tenmdat_buffers_reduced)+geom_sf()


#see how many sites contain asclepias and dont
sum(tenmdat_buffers_reduced$asclepias_present == 1)#18
sum(tenmdat_buffers_reduced$asclepias_present == 0)#30

tenmdat_final<-tenmdat_buffers_reduced

#for external mapping purposes
tenmdat_final_buffer<-tenmdat_final %>% 
  select(id)
st_write(tenmdat_final_buffer, "tenmdat_final_buffer.shp")

#sites for easy-to-see mapping

#extract centroid lat and lon from geometry

tenmdat_seeable <- tenmdat_final %>%
  mutate(
    centroid = st_centroid(geometry),  
    centroid_lat = st_coordinates(centroid)[,2],  
    centroid_lon = st_coordinates(centroid)[,1]   
  ) %>%
  select(id, centroid_lat, centroid_lon, asclepias_present) %>%  # Keep asclepias_present
  st_drop_geometry()  

# Save to CSV
write.csv(tenmdat_seeable, "tenmdat_seeable.csv", row.names = FALSE)

# Assign Occurrence Data to Buffers ---------------------------------------
ab_dat<-read.csv("C:/Users/torya/OneDrive/Documents/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025/ab_dat.csv")#all the observations, without geometries

abdat_observations<-ab_dat %>% 
  filter(year>=2019) %>% #make sure the observations are filtered by year
  filter(class %in% flowering$class) %>% 
  select(-id)#remove the id column, because you don't need it


# Convert occurrence data to sf
ab_observations <- abdat_observations %>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
  st_transform(st_crs(tenmdat_final))  # Ensure same CRS


# Ensure tenmdat_final is an sf object
tenmdat_final <- st_as_sf(tenmdat_final)

# Spatial join: Assign polygon ID to overlapping observations
tenmdat_overlap <- st_join(ab_observations, tenmdat_final["id"], left = FALSE)

# Create Presence-Absence Matrix ------------------------------------------
# Rename the ID column to "site", and select only what you'll need for the presence-absence matrix
tenmdat_pa <- tenmdat_overlap %>%
  rename(site = id) %>%
  select(species, site) %>%
  filter(species != "" & !is.na(species)) %>%  # Remove blank or NA species
  mutate(site = paste0("site", site)) %>%  # Add "site" prefix to site numbers
  st_drop_geometry()  # Removes the geometry and converts it into a standard dataframe

# Transform into correct Presence-Absence for Analysis --------------------

# Count the occurrences of each species at each site
tenmdat_pa_counts <- tenmdat_pa %>%
  count(species, site)

# Transform the 'n' column: set values to 1 if greater than 0
tenmdat_pa_counts <- tenmdat_pa_counts %>%
  mutate(n = ifelse(n > 0, 1, 0))  # Change n to 1 if greater than 0, else 0

# Reshape the data to have species as rows and sites as columns
tenmdat_matrix <- tenmdat_pa_counts %>%
  pivot_wider(names_from = site, values_from = n, values_fill = 0)

View(tenmdat_matrix)

glimpse(tenmdat_matrix)

unique(tenmdat_matrix$species)
#print tenmdat_matrix so that you have a copy of a csv

write.csv(tenmdat_matrix, "tenmdat_asclepias.csv", row.names = FALSE)


# Rarefy the data ---------------------------------------------------------
# Count the occurrences of each species at each site
tenmdat_pa_counts <- tenmdat_pa %>%
  count(species, site)

tenmdat_wide<-tenmdat_pa_counts %>% 
  pivot_wider(names_from="species", values_from="n", values_fill = 0) %>% 
  as.data.frame()

rownames(tenmdat_wide) <- tenmdat_wide$site #because tibbles do not allow row names, make the group name the row name
tenmdat_wide <- tenmdat_wide[,-1] #now remove the group name from the dataframe

View(tenmdat_wide)

library(vegan)

#find the smallest sized sample
min_n_seqs <- tenmdat_pa_counts %>%
  group_by(site) %>% #groups are like sites and value is like the number of observations
  summarize(n_seqs = sum(n)) %>%
  summarize(min = min(n_seqs)) %>%
  pull(min)

min_n_seqs #44

vegans <- rarefy(tenmdat_wide, min_n_seqs) %>% #going to rarefy to the smallest sized sample. a vector of values that are rarefied to a specific observation
  as_tibble(rownames="site")
select(site, vegan=value)

View(vegans)
