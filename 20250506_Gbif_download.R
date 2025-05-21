################################################
### Download gbif occurrence data and filter ###
################################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site Download May 2025")

#load libraries
set.seed(123)
#load libraries
{library(tidyverse)
  library(rgbif)
  library(sf)
  library(sp)
  library(ggplot2)}


# Download Plant Species within Boundary ----------------------------------

#load the geometry of Alberta boundary that was simplified using MapShaper to 2.2%. 
bbox<-st_read("AB_only_simplified.json")

# Convert to a form that you can use to download species occurrences from rgbif
#extract geometry
bbox_geometry <- st_geometry(bbox)

# Convert the geometry to WKT (Well-Known Text) string
bbox_wkt <- st_as_text(bbox_geometry)


#download plant species within the boundary 
occ_download(pred("taxonKey", 6), pred_within(bbox_wkt), format = "SIMPLE_CSV",user="toryanse",pwd="Gbitwb@6516*", email="toryanse.blanchard@ucalgary.ca")

#<<gbif download>>
#Your download is being processed by GBIF:
# https://www.gbif.org/occurrence/download/0005614-250325103851331
#Most downloads finish within 15 min.
#Check status with
occ_download_wait('0005614-250325103851331')
#After it finishes, use
d <- occ_download_get('0005614-250325103851331') %>%
  occ_download_import()
#to retrieve your download.
#Download Info:
#  Username: toryanse
#E-mail: toryanse.blanchard@ucalgary.ca
#Format: SIMPLE_CSV
#Download key: 0005614-250325103851331
#Created: 2025-03-28T16:08:38.776+00:00
#Citation Info:  
#  Please always cite the download DOI when using this data.
#https://www.gbif.org/citation-guidelines
#DOI: 
#  Citation:
#  GBIF Occurrence Download https://www.gbif.org/occurrence/download/0005614-250325103851331 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-03-28

# Preliminary Filter your dataset -----------------------------------------

#check that all your observations made it 
d %>% 
  filter(species=="Geranium viscosissimum", coordinateUncertaintyInMeters<=10, recordedBy=="toryanse") %>% 
  View()

max(d$coordinateUncertaintyInMeters, na.rm = TRUE)
min(d$coordinateUncertaintyInMeters, na.rm = TRUE)
as.numeric(names(sort(table(d$coordinateUncertaintyInMeters), decreasing = TRUE)[1]))#the mode is 4
mean(d$coordinateUncertaintyInMeters, na.rm = TRUE)


#filter dataset to include relevant points and add an id column
ab_dat<-d %>% 
  filter(stateProvince=="Alberta",coordinateUncertaintyInMeters<=10) %>% #filter so that only points within alberta are included and so observations with a coordinate uncertainty less than or equal to 10m are included. 
  mutate(id=1:n())

write.csv(ab_dat, "ab_dat.csv", row.names = FALSE)

ab_dat<-read.csv("ab_dat.csv")
