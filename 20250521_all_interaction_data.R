#######################################
### All pollinator interaction data ###
#######################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Pollinator_interactions")

#load libraries
library(tidyverse)
library(ggplot2)
library(stringr)

# Load Data ---------------------------------------------------------------
globi_final_data<-read.csv("globi_final_data.csv")
cpdat<- read.csv("20250505_calgarypollinators_interactions.csv")
noglobi_dat<-read.csv("noglobi_dat.csv")
unique_species<-read.csv("unique_species.csv")


# Clean Data --------------------------------------------------------------
#calgary pollinators
cpdat_cleaned<-cpdat %>% 
  rename(
    source_taxon_name = field.interaction..visited.flower.of,
    target_taxon_genus = taxon_genus_name,
    target_taxon_species = taxon_species_name,
    target_taxon_name = scientific_name,
    study_citation = url
  ) %>% 
  mutate(interaction_type = "flowersVisitedBy") %>%
  filter(source_taxon_name %in% unique_species$species) %>% 
  select(
    source_taxon_name,
    interaction_type,
    target_taxon_name, 
    target_taxon_genus, 
    target_taxon_species, 
    latitude, 
    longitude, 
    study_citation
  )

#globi data
globidat_cleaned<-globi_final_data %>%
  mutate(
    # Create genus column
    target_taxon_genus = case_when(
      target_taxon == "genus" ~ target_taxon_name,
      target_taxon == "species" ~ word(target_taxon_name, 1),  # extract first word
      TRUE ~ NA_character_
    ),
    # Create species column
    target_taxon_species = case_when(
      target_taxon == "species" ~ target_taxon_name,
      TRUE ~ NA_character_
    )
  ) %>% 
  select(
    source_taxon_name,
    interaction_type,
    target_taxon_name, 
    target_taxon_genus, 
    target_taxon_species, 
    latitude, 
    longitude, 
    study_citation
  )


#lit review data
litdat<-noglobi_dat %>%
  mutate(
    # Create genus column
    target_taxon_genus = case_when(
      target_taxon == "genus" ~ target_taxon_name,
      target_taxon == "species" ~ word(target_taxon_name, 1),  # extract first word
      TRUE ~ NA_character_
    ),
    # Create species column
    target_taxon_species = case_when(
      target_taxon == "species" ~ target_taxon_name,
      TRUE ~ NA_character_
    )
  ) %>% 
  select(
    source_taxon_name,
    interaction_type,
    target_taxon_name, 
    target_taxon_genus, 
    target_taxon_species, 
    latitude, 
    longitude, 
    study_citation
  )
  
# Combine datasets --------------------------------------------------------

#combine cpdat_cleaned, globidat_cleaned, and litdat
all_polldat<-bind_rows(cpdat_cleaned, globidat_cleaned, litdat) %>% 
  arrange(source_taxon_name)

write.csv(all_polldat, "all_polldat.csv", row.names=FALSE)

#View unique combinations of source_taxon_name and target_taxon_genus
all_polldat_genus <- all_polldat %>%
  filter(!is.na(target_taxon_genus) & target_taxon_genus != "") %>%
  distinct(source_taxon_name, target_taxon_genus, .keep_all = TRUE)

#View unique combinations of source_taxon_name and target_taxon_species
all_polldat_species<- all_polldat %>%
  filter(!is.na(target_taxon_species) & target_taxon_species != "") %>%
  distinct(source_taxon_name, target_taxon_species, .keep_all = TRUE)

#see what species aren't included
anti_join(unique_species, all_polldat, 
          by = c("species" = "source_taxon_name"))# 5 species with no interaction data: 
#species
#1    Artemisia longifolia
#2       Atriplex gardneri
#3             Carex aurea
#4         Carex filifolia
#5 Paronychia sessiliflora

#see what species aren't included
anti_join(unique_species, all_polldat_genus, 
          by = c("species" = "source_taxon_name"))# 8 species not accurate to genus
#species
#1     Artemisia longifolia
#2    Artemisia ludoviciana
#3        Atriplex gardneri
#4              Carex aurea
#5          Carex filifolia
#6 Krascheninnikovia lanata
#7      Oreocarya glomerata
#8  Paronychia sessiliflora

#see what species aren't included
anti_join(unique_species, all_polldat_species, 
          by = c("species" = "source_taxon_name"))# 8 species not accurate to species (same as genus)

