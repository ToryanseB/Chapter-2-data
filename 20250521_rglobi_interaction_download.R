###################################
### rglobi interaction download ###
###################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Pollinator_interactions")

# Load Libraries ----------------------------------------------------------
{library(tidyverse)
  library(rglobi)
  library(sf)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(dplyr)}

# Load Data ---------------------------------------------------------------
unique_species<-read.csv("unique_species.csv")#list of the species you need to find pollinator interaction data for

# Download from rglobi ----------------------------------------------------

# 1. Safely call get_interactions() for each species
interaction_nobox <- map(unique_species$species, function(sp) {
  tryCatch({
    df <- get_interactions(
      sp,
      interaction.type = "flowersVisitedBy",
      returnobservations = TRUE
    )
    
    # Ensure it's a non-empty dataframe with rows
    if (is.data.frame(df) && nrow(df) > 0) {
      df$species <- sp  # Optionally tag original species
      return(df)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    message(paste("No data for:", sp))
    return(NULL)
  }, warning = function(w) {
    message("Warning for species: ", sp, " - ", conditionMessage(w))
    return(NULL)  # or return some default value
  })
})

# View the list of warning messages in a dataframe
warnings_text <- c(
  "Warning for species: Achillea millefolium - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Chamaenerion angustifolium - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Cirsium arvense - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Cirsium vulgare - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Helianthus annuus - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Heterotheca villosa - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Leucanthemum vulgare - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Lotus corniculatus - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Medicago sativa - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Melilotus albus - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Melilotus officinalis - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Monarda fistulosa - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Rubus idaeus - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Symphyotrichum ericoides - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Tanacetum vulgare - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Taraxacum officinale - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Trifolium pratense - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters.",
  "Warning for species: Vicia cracca - Default results limit reached. Consider increasing limit parameter and/or using pagination to retrieve all results. See rglobi vignette section on pagination for help modifying limit/skip parameters."
)

# Extract species names using a regular expression
species_names <- gsub("Warning for species: (.*) - .*", "\\1", warnings_text)

# Create a dataframe
df_warnings <- data.frame(species = species_names)

# View the dataframe to see the species that had too many results for the limit parameters
View(df_warnings)

# 2. Remove NULLs and bind rows
interaction_nobox_all <- interaction_nobox %>%
  compact() %>%     # Removes NULL entries
  bind_rows()

#see the species that were not included
missing_species<-setdiff(unique_species$species, interaction_nobox_all$species)
missing_species_df <- data.frame(missing_species)

missing_species_df<-missing_species_df %>% 
  rename(species = missing_species)
View(missing_species_df)

# Combine manually download interactions with larger dataset --------------

#load larger dataset (less environment memory)
interaction_nobox_all<-read.csv("interaction_nobox_all.csv")

#filter to only include the columns you need
interaction_nobox_all<-interaction_nobox_all %>% 
  select(source_taxon_name, interaction_type, target_taxon_name, latitude, longitude, study_citation)

#load the manually downloaded interactions (over-limit)
# List all relevant CSV files
csv_files <- list.files(pattern = "_direct\\.csv$")

# Load and combine with filename info
over_limit <- do.call(rbind, lapply(csv_files, function(file) {
  # Read the CSV
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Extract metadata from filename
  parts <- strsplit(file, "_")[[1]]
  date <- parts[1]
  species <- paste(parts[2:(length(parts)-1)], collapse = "_")
  
  # Add metadata columns
  data$date <- as.Date(date, format = "%Y%m%d")
  data$species <- species
  
  return(data)
}))

over_limit_selected<- over_limit %>% 
  select(source_taxon_name, interaction_type, target_taxon_name, latitude, longitude, study_citation)

#combine over_limit_selected with interaction_nobox_all

# Combine the two data frames row-wise
globi_data_all <- rbind(over_limit_selected, interaction_nobox_all)

# Assign genus and species classification  --------------------------------

#load dataset with known species names
joined_globi<-read.csv("joined_globi.csv")
#filter to just the two columns we need
globi_names<-joined_globi %>% 
  select(target_taxon_name, target_taxon)

#first assign what we can based on taxonomic conventions
globi_data_all_named<-globi_data_all %>% 
  mutate(
    target_taxon = case_when(
      # Two-word name with "sp." where the first word ends in a taxonomic suffix
      str_detect(target_taxon_name, "^\\w+idae sp\\.$") ~ "family",
      str_detect(target_taxon_name, "^\\w+inae sp\\.$") ~ "subfamily",
      str_detect(target_taxon_name, "^\\w+tera sp\\.$") ~ "order",
      str_detect(target_taxon_name, "^\\w+idea sp\\.$") ~ "superfamily",
      
      # Regular two-word species name (not ending in sp.)
      str_detect(target_taxon_name, "^\\w+ \\w+$") & !str_detect(target_taxon_name, " sp\\.$") ~ "species",
      
      # One-word name classifications
      str_detect(target_taxon_name, "idae$") ~ "family",
      str_detect(target_taxon_name, "inae$") ~ "subfamily",
      str_detect(target_taxon_name, "tera$") ~ "order",
      str_detect(target_taxon_name, "idea$") ~ "superfamily",
      
      TRUE ~ NA_character_
    )
  )

#now add the data from globi names
globi_data_all_knownname <- globi_data_all_named %>%
  left_join(globi_names %>% select(target_taxon_name, target_taxon_new = target_taxon), 
            by = "target_taxon_name") %>%
  mutate(target_taxon = coalesce(target_taxon_new, target_taxon)) %>%
  select(-target_taxon_new)

#how many are undefined
globi_data_all_knownname %>% 
  distinct(target_taxon_name, .keep_all = TRUE) %>% 
  summarise(num_NAs = sum(is.na(target_taxon)))

#view those that are undefined
globi_undefined<-globi_data_all_knownname %>% 
  distinct(target_taxon_name, .keep_all = TRUE) %>% 
  filter(is.na(target_taxon))



#remove items in target_taxon_name where the entry starts with "Unidentified"
#when the item is one word and ends in a number, remove the number
globi_data_filtered <- globi_data_all_knownname %>%
  distinct(target_taxon_name, .keep_all = TRUE) %>%
  
  # Remove entries starting with "Unidentified"
  filter(!str_starts(target_taxon_name, "Unidentified")) %>%
  
  # Trim whitespace
  mutate(target_taxon_name = str_trim(target_taxon_name)) %>%
  
  # Remove trailing numbers on single-word names
  mutate(
    target_taxon_name = if_else(
      !str_detect(target_taxon_name, "\\s") & str_detect(target_taxon_name, "\\d+$"),
      str_replace(target_taxon_name, "\\d+$", ""),
      target_taxon_name
    )
  ) %>%
  
  # Remove "sp." or "spp." at end of single-word names
  mutate(
    target_taxon_name = if_else(
      !str_detect(target_taxon_name, "\\s") & str_detect(target_taxon_name, "(?i)(spp\\.|sp\\.)$"),
      str_replace(target_taxon_name, "(?i)(spp\\.|sp\\.)$", ""),
      target_taxon_name
    )
  )

#check to see what remains
globi_data_check<-globi_data_filtered %>% 
  mutate(
    target_taxon = case_when(
      # Two-word name with "sp." where the first word ends in a taxonomic suffix
      str_detect(target_taxon_name, "^\\w+idae sp\\.$") ~ "family",
      str_detect(target_taxon_name, "^\\w+inae sp\\.$") ~ "subfamily",
      str_detect(target_taxon_name, "^\\w+tera sp\\.$") ~ "order",
      str_detect(target_taxon_name, "^\\w+idea sp\\.$") ~ "superfamily",
      
      # Regular two-word species name (not ending in sp.)
      str_detect(target_taxon_name, "^\\w+ \\w+$") & !str_detect(target_taxon_name, " sp\\.$") ~ "species",
      
      # One-word name classifications
      str_detect(target_taxon_name, "idae$") ~ "family",
      str_detect(target_taxon_name, "inae$") ~ "subfamily",
      str_detect(target_taxon_name, "tera$") ~ "order",
      str_detect(target_taxon_name, "idea$") ~ "superfamily",
      
      TRUE ~ NA_character_
    )
  )

globi_data_check <- globi_data_check %>%
  left_join(globi_names %>% select(target_taxon_name, target_taxon_new = target_taxon), 
            by = "target_taxon_name") %>%
  mutate(target_taxon = coalesce(target_taxon_new, target_taxon)) %>%
  select(-target_taxon_new)

#how many are undefined
globi_data_check %>% 
  distinct(target_taxon_name, .keep_all = TRUE) %>% 
  summarise(num_NAs = sum(is.na(target_taxon)))

#view those that are undefined
globi_undefined_still<-globi_data_check %>% 
  distinct(target_taxon_name, .keep_all = TRUE) %>% 
  filter(is.na(target_taxon))
View(globi_undefined_still)


# manually fill in and correct errors -------------------------------------

globi_undefined_manual<- read.csv("globi_undefined_still.csv")

#filter
globi_manual_filtered <- globi_undefined_manual %>%
  select(-target_taxon_name_poor) %>%
  filter(target_taxon_name != "remove") %>%
  mutate(target_taxon = na_if(target_taxon, "")) %>%  # Convert "" to NA
  group_by(target_taxon_name) %>%
  fill(target_taxon, .direction = "downup") %>%
  ungroup()

#check to see what remains
globi_undefined_check2 <- globi_manual_filtered %>% 
  mutate(
    target_taxon = if_else(
      is.na(target_taxon) | str_trim(target_taxon) == "",
      case_when(
        # Two-word name with "sp." where the first word ends in a taxonomic suffix
        str_detect(target_taxon_name, "^\\w+idae sp\\.$") ~ "family",
        str_detect(target_taxon_name, "^\\w+inae sp\\.$") ~ "subfamily",
        str_detect(target_taxon_name, "^\\w+tera sp\\.$") ~ "order",
        str_detect(target_taxon_name, "^\\w+idea sp\\.$") ~ "superfamily",
        
        # Regular two-word species name (not ending in sp.)
        str_detect(target_taxon_name, "^\\w+ \\w+$") & !str_detect(target_taxon_name, " sp\\.$") ~ "species",
        
        # One-word name classifications
        str_detect(target_taxon_name, "idae$") ~ "family",
        str_detect(target_taxon_name, "inae$") ~ "subfamily",
        str_detect(target_taxon_name, "tera$") ~ "order",
        str_detect(target_taxon_name, "idea$") ~ "superfamily",
        
        TRUE ~ NA_character_
      ),
      target_taxon  # keep existing value if not NA or empty
    )
  )

globi_undefined_check2 <- globi_undefined_check2 %>%
  left_join(
    globi_names %>% select(target_taxon_name, target_taxon_new = target_taxon),
    by = "target_taxon_name"
  ) %>%
  mutate(
    # Prefer non-NA from globi_names if available
    target_taxon = coalesce(target_taxon_new, target_taxon)
  ) %>%
  select(-target_taxon_new) %>%
  mutate(
    # Only apply "subspecies" if current value is NA or blank
    target_taxon = if_else(
      (is.na(target_taxon) | str_trim(target_taxon) == "") & str_count(target_taxon_name, "\\S+") == 3,
      "subspecies",
      target_taxon
    )
  )


#how many are undefined
globi_undefined_check2 %>% 
  distinct(target_taxon_name, .keep_all = TRUE) %>% 
  summarise(num_NAs = sum(is.na(target_taxon)))

#view those that are undefined
globi_undefined_check2<-globi_undefined_check2 %>% 
  distinct(target_taxon_name, .keep_all = TRUE) %>% 
  filter(is.na(target_taxon))
View(globi_undefined_check2)

# Add known genus classification back to main dataset ---------------------
globi_new_names<-read.csv("globi_undefined_check.csv")

#add globi_new_names to globi_undefined_check
globi_all_names<- globi_undefined_check2 %>%
  left_join(globi_new_names %>% select(target_taxon_name, target_taxon_new = target_taxon), 
            by = "target_taxon_name") %>%
  mutate(target_taxon = coalesce(target_taxon_new, target_taxon)) %>%
  select(-target_taxon_new) %>% 
  distinct(target_taxon_name, .keep_all = TRUE)


#rename input in target_taxon_name in globi_data_all. the rename info is found in globi_undefined_manual
globi_data_all_updated <- globi_data_all_named %>%
  left_join(
    globi_undefined_manual %>% select(target_taxon_name_poor, target_taxon_name_new = target_taxon_name),
    by = c("target_taxon_name" = "target_taxon_name_poor")
  ) %>%
  mutate(
    target_taxon_name = coalesce(target_taxon_name_new, target_taxon_name)
  ) %>%
  select(-target_taxon_name_new)



#then fill in the names in globi_data_check with names from globi_all_names
globi_final_data <- globi_data_all_updated %>%
  left_join(
    globi_all_names %>% select(target_taxon_name, target_taxon_new = target_taxon),
    by = "target_taxon_name"
  ) %>%
  mutate(
    target_taxon = coalesce(target_taxon, target_taxon_new)
  ) %>%
  select(-target_taxon_new)

View(globi_final_data)

#see what species aren't included
anti_join(unique_species, globi_final_data, 
          by = c("species" = "source_taxon_name"))# 14 species


write.csv(globi_final_data, "globi_final_data.csv", row.names = FALSE)
