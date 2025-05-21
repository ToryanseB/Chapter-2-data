###########################
### Analysis in Cooccur ###
###########################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025")

#set seed for reproducible results
set.seed(123)
#load libraries
{library(tidyverse)
  library(cooccur)
  library(sf)
  library(sp)
  library(ggplot2)}


# Geranium two km ---------------------------------------------------------

# Load Dataset and Structure Properly -------------------------------------

twokmdat_matrix<-read.csv("twokmdat_geranium.csv")

#make sure species is a factor
twokm_gdat<-twokmdat_matrix %>% 
  mutate(species = as.factor(species)) 

# Convert to data frame first, then use column_to_rownames
twokm_gdat <- as.data.frame(twokm_gdat) %>%
  column_to_rownames("species")

cooccur.twokmgdat <- cooccur(mat=twokm_gdat,
                           type="spp_site",
                           thresh=TRUE,
                           spp_names=TRUE)


# Cooccurrence Summary ----------------------------------------------------


class(cooccur.twokmgdat)#is a cooccurence
summary(cooccur.twokmgdat)#to look at overall summary
print_twokm<-print(cooccur.twokmgdat)#gives you only the significant species combinations

geranium_twokm<-print_twokm %>% 
  filter(sp1_name=="Geranium viscosissimum"| sp2_name == "Geranium viscosissimum")

View(geranium_twokm)

# Only flowering plants ---------------------------------------------------
#load taxonomy data
taxonomy<-read.csv("plant_taxonomy.csv")

#flowering plants are in subphylum Angiospermae, which is only the class Magnoliopsida and Liliopsida
flowering<-taxonomy %>% 
  filter(class %in% c("Magnoliopsida", "Liliopsida"))

#now we want to filter the plants in the print_twokm to only include species that occur in flowering dataset
filtered_twokm <- print_twokm %>%
  filter(sp1_name %in% flowering$species & sp2_name %in% flowering$species)

#and get a list of the species that have significant associations
unique_twokm <- filtered_twokm %>%
  select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species) %>%
  select(species)  # Keep only the species column

View(unique_twokm)


# Look at Effect Sizes ----------------------------------------------------



cooccur(mat = twokm_gdat, type = "spp_site", thresh = TRUE,
        spp_names = TRUE, only_effects = TRUE, eff_standard = TRUE,
        eff_matrix = TRUE)


View(print_twokm)

cooccur.twokmgdat$results#to obtain the complete set of species pairs. But if thresh is TRUE, won't have all of them
prob.table(cooccur.twokmgdat)#does the same thing

plot(cooccur.twokmgdat)#visualization of all the pairwise combinations of species and their co-occurrence signs (positive or negative) using ggplot2 heatmap. Plot trims species that do not have significant negative or positive associations. orders most negative pairs to most positive, left to right. 

pair(mod=cooccur.twokmgdat, "Geospiza fortis")#you can specify a specific species, by name or number. by default, only significant results are shown, but you can say all=TRUE for all results. 

pair.attributes(cooccur.twokmgdat)#produces a table of the percentage of each species total pairings that were positive, negative, and random. Weight the degree of significant interactions. This treats unclassifiable pairings as random. 


pair.profile(cooccur.twokmgdat)#you can use this to visualize pair.attributes across all species. creates a box plot of these percentages. Percent of species pairs that were positive, negative, and random for all species. are interactions mostly negative or positive? are these interactions evenly distributed among species? This can be used in combination with phylogenetic data or trait and resource use differences. 


# Geranium one km ---------------------------------------------------------

# Load Dataset and Structure Properly -------------------------------------

onekmdat_matrix<-read.csv("onekmdat_geranium.csv")

#make sure species is a factor
onekm_gdat<-onekmdat_matrix %>% 
  mutate(species = as.factor(species)) 

# Convert to data frame first, then use column_to_rownames
onekm_gdat <- as.data.frame(onekm_gdat) %>%
  column_to_rownames("species")

cooccur.onekmgdat <- cooccur(mat=onekm_gdat,
                             type="spp_site",
                             thresh=TRUE,
                             spp_names=TRUE)


# Cooccurrence Summary ----------------------------------------------------

class(cooccur.onekmgdat)#is a cooccurence
summary(cooccur.onekmgdat)#to look at overall summary
print_onekm<-print(cooccur.onekmgdat)#gives you only the significant species combinations

geranium_onekm<-print_onekm %>% 
  filter(sp1_name=="Geranium viscosissimum"| sp2_name == "Geranium viscosissimum")

View(geranium_onekm)

# Only flowering plants ---------------------------------------------------
#load taxonomy data
taxonomy<-read.csv("plant_taxonomy.csv")

#flowering plants are in subphylum Angiospermae, which is only the class Magnoliopsida and Liliopsida
flowering<-taxonomy %>% 
  filter(class %in% c("Magnoliopsida", "Liliopsida"))

#now we want to filter the plants in the print_onekm to only include species that occur in flowering dataset
filtered_onekm <- print_onekm %>%
  filter(sp1_name %in% flowering$species & sp2_name %in% flowering$species)

#and get a list of the species that have significant associations
unique_onekm <- filtered_onekm %>%
  select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species) %>%
  select(species)  # Keep only the species column

View(unique_onekm)
#259 species

# Look at Effect Sizes ----------------------------------------------------


onekm_effects<-cooccur(mat = onekm_gdat, type = "spp_site", thresh = TRUE,
        spp_names = TRUE, only_effects = TRUE, eff_standard = TRUE,
        eff_matrix = TRUE)





# Geranium 125m ---------------------------------------------------------

# Load Dataset and Structure Properly -------------------------------------

onetfmdat_matrix<-read.csv("onetfmdat_geranium.csv")

#make sure species is a factor
onetfm_gdat<-onetfmdat_matrix %>% 
  mutate(species = as.factor(species)) 

# Convert to data frame first, then use column_to_rownames
onetfm_gdat <- as.data.frame(onetfm_gdat) %>%
  column_to_rownames("species")

View(onetfm_gdat)

cooccur.onetfmgdat <- cooccur(mat=onetfm_gdat,
                              type="spp_site",
                              thresh=TRUE,
                              spp_names=TRUE)


# Cooccurrence Summary ----------------------------------------------------

class(cooccur.onetfmgdat)#is a cooccurence
summary(cooccur.onetfmgdat)#to look at overall summary
print_onetfm<-print(cooccur.onetfmgdat)#gives you only the significant species combinations

geranium_onetfm<-print_onetfm %>% 
  filter(sp1_name=="Geranium viscosissimum"| sp2_name == "Geranium viscosissimum")

View(geranium_onetfm)

unique_onetfm <- print_onetfm %>%
  select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species) %>%
  select(species)  # Keep only the species column

View(unique_onetfm)

plot(cooccur.onetfmgdat)
onetfmgdat_pair<-pair(mod=cooccur.onetfmgdat, "Geranium viscosissimum")#you can specify a specific species, by name or number. by default, only significant results are shown, but you can say all=TRUE for all results. 

onetfmgdat_attributes<-pair.attributes(cooccur.onetfmgdat)#produces a table of the percentage of each species total pairings that were positive, negative, and random. Weight the degree of significant interactions. This treats unclassifiable pairings as random. 

# Look at Effect Sizes ----------------------------------------------------

effects.onetfmgdat<-effect.sizes(cooccur.onetfmgdat, standardized = TRUE)

max(effects.onetfmgdat$effects)#0.2038462
min(effects.onetfmgdat$effects)#-0.1346154

# 8m radius cooccurrence analysis -----------------------------------------

# Load Dataset and Structure Properly -------------------------------------

eightdat_matrix<-read.csv("eightdat_geranium.csv")

#make sure species is a factor
eight_gdat<-eightdat_matrix %>% 
  mutate(species = as.factor(species)) 

# Convert to data frame first, then use column_to_rownames
eight_gdat <- as.data.frame(eight_gdat) %>%
  column_to_rownames("species")

View(eight_gdat)

cooccur.eightgdat <- cooccur(mat=eight_gdat,
                             type="spp_site",
                             thresh=TRUE,
                             spp_names=TRUE)


# Cooccurrence Summary ----------------------------------------------------

class(cooccur.eightgdat)#is a cooccurence
summary(cooccur.eightgdat)#to look at overall summary
print_eight<-print(cooccur.eightgdat)#gives you only the significant species combinations

geranium_eight<-print_eight %>% 
  filter(sp1_name=="Geranium viscosissimum"| sp2_name == "Geranium viscosissimum")

View(geranium_eight)

unique_eight <- print_eight %>%
  select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species) %>%
  select(species)  # Keep only the species column

View(unique_eight)

plot(cooccur.eightgdat)
eightgdat_pair<-pair(mod=cooccur.eightgdat, "Geranium viscosissimum")#you can specify a specific species, by name or number. by default, only significant results are shown, but you can say all=TRUE for all results. 

eightgdat_attributes<-pair.attributes(cooccur.eightgdat)#produces a table of the percentage of each species total pairings that were positive, negative, and random. Weight the degree of significant interactions. This treats unclassifiable pairings as random. 

# Look at Effect Sizes ----------------------------------------------------

effects.eightgdat<-effect.sizes(cooccur.eightgdat, standardized = TRUE)

max(effects.eightgdat$effects)#0.2038462
min(effects.eightgdat$effects)#-0.1346154


# 100m radius cooccurrence analysis -----------------------------------------

# Load Dataset and Structure Properly -------------------------------------

onehmdat_matrix<-read.csv("onehdat_geranium.csv")

#make sure species is a factor
onehm_gdat<-onehmdat_matrix %>% 
  mutate(species = as.factor(species)) 

# Convert to data frame first, then use column_to_rownames
onehm_gdat <- as.data.frame(onehm_gdat) %>%
  column_to_rownames("species")

View(onehm_gdat)

cooccur.onehmgdat <- cooccur(mat=onehm_gdat,
                             type="spp_site",
                             thresh=TRUE,
                             spp_names=TRUE)


# Co-occurrence Summary ----------------------------------------------------

class(cooccur.onehmgdat)#is a cooccurence
summary(cooccur.onehmgdat)#to look at overall summary
print_onehm<-print(cooccur.onehmgdat)#gives you only the significant species combinations
View(print_onehm)
geranium_onehm<-print_onehm %>% 
  filter(sp1_name=="Geranium viscosissimum"| sp2_name == "Geranium viscosissimum")

View(geranium_onehm)



plot(cooccur.onehmgdat)
onehmgdat_pair<-pair(mod=cooccur.onehmgdat, "Geranium viscosissimum")#you can specify a specific species, by name or number. by default, only significant results are shown, but you can say all=TRUE for all results. 

onehmgdat_attributes<-pair.attributes(cooccur.onehmgdat)#produces a table of the percentage of each species total pairings that were positive, negative, and random. Wonehm the degree of significant interactions. This treats unclassifiable pairings as random. 

View(onehmgdat_pair)
max(onehmgdat_attributes$num_pos)#35

onehmgdat_attributes %>% 
  filter(num_pos==35)
write.csv(onehmgdat_pair, "onehmgdat_pair.csv", row.names=FALSE)

# Look at Effect Sizes ----------------------------------------------------

effects.onehmgdat<-effect.sizes(cooccur.onehmgdat, standardized = TRUE)

max(effects.onehmgdat$effects)#0.1268293
min(effects.onehmgdat$effects)#-0.07073171

View(effects.onehmgdat)


# Look at effect sizes with the cooccur data ------------------------------

glimpse(print_onehm)
glimpse(effects.onehmgdat)

onehm_final_dat <- print_onehm %>%
  left_join(effects.onehmgdat %>% rename(sp1_name = sp1, sp2_name = sp2), 
            by = c("sp1_name", "sp2_name")) %>% 
  mutate(quality = case_when(
    p_lt < 0.05 ~ "negative",
    p_gt < 0.05 ~ "positive",
    TRUE ~ "random"
  ))
View(onehm_final_dat)

write.csv(onehm_final_dat, "onehm_final_dat.csv", row.names = FALSE)


# Compare with phenology data ---------------------------------------------

phen_dat<-read.csv("alberta_phen_dat.csv")

View(phen_dat)

phen_cooccur <- onehm_final_dat %>%
  # Join sp1 flowering data
  left_join(phen_dat %>% select(sp1_name, start_flowering, end_flowering, flowering_length), by = "sp1_name") %>%
  rename(sp1_start_flowering = start_flowering, 
         sp1_end_flowering = end_flowering,
         sp1_flowering_length = flowering_length) %>%
  
  # Join sp2 flowering data
  left_join(phen_dat %>% select(sp2_name = sp1_name, start_flowering, end_flowering, flowering_length), by = "sp2_name") %>%
  rename(sp2_start_flowering = start_flowering, 
         sp2_end_flowering = end_flowering,
         sp2_flowering_length = flowering_length) %>%
  
  # Add flowering overlap and synchrony
  mutate(
    # Convert relevant columns to numeric
    across(c(sp1_start_flowering, sp1_end_flowering, 
             sp2_start_flowering, sp2_end_flowering, 
             sp1_flowering_length, sp2_flowering_length), as.numeric),
    
    # Calculate flowering overlap
    flowering_overlap = pmax(0, pmin(sp1_end_flowering, sp2_end_flowering) - pmax(sp1_start_flowering, sp2_start_flowering) + 1),
    
    # Calculate flowering synchrony for f sp1 (x = (1/f) * e)
    flowering_synchrony_sp1 = (1 / sp1_flowering_length) * flowering_overlap,
    
    # Calculate flowering synchrony for f sp2 (x = (1/f) * e)
    flowering_synchrony_sp2 = (1 / sp2_flowering_length) * flowering_overlap
  )

View(phen_cooccur)

write.csv(phen_cooccur, "phen_cooccur.csv", row.names = FALSE)


# Find unique species for pollinator data collection ----------------------


unique_onehm <- phen_cooccur %>%
  select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species) %>%
  select(species)  # Keep only the species column

View(unique_onehm)

write.csv(unique_onehm, "unique_onehm.csv", row.names = FALSE)


# Look at max flowering overlap -------------------------------------------


max(phen_cooccur$flowering_overlap, na.rm = TRUE)#8

phen_cooccur %>% 
filter(flowering_overlap==8) %>% 
  View()

unique(phen_cooccur$flowering_overlap)


# Find any species that you need data for ---------------------------------

need_data_sp1 <- phen_cooccur %>% 
  select(sp1_name, sp1_start_flowering, sp1_end_flowering) %>% 
  filter(is.na(sp1_start_flowering) | is.na(sp1_end_flowering)) %>% 
  distinct(sp1_name, .keep_all = TRUE)

need_data_sp2 <- phen_cooccur %>% 
  select(sp2_name, sp2_start_flowering, sp2_end_flowering) %>% 
  filter(is.na(sp2_start_flowering) | is.na(sp2_end_flowering)) %>% 
  distinct(sp2_name, .keep_all = TRUE)

need_data <- bind_rows(
  need_data_sp1 %>% rename(name = sp1_name, start_flowering = sp1_start_flowering, end_flowering = sp1_end_flowering),
  need_data_sp2 %>% rename(name = sp2_name, start_flowering = sp2_start_flowering, end_flowering = sp2_end_flowering)
) %>%
  distinct(name, .keep_all = TRUE)
View(need_data)

