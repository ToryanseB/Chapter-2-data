########################################
### Milkweed 10m Analysis in Cooccur ###
########################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site Download May 2025")

#set seed for reproducible results
set.seed(123)
#load libraries
{library(tidyverse)
  library(cooccur)
  library(sf)
  library(sp)
  library(ggplot2)}

# 100m radius cooccurrence analysis -----------------------------------------

# Load Dataset and Structure Properly -------------------------------------

tenmdat_matrix<-read.csv("tenmdat_asclepias.csv")

#make sure species is a factor
tenm_dat<-tenmdat_matrix %>% 
  mutate(species = as.factor(species)) 

# Convert to data frame first, then use column_to_rownames
tenm_dat <- as.data.frame(tenm_dat) %>%
  column_to_rownames("species")

View(tenm_dat)

cooccur.tenmdat <- cooccur(mat=tenm_dat,
                             type="spp_site",
                             thresh=TRUE,
                             spp_names=TRUE)


# Co-occurrence Summary ----------------------------------------------------

class(cooccur.tenmdat)#is a cooccurence
summary(cooccur.tenmdat)#to look at overall summary
print_tenm<-print(cooccur.tenmdat)#gives you only the significant species combinations
View(print_tenm)
asclepias_tenm<-print_tenm %>% 
  filter(sp1_name=="Asclepias viridiflora"| sp2_name == "Asclepias viridiflora")

View(asclepias_tenm)



plot(cooccur.tenmdat)
tenmdat_pair<-pair(mod=cooccur.tenmdat, "Asclepias viridiflora")#you can specify a specific species, by name or number. by default, only significant results are shown, but you can say all=TRUE for all results. 

tenmdat_attributes<-pair.attributes(cooccur.tenmdat)#produces a table of the percentage of each species total pairings that were positive, negative, and random. Wtenm the degree of significant interactions. This treats unclassifiable pairings as random. 

View(tenmdat_attributes)
max(tenmdat_attributes$num_pos)#35

tenmdat_attributes %>% 
  filter(num_pos==35)
write.csv(tenmdat_pair, "tenmdat_pair.csv", row.names=FALSE)

# Look at Effect Sizes ----------------------------------------------------

effects.tenmdat<-effect.sizes(cooccur.tenmdat, standardized = TRUE)

max(effects.tenmdat$effects)#0.08125
min(effects.tenmdat$effects)#-0.03958333

View(effects.tenmdat)


# Look at effect sizes with the cooccur data ------------------------------

glimpse(print_tenm)
glimpse(effects.tenmdat)

tenm_final_dat <- print_tenm %>%
  left_join(effects.tenmdat %>% rename(sp1_name = sp1, sp2_name = sp2), 
            by = c("sp1_name", "sp2_name")) %>% 
  mutate(quality = case_when(
    p_lt < 0.05 ~ "negative",
    p_gt < 0.05 ~ "positive",
    TRUE ~ "random"
  ))
View(tenm_final_dat)

write.csv(tenm_final_dat, "tenm_final_dat.csv", row.names = FALSE)


# Compare with phenology data ---------------------------------------------

phen_dat<-read.csv("C:/Users/torya/OneDrive/Documents/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025/alberta_phen_dat.csv")

View(phen_dat)

phen_cooccur <- tenm_final_dat %>%
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

write.csv(phen_cooccur, "phen_cooccur_milkweed.csv", row.names = FALSE)


# Find unique species for pollinator data collection ----------------------


unique_tenm <- phen_cooccur %>%
  select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species) %>%
  select(species)  # Keep only the species column

View(unique_tenm)

write.csv(unique_tenm, "unique_tenm.csv", row.names = FALSE)


# Look at max flowering overlap -------------------------------------------


max(phen_cooccur$flowering_overlap, na.rm = TRUE)#4



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
