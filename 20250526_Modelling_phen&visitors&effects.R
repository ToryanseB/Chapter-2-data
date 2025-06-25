###############################################################################
### Modeling relationship between effect sizes, phenology, and visitor data ###
###############################################################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025")

#load libraries
set.seed(123)
#load libraries
{library(tidyverse)
library(ggplot2)
library(ggeffects)
library(lme4)
library(MASS)#for negative binomial
library(purrr)
library(vegan)
  library(nlme)}
# Load Data ---------------------------------------------------------------

#load data for Geranium 100m
phen_cooccur<-read.csv("phen_cooccur.csv")

#load data for milkweed 10m
phen_milkweed<-read.csv("C:/Users/torya/OneDrive/Documents/R/R_UCThesis_Rare_Plants_Alberta/Site Download May 2025/phen_cooccur_milkweed.csv")

#load pollinator interaction data
all_polldat<-read.csv("C:/Users/torya/OneDrive/Documents/R/R_UCThesis_Rare_Plants_Alberta/Pollinator_interactions/all_polldat.csv")

all_polldat <- all_polldat %>%
  mutate(
    type = if_else(
      str_detect(tolower(study_citation), "inaturalist"),
      "community science",
      "literature"
    )
  )

#unique species name for geranium
unique_species <- read.csv("unique_species.csv")

#native-non_native status

ACIMS_data_2023<-read.csv("ACIMS_data_2023.csv")

# Join data together ------------------------------------------------------

{# Step 1: Filter and summarize species-level visitors
polldat_species <- all_polldat %>%
  filter(!is.na(target_taxon_species) & str_trim(target_taxon_species) != "") %>%
  group_by(source_taxon_name) %>%
  summarise(species_visitors = list(unique(target_taxon_species)), .groups = "drop")

# Step 2: Filter and summarize genus-level visitors
polldat_genus <- all_polldat %>%
  filter(!is.na(target_taxon_genus) & str_trim(target_taxon_genus) != "") %>%
  group_by(source_taxon_name) %>%
  summarise(genus_visitors = list(unique(target_taxon_genus)), .groups = "drop")

# Step 3: Merge both into a combined summary
pollinator_summary <- full_join(polldat_species, polldat_genus, by = "source_taxon_name") %>%
  mutate(
    species_visitor_count = lengths(species_visitors),
    genus_visitor_count = lengths(genus_visitors)
  )

# Step 4: Make a second copy of pollinator_summary for sp2 with renamed columns
pollinator_summary_sp2 <- pollinator_summary %>%
  rename_with(~ paste0(.x, "_sp2"), -source_taxon_name)

# Step 5: Join both sp1 and sp2 data
cooccur_traits_ger <- phen_cooccur %>%
  # Join for sp1
  left_join(pollinator_summary, by = c("sp1_name" = "source_taxon_name")) %>%
  rename(
    species_visitors_sp1 = species_visitors,
    genus_visitors_sp1 = genus_visitors,
    species_visitor_count_sp1 = species_visitor_count,
    genus_visitor_count_sp1 = genus_visitor_count
  ) %>%
  # Join for sp2 with pre-renamed columns
  left_join(pollinator_summary_sp2, by = c("sp2_name" = "source_taxon_name"))


#add a count of shared visitors for both genus and species and a column that lists those that are shared
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    # Matching species visitors
    shared_species_visitors = map2(species_visitors_sp1, species_visitors_sp2, intersect),
    shared_species_visitor_count = map2_int(species_visitors_sp1, species_visitors_sp2, ~length(intersect(.x, .y))),
    
    # Matching genus visitors
    shared_genus_visitors = map2(genus_visitors_sp1, genus_visitors_sp2, intersect),
    shared_genus_visitor_count = map2_int(genus_visitors_sp1, genus_visitors_sp2, ~length(intersect(.x, .y)))
  )

#add a count of records for each species (a proxy for effort)
# Step 1: Count unique records per source_taxon_name
record_counts <- all_polldat %>%
  distinct(source_taxon_name, study_citation, target_taxon_name) %>%
  group_by(source_taxon_name) %>%
  summarise(sp1_records = n(), .groups = "drop")

# Step 2: Join this summary to cooccur_traits_ger by matching sp1_name and sp2_name to source_taxon_name
cooccur_traits_ger <- cooccur_traits_ger %>%
  left_join(record_counts, by = c("sp1_name" = "source_taxon_name"))

cooccur_traits_ger <- cooccur_traits_ger %>%
  left_join(record_counts, by = c("sp2_name" = "source_taxon_name"), suffix = c("", "_sp2")) %>%
  rename(sp2_records = sp1_records_sp2)

#add visitor similarity column
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    visitor_similarity = if_else(
      (genus_visitor_count_sp1 + genus_visitor_count_sp2) > 0,
      shared_genus_visitor_count / (genus_visitor_count_sp1 + genus_visitor_count_sp2),
      NA_real_  # Handle division by zero
    )
  )

#create a total record column
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(total_records = sp1_records + sp2_records)

#create a column that looks at community science records
cs_counts <- all_polldat %>%
  filter(type == "community science") %>%
  group_by(source_taxon_name) %>%
  summarise(cs_records = n(), .groups = "drop")
# Join sp1
cooccur_traits_ger <- cooccur_traits_ger %>%
  left_join(cs_counts %>% rename(sp1_cs_records = cs_records),
            by = c("sp1_name" = "source_taxon_name"))

# Join sp2
cooccur_traits_ger <- cooccur_traits_ger %>%
  left_join(cs_counts %>% rename(sp2_cs_records = cs_records),
            by = c("sp2_name" = "source_taxon_name"))
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    sp1_cs_records = replace_na(sp1_cs_records, 0),
    sp2_cs_records = replace_na(sp2_cs_records, 0)
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    community_science_percent = ((sp1_cs_records + sp2_cs_records) / (sp1_records + sp2_records)) * 100
  )

#make a transformed (only positive) effects variable
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(effects_transformed = effects + 1)

cooccur_traits_ger <- cooccur_traits_ger %>%
  left_join(
    ACIMS_data_2023 %>% dplyr::select(species, origin) %>% rename(sp1_name = species, origin_sp1 = origin),
    by = "sp1_name"
  ) %>%
  left_join(
    ACIMS_data_2023 %>% dplyr::select(species, origin) %>% rename(sp2_name = species, origin_sp2 = origin),
    by = "sp2_name"
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  # Join s_rank for sp1
  left_join(
    ACIMS_data_2023 %>% dplyr::select(species, s_rank) %>% rename(sp1_name = species, s_rank_sp1 = s_rank),
    by = "sp1_name"
  ) %>%
  # Join s_rank for sp2
  left_join(
    ACIMS_data_2023 %>% dplyr::select(species, s_rank) %>% rename(sp2_name = species, s_rank_sp2 = s_rank),
    by = "sp2_name"
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    origin_combination = paste(origin_sp1, origin_sp2, sep = "-")
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    origin_combination = purrr::map2_chr(origin_sp1, origin_sp2, ~paste(sort(c(.x, .y)), collapse = "-"))
  )

cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    s_rank_combination = paste(s_rank_sp1, s_rank_sp2, sep = "-")
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    s_rank_combination = purrr::map2_chr(s_rank_sp1, s_rank_sp2, ~paste(sort(c(.x, .y)), collapse = "-"))
  )

cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    abundance_sp1 = case_when(
      s_rank_sp1 %in% c("S1", "S2", "S3", "S1S2", "S2S3", "S3S4") ~ "rare",
      s_rank_sp1 %in% c("S4", "S5", "S4S5") ~ "peripherally rare",
      s_rank_sp1 == "SNA" ~ "common",
      s_rank_sp1 %in% c("SU", "SNR") ~ "unknown",
      TRUE ~ NA_character_
    ),
    
    abundance_sp2 = case_when(
      s_rank_sp2 %in% c("S1", "S2", "S3", "S1S2", "S2S3", "S3S4") ~ "rare",
      s_rank_sp2 %in% c("S4", "S5", "S4S5") ~ "peripherally rare",
      s_rank_sp2 == "SNA" ~ "common",
      s_rank_sp2 %in% c("SU", "SNR") ~ "unknown",
      TRUE ~ NA_character_
    )
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    abundance_combination = paste(abundance_sp1, abundance_sp2, sep = "-")
  )
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(
    abundance_combination = purrr::map2_chr(abundance_sp1, abundance_sp2, ~paste(sort(c(.x, .y)), collapse = "-"))
  )




# Automatically collapse all list columns, so that you can write as a csv
cooccur_traits_ger[] <- lapply(cooccur_traits_ger, function(col) {
  if (is.list(col)) sapply(col, function(x) paste(x, collapse = ", ")) else col
})
}



# Find rows where origin is NA for either sp1 or sp2
missing_origin_species <- cooccur_traits_ger %>%
  filter(is.na(origin_sp1) | is.na(origin_sp2)) %>%
  dplyr::select(sp1_name, origin_sp1, sp2_name, origin_sp2)

# View the distinct species with missing origin
missing_sp1 <- missing_origin_species %>%
  filter(is.na(origin_sp1)) %>%
  distinct(sp1_name)

missing_sp2 <- missing_origin_species %>%
  filter(is.na(origin_sp2)) %>%
  distinct(sp2_name)

missing_species <- bind_rows(
  missing_sp1 %>% rename(species = sp1_name),
  missing_sp2 %>% rename(species = sp2_name)
) %>%
  distinct(species)


# model-based approach to standardization ---------------------------------

library(dplyr)
library(tidyr)
library(tibble)
library(MASS)  # for glm.nb

cooccur_model_df <- cooccur_traits_ger %>%
  mutate(log_effort_product = log(sp1_records * sp2_records + 1))  # +1 avoids log(0)

#start with poisson glm
poisson_model <- glm(
  shared_genus_visitor_count ~ log_effort_product,
  data = cooccur_model_df,
  family = poisson()
)

#check for overdispersion
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / df.residual(poisson_model)
print(dispersion)  # 1.963365>1.5 = likely overdispersed

#Because it's overdispersed, switch to a negative binomial model:
nb_model <- glm.nb(
  shared_genus_visitor_count ~ log_effort_product,
  data = cooccur_model_df
)
#check for dispersion
dispersion <- sum(residuals(nb_model, type = "pearson")^2) / df.residual(nb_model)
print(dispersion)

# Find the rows actually used in the model (no missing data)
used_rows <- as.numeric(rownames(nb_model$model))

# Create a vector of NA of full length
residuals_full <- rep(NA, nrow(cooccur_model_df))

# Assign residuals to the used rows
residuals_full[used_rows] <- residuals(nb_model, type = "pearson")

# Add to dataframe
cooccur_model_df$standardized_shared_visitors <- residuals_full


#add standardized values to the main dataframe
#ensure they have row numbers
cooccur_model_df <- cooccur_model_df %>%
  mutate(row_id = row_number())

cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(row_id = row_number())
#Join the residuals into cooccur_ger_filtered using row_id
cooccur_traits_ger <- cooccur_traits_ger %>%
  left_join(
    cooccur_model_df %>% dplyr::select(row_id, standardized_shared_visitors),
    by = "row_id"
  )

write.csv(cooccur_traits_ger, "cooccur_traits_ger.csv", row.names = FALSE)
# write csv for student ---------------------------------------------------

View(cooccur_traits_ger)

cooccur_traits<-cooccur_traits_ger %>% 
  dplyr::select(1:13, 23:30, 32:39, standardized_shared_visitors)

write.csv(cooccur_traits, "cooccur_traits_shared.csv", row.names = FALSE)


# the final dataframe -----------------------------------------------------

#filter for only those columns you need for modelling
cooccur_ger_filtered <- cooccur_traits_ger %>% 
  dplyr::select(
    sp1_name, sp2_name, effects, effects_transformed,quality,
    flowering_overlap, flowering_synchrony, shared_genus_visitors, shared_genus_visitor_count,
    visitor_similarity,standardized_shared_visitors, community_science_percent, origin_combination, s_rank_combination,
    abundance_sp1, abundance_sp2, abundance_combination
  ) %>%
  filter(abundance_sp1 != "unknown", abundance_sp2 != "unknown") %>%
  mutate(
    community_science_bin = if_else(
      community_science_percent >= 50,
      "community science",
      "literature"
    ),
    visitor_similarity_bin = if_else(
      visitor_similarity == 0,
      "no-shared",
      "shared"
    )
  )





# Exploring the data (the right way) --------------------------------------
cooccur_ger_filtered$f_abundance_combination <- factor(cooccur_ger_filtered$abundance_combination)
model_data <- cooccur_ger_filtered %>%
  dplyr::select(effects, quality, flowering_overlap, flowering_synchrony, visitor_similarity,standardized_shared_visitors, f_abundance_combination, community_science_bin) %>%
  drop_na()
View(model_data)

#outliers

op<-par(mfrow = c(2,2), mar = c(3,3,3,1))
dotchart(model_data$effects, main = "effects", group = model_data$f_abundance_combination)
plot(0,0, type="n", axes = "FALSE")
dotchart(model_data$flowering_overlap, main = "flowering_overlap", group = model_data$f_abundance_combination)
dotchart(model_data$standardized_shared_visitors, main = "standardized_shared_visitors", group = model_data$f_abundance_combination)
dotchart(model_data$flowering_synchrony, main = "flowering synchrony", group= model_data$f_abundance_combination)

model_data %>% 
  filter(flowering_overlap==9)
model_data %>% 
  filter(effects>0.1)
  #the outliers are not the same observation so we can't just remove them

#colinearity

# Load library
library(GGally)

# Create a cleaner version of your data
Z <- model_data %>%
  dplyr::select(
    effects,
    flowering_overlap,
    flowering_synchrony,
    standardized_shared_visitors
  )

# Rename for nicer axis labels
colnames(Z) <- c("Effects", "Flowering Overlap","flowering synchrony", "Standardized Shared Visitors")

# Plot pairwise relationships
ggpairs(Z,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5)),
        diag = list(continuous = wrap("barDiag", bins = 20))
)

#normality: in terms of when you look at a scatter plot with effects as the y
  #flowering overlap is fairly normal, except for high values of overlap
  #flowering synchrony is normally distributed , but there is a big break at some points
  #shared visitors is normal

#homogeneity in terms of effects
  #the variance is different for all points, which makes sense. 




# relationships

coplot(effects ~ flowering_synchrony | standardized_shared_visitors, 
       data = model_data,
       number = 4,      # Number of conditioning intervals
       overlap = 0.3,   # Degree of overlap between intervals
       panel = panel.smooth,  # Add smoothed trend lines
       pch = 19, col = "blue")

coplot(effects ~ standardized_shared_visitors | flowering_synchrony, 
       data = model_data,
       number = 4,
       overlap = 0.3,
       panel = panel.smooth,
       pch = 19, col = "darkgreen")

coplot(effects ~ flowering_synchrony | f_abundance_combination, 
       data = model_data,
       number = 4,      # Number of conditioning intervals
       overlap = 0.3,   # Degree of overlap between intervals
       panel = panel.smooth,  # Add smoothed trend lines
       pch = 19, col = "blue")

coplot(effects ~ standardized_shared_visitors |f_abundance_combination, 
       data = model_data,
       number = 6,
       overlap = 0.3,
       panel = panel.smooth,
       pch = 19, col = "darkgreen")
  #SO, this means that the relationship between effects and flowering_synchrony is dependent on f_abundance_combination, and the same for standardized shared visitors


#linear regression
M1<-lm(effects~flowering_overlap+f_abundance_combination+ standardized_shared_visitors, data=model_data)
summary(M1)
drop1(M1, test = "F")
anova(M1)
#add interaction terms that may be important
M2<-lm(effects~flowering_overlap*f_abundance_combination+ standardized_shared_visitors*f_abundance_combination, data=model_data)
AIC(M1, M2)#AIC is higher for M2
#drop terms one by one to see which can be let go
M3<-lm(effects~flowering_overlap+ standardized_shared_visitors, data=model_data)#drop abundance
M4<-lm(effects~flowering_overlap+f_abundance_combination, data=model_data)#drop visitors
M5<-lm(effects~standardized_shared_visitors+f_abundance_combination, data=model_data)#drop flowering

anova(M1,M3)#anova says abundance is significant (reject null hypothesis).
anova(M1, M4)# visitors is significant.
anova(M1, M5)#flowering overlap might not be



M6<-lm(effects~flowering_overlap*f_abundance_combination+ standardized_shared_visitors, data=model_data)
M7<-lm(effects~standardized_shared_visitors*f_abundance_combination+flowering_overlap, data=model_data)
M8<-lm(effects~standardized_shared_visitors*f_abundance_combination, data=model_data)
M9<-lm(effects~flowering_overlap*f_abundance_combination, data=model_data)

AIC(M1,M2, M3, M4, M5, M6, M7, M8, M9)
#df       AIC
#M1   9 -32982.09
#M2  19 -32982.42
#M3   4 -32782.03
#M4   8 -32946.68
#M5   8 -32983.04
#M6  14 -32988.46 #M6 is the best
#M7  14 -32975.98
#M8  13 -32976.94
#M9  13 -32950.77

BIC(M1,M2, M3, M4, M5, M6, M7, M8, M9)
#df       BIC
#M1  9 -32920.74
#M2 19 -32852.89
#M3  4 -32754.76
#M4  8 -32892.14
#M5  8 -32928.51 #M5 is the best
#M6 14 -32893.02 
#M7 14 -32880.54
#M8 13 -32888.32
#M9 13 -32862.15

#try with synchrony to compare

#linear regression
M1<-lm(effects~flowering_synchrony+f_abundance_combination+ standardized_shared_visitors, data=model_data)
summary(M1)
drop1(M1, test= "F")#flowering synchrony may not be significant. 
anova(M1)
#add interaction terms that may be important
M2<-lm(effects~flowering_synchrony*f_abundance_combination+ standardized_shared_visitors*f_abundance_combination, data=model_data)
AIC(M1, M2)#AIC is higher for M1
#drop terms one by one to see which can be let go
M3<-lm(effects~flowering_synchrony+ standardized_shared_visitors, data=model_data)#drop abundance
M4<-lm(effects~flowering_synchrony+f_abundance_combination, data=model_data)#drop visitors
M5<-lm(effects~standardized_shared_visitors+f_abundance_combination, data=model_data)#drop flowering

anova(M1,M3)#anova says abundance is significant (reject null hypothesis).
anova(M1, M4)# visitors is significant.
anova(M1, M5)#flowering synchrony might not be

M6<-lm(effects~flowering_synchrony*f_abundance_combination+ standardized_shared_visitors, data=model_data)
M7<-lm(effects~standardized_shared_visitors*f_abundance_combination+flowering_synchrony, data=model_data)
M8<-lm(effects~standardized_shared_visitors*f_abundance_combination, data=model_data)
M9<-lm(effects~flowering_synchrony*f_abundance_combination, data=model_data)

AIC(M1,M2, M3, M4, M5, M6, M7, M8, M9)
#df       AIC
#M1  9 -32981.79
#M2 19 -32976.59
#M3  4 -32782.03
#M4  8 -32946.68
#M5  8 -32983.04 #M5 is the best
#M6 14 -32982.60
#M7 14 -32975.67
#M8 13 -32976.94
#M9 13 -32949.16
BIC(M1,M2, M3, M4, M5, M6, M7, M8, M9)
#df       BIC
#M1  9 -32920.43
#M2 19 -32847.07
#M3  4 -32754.76
#M4  8 -32892.14
#M5  8 -32928.51 #M5 is the best
#M6 14 -32887.16
#M7 14 -32880.23
#M8 13 -32888.32
#M9 13 -32860.54


#so overall, the plot M6 with flowering_overlap fits the best, in terms of AIC. 
lm1<-lm(effects~flowering_overlap*f_abundance_combination+ standardized_shared_visitors, data=model_data)
summary(lm1)

drop1(lm1, test = "F") #confirming that removing either term would make the model worse. if the F value is greater than 1 and p value less than 0.05, then that term is important to keep in the model. 


#validate the model
op<- par(mfrow=c(2,2))
plot(lm1) #standard graphical output
win.graph(); op<- par(mfrow=c(2,2))
#check for normality
E<-rstandard(lm1)
hist(E, main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals")
qqnorm(E)#roughly bell shaped. centered around zero (expected). tapering tails. but there is a slight right skew (positive skewness) and heavy tails which may indicate outliers or milkd non-normality. high peak near zero maybe due to discrete predictors (like flowering overlap) or overfitting (lots of residuals close to zero). 
qqline(E)#lots of right skew, somore positive residuals than expected. if your sample size is large, like greater than 500, then this is likely not a problem. could affect tye 1 and 2 error rates for p-values. 

#check for independence and homogeneity: residuals versus individual explanatory variables
plot(y=E, x=model_data$flowering_overlap, xlab="flowering overlap", ylab= "residuals")
abline(0,0)
  #more positive than negative residuals especially at 2 and 5 suggests non-normality
  #variance is roughly similar, though slight decrease in spread at higher values, so mild heteroscedasticity
  #no clear pattern or curve, so there's no strong evidence of dependence. 
  #a few large residuals could be influenetial, consider checking leverage and cook's distance
plot(E~model_data$standardized_shared_visitors, xlab="standardized shared visitors", ylab= "residuals")
abline(0,0)
  #no clear violation of independence
  #slight concern of homoscedasticity. residual spread is larger at low shared visitors. 
  #there are a few outliers so check influence diagnostics like cook's distance

plot(y=E, x=model_data$f_abundance_combination, xlab="abundance combination", ylab= "residuals")
abline(0,0)
par(op)

# Better: Residuals by factor level
boxplot(residuals(lm1) ~ model_data$f_abundance_combination,
        ylab = "Residuals", xlab = "f_abundance_combination",
        main = "Residuals by Abundance Combination")
abline(h = 0, lty = 2)
library(car)
leveneTest(residuals(lm1) ~ model_data$f_abundance_combination)
#you have heteroscedacisity in the abundance combination, so you have to correct for that using something like gls with nlme from varident

plot(cooks.distance(lm1), type = "h")
abline(h = 4 / nrow(model_data), col = "red", lty = 2)
  #there are some points that are drastically above the red line, which means they could be influential. 
influential_points <- which(cooks.distance(lm1) > 0.004)
model_data[influential_points, ]
#remove them to see how influential they are
lm1_no_infl <- lm(effects ~ flowering_overlap*f_abundance_combination+ standardized_shared_visitors, data = model_data[-influential_points, ])
summary(lm1_no_infl)
#"To assess the influence of potential outliers, we removed 6 points with high Cook's distance (>0.004). Results were qualitatively similar (see Appendix Table X), suggesting the model is robust to influential values."
  #this is what you could say in your model validation. 


# Lets try a gls to correct for heteroscedacisity -------------------------

#step 2, fit the model with all explanatory variables and interactions using gls
M.lm1 <- gls(effects ~ flowering_overlap*f_abundance_combination+ standardized_shared_visitors*f_abundance_combination, method="REML",data = model_data)


#step 3, choose a variance structure

#try correcting for heterogeneity using varIdent variance structure because f_abundance_combination is ordinal
vf1 <- varIdent(form = ~1| f_abundance_combination)
M.gls1 <- gls(effects ~ flowering_overlap*f_abundance_combination+ standardized_shared_visitors*f_abundance_combination, weights = vf1, method= "REML", data = model_data)

AIC(M.lm1, M.gls1)#the gls fits way better than the linear







# when you picked a model, visualize --------------------------------------

#visualize the relationships
ggplot(model_data, aes(x = flowering_overlap, y = effects, color = f_abundance_combination)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", aes()) +
  theme_minimal()

ggplot(model_data, aes(x = flowering_overlap, y = effects, color = f_abundance_combination)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Interaction between Flowering Overlap and Abundance Combination",
       x = "Flowering Overlap", y = "Effects")
ggplot(model_data, aes(x = flowering_overlap, y = effects)) +
  geom_point(alpha = 0.3, aes(color = f_abundance_combination)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ f_abundance_combination) +
  theme_minimal()
ggplot(model_data, aes(x = flowering_overlap, y = effects)) +
  geom_point(alpha = 0.3, aes(color = f_abundance_combination)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # <-- this line adds it
  facet_wrap(~ f_abundance_combination) +
  theme_minimal()

ggplot(model_data, aes(x = standardized_shared_visitors, y = effects, color = f_abundance_combination)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Interaction between Visitor Similarity and Abundance Combination",
       x = "Standardized Visitor Similarity", y = "Effects")

library(ggeffects)

# Get predicted values for one predictor at a time, conditioned on the factor
pred_overlap <- ggpredict(lm1, terms = c("flowering_overlap", "f_abundance_combination"))
pred_visitors <- ggpredict(lm1, terms = c("standardized_shared_visitors", "f_abundance_combination"))
plot(pred_overlap) + 
  labs(title = "Effect of Flowering Overlap by Abundance Combination")

plot(pred_visitors) + 
  labs(title = "Effect of Visitor Similarity by Abundance Combination")

#relationship between abundance and effects
ggplot(model_data, aes(x = f_abundance_combination, y = effects)) +
  geom_boxplot() +
  theme_minimal()


# Try a GAM ---------------------------------------------------------------

library(mgcv)
AM1 <- gam(effects~ s(flowering_overlap) + s(standardized_shared_visitors) + f_abundance_combination, data = model_data)
anova(AM1)



# The 10-step Approach ----------------------------------------------------

#try from the book, the 10 step approach


#step 1, linear regression
cooccur_ger_filtered$f_abundance_combination <- factor(cooccur_ger_filtered$abundance_combination)
cooccur_ger_filtered$f_flowering_overlap <- factor(cooccur_ger_filtered$flowering_overlap)
cooccur_ger_filtered$visitor_similarity_bin <- factor(cooccur_ger_filtered$visitor_similarity_bin)

model_data <- cooccur_ger_filtered %>%
  dplyr::select(effects, flowering_overlap, visitor_similarity_bin, f_abundance_combination) %>%
  drop_na()


M.lm<- lm(effects~flowering_overlap*f_abundance_combination+visitor_similarity*f_abundance_combination, data=model_data)
summary(M.lm)




#dealing with Heterogeneity
plot(M.lm, which = 1)#the spread is not even, so there's heterogeneity, let's see where it comes from

  #for the NA values you removed
  model_data <- model.frame(M.lm)
  # Add residuals
  model_data$resid <- resid(M.lm)

# Plot the visitor similarity versus residuals
plot(model_data$visitor_similarity, model_data$resid,
     xlab = "Visitor Similarity", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
  #there is a pattern here. as the visitors get more similar, the variance decreases

#plot flowering overlap (factor) versus residuals
plot(model_data$flowering_overlap, model_data$resid,
     xlab = "Flowering overlap", ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)
  #this looks fine

# Plot the abundance combination versus residuals
plot(model_data$f_abundance_combination, model_data$resid,
       xlab = "Abundance Combination", ylab = "Residuals")
  #also not a problem

#SO it looks like variance is dependent on visitor similarity
library(nlme) 
model_data <- cooccur_ger_filtered %>%
  filter(
    !is.na(effects),
    !is.na(visitor_similarity_bin),
    !is.na(f_flowering_overlap),
    !is.na(f_abundance_combination),
    !is.na(community_science_bin)
  )


#step 2, fit the model with gls
M.lm1 <- gls(effects~flowering_overlap*f_abundance_combination+visitor_similarity*f_abundance_combination, data=model_data)


#step 3, choose a variance structure

#try correcting for heterogeneity using varIdent variance structure because visitor_similarity_bin is ordinal
M.lm2 <- gls(effects ~flowering_overlap * visitor_similarity_bin+f_abundance_combination,data = model_data)
vf1 <- varIdent(form = ~1| visitor_similarity_bin)
M.gls1 <- gls(effects ~ flowering_overlap*visitor_similarity_bin+f_abundance_combination, weights = vf1, data = model_data)

#notice that variance also changes with flowering_overlap, so lets try VarIdent Variance structure on flowering_overlap as a factor
model_data$f_flowering_overlap <- factor(model_data$flowering_overlap)
vf2<-varIdent(form=~1|f_flowering_overlap)
M.gls2<-gls(effects~f_flowering_overlap*visitor_similarity_bin+f_abundance_combination,data=model_data, weights = vf2)

summary(M.lm2)
summary(M.gls2)
#this model likely overfits for variance, so flowering_overlap should be treated as a continuous variable
#try using VarExp but with flowering_overlap

vf2exp<-varExp(form=~flowering_overlap)
M.gls3<- gls(effects ~ flowering_overlap*visitor_similarity_bin+f_abundance_combination, weights = vf2exp, data = model_data)

#varConstPower also didn't work
#varCombination didn't work

AIC(M.lm2,M.gls1, M.gls3)
#the original model is still the best, so these ways of correcting didn't work. 
#so, i don't think this method will work for heteroscedasticity. 

M.gls<- gls(effects ~flowering_overlap * visitor_similarity_bin+f_abundance_combination,data = model_data)


# step 4, fit the model

M1.lme<-lme(effects ~flowering_overlap * visitor_similarity_bin+f_abundance_combination,random=~1|community_science_bin, method = "REML", data = model_data)


#step 5, compare new model with old model

anova(M.gls, M1.lme)
#because we are testing on the boundary, we need the converted p value
0.5*(1-pchisq(1.522021, 1))
  #0.02294259
#the likelihood ratio test indicates that the new model, M1.lme is slightly better with an L.Ratio of 4.45144 and a p-value of 0.01743591. The AIC is also smaller.


#step 6, Everything okay?

#then we plot the residuals now
E2<- resid(M1.lme, type = "normalized")
F2<- fitted (M1.lme)
op<-par(mfrow = c(2,2), mar=c(4,4,3,2))
MyYlab<-"residuals"
plot(x=F2, y=E2, xlab= "fitted values", ylab = MyYlab)
boxplot(E2~flowering_overlap, data=model_data, main= "Flowering Overlap", ylab= MyYlab)
boxplot(E2~f_abundance_combination, data=model_data, main="Abundance Combination", ylab = MyYlab)
plot(x=model_data$log_visitor_similarity, y=E2, ylab = MyYlab, main= "log visitor similarity")
par(op)

  #again, visitor similarity and flowering overlap are creating independence problems and violating heterogeneity.   we will return to this problem later in the section. 

#step 7 and 8: the optimal fixed structure

summary(M1.lme)
#looks like log_visitor_similarity is not significant and the interaction between flowering_overlap and log_visitor_similarity might not be significant either. 

M1.Full<-lme(effects ~flowering_overlap * log_visitor_similarity+f_abundance_combination,random= ~ 1 | community_science_bin, method = "ML", data = model_data)
M1.A<-update(M1.Full, .~. -flowering_overlap:log_visitor_similarity)
M2.Full<-lme(effects ~flowering_overlap*log_visitor_similarity,random= ~ 1 | community_science_bin, method = "ML", data = model_data)
anova(M1.Full, M1.A, M2.Full)
M3.Full<-lme(effects ~flowering_overlap+log_visitor_similarity+f_abundance_combination,random= ~ 1 | community_science_bin, method = "ML", data = model_data)
M3.A <-update(M3.Full, .~. -flowering_overlap)
M3.B <-update(M3.Full, .~. -log_visitor_similarity)
M3.C <-update(M3.Full, .~. -f_abundance_combination)
anova(M1.Full,M3.Full, M3.A, M3.B, M3.C)

#my full model M1.Full with the interaction term is the best fit model based on AIC and logLik

#step 9: refit with REML and validate the model


#Add correction for heteroscedasticity

model_data$visitor_similarity_bin <- ifelse(model_data$log_visitor_similarity == 0, "zero", "nonzero")

vf_ident <- varIdent(form = ~1 | visitor_similarity_bin)

M5<- lme(effects ~flowering_overlap * log_visitor_similarity+f_abundance_combination,random= ~ 1 | community_science_bin, method = "REML", weight= vf_ident,data = model_data)
summary(M5)

#then we plot the residuals now
E2<- resid(M5, type = "normalized")
F2<- fitted (M5)
op<-par(mfrow = c(2,2), mar=c(4,4,3,2))
MyYlab<-"residuals"
plot(x=F2, y=E2, xlab= "fitted values", ylab = MyYlab)
boxplot(E2~flowering_overlap, data=model_data, main= "Flowering Overlap", ylab= MyYlab)
boxplot(E2~f_abundance_combination, data=model_data, main="Abundance Combination", ylab = MyYlab)
plot(x=model_data$log_visitor_similarity, y=E2, ylab = MyYlab, main= "log visitor similarity")
par(op)

















# Find which species need more data ---------------------------------------

# Step 1: Mark missing values
missing_dat <- cooccur_traits_ger %>%
  mutate(
    sp1_flowering_missing = is.na(sp1_start_flowering) | is.na(sp1_end_flowering) |
      sp1_start_flowering == "" | sp1_end_flowering == "",
    sp2_flowering_missing = is.na(sp2_start_flowering) | is.na(sp2_end_flowering) |
      sp2_start_flowering == "" | sp2_end_flowering == "",
    sp1_visitor_missing = is.na(genus_visitors_sp1) | genus_visitors_sp1 == "",
    sp2_visitor_missing = is.na(genus_visitors_sp2) | genus_visitors_sp2 == "",
    
    # Mark species missing flowering or visitor data
    sp1_missing_type = case_when(
      sp1_flowering_missing ~ "flowering",
      sp1_visitor_missing ~ "visitor",
      TRUE ~ NA_character_
    ),
    sp2_missing_type = case_when(
      sp2_flowering_missing ~ "flowering",
      sp2_visitor_missing ~ "visitor",
      TRUE ~ NA_character_
    )
  )
# Step 2: Create list of species with missing data
sp1_missing <- missing_dat %>%
  filter(!is.na(sp1_missing_type)) %>%
  dplyr::select(species = sp1_name, missing_type = sp1_missing_type)

sp2_missing <- missing_dat %>%
  filter(!is.na(sp2_missing_type)) %>%
  dplyr::select(species = sp2_name, missing_type = sp2_missing_type)

# Step 3: Combine and remove duplicates
species_missing_data <- bind_rows(sp1_missing, sp2_missing) %>%
  distinct()

write.csv(species_missing_data, "species_missing_data.csv", row.names = FALSE)

species_missing_flowering<-species_missing_data %>% 
  filter(missing_type=="flowering")

species_missing_visitors<-species_missing_data %>% 
  filter(missing_type=="visitor")






# standardize species visitor counts --------------------------------------

min(record_counts$sp1_records)#1
max(record_counts$sp1_records)#3909
mean(record_counts$sp1_records)#120.3
median(record_counts$sp1_records)#21

#use a simple method to standardize the shared visitor counts
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(standard_shared_visitors = shared_genus_visitor_count / pmin(sp1_records, sp2_records))

#use rarefy and bootstrapping to standardize
all_polldat_standard <- all_polldat %>%
  filter(source_taxon_name %in% unique_species$species) %>%  #only include species that are in the analysis
  filter(!is.na(target_taxon_genus), target_taxon_genus != "") %>% #remove NA or blank spaces from genus 
  distinct(source_taxon_name, target_taxon_genus, study_citation) %>% # keep unique interactions
  count(source_taxon_name, target_taxon_genus)#count unique interactions

#find the smallest sized sample
min_n_seqs <- all_polldat_standard  %>%
  group_by(source_taxon_name) %>% #groups are like plant species and value is like the number of observed interactions with a genus
  summarize(n_seqs = sum(n)) %>%
  summarize(min = min(n_seqs)) %>%
  pull(min)
  
all_poll_wide<-all_polldat_standard %>% 
  pivot_wider(
    names_from = target_taxon_genus,
    values_from = n,
    values_fill = 0
  ) %>% as.data.frame()

#rearrange in proper format for rarefy()
rownames(all_poll_wide) <- all_poll_wide$source_taxon_name #because tibbles do not allow row names, make the group name the row name
all_poll_wide <- all_poll_wide[,-1] #now remove the group name from the dataframe. 

View(all_poll_wide)

#use Vegan and rarefy()
vegans <- rarefy(all_poll_wide, sample=min_n_seqs)#going to rarefy to the smallest sized sample. a vector of values that are rarefied to a specific observation

# Convert to tibble with proper column names
polldat_vegans <- tibble(
  Group = names(vegans),
  vegan = as.numeric(vegans)
)
    #this takes away too many species because there are many that only have 1 observation


# model-based approach to standardization ---------------------------------

library(dplyr)
library(tidyr)
library(tibble)
library(MASS)  # for glm.nb

cooccur_model_df <- cooccur_traits_ger %>%
  mutate(log_effort_product = log(sp1_records * sp2_records + 1))  # +1 avoids log(0)

#start with poisson glm
poisson_model <- glm(
  shared_genus_visitor_count ~ log_effort_product,
  data = cooccur_model_df,
  family = poisson()
)

#check for overdispersion
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / df.residual(poisson_model)
print(dispersion)  # 2.030945>1.5 = likely overdispersed

#If it's overdispersed (likely), switch to a negative binomial model:
nb_model <- glm.nb(
  shared_genus_visitor_count ~ log_effort_product,
  data = cooccur_model_df
)
#check for dispersion
dispersion <- sum(residuals(nb_model, type = "pearson")^2) / df.residual(nb_model)
print(dispersion)


# Find the rows actually used in the model (no missing data)
used_rows <- as.numeric(rownames(nb_model$model))

# Create a vector of NA of full length
residuals_full <- rep(NA, nrow(cooccur_model_df))

# Assign residuals to the used rows
residuals_full[used_rows] <- residuals(nb_model, type = "pearson")

# Add to dataframe
cooccur_model_df$standardized_shared_visitors <- residuals_full


# Simple linear regression
lm_model <- lm(standardized_shared_visitors ~ flowering_overlap, data = cooccur_model_df)
summary(lm_model)

ggplot(cooccur_model_df, aes(x = flowering_overlap, y = standardized_shared_visitors)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Standardized Shared Visitors vs. Flowering Overlap",
    x = "Flowering Overlap",
    y = "Effort-Corrected Shared Visitors (Residuals)"
  )

#check residuals
plot(lm_model, which=1)
plot(lm_model, which=2)



# Bootstrap Approach ------------------------------------------------------

# Assume `all_polldat` has: source_taxon_name, target_taxon_genus, study_citation
interaction_df <- all_polldat %>%
  filter(!is.na(target_taxon_genus), target_taxon_genus != "") %>%
  distinct(source_taxon_name, target_taxon_genus, study_citation)

bootstrap_shared_visitors <- function(df, sp1, sp2, n_iter = 1000) {
  sp1_visitors <- df %>% filter(source_taxon_name == sp1)
  sp2_visitors <- df %>% filter(source_taxon_name == sp2)
  
  # How many records to sample (min of available records or known sampling effort)
  n1 <- nrow(sp1_visitors)
  n2 <- nrow(sp2_visitors)
  
  if (n1 < 2 | n2 < 2) return(NA)  # skip if not enough data
  
  shared_counts <- numeric(n_iter)
  
  for (i in seq_len(n_iter)) {
    sample1 <- sample(sp1_visitors$target_taxon_genus, size = n1, replace = TRUE)
    sample2 <- sample(sp2_visitors$target_taxon_genus, size = n2, replace = TRUE)
    
    shared_counts[i] <- length(intersect(unique(sample1), unique(sample2)))
  }
  
  return(shared_counts)
}

# For cooccur_traits_ger with sp1_name, sp2_name, shared_genus_visitor_count
library(purrr)

cooccur_traits_boot <- cooccur_traits_ger %>%
  rowwise() %>%
  mutate(
    boot_shared = list(bootstrap_shared_visitors(interaction_df, sp1_name, sp2_name))
  ) %>%
  ungroup() %>%
  mutate(
    expected_mean = map_dbl(boot_shared, mean, na.rm = TRUE),
    expected_sd = map_dbl(boot_shared, sd, na.rm = TRUE),
    z_score = (shared_genus_visitor_count - expected_mean) / expected_sd
  )


lm_boot <- lm(z_score ~ flowering_overlap, data = cooccur_traits_boot)
summary(lm_boot)

#separate positive and negative effects
cooccur_ger_positive<-cooccur_traits_boot %>% 
  filter(quality=="positive") %>% 
  mutate(effects_transform = 1 / effects)
cooccur_ger_negative <- cooccur_traits_boot %>%
  filter(quality == "negative") %>%
  mutate(negative_effects = effects * -1) %>% 
  mutate(effects_transform = 1 / negative_effects)

#try a linear model
model_positive <- lm(effects_transform ~ z_score*flowering_overlap, data = cooccur_ger_positive)
summary(model_positive)

plot(model_positive, which =1)
plot(model_positive, which =2)

#try a linear model
model_negative <- lm(effects_transform ~ z_score*flowering_overlap, data = cooccur_ger_negative)
summary(model_negative)

plot(model_negative, which =1)
plot(model_negative, which =2)


# for if you wanted to look at all effect sizes ---------------------------

# Step 1: Filter the dataset
cooccur_traits_ger_missing <- cooccur_traits_ger %>%
  filter(
    is.na(genus_visitors_sp1) |
      is.na(genus_visitors_sp2) |
      is.na(sp1_flowering_length) |
      is.na(sp2_flowering_length)
  )

names(cooccur_traits_ger_missing)

# Step 2: Extract and deduplicate species names
species_list <- cooccur_traits_ger_missing %>%
  dplyr::select(sp1_name, sp2_name) %>%
  pivot_longer(cols = everything(), values_to = "species") %>%
  distinct(species) %>%
  arrange(species)

View(species_list)

# Automatically collapse all list columns, so that you can write as a csv
cooccur_traits_ger[] <- lapply(cooccur_traits_ger, function(col) {
  if (is.list(col)) sapply(col, function(x) paste(x, collapse = ", ")) else col
})
write.csv(cooccur_traits_ger, "cooccur_traits_ger.csv", row.names = FALSE)


#look at Geranium's visitors
cooccur_traits_ger %>% 
  dplyr::select(sp1_name, sp2_name, effects, quality, shared_species_visitors, shared_species_visitor_count, shared_genus_visitors, shared_genus_visitor_count) %>% 
  filter(sp1_name == "Geranium viscosissimum" | sp2_name == "Geranium viscosissimum") %>% 
  View()

# Visualize the data ------------------------------------------------------

ggplot(cooccur_model_df, aes(x = flowering_overlap, y = standard_shared_visitors)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.6) +
  labs(x = "Flowering Overlap", y = "Shared Visitors") +
  theme_minimal()

#look at basic scatter plot
ggplot(cooccur_traits_ger, aes(x = flowering_overlap, y = standard_shared_visitors)) +
  geom_point() +
  theme_minimal() +
  labs(x = "flowering overlap (months)", y = "Number of Shared Genus Visitors")

#frequency histogram/density plot
ggplot(cooccur_traits_ger, aes(x = flowering_overlap)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Flowering Overlap", y = "Frequency") +
  theme_minimal()
ggplot(cooccur_traits_ger, aes(x = (flowering_synchrony^2))) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(x = "Shared Flowering Synchrony", y = "Frequency") +
  theme_minimal()


ggplot(cooccur_model_df, aes(x = standardized_shared_visitors)) +
  geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
  labs(x = "shared visitors", y = "Frequency") +
  theme_minimal()

ggplot(cooccur_model_df, aes(x = log(standardized_shared_visitors+1))) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(x = "log(Number of Shared Visitors+1)", y = "Frequency") +
  theme_minimal()

ggplot(cooccur_traits_ger, aes(x = sqrt(standard_shared_visitors+0.5))) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(x = "???(Number of Shared Visitors+1)", y = "Frequency") +
  theme_minimal()


#histogram but with smooth overlay
ggplot(cooccur_traits_ger, aes(x = flowering_overlap, y = log(shared_genus_visitor_count+1))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", color = "blue") +
  labs(x = "Flowering Overlap", y = "log(Shared Visitors+1)") +
  theme_minimal()

#see what the distribution of effects is 
ggplot(cooccur_traits_ger, aes(x = effects)) +
  geom_histogram(binwidth = 0.03, fill = "skyblue", color = "black") +
  labs(x = "effects", y = "Frequency") +
  theme_minimal()


# Modelling relationship between phenology and visitors -------------------

#try a linear model
model <- lm(visitor_similarity ~ flowering_overlap, data = cooccur_traits_ger)

summary(model)

#check residuals
plot(model, which=1)
plot(model, which=2)

#try and log transform your response variable

model2 <- lm(log(visitor_similarity+1) ~ flowering_overlap, data = cooccur_traits_ger)
summary(model2)

plot(model2, which=1)
plot(model2, which=2)

#visualize the model
ggplot(cooccur_traits_ger, aes(x = flowering_overlap, y = log(visitor_similarity + 1))) +
  geom_jitter(alpha = 0.5, width = 0.02, height = 0) +  # Jitter only on x-axis
  geom_smooth(method = "lm", color = "blue") +
  labs(
    x = "Flowering Overlap (Months)",
    y = "Log(Number of Shared Visitors + 1)",
  ) +
  theme_minimal()

#now look at flowering synchrony

cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(flowering_synchrony_sq = flowering_synchrony^2)

model3 <- lm(log(shared_genus_visitor_count+1) ~ flowering_synchrony_sq, data = cooccur_traits_ger)
summary(model3)

plot(model3, which=1)
plot(model3, which=2)

#visualize the model
ggplot(cooccur_traits_ger, aes(x = flowering_synchrony_sq, y = log(shared_genus_visitor_count + 1))) +
  geom_jitter(alpha = 0.5, width = 0.02, height = 0) +  # Jitter only on x-axis
  geom_smooth(method = "lm", color = "blue") +
  labs(
    x = "(Flowering Synchrony)^2",
    y = "Log(Number of Shared Visitors + 1)",
  ) +
  theme_minimal()


# Modelling relationship with effect sizes --------------------------------

#separate positive and negative effects
cooccur_ger_positive<-cooccur_model_df %>% 
  filter(quality=="positive") %>% 
  mutate(effects_transform = 1 / effects)
cooccur_ger_negative <- cooccur_model_df %>%
  filter(quality == "negative") %>%
  mutate(negative_effects = effects * -1) %>% 
  mutate(effects_transform = 1 / negative_effects)


#look at positive effects distribution
ggplot(cooccur_ger_positive, aes(x = effects)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for Positive Cooccurrence", y = "Frequency") +
  theme_minimal()
  #with a log transformation
ggplot(cooccur_ger_positive, aes(x = log(effects+1))) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for Positive Cooccurrence", y = "Frequency") +
  theme_minimal()
  #with a square-root transformation
ggplot(cooccur_ger_positive, aes(x = sqrt(effects+0.5))) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for Positive Cooccurrence", y = "Frequency") +
  theme_minimal()
  #with a reciprocal transformation
ggplot(cooccur_ger_positive, aes(x = (1/effects))) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for Positive Cooccurrence", y = "Frequency") +
  theme_minimal()
  #the reciprocal transformation creates the most normally distributed data

#look at negative effects distribution
ggplot(cooccur_ger_negative, aes(x = negative_effects)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for negative Cooccurrence", y = "Frequency") +
  theme_minimal()
#with a log transformation
ggplot(cooccur_ger_negative, aes(x = log(negative_effects+1))) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for negative Cooccurrence", y = "Frequency") +
  theme_minimal()
#with a square-root transformation
ggplot(cooccur_ger_negative, aes(x = sqrt(negative_effects+0.5))) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for negative Cooccurrence", y = "Frequency") +
  theme_minimal()
#with a reciprocal transformation
ggplot(cooccur_ger_negative, aes(x = (1/negative_effects))) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(x = "Effect Sizes for negative Cooccurrence", y = "Frequency") +
  theme_minimal()

  #try a LINEAR MODEL ---------------------------------------------

#first look at the partial effects


ggplot(cooccur_ger_positive, aes(x = flowering_overlap, y = effects_transform)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Flowering overlap",
    y = "1/Effects"
  ) +
  theme_minimal()

ggplot(cooccur_ger_negative, aes(x = flowering_overlap, y = effects_transform)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Flowering overlap",
    y = "1/Effects"
  ) +
  theme_minimal()


#partial effects plot

model_visitor <- lm(effects_transform~log(shared_genus_visitor_count+1)+flowering_overlap,data=cooccur_ger_positive)

library(effects) #Load effects package
model_visitor_Eff <- predictorEffects(model_visitor,partial.residuals=TRUE) #Calculate partial effects
#Plot partial effects
plot(model_visitor_Eff,lines=list(col='red'), partial.residuals=list(pch=19,col='black',cex=0.25))
  

model_visitor2 <- lm(effects_transform~log(shared_genus_visitor_count+1)+flowering_overlap,data=cooccur_ger_negative)
model_visitor2_Eff <- predictorEffects(model_visitor2,partial.residuals=TRUE) #Calculate partial effects
#Plot partial effects
plot(model_visitor2_Eff,lines=list(col='red'), partial.residuals=list(pch=19,col='black',cex=0.25))








#try a linear model
model_positive <- lm(effects_transform ~ standardized_shared_visitors*flowering_overlap, data = cooccur_ger_positive)
summary(model_positive)

plot(model_positive, which =1)
plot(model_positive, which =2)

#try a linear model
model_negative <- lm(effects_transform ~ standardized_shared_visitors*flowering_overlap, data = cooccur_ger_negative)
summary(model_negative)

plot(model_negative, which =1)
plot(model_negative, which =2)



#visualization

cooccur_traits_ger_binned <- cooccur_traits_ger %>%
  mutate(flowering_overlap_bin = cut(flowering_overlap, breaks = 3, labels = c("Low", "Medium", "High")))

ggplot(cooccur_traits_ger_binned, aes(x = log(shared_genus_visitor_count + 1), y = effects)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ flowering_overlap_bin) +
  labs(
    x = "Log(Shared Genus Visitor Count + 1)",
    y = "Effects",
    title = "Effects by Flowering Overlap Bin"
  ) +
  theme_minimal()

#back-transform to interpret

pred_log <- predict(model, newdata = cooccur_traits_ger)

pred_original <- exp(pred_log) - 1


cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(pred_log = predict(model, newdata = .),
         pred_original = exp(pred_log) - 1)


ggplot(cooccur_traits_ger, aes(x = flowering_overlap)) +
  geom_point(aes(y = shared_genus_visitor_count), alpha = 0.5) +  # observed counts
  geom_line(aes(y = pred_original), color = "blue", size = 1) +  # model prediction (back-transformed)
  labs(
    x = "Flowering Overlap",
    y = "Shared Genus Visitor Count",
    title = "Observed vs Predicted Shared Genus Visitors"
  ) +
  theme_minimal()

  #try linear with flowering synchrony ----------------------------
cooccur_traits_ger <- cooccur_traits_ger %>%
  mutate(flowering_synchrony_sq = flowering_synchrony^2)

model_multivariate2<- lm(effects ~ log(shared_genus_visitor_count+1)*flowering_synchrony_sq, data = cooccur_traits_ger)
summary(model_multivariate2)

plot(model_multivariate2, which =1)
plot(model_multivariate2, which =2)

  #try a GLM ------------------------------------------------------

mod1 <- glm(effects_transformed ~ shared_genus_visitor_count + flowering_overlap, data = cooccur_traits_ger, family = '')
