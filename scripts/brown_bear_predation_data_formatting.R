# R code for data formatting for glmm analysis published in Pop et al., 2022 
# Citation: TBD


# Summary -----------------------------------------------------------------

# this R script contains code for the data formatting/wrangling process some additional data were extracted using GIS. We analyzed each livestock category separately from a larger data set that included all livestock groups and all pseudo-absences, thus separate data files had to be created for each livestock category with a random subset of the pseudo-absences. Files were saved after the formatting process to be used for analysis and are available in this GitHub repository. Code is included here solely to show how data were formatted. If you run the code below and use the resulting data sets you may generate slightly different results due to randomization of pseudo-absences. Use the script 'brown_damage_bear_predation_glmm' and load the data sets in GitHub if you want to avoid this issue.


# Libraries ---------------------------------------------------------------

library(readr)
library(tidyverse)

# Data --------------------------------------------------------------------

# this sections uses packages readr and tidyverse

# read in full data

bear_damage_romania <- 
  read_csv('data/pagube_2008_2016_spatial.csv') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>%  # set variable names to lowercase
  
  # change class of variables
  
  mutate(
    landcover_code = as.factor(landcover_code),
    targetspp = as.factor(targetspp),
    year = as.factor(year),
    month = as.factor(month))
  
# Format data -------------------------------------------------------------

# this section uses package tidyverse

# create separate data frames for each target species

damage_cows <- 
  bear_damage_romania %>%
  filter(targetspp == "bovine")

damage_sheep <- 
  bear_damage_romania %>%
  filter(targetspp == "ovine")

damage_other <- 
  bear_damage_romania %>%
  filter(targetspp == "alte")

damage_pseudo <- 
  bear_damage_romania %>%
  filter(damage == 0)

# randomly sample pseudo points for each sub dataset from the full set of pseudo points, there needs to be 3 times more random points than damage points

pseudo.cows <- 
  sample_n(
    damage_pseudo, 
    3*nrow(damage_cows))

pseudo.sheep <- 
  sample_n(
    damage_pseudo, 
    3*nrow(damage_sheep))

pseudo.other <- 
  sample_n(
    damage_pseudo, 
    3*nrow(damage_other))

# merge damages and pseudo absences for each livestock type

cows <- 
  data.frame(
    rbind(damage_cows, 
          pseudo.cows))

sheep <- 
  data.frame(
    rbind(damage_sheep, 
          pseudo.sheep))

other <- 
  data.frame(
    rbind(damage_other, 
          pseudo.other))

# merge land cover types into similar categories to reduce levels of this factor for analysis

# set new grouping names; comments for each grouping show the corine landcover descriptions

# discontinuous urban fabric, industrial or commercial units, and mineral extraction sites

art_surfaces <- 
  c(112,121,131) 

# non-irrigated arable land, vineyards, fruit trees and berry plantations

ag_land <- 
  c(211,221,222)

# pastures, natural grasslands 

open <- 
  c(231, 321) 

# complex cultivation patters, Land principally occupied by agriculture, with significant areas of natural vegetation 

hetero_ag <- 
  c(242,243)  

# broad-leaved forests, coniferous forests, mixed forests

forests <- 
  c(311,312,313) 

# transitional woodland shrub-  did not use for this analysis, not enough points and not similar enough to other categories to group

transition <- 
  324  

# cows

cows <- 
  cows %>%
  filter(landcover_code != 511) %>%
  mutate(landcover_2 = 
           case_when(landcover_code %in% art_surfaces ~ 100,
                     landcover_code %in% ag_land ~ 210,
                     landcover_code %in% open ~ 231,
                     landcover_code %in% hetero_ag ~ 240,
                     landcover_code %in% forests ~ 310,
                     landcover_code %in% transition ~ 324),
         prop_natural = (prop_for_regen 
                         + prop_seminatural),
         prop_forest = (prop_deciduous
                        + prop_coniferous
                        + prop_mixedforest),
         prop_ag = (prop_arable
                    + prop_orchards
                    + prop_ag_mosaic),
         prop_open = (prop_grassland
                      + prop_pasture),
         hunting = case_when(year == 2016 ~ 0,
                             TRUE ~ 1),
         s.bear_abund = scale(bear_abund),
         s.altitude = scale(altitude),
         s.human_population = scale(human_population),
         s.dist_to_forest = scale(dist_to_forest),
         s.dist_to_town.s = scale(dist_to_town),
         s.shannondivindex = scale(shannondivindex),
         s.prop_natural = scale(prop_natural),
         s.prop_ag = scale(prop_ag),
         s.prop_open = scale(prop_open),
         s.prop_forest = scale(prop_forest))

# sheep

sheep <- 
  sheep %>%
  filter(landcover_code != 511) %>%
  mutate(landcover_2 = 
           case_when(landcover_code %in% art_surfaces ~ 100,
                     landcover_code %in% ag_land ~ 210,
                     landcover_code %in% open ~ 231,
                     landcover_code %in% hetero_ag ~ 240,
                     landcover_code %in% forests ~ 310,
                     landcover_code %in% transition ~ 324),
         prop_natural = (prop_for_regen 
                         + prop_seminatural),
         prop_forest = (prop_deciduous 
                        + prop_coniferous 
                        + prop_mixedforest),
         prop_ag = (prop_arable 
                    + prop_orchards 
                    + prop_ag_mosaic),
         prop_open = (prop_grassland 
                      + prop_pasture),
         hunting = case_when(year == 2016 ~ 0,
                             TRUE ~ 1),        
         s.bear_abund = scale(bear_abund),
         s.altitude = scale(altitude),
         s.human_population = scale(human_population),
         s.dist_to_forest = scale(dist_to_forest),
         s.dist_to_town.s = scale(dist_to_town),
         s.shannondivindex = scale(shannondivindex),
         s.prop_natural = scale(prop_natural),
         s.prop_ag = scale(prop_ag),
         s.prop_open = scale(prop_open),
         s.prop_forest = scale(prop_forest))

# other

other <- 
  other %>%
  filter(landcover_code != c(511)) %>%
  mutate(landcover_2 = 
           case_when(landcover_code %in% art_surfaces ~ 100,
                     landcover_code %in% ag_land ~ 210,
                     landcover_code %in% open ~ 231,
                     landcover_code %in% hetero_ag ~ 240,
                     landcover_code %in% forests ~ 310,
                     landcover_code %in% transition ~ 324),
         prop_natural = (prop_for_regen 
                         + prop_seminatural),
         prop_forest = (prop_deciduous 
                        + prop_coniferous 
                        + prop_mixedforest),
         prop_ag = (prop_arable 
                    + prop_orchards 
                    + prop_ag_mosaic),
         prop_open = (prop_grassland 
                      + prop_pasture),
         hunting = case_when(year == 2016 ~ 0,
                             TRUE ~ 1),
         s.bear_abund = scale(bear_abund),
         s.altitude = scale(altitude),
         s.human_population = scale(human_population),
         s.dist_to_forest = scale(dist_to_forest),
         s.dist_to_town.s = scale(dist_to_town),
         s.shannondivindex = scale(shannondivindex),
         s.prop_natural = scale(prop_natural),
         s.prop_ag = scale(prop_ag),
         s.prop_open = scale(prop_open),
         s.prop_forest = scale(prop_forest))


# save data ---------------------------------------------------------------

# cows

write.csv(cows, 'cows.csv')

# sheep

write.csv(sheep, 'sheep.csv')

# other

write.csv(other, 'other.csv')