# R code for generalized mixed effects model analysis and graphs published in Pop et al., 2022 
# Citation: TBD


# Libraries ---------------------------------------------------------------

# add the start of each section I identify which libraries are in that section in case you need to reproduce code for a particular section and don't want to load all of the libraries we used for this analysis

library(readr)
library(tidyverse)
library(rphylopic)
library(Hmisc)
library(lme4)
library(MuMIn)
library(car)
library(pROC)
library(ggplot2)
library(stringr)
library(gridExtra)

# Graph elements ----------------------------------------------------------

# this section uses package rphylopic

# assign phylopic images for plots to the global environment

cow_phylo <- 
  image_data("415714b4-859c-4d1c-9ce0-9e1081613df7", 
             size = 256)[[1]]

sheep_phylo <- 
  image_data("a9297cbd-10ca-457b-b5d5-a0b038720df7", 
             size = 256)[[1]]

other_phylo <- 
  image_data("aff847b0-ecbd-4d41-98ce-665921a6d96e", 
             size = 256)[[1]]

# create vector for plot line types and assign to global environment

spp_lines <- 
  c("solid", 
    "longdash", 
    "dotted")

# create vector for graph line type labels and assign to global environment

species <- 
  c("Cows", 
    "sheep", 
    "Other")


# Cows analysis -----------------------------------------------------------

# this section uses packages readr and tidyverse

# read in saved data created in earlier script ('brown_bear_predation_data_foramtting.R')

cows <- 
  read_csv('data/cows.csv') %>% 
  mutate(landcover_2 = as.factor(landcover_2))


# Correlations (cows) -----------------------------------------------------

# this section uses package Hmisc

# subset full cows data set with numeric variables of interest

cows_cor <- 
  cows %>% 
  select(s.bear_abund,
         s.dist_to_forest,
         s.dist_to_town,
         openhab_10k,
         ag_10k,
         heteroag_10k,
         forest_10k,
         urban_10k,
         shdi_10k)

# plotting correlation matrix (run all lines)

plot(cows_cor)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}  
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="pairwise.complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}
pairs(cows_cor,upper.panel=panel.cor,diag.panel = panel.hist)

# correlations with spearman since many relationship are not linear

cows_cor %>% 
  as.matrix() %>% 
  rcorr(., 
        type = "spearman") 

# remove cows_cor  from global environment as we will not use again

rm(cows_cor)


# Random structure (cows) -------------------------------------------------

# this section uses packages lme4 and MuMIn

# determine best suited random structure for GLM

# year as random effect

c_glmer1 <- 
  glmer(damage ~ s.bear_abund 
        + (1|year),
        data = cows, 
        family = binomial)

# landcover as random effect

c_glmer2 <-  
  glmer(damage ~ s.bear_abund 
        + (1| landcover_2),
        data = cows, 
        family = binomial)

# year and landcover as random effects

c_glmer3 <- 
  glmer(damage ~ s.bear_abund 
        + (1| year) 
        + (1|landcover_2),
        data = cows, 
        family = binomial )

# no random effect

c_glm <- 
  glm(damage~ s.bear_abund,
      data = cows, 
      family = binomial)

# model selection

model.sel(c_glmer1, 
          c_glmer2, 
          c_glmer3, 
          c_glm)


# Fixed structure (cows) --------------------------------------------------

# this section uses packages lme4 and MuMIn

# global model: does not include all variables since some were correlated

c_global <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town +
          openhab_10k +
          ag_10k + 
          shdi_10k -1 +
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)


# null model

c_null <- 
  glmer(damage ~ 1 -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# models with bear abundance

# bears + broad landscape model

c_1_b_o_ag  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k + 
          ag_10k  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# bears + landscape heterogeneity model

c_2_b_shdi  <- 
  glmer(damage ~ s.bear_abund + 
          shdi_10k  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# bears + broad landscape + heterogeneity model

c_3_b_o_ag_shdi  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)


# bears + proximity of grazing catlle to bear habitat

c_4_b_df_dt <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# bears + landscape cattle typically grazed on

c_5_b_o  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k-1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# bears + cattle proximity to bears and landscape used for grazing

c_6_b_df_dt_o <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town + 
          openhab_10k -1 + 
          (1|year) + (1|landcover_2),
        data = cows, 
        family = binomial)

# bears + cattle proximity to bears + landscape used for grazing + landscape heterogeneity

c_7_b_df_dt_o_shdi <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town + 
          openhab_10k +
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# models without bear abundance

# landscape + landscape heterogeneity

c_8_o_shdi_ag <- 
  glmer(damage ~  openhab_10k + 
          shdi_10k + 
          ag_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# proximity to bear habitat and human settlements

c_9_df_dt <- 
  glmer(damage ~ s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# landscape + proximity to bear habitat and human settlements

c_10_o_df_dt <- 
  glmer(damage ~  openhab_10k + 
          s.dist_to_forest + 
          s.dist_to_town  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# landscape + proximity to bear habitat and human settlements + landscape heterogeneity 

c_11_o_shdi_df_dt <- 
  glmer(damage ~  openhab_10k + 
          shdi_10k + 
          s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# preliminary model selection

model.sel(c_global,
          c_null,
          c_1_b_o_ag, 
          c_2_b_shdi, 
          c_3_b_o_ag_shdi, 
          c_4_b_df_dt, 
          c_5_b_o, 
          c_6_b_df_dt_o, 
          c_7_b_df_dt_o_shdi, 
          c_8_o_shdi_ag, 
          c_9_df_dt, 
          c_10_o_df_dt,
          c_11_o_shdi_df_dt)

# the global model and C.7 are within 2 delta AIC and rank better than the others

# adding interaction between distance to from bear habitat and distance from human settlement to top models (C.global and C.7)

# C.7 with interaction and individual variables

c_7a <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest * s.dist_to_town + 
          openhab_10k +
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# C.7 with just interaction without estimating variables individually

c_7b <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest : s.dist_to_town + 
          openhab_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# global model with interaction and individual variables

c_global_a <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest * s.dist_to_town +
          openhab_10k + ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)

# global model with just interaction without estimating variables individually

c_global_b <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest:s.dist_to_town +
          openhab_10k + ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = cows, 
        family = binomial)


# full model selection

model.sel(c_1_b_o_ag, 
          c_2_b_shdi, 
          c_3_b_o_ag_shdi, 
          c_4_b_df_dt, 
          c_5_b_o,
          c_6_b_df_dt_o, 
          c_7_b_df_dt_o_shdi, 
          c_8_o_shdi_ag, 
          c_9_df_dt, 
          c_10_o_df_dt, 
          c_11_o_shdi_df_dt,
          c_7a, 
          c_7b, 
          c_global, 
          c_null, 
          c_global_a, 
          c_global_b)


# Model results (cows) ----------------------------------------------------

# this section uses package car

# model summary
summary(c_global_a)

# summary estimates are in log-odds need to transform to interpret, use plogis

plogis(coeffs(c_global_a)) # probability- magnitude and direction of effect

# analysis of deviance table from package 'car'; Fox and Weisberg 2019- include variable, Chisq, df, and p

Anova(c_global_a, 
      type = "III")


# Model fit (cows) --------------------------------------------------------

# this section uses package pROC and tidyverse

# use code below to calculate area under the curve for the top model

c_global_a %>% 
  predict(type = 'response') %>% 
  roc(cows$damage,
      .) %>% 
  auc()

# calculate r square

r.squaredGLMM(c_global_a)

# Odds calculations (cows) -----------------------------------------------

# extract odds ratios for top model
c_odds <- 
  c_global_a %>% 
  coef() %>% 
  
  # select one year since all odds ratios are the same
  
  .$year %>% 
  .[1,-1] %>% 
  exp() %>%
  as.numeric() %>% 
  .[1:6]

# extract confidence intervals for model

c_confint <- 
  exp(confint(c_global_a,
              parm = c('s.bear_abund',
                       's.dist_to_forest',
                       's.dist_to_town',
                       'openhab_10k',
                       'shdi_10k',
                       'ag_10k'))) # this step takes a minute

c_odds_labels <- 
  c("Bear Abundance", 
    "Distance to Forest", 
    "Distance to Settlement", 
    "Proportion Open Habitat", 
    "Shannon Diversity Index", 
    "Proportion Agricultural Habitat") 

# combine data into data frame for graphing

c_odds.df <- 
  data.frame(yAxis = length(c_odds_labels):1,
             odds = c_odds,
             high = c_confint[,2],
             low = c_confint[,1],
             row.names = c_odds_labels)


# remove variables from global env

rm(c_odds, c_confint)

# Odds graph (cows) -------------------------------------------------------

# this section uses package ggplot2, rphylopic, and stringr

cow_odds_plot <- 
  ggplot(data = c_odds.df, 
          aes(x = odds, 
              y = c_odds_labels)) +
  
  # add dashed line at 1
  
  geom_vline(
    aes(xintercept = 1), 
    size = .25, 
    linetype = 'dashed') +
  
  # add errorbars and points for odds ratio
  
  geom_errorbarh(
    aes(xmax = high, 
        xmin = low), 
    size = .6, 
    height = .2, 
    color = 'gray50') +
  geom_point(size = 2.2) +
  
  # re scale plot 
  coord_cartesian(xlim = c(0,3))+
  scale_x_continuous(breaks = seq(0,3,0.5)) +
  
  # wrap axis text
  
  scale_y_discrete(labels = function(y) str_wrap(y, width = 12)) +
  
  # change axis text size
  
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22)) +
  ylab('') +
  xlab('Odds ratios') +
  
  # add letter in upper right corner of plot for identification in-text
  annotate("text", 
           x = 3.0, 
           y = 6.4, 
           size = 7.5, 
           label = "(a)") +
  
  # add silhouette to identify livestock type
  
  add_phylopic(cow_phylo, 
               alpha = 1, 
               x = 2.65, 
               y = 6.25, 
               ysize = 0.5)

# view plot

cow_odds_plot


# Sheep analysis ----------------------------------------------------------

# this section uses packages readr and tidyverse

sheep <- 
  read_csv('data/sheep.csv') %>% 
  mutate(landcover_2 = as.factor(landcover_2))


# Correlations (sheep) ----------------------------------------------------

# this section uses package Hmisc

# subset full cows data set with numeric variables of interest

sheep_cor <- 
  sheep %>% 
  select(s.bear_abund,
         dist_to_forest,
         dist_to_town,
         openhab_10k,
         ag_10k,
         heteroag_10k,
         forest_10k,
         urban_10k,
         shdi_10k)

# correlations with spearman since many relationship are not linear as noted previously with cows data

sheep_cor.m <-
  as.matrix(sheep_cor)

rcorr(sheep_cor.m, 
      type = "spearman") 


# Fixed structure (sheep) --------------------------------------------------


# this section uses packages lme4 and MuMIn

# global model

s_global <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town +
          openhab_10k +
          ag_10k + 
          shdi_10k -1 +
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)


# null model

s_null <- 
  glmer(damage ~ 1 -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# models with bear abundance

# bears + broad landscape model

s_1_b_o_ag  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k + 
          ag_10k  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# bears + landscape heterogeneity model

s_2_b_shdi  <- 
  glmer(damage ~ s.bear_abund + 
          shdi_10k  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# bears + broad landscape + heterogeneity model

s_3_b_o_ag_shdi  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)


# bears + proximity of grazing livestock to bear habitat

s_4_b_df_dt <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# bears + landscape livestock typically grazed on

s_5_b_o  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k-1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# bears + livestock proximity to bears and landscape used for grazing

s_6_b_df_dt_o <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town + 
          openhab_10k -1 + 
          (1|year) + (1|landcover_2),
        data = sheep, 
        family = binomial)

# bears + cattle proximity to bears + landscape used for grazing + landscape heterogeneity

s_7_b_df_dt_o_shdi <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town + 
          openhab_10k +
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# models without bear abundance

# landscape + landscape heterogeneity

s_8_o_shdi_ag <- 
  glmer(damage ~  openhab_10k + 
          shdi_10k + 
          ag_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# proximity to bear habitat and human settlements

s_9_df_dt <- 
  glmer(damage ~ s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# landscape + proximity to bear habitat and human settlements

s_10_o_df_dt <- 
  glmer(damage ~  openhab_10k + 
          s.dist_to_forest + 
          s.dist_to_town  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# landscape + proximity to bear habitat and human settlements + landscape heterogeneity 

s_11_o_shdi_df_dt <- 
  glmer(damage ~  openhab_10k + 
          shdi_10k + 
          s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# preliminary model selection

model.sel(s_global,
          s_null,
          s_1_b_o_ag, 
          s_2_b_shdi, 
          s_3_b_o_ag_shdi, 
          s_4_b_df_dt, 
          s_5_b_o, 
          s_6_b_df_dt_o, 
          s_7_b_df_dt_o_shdi, 
          s_8_o_shdi_ag, 
          s_9_df_dt, 
          s_10_o_df_dt,
          s_11_o_shdi_df_dt)


# adding interaction between distance to from bear habitat and distance from human settlement to top model and global model

# s.4 with interaction and individual variables

s_4a <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest * s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# s.4 with just interaction 

s_4b <- glmer(damage ~ s.bear_abund + 
                s.dist_to_forest : s.dist_to_town -1 + 
                (1|year) +
                (1|landcover_2),
              data = sheep,
              family = binomial)

# global model with interaction and individual variables

s_global_a <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest * s.dist_to_town +
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# global with just interaction

s_global_b <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest : s.dist_to_town +
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = sheep, 
        family = binomial)

# full model selection (sheep)

model.sel(s_1_b_o_ag, 
          s_2_b_shdi, 
          s_3_b_o_ag_shdi, 
          s_4_b_df_dt, 
          s_5_b_o,
          s_6_b_df_dt_o, 
          s_7_b_df_dt_o_shdi, 
          s_8_o_shdi_ag, 
          s_9_df_dt, 
          s_10_o_df_dt, 
          s_11_o_shdi_df_dt,
          s_4a, 
          s_4b, 
          s_global, 
          s_null, 
          s_global_a, 
          s_global_b)


# Model average (sheep) ---------------------------------------------------

s_models <- 
  list(s1 = s_1_b_o_ag,
       s2 = s_2_b_shdi,
       s3 = s_3_b_o_ag_shdi,
         s4 = s_4_b_df_dt,
       s5 = s_5_b_o,
       s6 = s_6_b_df_dt_o, 
       s7 = s_7_b_df_dt_o_shdi,
       s8 = s_8_o_shdi_ag,
       s9 = s_9_df_dt,
       s10 = s_10_o_df_dt,
       s11 = s_11_o_shdi_df_dt,
       s4a = s_4a,
       s4b = s_4b,
       sg = s_global, 
       sga = s_global_a,
       sgb = s_global_b,
       sn = s_null)

# select just the model with a cumulative weight of 0.95

s_avg_models <- 
  model.sel(s_models,
            cumsum(weight) <= 0.95)

# create a variable with estimates from model averaged models

s_avgMod_95 <- 
  model.avg(s_avg_models,
            cumsum(weight) <= 0.95)


# Model results (sheep) ---------------------------------------------------

summary(s_avgMod_95)

# summary estimates are in log-odds need to transform to interpret, use plogis

plogis(coef(s_avgMod_95)) # probability- magnitude and direction of effect

# analysis of deviance table from package 'car'; Fox and Weisberg 2019- include variable, Chisq, df, and p -> doesn't work with model avg so use global model

Anova(s_global_a, 
      type = "III")

# Model fit (sheep) -------------------------------------------------------

# this section uses package pROC

# use code below to calculate area under the curve for the top model

s_avgMod_95 %>% 
  predict(type = 'response') %>% 
  roc(sheep$damage,
      .) %>% 
  auc()

# calculating r-squared to assess model fit -> does not work with model average models therfore we used the top model included in the model average set

r.squaredGLMM(s_global_a)


# Odds calculations (sheep) -----------------------------------------------

# extract odds ratios for model

s_odds <- 
  s_avgMod_95 %>% 
  coef() %>% 
  exp() %>%
  as.numeric() %>% 
  .[c(1:5,7)]

# exttract confidence intervals for model

s_confint <- 
  exp(confint(s_avgMod_95,
              parm = c('s.bear_abund',
                       's.dist_to_forest',
                       's.dist_to_town',
                       'openhab_10k',
                       'shdi_10k',
                       'ag_10k')))

s_odds_labels <- 
  c("Bear Abundance", 
    "Distance to Forest", 
    "Distance to Settlement", 
    "Proportion Open Habitat", 
    "Shannon Diversity Index", 
    "Proportion Agricultural Habitat") 

# combine data into data frame for graphing

s_odds.df <- 
  data.frame(yAxis = length(s_odds_labels):1,
             odds = s_odds,
             high = s_confint[,2],
             low = s_confint[,1],
             row.names = s_odds_labels)


# remove variables from global env

rm(s_odds, s_confint)

# Odds graph (sheep) ------------------------------------------------------

# this section uses packages ggplot, rphylopic, and stringr

sheep_odds_plot <- 
  ggplot(s_odds.df, 
         aes(x = odds, 
             y = s_odds_labels)) +
  geom_vline(
    aes(xintercept = 1), 
    size = .25, 
    linetype = 'dashed') +
  geom_errorbarh(
    aes(xmax = high, 
        xmin = low),
    size = .6, 
    height = .2, 
    color = 'gray50') +
  geom_point(size = 2.2) +
  coord_cartesian(xlim = c(0,3)) +
  scale_x_continuous(breaks = seq(0,3,0.5)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 12)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_blank()) +
  xlab('Odds ratios')+
  annotate("text", 
           x = 3.0, 
           y = 6.4, 
           size = 7.5, 
           label = "(b)")+
  add_phylopic(sheep_phylo, 
               alpha = 1, 
               x = 2.7, 
               y = 6.25, 
               ysize = 0.5)

sheep_odds_plot


# Other livestock analysis ------------------------------------------------

# this section uses packages readr and tidyverse

# read in data
other <- 
  read_csv('data/other.csv') %>% 
  mutate(landcover_2 = as.factor(landcover_2))


# Correlations (other) ----------------------------------------------------

other_cor <- 
  other %>% 
  select(s.bear_abund,
         dist_to_forest,
         dist_to_town,
         openhab_10k,
         ag_10k,
         heteroag_10k,
         forest_10k,
         urban_10k,
         shdi_10k)

# correlations with spearman since many relationship are not linear as noted previously with cows data

other_cor.m <-
  as.matrix(other_cor)

rcorr(other_cor.m, 
      type = "spearman")


# Fixed structure (other) -------------------------------------------------


# this section uses packages lme4 and MuMIn

# global model

o_global <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town +
          openhab_10k +
          ag_10k + 
          shdi_10k -1 +
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)


# null model

o_null <- 
  glmer(damage ~ 1 -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# models with bear abundance

# bears + broad landscape model

o_1_b_o_ag  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k + 
          ag_10k  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# bears + landscape heterogeneity model

o_2_b_shdi  <- 
  glmer(damage ~ s.bear_abund + 
          shdi_10k  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# bears + broad landscape + heterogeneity model

o_3_b_o_ag_shdi  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)


# bears + proximity of grazing livestock to bear habitat

o_4_b_df_dt <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# bears + landscape livestock typically grazed on

o_5_b_o  <- 
  glmer(damage ~ s.bear_abund + 
          openhab_10k-1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# bears + livestock proximity to bears and landscape used for grazing

o_6_b_df_dt_o <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town + 
          openhab_10k -1 + 
          (1|year) + (1|landcover_2),
        data = other, 
        family = binomial)

# bears + livestock proximity to bears + landscape used for grazing + landscape heterogeneity

o_7_b_df_dt_o_shdi <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest + 
          s.dist_to_town + 
          openhab_10k +
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# models without bear abundance

# landscape + landscape heterogeneity

o_8_o_shdi_ag <- 
  glmer(damage ~  openhab_10k + 
          shdi_10k + 
          ag_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# proximity to bear habitat and human settlements

o_9_df_dt <- 
  glmer(damage ~ s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# landscape + proximity to bear habitat and human settlements

o_10_o_df_dt <- 
  glmer(damage ~  openhab_10k + 
          s.dist_to_forest + 
          s.dist_to_town  -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# landscape + proximity to bear habitat and human settlements + landscape heterogeneity 

o_11_o_shdi_df_dt <- 
  glmer(damage ~  openhab_10k + 
          shdi_10k + 
          s.dist_to_forest + 
          s.dist_to_town -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# preliminary model selection

model.sel(o_global,
          o_null,
          o_1_b_o_ag, 
          o_2_b_shdi, 
          o_3_b_o_ag_shdi, 
          o_4_b_df_dt, 
          o_5_b_o, 
          o_6_b_df_dt_o, 
          o_7_b_df_dt_o_shdi, 
          o_8_o_shdi_ag, 
          o_9_df_dt, 
          o_10_o_df_dt,
          o_11_o_shdi_df_dt)


# adding interaction between distance to from bear habitat and distance from human settlement to top model and global model

# o_1 with interaction and individual variables

o_1a <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest * s.dist_to_town -1 + 
          openhab_10k +
          ag_10k +
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# o_1 with just interaction 

o_1b <- glmer(damage ~ s.bear_abund + 
                s.dist_to_forest : s.dist_to_town -1 + 
                openhab_10k +
                ag_10k +
                (1|year) +
                (1|landcover_2),
              data = other,
              family = binomial)

# global model with interaction and individual variables

o_global_a <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest * s.dist_to_town +
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# global with just interaction

o_global_b <- 
  glmer(damage ~ s.bear_abund + 
          s.dist_to_forest : s.dist_to_town +
          openhab_10k + 
          ag_10k + 
          shdi_10k -1 + 
          (1|year) + 
          (1|landcover_2),
        data = other, 
        family = binomial)

# full model selection (other)

model.sel(o_1_b_o_ag, 
          o_2_b_shdi, 
          o_3_b_o_ag_shdi, 
          o_4_b_df_dt, 
          o_5_b_o,
          o_6_b_df_dt_o, 
          o_7_b_df_dt_o_shdi, 
          o_8_o_shdi_ag, 
          o_9_df_dt, 
          o_10_o_df_dt, 
          o_11_o_shdi_df_dt,
          o_1a, 
          o_1b, 
          o_global, 
          o_null, 
          o_global_a, 
          o_global_b)


# Model average (other) ---------------------------------------------------

o_models <- 
  list(o1 = o_1_b_o_ag,
       o2 = o_2_b_shdi,
       o3 = o_3_b_o_ag_shdi,
       o4 = o_4_b_df_dt,
       o5 = o_5_b_o,
       o6 = o_6_b_df_dt_o,
       o7 = o_7_b_df_dt_o_shdi,
       o8 = o_8_o_shdi_ag,
       o9 = o_9_df_dt,
       o10 = o_10_o_df_dt,
       o11 = o_11_o_shdi_df_dt,
       o1a = o_1a,
       o1b = o_1b,
       og = o_global,
       oga = o_global_a,
       ogb = o_global_b,
       on = o_null)

# select just models with cumulative weight of 0.95

o_avg_models <- 
  model.sel(o_models,
            cumsum(weight) <= 0.95)

# create variable with parameter estimates based off model averaged models

o_avgMod_95 <- 
  model.avg(o_avg_models,
            cumsum(weight) <= 0.95)


# Model results (other) ---------------------------------------------------

summary(o_avgMod_95)

# summary estimates are in log-odds need to transform to interpret, use plogis

plogis(coef(o_avgMod_95)) # probability- magnitude and direction of effect

# analysis of deviance table from package 'car'; Fox and Weisberg 2019- include variable, Chisq, df, and p -> doesn't work with model avg so use global model

Anova(o_global_a, 
      type = "III")


# Model fit (other) -------------------------------------------------------

# this section uses package pROC

# use code below to calculate area under the curve for the top model

o_avgMod_95 %>% 
  predict(type = 'response') %>% 
  roc(other$damage,
      .) %>% 
  auc()

#calculating r-squared to assess model fit does not work for model average objects so used global model

r.squaredGLMM(o_global_a)


# Odds calculations (other) -----------------------------------------------

# extract odds ratios for model

o_odds <- 
  o_avgMod_95 %>% 
  coef() %>% 
  exp() %>%
  as.numeric() %>% 
  .[c(1:3,5:7)]

# extract confidence intervals for model

o_confint <- 
  exp(confint(o_avgMod_95,
              parm = c('s.bear_abund',
                       'openhab_10k',
                       'ag_10k',
                       'shdi_10k',
                       's.dist_to_forest',
                       's.dist_to_town')))

o_odds_labels <- 
  c("Bear Abundance",
    "Proportion Open Habitat",
    "Proportion Agricultural Habitat",
    "Shannon Diversity Index",
    "Distance to Forest", 
    "Distance to Settlement") 

# combine data into data frame for graphing

o_odds.df <- 
  data.frame(yAxis = length(o_odds_labels):1,
             odds = o_odds,
             high = o_confint[,2],
             low = o_confint[,1],
             row.names = o_odds_labels)


# remove variables from global env

rm(o_odds, o_confint)


# Odds graph (other) ------------------------------------------------------


other_odds_plot <-
  ggplot(o_odds.df, 
         aes(x = odds, 
             y = o_odds_labels)) +
  geom_vline(
    aes(xintercept = 1), 
    size = .25, 
    linetype = 'dashed') +
  geom_errorbarh(
    aes(xmax = high, 
        xmin = low), 
    size = .6, 
    height = .2, 
    color = 'gray50') +
  geom_point(size = 2.2) +
  coord_cartesian(xlim = c(0,3))+
  scale_x_continuous(breaks = seq(0,3,0.5)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 12)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22)) +
  ylab('') +
  xlab('Odds ratios')+
  annotate("text", 
           x = 3.0, 
           y = 6.4, 
           size = 7.5, 
           label = "(c)")+
  add_phylopic(other_phylo, 
               alpha = 1, 
               x = 2.77, 
               y = 6.25, 
               ysize = 0.55)

other_odds_plot


# Predictive graphs ----------------------------------------------

# this section uses packages ggplot2 and rphylopic

# combine species data frames 

 all_bpe<- 
  rbind(cows, 
        sheep, 
        other)


# Bear abundance ----------------------------------------------------------

new_data_bear <- 
  expand.grid(s.bear_abund = seq(min(all_bpe$s.bear_abund), 
                                 max(all_bpe$s.bear_abund), 
                                 0.01),
              s.dist_to_forest = mean(all_bpe$s.dist_to_forest),
              s.dist_to_town = mean(all_bpe$ s.dist_to_town),
              ag_10k = mean(all_bpe$ag_10k),
              openhab_10k = mean(all_bpe$openhab_10k),
              shdi_10k = mean(all_bpe$shdi_10k))

# get predicted damage values for each livestock type based on top model or model average and add them as new columns to the joint data frame

# cows

new_data_bear$pred_c <- 
  predict(c_global_a, 
          newdata = new_data_bear, 
          re.form = NA, 
          type = "response")

# sheep

new_data_bear$pred_s <- 
  predict(s_avgMod_95, 
          newdata = new_data_bear, 
          re.form = NA, 
          type = "response")

# other

new_data_bear$pred_o <-  
  predict(o_avgMod_95, 
          newdata = new_data_bear, 
          re.form = NA, 
          type = "response")

# retrieve unscaled bear_abund values for x axis

summary(all_bpe$bear_abund)

# create new vector with values that is the same length as the new data frame or plotting on x-xis

bear_raw <- 
  seq(0, 77, length.out = 515)

new_data_bear <- 
  cbind(bear_raw, 
        new_data_bear)

# graph with ggplot

bear_graph <- 
  ggplot() +
  
  # add lines for predicted BPE for each livestock type based on chosen model or model avg set
  geom_line(data = new_data_bear, 
            aes(x = bear_raw, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  geom_line(data = new_data_bear, 
            aes(x = bear_raw, 
                y = pred_s), 
            linetype = "longdash",
            size = 1) +
  geom_line(data = new_data_bear, 
            aes(x = bear_raw, 
                y = pred_o), 
            linetype = "dotted", 
            size = 1) +
  
  # alter x and y limits
  
  coord_cartesian(ylim = c(0,1),
                  xlim = c(2,77)) +
  
  # specify x and y axis labels
  
  labs(x = "Relative Bear Abundance",
       y = "Probability of Livestock Damage") +
  
  # add letter in upper right corner for in-text identification
  
  annotate("text", 
           x = 77, 
           y = 1, 
           size = 7.5, 
           label = "(a)") +
  
  # change theme elements and text size
  
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

bear_graph


# Distance to forest ------------------------------------------------------

new_data_dForest <- 
  expand.grid(s.dist_to_forest = seq(min(all_bpe$s.dist_to_forest),
                                     max(all_bpe$s.dist_to_forest),
                                     0.01),
              s.bear_abund = mean(all_bpe$s.bear_abund),
              s.dist_to_town = mean(all_bpe$ s.dist_to_town),
              ag_10k = mean(all_bpe$ag_10k),
              openhab_10k = mean(all_bpe$openhab_10k),
              shdi_10k = mean(all_bpe$shdi_10k))

# get predicted damage values for each livestock type based on top model or model average and add them as new columns to the joint data frame

# cows

new_data_dForest$pred_c <- 
  predict(c_global_a, 
          newdata = new_data_dForest, 
          re.form = NA, 
          type = "response")

# sheep

new_data_dForest$pred_s <- 
  predict(s_avgMod_95, 
          newdata = new_data_dForest, 
          re.form = NA, 
          type = "response")

# other

new_data_dForest$pred_o <-  
  predict(o_avgMod_95, 
          newdata = new_data_dForest, 
          re.form = NA, 
          type = "response")

# retrieve unscaled values

summary(all_bpe$dist_to_forest)

# create new vector with values that is the same length as the new data frame or plotting on x-xis

dForest_raw <- 
  seq(0, 7073.95, length.out = 647)

new_data_dForest <- 
  cbind(dForest_raw, 
        new_data_dForest)

# graph with ggplot

dForest_graph <- 
  ggplot() +
  geom_line(data = new_data_dForest, 
            aes(x = dForest_raw, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  geom_line(data = new_data_dForest, 
            aes(x = dForest_raw, 
                y = pred_s), 
            linetype = "longdash",
            size = 1) +
  geom_line(data = new_data_dForest, 
            aes(x = dForest_raw, 
                y = pred_o), 
            linetype = "dotted", 
            size = 1) +
coord_cartesian(ylim = c(0,1),
                  xlim = c(220,6850)) +
  labs(x = "Distance to Forest (m)",
       y = "Probability of Livestock Damage") +
  annotate("text", 
           x = 6900, 
           y = 1, 
           size = 7.5, 
           label = "(b)") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

dForest_graph


# Distance to town ----------------------------------------------------------------

new_data_dTown <- 
  expand.grid(s.dist_to_town = seq(min(all_bpe$s.dist_to_town),
                                     max(all_bpe$s.dist_to_town),
                                     0.01),
              s.bear_abund = mean(all_bpe$s.bear_abund),
              s.dist_to_forest = mean(all_bpe$ s.dist_to_forest),
              ag_10k = mean(all_bpe$ag_10k),
              openhab_10k = mean(all_bpe$openhab_10k),
              shdi_10k = mean(all_bpe$shdi_10k))

# get predicted damage values for each livestock type based on top model or model average and add them as new columns to the joint data frame

# cows

new_data_dTown$pred_c <- 
  predict(c_global_a, 
          newdata = new_data_dTown, 
          re.form = NA, 
          type = "response")

# sheep

new_data_dTown$pred_s <- 
  predict(s_avgMod_95, 
          newdata = new_data_dTown, 
          re.form = NA, 
          type = "response")

# other

new_data_dTown$pred_o <-  
  predict(o_avgMod_95, 
          newdata = new_data_dTown, 
          re.form = NA, 
          type = "response")

# retrieve unscaled values

summary(all_bpe$dist_to_town)

# create new vector with values that is the same length as the new data frame or plotting on x-xis

dTown_raw <- 
  seq(0, 13340.5, length.out = 522)

new_data_dTown <- 
  cbind(dTown_raw, 
        new_data_dTown)

# graph with ggplot

dTown_graph <- 
  ggplot() +
  geom_line(data = new_data_dTown, 
            aes(x = dTown_raw, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  geom_line(data = new_data_dTown, 
            aes(x = dTown_raw, 
                y = pred_s), 
            linetype = "longdash",
            size = 1) +
  geom_line(data = new_data_dTown, 
            aes(x = dTown_raw, 
                y = pred_o), 
            linetype = "dotted", 
            size = 1) +
  coord_cartesian(ylim = c(0,1),
                  xlim = c(400,13000)) +
  labs(x = "Distance to Town (m)",
       y = "Probability of Livestock Damage") +
  annotate("text", 
           x = 13100, 
           y = 1, 
           size = 7.5, 
           label = "(c)") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

dTown_graph



# Prop agriculture --------------------------------------------------------

new_data_ag <- 
  expand.grid(ag_10k = seq(min(all_bpe$ag_10k),
                                   max(all_bpe$ag_10k),
                                   0.01),
              s.bear_abund = mean(all_bpe$s.bear_abund),
              s.dist_to_forest = mean(all_bpe$ s.dist_to_forest),
              s.dist_to_town = mean(all_bpe$s.dist_to_town),
              openhab_10k = mean(all_bpe$openhab_10k),
              shdi_10k = mean(all_bpe$shdi_10k))

# get predicted damage values for each livestock type based on top model or model average and add them as new columns to the joint data frame

# cows

new_data_ag$pred_c <- 
  predict(c_global_a, 
          newdata = new_data_ag, 
          re.form = NA, 
          type = "response")

# sheep

new_data_ag$pred_s <- 
  predict(s_avgMod_95, 
          newdata = new_data_ag, 
          re.form = NA, 
          type = "response")

# other

new_data_ag$pred_o <-  
  predict(o_avgMod_95, 
          newdata = new_data_ag, 
          re.form = NA, 
          type = "response")

# graph with ggplot

ag_graph <- 
  ggplot() +
  geom_line(data = new_data_ag, 
            aes(x = ag_10k, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  geom_line(data = new_data_ag, 
            aes(x = ag_10k, 
                y = pred_s), 
            linetype = "longdash",
            size = 1) +
  geom_line(data = new_data_ag, 
            aes(x = ag_10k, 
                y = pred_o), 
            linetype = "dotted", 
            size = 1) +
  coord_cartesian(ylim = c(0,1),
                  xlim = c(-0.55,3)) +
  labs(x = "Proportion of Agriculture Habitat (scaled)",
       y = "Probability of Livestock Damage") +
  annotate("text", 
           x = 3, 
           y = 1, 
           size = 7.5, 
           label = "(d)") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

ag_graph


# Prop open ---------------------------------------------------------------

new_data_openhab <- 
  expand.grid(openhab_10k = seq(min(all_bpe$openhab_10k),
                           max(all_bpe$openhab_10k),
                           0.01),
              s.bear_abund = mean(all_bpe$s.bear_abund),
              s.dist_to_forest = mean(all_bpe$ s.dist_to_forest),
              s.dist_to_town = mean(all_bpe$s.dist_to_town),
              ag_10k = mean(all_bpe$ag_10k),
              shdi_10k = mean(all_bpe$shdi_10k))

# get predicted damage values for each livestock type based on top model or model average and add them as new columns to the joint data frame

# cows

new_data_openhab$pred_c <- 
  predict(c_global_a, 
          newdata = new_data_openhab, 
          re.form = NA, 
          type = "response")

# sheep

new_data_openhab$pred_s <- 
  predict(s_avgMod_95, 
          newdata = new_data_openhab, 
          re.form = NA, 
          type = "response")

# other

new_data_openhab$pred_o <-  
  predict(o_avgMod_95, 
          newdata = new_data_openhab, 
          re.form = NA, 
          type = "response")

# graph with ggplot

openhab_graph <- 
  ggplot() +
  geom_line(data = new_data_openhab, 
            aes(x = openhab_10k, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  geom_line(data = new_data_openhab, 
            aes(x = openhab_10k, 
                y = pred_s), 
            linetype = "longdash",
            size = 1) +
  geom_line(data = new_data_openhab, 
            aes(x = openhab_10k, 
                y = pred_o), 
            linetype = "dotted", 
            size = 1) +
  coord_cartesian(ylim = c(0,1),
                  xlim = c(-1,4.1)) +
  labs(x = "Proportion of Open Habitat (scaled)",
       y = "Probability of Livestock Damage") +
  annotate("text", 
           x = 4.1, 
           y = 1, 
           size = 7.5, 
           label = "(e)") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

openhab_graph


# SHDI --------------------------------------------------------------------

new_data_shdi <- 
  expand.grid(shdi_10k = seq(min(all_bpe$shdi_10k),
                                max(all_bpe$shdi_10k),
                                0.01),
              s.bear_abund = mean(all_bpe$s.bear_abund),
              s.dist_to_forest = mean(all_bpe$ s.dist_to_forest),
              s.dist_to_town = mean(all_bpe$s.dist_to_town),
              ag_10k = mean(all_bpe$ag_10k),
              openhab_10k = mean(all_bpe$openhab_10k))

# get predicted damage values for each livestock type based on top model or model average and add them as new columns to the joint data frame

# cows

new_data_shdi$pred_c <- 
  predict(c_global_a, 
          newdata = new_data_shdi, 
          re.form = NA, 
          type = "response")

# sheep

new_data_shdi$pred_s <- 
  predict(s_avgMod_95, 
          newdata = new_data_shdi, 
          re.form = NA, 
          type = "response")

# other

new_data_shdi$pred_o <-  
  predict(o_avgMod_95, 
          newdata = new_data_shdi, 
          re.form = NA, 
          type = "response")

# retrieve unscaled bear_abund values for x axis

summary(all_bpe$shannondivindex)

# create new vector with values that is the same length as the new data frame or plotting on x-xis

shdi_raw <- 
  seq(0, 1.9374, length.out = 475)

new_data_shdi <- 
  cbind(shdi_raw, 
        new_data_shdi)

# graph with ggplot

shdi_graph <- 
  ggplot() +
  geom_line(data = new_data_shdi, 
            aes(x = shdi_raw, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  geom_line(data = new_data_shdi, 
            aes(x = shdi_raw, 
                y = pred_s), 
            linetype = "longdash",
            size = 1) +
  geom_line(data = new_data_shdi, 
            aes(x = shdi_raw, 
                y = pred_o), 
            linetype = "dotted", 
            size = 1) +
  coord_cartesian(ylim = c(0,1),
                  xlim = c(0,1.9)) +
  labs(x = "Shannon Diversity Index",
       y = "Probability of Livestock Damage") +
  annotate("text", 
           x = 1.92, 
           y = 1, 
           size = 7.5, 
           label = "(f)") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

shdi_graph


# Combine graphs ----------------------------------------------------------

# this section uses library gridExtra and ggplot2

# odds

# odds

odds_plot <- 
  grid.arrange(cow_odds_plot, 
               sheep_odds_plot, 
               other_odds_plot, 
               nrow =2)

ggsave('odds_plot.jpeg',
       odds_plot,
       jpeg,
       width = 16,
       height = 14,
       units = 'in',
       dpi = 300)

# predictive graphs

pred_plot <- 
  grid.arrange(bear_graph, 
               dForest_graph, 
               dTown_graph,
               ag_graph, 
               openhab_graph, 
               shdi_graph, 
               nrow = 3)

ggsave('pred_plot.jpeg',
       pred_plot,
       jpeg,
       width = 13,
       height = 16,
       units = 'in',
       dpi = 300)
