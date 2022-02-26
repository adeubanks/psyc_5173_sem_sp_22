###############################
# PSYC 5173 - SEM - Spring 22
# Lab 2 - General things
# Authored by: Austin Eubanks 
# adeubank@uark.edu
##############################.


# Notes -------------------------------------------------------------------
# projects
# git

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lavaan)
library(broom)


# read in and explore data ------------------------------------------------
df <- read_csv("lab_2_video.csv")
 
df %>% glimpse

df %>% 
  select(where(is.character)) %>% 
  map(table)

df %>% 
  select(where(is.numeric)) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("smooth_loess", 
                                                         alpha = 0.1, 
                                                         size=0.1,
                                                         color = "red")))

# dummy coding ------------------------------------------------------------

df <- df %>% 
  mutate(birthregion_copy = birthregion,
         across(where(is.character), ~ as.factor(.x))) 

summary(lm(income3 ~ birthregion, data = df))

# broom::tidy() just extracts model info and tables it. I'm using it to make the
# model output more compact for the video.
lm(income3 ~ birthregion, data = df) %>% tidy

contrasts(df$birthregion)
df %>% group_by(birthregion) %>% summarise(M = mean(income3))

df %>% 
  ggplot(aes(x = birthregion, y = income3)) + 
  theme_classic()+
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange")

# How to change dummy codes: Option 1 - create numeric vars
df <- df %>% 
  mutate(birthregion_us_0 = ifelse(birthregion == "United States", 0, 1))

lm(income3 ~ birthregion_us_0, data = df) %>% tidy


# Option 2: change the contrasts of a factor
contrasts(df$birthregion_copy)
contrasts(df$birthregion_copy) <- c(1, 0) 

lm(income3 ~ birthregion, data = df) %>% tidy
lm(income3 ~ birthregion_copy, data = df) %>% tidy

# needs to be a matrix if more than 2
matrix(1:12, ncol = 3)



# p isn't everything with a big n's
lm(income3 ~ sex, data = df) %>% tidy
df %>% group_by(sex) %>% summarise(M = mean(income3))

df %>% 
  ggplot(aes(x = sex, y = income3)) + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange")

# Remember, p is testing if something is *exactly* zero.

# dummy codes for > 2 groups ----------------------------------------------

lm(income3 ~ mom_ed, data = df) %>% tidy
contrasts(df$mom_ed)
df %>% group_by(mom_ed) %>% summarise(M = mean(income3))

df <- df %>% 
  mutate(mom_ed = fct_relevel(mom_ed, "High school graduate"))

contrasts(df$mom_ed)
lm(income3 ~ mom_ed, data = df) %>% tidy


# Other option, manually make coded variables:
df <- df %>% 
  mutate(mom_ed_d1 = ifelse(mom_ed == "College graduate or more", 1, 0),
         mom_ed_d2 = ifelse(mom_ed == "less.than.hs", 1, 0),
         mom_ed_d3 = ifelse(mom_ed == "Some college/university", 1, 0))

lm(income3 ~ mom_ed_d1 + mom_ed_d2 + mom_ed_d3, data = df) %>% tidy


# Regular ol' regressions vs. lavaan --------------------------------------


birthregion_mod_1_spec <- 'income3 ~ 1 + birthregion' 
# Reminder: the 1 just puts the intercept in the output but it's just like
# "getting the receipt;" it doesn't change anything computationally.
birthregion_mod_2_spec <- 'income3 ~ birthregion' 

birthregion_mod_1 <- df %>% 
  sem(birthregion_mod_1_spec, data = .)

birthregion_mod_2 <- df %>% 
  sem(birthregion_mod_2_spec, data = .)

lm(income3 ~ birthregion, data = df) %>% tidy

summary(birthregion_mod_1)
summary(birthregion_mod_2)


mod_2_spec <- 'income3 ~ 1 + birthregion + sma' 
mod_2 <- df %>% sem(mod_2_spec, data = .)

lm(income3 ~ birthregion + sma, data = df) %>% tidy

summary(mod_2)

semPlot::semPaths(mod_2)
?semPlot::semPaths
semPlot::semPaths(mod_2, 'est',
         edge.label.cex = 2, 
         sizeMan = 10,
         fade = F,
         residuals = F,
         layout = "tree2") 
