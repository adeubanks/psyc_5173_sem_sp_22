###############################
# PSYC 5173 - SEM - Spring 22
# Lab 2 - General things
# Authored by: Austin Eubanks 
# adeubank@uark.edu
##############################.


# Notes -------------------------------------------------------------------
options(scipen = 20)

# projects
# git

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lavaan)


# Data,  structure,  descriptives -----------------------------------------

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
  mutate(across(where(is.character), ~ as.factor(.x))) 

summary(lm(income3 ~ birthregion, data = df))
contrasts(df$birthregion)
df %>% group_by(birthregion) %>% summarise(M = mean(income3))

df %>% 
  ggplot(aes(x = birthregion, y = income3)) + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange")

# How to change dummy codes: Option 1 - create numeric vars
df <- df %>% 
  mutate(birthregion_us_0 = ifelse(birthregion == "United States", 0, 1))

summary(lm(income3 ~ birthregion_us_0, data = df))

# Option 2: change the contrasts of a factor
contrasts(df$birthregion)
contrasts(df$birthregion) <- c(1, 0)

summary(lm(income3 ~ birthregion, data = df))



# p isn't everything with a big n's
summary(lm(income3 ~ sex, data = df))
df %>% group_by(sex) %>% summarise(M = mean(income3))

df %>% 
  ggplot(aes(x = sex, y = income3)) + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange")


# dummy codes for > 2 groups
summary(lm(income3 ~ mom_ed, data = df))
contrasts(df$mom_ed)
df %>% group_by(mom_ed) %>% summarise(M = mean(income3))

df <- df %>% 
  mutate(mom_ed = fct_relevel(mom_ed, "High school graduate"))

contrasts(df$mom_ed)
summary(lm(income3 ~ mom_ed, data = df))


# Other option, manually make coded variables:

df <- df %>% 
  mutate(mom_ed_d1 = ifelse(mom_ed == "less.than.hs", 1, 0),
         mom_ed_d2 = ifelse(mom_ed == "Some college/university", 1, 0),
         mom_ed_d3 = ifelse(mom_ed == "College graduate or more", 1, 0))


# HS grad as ref
contrasts(df$mom_ed) <- matrix(c(1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0), ncol = 3)



#

# Data,  structure,  descriptives -----------------------------------------
?carData::Prestige

df  <- carData::Prestige %>%
  mutate(education_c = education - mean(education), 
         prestige_c = prestige - mean(prestige),
         education_std = education, 
         income_std = income, 
         prestige_std = prestige,
         across(education_std:prestige_std, ~ scale(.x) %>% as.vector))

df %>% glimpse

df %>% psych::describe()


# Assumptions -------------------------------------------------------------

df %>% 
  select(education, income, prestige) %>% 
  map_dfc(is.na) %>% 
  colMeans() # no missing

df %>% 
  select(education, income, prestige, X, Y, M) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value))+ theme_classic()+
  geom_density()+
  facet_wrap(~name, scales = "free")

df %>% 
  select(education, income, prestige) %>% 
  pairs(panel = panel.smooth)

df %>% 
  select(X, Y, M) %>% 
  pairs(panel = panel.smooth)


df %>% 
  select(education, income, prestige) %>% 
  cor()

df %>% 
  select(X, Y, M) %>% 
  cor()


df %>% 
  select(education, income, prestige) %>% 
  var()

df %>% 
  select(education, income, prestige) %>% 
  var() %>% det()

df %>% 
  select(Y, X, M) %>% 
  var()

df %>% 
  select(Y, X, M) %>% 
  var() %>% det()

# Regular ol' regressions vs. lavaan --------------------------------------


edu_only <- 'income ~ 1 + education' # Reminder: the 1 just puts the intercept in the output
pre_only <- 'income ~ 1 + prestige' # but it's just like "getting the receipt;" it doesn't
eduand_pre <- 'income ~ 1 + education + prestige' # change anything computationally.

edu_mod <- df %>% 
  sem(edu_only, data = .) # A note on the warning message in a moment**

edu_lm <- lm(income ~ education, data = df)

summary(edu_mod)
summary(edu_lm)

# Warning message: this is 

pre_mod <- df %>% 
  sem(pre_only, data = .)

both_mod <- df %>% 
  sem(education_and_pre, data = .)



med_plot(pre_mod)
summary(pre_mod)
summary(lm(income ~ prestige, data = df))

summary(both_mod)
summary(lm(income ~ education + prestige, data = df))


# Mediation ---------------------------------------------------------------

# Suppose we had reason to believe that more years of education lead to larger
# incomes because of the prestige associated with a career (i.e. the relationship
# between more education and larger income is mediated by prestige of the career).

# Just a generic template of the standard mediation model
mediation_model <- 
  ' 
# direct effect
Y ~ c*X

# mediator
M ~ a*X
Y ~ b*M

# indirect effect (a*b)
ab := a*b

# total effect
total := c + (a*b)
'

df %>% 
  med_plot(mediation_model) # This function doesn't exist for you. 


prestige_mediation <- 
  ' 
# direct effect
income ~ c*education

# mediator
prestige ~ a*education
income ~ b*prestige

# indirect effect (a*b)
ab := a*b

# total effect
total := c + (a*b)
'

df %>% 
  med_plot(prestige_mediation) 
# Again, this function doesn't exist for you; it's for educational purposes. 

df %>% 
  sem(prestige_mediation, data = .) %>% 
  summary()


med_plot_info(df, prestige_mediation)

