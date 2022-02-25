###############################
# PSYC 5173 - SEM - Spring 22
# Lab 2 - Mediation
# Authored by: Austin Eubanks 
# adeubank@uark.education
##############################.


# Notes -------------------------------------------------------------------
source("source/med.R") # <-don't worry about this. It's only for my plots.
options(scipen = 20)

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lavaan)



# Data,  structure,  descriptives -----------------------------------------
?carData::Prestige

df  <- carData::Prestige %>%
  mutate(education = education - mean(education), 
         prestige = prestige - mean(prestige),
         X = education, 
         Y = income, 
         M = prestige,
         across(X:M, ~ scale(.x) %>% as.vector))

df %>% glimpse

df %>%   
  select(education, income, prestige) %>% 
  psych::describe()


# Assumptions -------------------------------------------------------------

df %>% 
  select(education, income, prestige) %>% 
  map_dfc(is.na) %>% 
  colMeans() # no missing

df %>% 
  select(education, income, prestige) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
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
  select(Y, X, M) %>% 
  var()
# Regular ol' regressions vs. lavaan --------------------------------------


education_only <- 'income ~ 1 + education' # Reminder: the 1 just puts the intercept in the output
pre_only <- 'income ~ 1 + prestige' # but it's just like "getting the receipt;" it doesn't
education_and_pre <- 'income ~ 1 + education + prestige' # change anything computationally.

education_mod <- df %>% 
  sem(education_only, data = .)

pre_mod <- df %>% 
  sem(pre_only, data = .)

both_mod <- df %>% 
  sem(education_and_pre, data = .)


summary(education_mod)
summary(lm(income ~ education, data = df))

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


df %>% med_plot_info(prestige_mediation)
