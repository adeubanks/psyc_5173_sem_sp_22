###############################
# PSYC 5173 - SEM - Spring 22
# Homework 4 - Assumptions
##############################.


# Notes -------------------------------------------------------------------

# If you haven't already, please give Austin feedback on the video that was
# posted so he can tailor the next one to your needs. There is a 3 question
# qualtrics survey here: https://uark.qualtrics.com/jfe/form/SV_efVZWXV1cDUiLAy

# For the assignment below, answer the questions in the script as comments.
# There isn't any one "right" way to format your responses as long as your
# answers are easily to identify and you include the code that brought you to
# that answer. 

# Libraries ---------------------------------------------------------------
library(tidyverse)


# Read in the data,  check structure and descriptives. --------------------
# The data is the file, "HW_4.csv" -- You'll notice these data are similar to
# the video, only it's some time-2 measure of self-esteem and depression, and
# reading achievement (sra in the video) has been replaced with a continuous
# measure of discrimination, "discrim2". 

df <- read_csv("HW_4.csv")

df %>% glimpse

df %>% psych::describe()

# Problem 1 - Missing data ------------------------------------------------
# Check the amount of missing data. Are any concerning (i.e., > 5%)?

df %>% 
  map_dfr(is.na) %>% 
  colMeans()



# Problem 2 - Normality ---------------------------------------------------
# Generate plots for the variables. Do any look skewed?  

df %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value))+
  geom_histogram(bins = 10)+
  facet_wrap(~name, scales = "free")



# Problem 3 - Linearity ---------------------------------------------------
# Generate bivariate scatterplots with loess lines of each pair of variables. Do
# any lines look curvilinear or do things look more or less linear?

df %>% 
  pairs(panel = panel.smooth)


# Problem 4 - Multicollinearity/singularity -------------------------------
# Generate a correlation matrix of all 5 variables that you can export as a csv.

# (Note: exporting as a csv isn't strictly necessary when you have so few
# variables, but if you imagine having dozens or hundreds of variables, you can
# imagine how you wouldn't be able to see the entire correlation matrix just in
# the console and exporting/opening a csv might be preferable.)

# Do you see any concerning high correlations (e.g., r > .7)?

df %>% 
  cor(use = "pair") %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column()  # %>% write_csv("cor.csv")


# Problem 5 - Variance/covariance -----------------------------------------
# Generate a variance/covariance (var/cov) matrix. 

# Are any of your variances close to 0?
# Are any of your covariances close to 1?

df %>% 
  var(use = "pair")


# Problem 6 - Determinant -------------------------------------------------
# Find the determinant of the var/cov matrix. 

# Is it too close to 0? 

df %>% 
  var(use = "pair") %>% 
  det()

# Problem 7 - Recommendations --------------------------------------------- 
# Now that you have thoroughly explored your data and examined your assumptions,
# what recommendations do you have for preparing your data for SEM?

# What do you need to do to implement your recommendations (e.g., if you
# recommend transformations based on your answers in problems 1 - 7 but didn't
# do the transformations, while answering the questions, explain/show below how
# you would accomplish this in R.)




# Save and submit --------------------------------------------------------- 
# Once you have finished your assignment, save this script as "LastName_HW4.R"
# and submit it via blackboard. 

