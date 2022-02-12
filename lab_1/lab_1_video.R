###############################
# PSYC 5173 - SEM - Spring 22
# Lab 1 - Assumptions
# Authored by: Austin Eubanks 
# adeubank@uark.edu
##############################.


# Notes -------------------------------------------------------------------

# Everything we go over is just *A* way of doing things, not necessarily *the*
# way of doing it. In fact, as I will discuss in the video, some things we'll do
# are specifically things I wouldn't do in "real-world" analyses, but rather are
# for educational purposes (e.g., see extra steps below for matrices to excel).

# For instance, I am going to do my best to stick with just tidyverse, base R,
# and lavaan. The upside of this decision is you don't have to try to remember a
# bunch of different functions from a bunch of different libraries. The downside
# is some things we'll do have easier ways of doing it because there is a
# function that does the exact thing we want. 

####################.
# If you don't have tidyverse or lavaan installed, install them both by
# running the lines below, after deleting the "#" in front of them:

# install.packages("tidyverse")
# install.packages("lavaan")

# The library, psych, is also my go-to for general descriptives and
# scale/measure reliabilities (alphas), so you also need to

# install.packages("psych")

####################.

# Because we use tidyverse, most things we use are technically "tibbles" and not
# "data frames," but I more or less use the terms interchangeably. My video at
# tinyurl.com/tidyverse explains the technical differences between the two. 


#                                               Mac         Windows
# Section labels (collapsible headers):  	 Cmd+Shift+R   Ctrl+Shift+R 	

# Reflow comments:  	                     Cmd+Shift+/   Ctrl+Shift+/

# Insert pipe:  	                         Cmd+Shift+M   Ctrl+Shift+M

# Explaining projects / working directories 

# NOT writing things to my computer, thanks (comment out (#) anything that
# writes, e.g., write_csv, exporting plots)

# Libraries ---------------------------------------------------------------
library(tidyverse)

# For Tidyverse overview, see my video at tinyurl.com/tidyverse


# Problem 1 - read in,  structure,  descriptives --------------------------
# Read in the data, look at it's strucute, check descriptives.

df <- read_csv("lab_1_video.csv") # read in

df %>% glimpse # structure

df %>% psych::describe() # descriptives

# Note: for now, we're giving you simple 5 var data sets, but for bigger
# datasets, you may need to sometimes use e.g., select() to select down to
# whatever subset of variables you're interested in

# Problem 2 - Check missingness -------------------------------------------

# you can use the map() family of functions, which apply some function you enter
# as the input across a data frame***. map() returns lists, but map_dfc() returns
# a data frame that column-binds the lists

df %>% 
  map_dfc(is.na) %>% 
  colMeans()

# *** Technically, map() applies the function across lists/vectors, which is why
# it returns lists unless you use e.., map_dfc(), but also technically, a data
# frame/tibble is just a bunch of lists in that each column is a list.

# Problem 3 - check distribution/outliers ---------------------------------

# check plots for all 5 vars (to look for outliers and show distributions)

df %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value))+
  geom_histogram(bins = 10)+
  #geom_density()+
  #geom_boxplot()+
  facet_wrap(~name, scales = "free")

# scales can be "free_x" "free_y" or "free" (meaning both x and y)


# Problem 4 - Transforming variables --------------------------------------

# --- Common heuristics transformations---

#                           Skew    
#                Positive               Negative
# moderate     square root      reflect then square root
#                sqrt(x)                sqrt(K - x)
#
# high        natural log       reflect then natural log
#                log(x)                 log(K - x)
#                                          -or- 
#                                         exp(x)
#
# extreme       inverse            reflect then inverse
#                 1/x                   1/(K - x)  

# the inverse of natural logs is exponentiation ( exp() )
log(exp(5)) # see? It cancels each other out. 

# "Reflect" basically means reverse score, which we do by subtracting each score
# from the (max possible score + 1), so K = max(x) + 1

# For square root, x must be >= 0
# For logs, x must be > 0
# For inverse, x != 0

df %>%
  select(se1, dep1) %>% 
  psych::describe()

# dep1 == positive skew
df %>% 
  ggplot(aes(x = dep1))+
  geom_density()

df %>% 
  mutate(dep1 = 1/(dep1)) %>% 
  ggplot(aes(x = dep1))+
  geom_density()

# se1 = negative skew
df %>% 
  ggplot(aes(x = se1))+
  geom_density()

df %>% 
  mutate(se1 = exp(se1)) %>% 
  ggplot(aes(x = se1))+
  geom_density()

# it's bad form to overwrite existing data; instead, when you transform vars,
# create new variables with labels that indicate what transformation has
# happened, e.g.,

df <- df %>%  
  mutate(dep1_inv = 1/dep1, 
         se1_exp = exp(se1)) %>% 
  select(-dep1, -se1) 
                  # Normally I would leave both the transformed and
                  # untransformed vars in, but for educational purposes
                  # (i.e., just to make things easy for the HW), I want 
                  # to only keep 5 vars in the data set (instead of having
                  # to select down to just the 5 we want for everything)


# Problem 5 - BIvariate scatterplots --------------------------------------
# check bivariate scatterplots with loess lines for the all pairings.

df %>% 
  pairs(panel = panel.smooth) # panel = panel.smooth adds the loess lines


# Problem 6 - Variance-covariance matrix ----------------------------------
# Create the variance-covariance matrix and export it as a csv

df %>% 
  var(use = "pair") %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() # %>% write_csv("vcov.csv")


# Problem 7 - Correlation matrix ------------------------------------------
# Create a correlation matrix and export it as a csv
df %>% 
  cor(use = "pair") %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() # %>% write_csv("cor.csv")


# Problem 8 - The determinant ---------------------------------------------
# Calculate the determinant of the variance-covariance matrix

df %>% 
  var(use = "pair") %>% 
  det()

# Problem 9 - Dichotomizing vars ------------------------------------------

df <- df %>% 
  mutate(sra_dich_high = ifelse(sra >= mean(sra, na.rm = T), 1, 0))


# Additional Info ---------------------------------------------------------

# Problem 2

df %>% 
  # map_dfc(is.na) %>% 
  
  # mutate(across(everything(), is.na)) %>% 
  # mutate(across(sex_f:sra, is.na)) %>% 
  # mutate(across(c(sex_f, birthregion_us, dep1, se1, sra), is.na)) %>% 
  
  colSums() # gets counts
#colMeans() # get percents



# Problem 4
# dep1 == positive skew
df %>% 
  ggplot(aes(x = dep1))+
  geom_density()

df %>% 
  mutate(dep1 = sqrt(dep1)) %>% 
  ggplot(aes(x = dep1))+
  geom_density()

df %>% 
  mutate(dep1 = log(dep1)) %>% 
  ggplot(aes(x = dep1))+
  geom_density()

df %>% 
  mutate(dep1 = 1/(dep1)) %>% 
  ggplot(aes(x = dep1))+
  geom_density()

df %>% 
  mutate(sqrt = sqrt(dep1),
         log = log(dep1),
         inv = 1/dep1) %>% 
  select(sqrt:inv) %>% 
  psych::describe()


# se1 = negative skew
df %>% 
  ggplot(aes(x = se1))+
  geom_density()

df %>% 
  mutate(se1 = sqrt(5 - se1)) %>% 
  ggplot(aes(x = se1))+
  geom_density()

df %>% 
  mutate(se1 = log(5 - se1)) %>% 
  ggplot(aes(x = se1))+
  geom_density()

df %>% 
  mutate(se1 = 1/(5 - se1)) %>% 
  ggplot(aes(x = se1))+
  geom_density()

df %>% 
  mutate(se1 = exp(se1)) %>% 
  ggplot(aes(x = se1))+
  geom_density()

df %>% 
  mutate(sqrt = sqrt(5 - se1),
         log = log(5 - se1),
         inv = 1/(5 - se1),
         exp = exp(se1)) %>% 
  select(sqrt:exp) %>% 
  psych::describe()



# Problem 9 
# Just FYI: If you neen more than 2 groups, ifelse() won't work becasue it only
# has 2 outcomes (TRUE/FALSE). For 3+ conditional statements, use case_when()

df %>% 
  mutate(sra_tri = case_when(sra <  mean(sra, na.rm = T) - sd(sra, na.rm = T) ~ "low",
                             sra >  mean(sra, na.rm = T) + sd(sra, na.rm = T) ~ "high",
                             TRUE ~ "mid")) %>%  # "TRUE" is the catchall meaning
  select(sra_tri) %>%                            # "everything else"
  table
