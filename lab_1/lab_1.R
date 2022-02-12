
#####
#Create a new subset of 5 variables from the data I sent you and make that their
#homework assignment where students have to basically run descriptives,
#histograms, bivariate scatterplots, missing data analyses, perhaps transform a
#variable, look over v-cov matrix, look over R matrix, and calculate a v-cov
#determinant. They will need to both write the code AND answer questions about
#the meaning of the numbers (e.g., do histograms look normal? Do bivariate
#scatterplots look linear? Are there too many missing data points in a
#variable-- i.e., >5%?, are there any correlations that are multicollinear--
#e.g., >.70? Is the determinant of the matrix too close to 0 and therefore will
#it create problems for us? etc etc)

library(tidyverse)

lab_1 <- read_csv("lab_1.csv") 


# 1 -----------------------------------------------------------------------
# Given a data frame with raw data for variables x1-x5, calculate (and export
# into an excel table) the variance-covariance matrix
lab_1 %>% 
  var(use = "pair") %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  write_csv("vcov.csv")

# 2 -----------------------------------------------------------------------
# Calculate (and export into an excel table) the correlation matrix
lab_1 %>% 
  cor(use = "pair") %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  write_csv("cor.csv")


# 3 -----------------------------------------------------------------------
# Calculate the determinant of the variance-covariance matrix
lab_1 %>% 
  var(use = "pair") %>% 
  det()


# 4 -----------------------------------------------------------------------
# check descriptives for x1-x5
lab_1 %>% psych::describe()


# 5 -----------------------------------------------------------------------
# check histograms for x1-x5 (to identify outliers and show distribution)
lab_1 %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value))+
  #geom_histogram(bins = 10)+
  geom_density()+
  #geom_boxplot()+
  facet_wrap(~name, scales = "free")


# 6 -----------------------------------------------------------------------
# check bivariate scatterplots with loess lines for the x1-x5 pairings (x1-x2, x1-x3, x2-x3, etc.)

lab_1 %>% pairs(panel = panel.smooth)


# 7 -----------------------------------------------------------------------
# Transform a variable (square root transformation of a moderately positively skewed variable)

#untransformed
lab_1 %>% 
  ggplot(aes(x = se2))+ 
  geom_density()

#transformed
lab_1 %>% 
  mutate(se2_tx = exp(se2)) %>% 
  ggplot(aes(x = se2_tx))+
  geom_density()

lab_1 %>% 
  mutate(se2_tx = exp(se2)) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value))+
  #geom_histogram(bins = 10)+
  geom_density()+
  #geom_boxplot()+
  facet_wrap(~name, scales = "free")

lab_1 %>% 
  mutate(se2_tx = exp(se2)) %>% 
pairs(panel = panel.smooth)
# 8 -----------------------------------------------------------------------
# dichotomize a continuous variable
lab_1 %>% 
  mutate(sra_dich_high = ifelse(sra >= mean(sra, na.rm = T), 1, 0))


# 9 -----------------------------------------------------------------------
# Calculate amount of missingness of a variable
lab_1 %>% 
  mutate(across(everything(), is.na)) %>% 
  colSums()
