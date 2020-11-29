library(tidyverse)
library(readr)
library(gtsummary)
library(broom.mixed)
library(gt)


mydata <- read_csv("Final/ec_ter.csv")
  
fit <- MASS::glm.nb(attacks_per_mil ~ econ_equal, data = mydata)

tbl_regression(fit, intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Terror Attack Frequency", 
             subtitle = "The Effect of Economic Equality on Terror Attacks per Million") 

