library(tidyverse)

ec_ter <- read_csv("Final/ec_ter.csv")

ec_ter %>%
  ggplot(aes(x = attacks_per_mil)) +
    geom_density()

ec_ter %>%
  ggplot(aes(x = log(attacks_per_mil))) +
  geom_density()

######

require(ggplot2)
require(pscl)
require(MASS)
require(boot)

model <- glm.nb(attacks_per_mil ~ econ_equal,
                data = ec_ter)

model3 <- glm.nb(attacks_per_mil ~ econ_equal + year + as.factor(country),
                 data = ec_ter)