combo <- read_csv("Final/ec_ter.csv")

combo %>% 
  ggplot(aes(x = econ_equal, y = attacks_per_mil)) +
  geom_point() +
  geom_smooth(method = MASS::glm.nb, color = "blue") +
  theme_classic()