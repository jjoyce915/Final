mydata <- read_csv("Final/ec_ter.csv")
  
stanglam <- stan_glm(attacks_per_mil ~ econ_equal,
            data = mydata,
            refresh = 0,
            family = gaussian())

# higher econ_equal is better


mydata %>% 
  ggplot(aes(x = econ_equal, y = attacks_per_mil)) +
  geom_point() +
  geom_line(aes(y = fitted(stanglam)), color = "blue") +
  labs(title = "Regression Between Economic Equality and Terror Attack Frequency",
       subtitle = "There is a correlation between less economic equality and more terror attacks", 
       x = "Equal Distribution of Resources Index Score",
       y = "Number of Terror Attacks") +
  ylim(0, 10) +
  theme_classic()


