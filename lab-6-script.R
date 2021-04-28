
# load packages -----------------------------------------------------------
install.packages("palmerpenguins")
library(tidyverse)
library(palmerpenguins)

# read data ---------------------------------------------------------------
data("penguins")

fish <- read_csv("chap12q19ElectricFish.csv")

head(penguins)

str(penguins)

# do stuff ----------------------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

fish_long_summary <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

t.test(formula = species ~ location, data = fish_long)

fish_long %>% 
  ggplot(aes(x = location, y = species)) +
  geom_jitter(aes(color = location), 
              shape = 16, size = 3, 
              alpha = 0.3, width = 0.4) +
  geom_errorbar(aes(y = mean, ymax = upper, ymin = lower), 
                data = fish_long_summary, 
                width = .1, size = .8) +
  geom_point(aes(y = mean), 
             data = fish_long_summary, 
             size = 3) +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal() +
  guides(color = "none")

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

# crab stuff----------------------------------------------------------------

crab <- read_csv("chap15q27FiddlerCrabFans.csv")

crab_summary <-
  crab %>% 
  group_by(crabType) %>% 
  filter(!is.na(bodyTemperature)) %>%      # remove missing values
  summarize(
    n = n(),
    mean = mean(bodyTemperature),
    sd = sd(bodyTemperature),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

crab %>% 
  ggplot(aes(x = crabType, y = bodyTemperature)) +
  geom_jitter(aes(color = crabType), 
              shape = 16, size = 3, 
              alpha = 0.3, width = 0.4) +
  geom_errorbar(aes(y = mean, ymax = upper, ymin = lower), 
                data = crab_summary, 
                width = .1, size = .8) +
  geom_point(aes(y = mean), 
             data = crab_summary, 
             size = 3) +
  scale_color_manual(values = c("darkorange","cyan4","red", "green")) +
  theme_minimal() +
  guides(color = "none")

crab %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType),
    bins = 12, 
    alpha = 0.5, 
    position = "identity", 
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c("darkorange","cyan4", "red", "green")) +
  theme_minimal()

aov_crabby <-
  aov(bodyTemperature ~ crabType, data = crab)
aov_crabby

summary(aov_crabby)
