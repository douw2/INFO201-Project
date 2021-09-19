library("dplyr")
library("tidyr")
library("ggplot2")

# Ramen Ratings table
ramen_ratings <- read.csv("data/ramen-ratings.csv", stringsAsFactors = FALSE)
ramen_sample <- ramen_ratings %>% 
  select(Brand, Variety, Style, Country, Stars) %>% 
  head()

#2.2 Q1
ramen_df <- read.csv("data/ramen-ratings.csv") %>%
  select(Brand, Variety, Style, Country, Stars) %>%
  drop_na(Stars)

ramen_df$Stars <- as.numeric(as.character(ramen_df$Stars))

#Wine Ratings table
wine_ratings <- 
  read.csv("data/winemag-data_first150k.csv", stringsAsFactors = FALSE)
wine_sample <- wine_ratings %>% 
  select(winery, variety, country, points, price) %>%
  head()
wine_df <- read.csv("data/winemag-data_first150k.csv") %>%
  select(winery, variety, country, points, price) %>%
  drop_na(price)

# Michelin Ratings table
one_star <- 
  read.csv("data/one-star-michelin-restaurants.csv", stringsAsFactors = FALSE) %>% 
  mutate(star = "one")
two_star <- 
  read.csv("data/two-stars-michelin-restaurants.csv", stringsAsFactors = FALSE) %>% 
  mutate(star = "two")
three_star <- 
  read.csv("data/three-stars-michelin-restaurants.csv", stringsAsFactors = FALSE) %>% 
  mutate(star = "three")

# Micheline Ratings table for summary (stringsAsFactors = TRUE)
one_star_analysis <- 
  read.csv("data/one-star-michelin-restaurants.csv") %>% 
  mutate(star = as.factor("one"))
two_star_analysis <- 
  read.csv("data/two-stars-michelin-restaurants.csv") %>% 
  mutate(star = as.factor("two"))
three_star_analysis <- 
  read.csv("data/three-stars-michelin-restaurants.csv") %>% 
  mutate(star = as.factor("three"))

michelin_ratings <- 
  rbind(one_star, two_star, three_star) %>% 
  mutate(country = region) %>% 
  select(name, country, star, price)

# Change the city/state in United States into US
michelin_ratings$country[michelin_ratings$country == "California"] <- "US"
michelin_ratings$country[michelin_ratings$country == "Chicago"] <- "US"
michelin_ratings$country[michelin_ratings$country == "New York City"] <- "US"
michelin_ratings$country[michelin_ratings$country == "Washington DC"] <- "US"

# Change the scale of price
michelin_ratings$price[michelin_ratings$price == "$"] <- "1"
michelin_ratings$price[michelin_ratings$price == "$$"] <- "2"
michelin_ratings$price[michelin_ratings$price == "$$$"] <- "3"
michelin_ratings$price[michelin_ratings$price == "$$$$"] <- "4"
michelin_ratings$price[michelin_ratings$price == "$$$$$"] <- "5"

# get samples for michelin ratings
michelin_sample <- 
  head(michelin_ratings)

# Combine one star too three star tables for summary
michelin_df <- 
  rbind(one_star_analysis, two_star_analysis, three_star_analysis) %>% 
  mutate(country = region) %>% 
  select(name, country, star, price)

# Change country and price columns into factors and filter out restaurants without price
michelin_df <- 
  mutate(michelin_df, 
         country = as.factor(michelin_ratings$country),
         price = as.factor(michelin_ratings$price)) %>% 
  filter(price != "N/A")

