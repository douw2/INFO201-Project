source("data_description.R")
library("ggplot2")

#############
### Ramen ###
#############

# Ramen Ratings Table
ramen_df_2.2 <- ramen_ratings %>%
  select(Brand, Country, Stars)
ramen_df_2.2$Country[ramen_df_2.2$Country == "United States"] <- "USA"
ramen_df_2.2$Stars <- suppressWarnings(as.double(ramen_df_2.2$Stars))
ramen_df_2.2 <- drop_na(ramen_df_2.2, Stars)

# Ramen Average Rating Table
ramen_df_2.2_1 <- ramen_df_2.2 %>%
  group_by(Country) %>%
  summarize(average_rating = mean(Stars))

# Ramen Number of Brands Table
ramen_df_2.2_2 <- ramen_df_2.2 %>%
  group_by(Brand, Country) %>%
  summarize(num_brand = n()) %>%
  group_by(Country) %>%
  summarize(num_brand = n())

# Ramen Average Rating Plot
ramen_plot_rating <- ggplot(data = ramen_df_2.2_1) +
  geom_col(mapping = aes(x = Country, y = average_rating, colour = Country)) +
  labs(title = "Average Rating of Ramen for Each Country") +
  ylab("Average Rating") +
  theme(axis.text.x = element_text(size = 7, angle = 90), axis.text.y = element_text(size = 8))

# Ramen Outliers 1
Nigeria_rating <- ramen_df_2.2_1 %>%
  filter(Country == "Nigeria") %>%
  pull(average_rating)
Canada_rating <- ramen_df_2.2_1 %>%
  filter(Country =="Canada") %>%
  pull(average_rating)
Netherlands_rating <- ramen_df_2.2_1 %>%
  filter(Country == "Netherlands") %>%
  pull(average_rating)

# Ramen Number of Brands Plot
ramen_plot_num_brand <- ggplot(data = ramen_df_2.2_2) +
  geom_col(mapping = aes(x = Country, y = num_brand, colour = Country)) +
  labs(title = "Number of Ramen Brands for Each Country") +
  ylab("Number of Brand") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8))

# Ramen Outliers 2
USA_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "USA") %>%
  pull(num_brand)
Thailand_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "Thailand") %>%
  pull(num_brand)
Taiwan_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "Taiwan") %>%
  pull(num_brand)
South_Korea_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "South Korea") %>%
  pull(num_brand)
Malaysia_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "Malaysia") %>%
  pull(num_brand)
Japan_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "Japan") %>%
  pull(num_brand)
China_num_brand <- ramen_df_2.2_2 %>%
  filter(Country == "China") %>%
  pull(num_brand)

############
### Wine ###
############

# load wine reviews data and wrangle it to illustrate the distribution between countries and variety count
wine_distribution <- wine_ratings %>% 
  select(winery, variety, country, points, price) %>% 
  group_by(country) %>% 
  summarize(count = length(country)) %>% 
  arrange(-count)

wine_distribution_for_plot <- wine_ratings %>% 
  select(winery, variety, country, points, price) %>% 
  group_by(country) %>% 
  summarize(count = length(country)) %>% 
  arrange(-count) %>% 
  filter(count > 100)

# wine outliers
wine_outliers <- wine_distribution %>% 
  filter(count < 100, country != "") %>%
  pull(country)

# wine bar plot to show distribution
wine_distribution_plot <- ggplot(data = wine_distribution_for_plot) +
  geom_col(mapping = aes(reorder(country, count), y = count)) +
  labs(title = "Wine Variety Count Distribution", 
       x = "Country", 
       y = "Variety Counts") +
  theme(axis.text.x = element_text(size = 10, angle = 90))

################
### Michelin ###
################

# Michelin Plot1
country_star_df <- michelin_df %>% 
  select(country, star) %>% 
  group_by(country) %>% 
  summarize(one_star = sum(star == "one"),
            two_stars = sum(star == "two"),
            three_stars = sum(star == "three")) %>%
  gather(key = star, value = number, -country)

country_star_plot <- ggplot(data = country_star_df) +
  geom_col(mapping = aes(x = country, y = number, 
                         fill = star), position = "dodge") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Numbers of Different Stars of Michelin Restaurants among Countries",
       x = "Country",
       y = "Numbers of Michelin Restaurants",
       fill = "Stars") +
  theme(axis.text.x = element_text(size = 7, angle = 90),
        legend.position = c(0.15, 0.8))
michelin_2.2_outlier1 <- as.integer(country_star_df$number[country_star_df$country == "United Kingdom"][country_star_df$star == "star_one"])[1]
michelin_2.2_outlier2 <- as.integer(country_star_df$number[country_star_df$country == "US"][country_star_df$star == "star_one"])[1]

# Michelin Plot2
star_price_df <- michelin_df %>% 
  select(star, price) %>% 
  group_by(star) %>% 
  filter(price != "N/A") %>% 
  mutate(price = as.numeric(price)) %>% 
  group_by(star) %>% 
  summarize(average_price = mean(price))

star_price_plot <- ggplot(data = star_price_df) +
  geom_col(mapping = aes(x = star, y = average_price, fill = star)) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Average Price among Different Stars of Michelin Restaurants",
       x = "Stars of Restaurant",
       y = "Average Price (1 - 5)")

