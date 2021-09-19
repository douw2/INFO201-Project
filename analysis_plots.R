source("data_description.R")
library("ggplot2")
library("maps")

#Q1

# Wine Ratings
wine_plot1 <- 
  wine_ratings %>% 
  select(country, points) %>%
  group_by(country) %>% 
  summarize(average_rating_wine = mean(points)) %>% 
  filter(country != "")

# Michelin Ratings
michelin_plot1 <- 
  michelin_ratings %>% 
  select(country, star) %>% 
  filter(country != "N/A")

# other data wrangling [change scale of star rating]
michelin_plot1$star[michelin_plot1$star == "one"] <- 80
michelin_plot1$star[michelin_plot1$star == "two"] <- 90
michelin_plot1$star[michelin_plot1$star == "three"] <- 100
michelin_plot1 <- mutate(michelin_plot1, star = as.numeric(star))
michelin_plot1 <- michelin_plot1 %>% 
  group_by(country) %>% 
  summarize(average_rating_michelin = mean(star))

# Table
q1_rating_wine_michelin <- left_join(michelin_plot1, wine_plot1, by = "country")
q1_rating_wine_michelin <- filter(q1_rating_wine_michelin,
                                  !is.na((q1_rating_wine_michelin$average_rating_wine)))

# Plot
rwm_df <- gather(q1_rating_wine_michelin,
                 key = type, value = number, -country)
q1_rating_wine_michelin_plot <- ggplot(data = rwm_df) +
  geom_col(mapping = aes(x = country, y = number, fill = type), position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(75, 90)) +
  labs(title = "Ratings of Wines and Michelin restaurants",
       x = "Country",
       y = "Rating",
       fill = "Type")

#Q2

# store the ramen ratings into q2_ramen_df, change column name of country and select brand column and country column
q2_ramen_df <- 
  ramen_ratings %>% 
  mutate(country = Country) %>%
  select(Brand, country)

# Change the country name United States to USA
q2_ramen_df$country[q2_ramen_df$country == "United States"] <- "USA"

# Combine one star, two star and three star restaurants and add a new column "country"
q2_michelin_df <- 
  michelin_ratings %>% 
  select(name, country, star)

# Change the region name in country columns into country names
q2_michelin_df$country[q2_michelin_df$country == "California"] <- "USA"
q2_michelin_df$country[q2_michelin_df$country == "Chicago"] <- "USA"
q2_michelin_df$country[q2_michelin_df$country == "New York City"] <- "USA"
q2_michelin_df$country[q2_michelin_df$country == "Washington DC"] <- "USA"
q2_michelin_df$country[q2_michelin_df$country == "Macau"] <- "China"
q2_michelin_df$country[q2_michelin_df$country == "Taipei"] <- "Taiwan"
q2_michelin_df$country[q2_michelin_df$country == "United Kingdom"] <- "UK"

# Count the number of ramen brands in each country
q2_ramen_number_of_brands <- 
  q2_ramen_df %>%
  group_by(country, Brand) %>% 
  summarise(num_of_brands = n()) %>%
  group_by(country) %>%
  summarise(num_of_brands = n())

# Count the number of michelin restaurants in each country
q2_michelin_number_of_restaurants <- 
  q2_michelin_df %>%
  group_by(country) %>%
  summarize(num_Michelin = n())

# Join the two tables by country
question2_raw_df <- 
  left_join(q2_ramen_number_of_brands, q2_michelin_number_of_restaurants, by = "country") %>% 
  drop_na(num_Michelin)
question2_df <-
  gather(question2_raw_df, key = type, value = number, -country)

# Plot the table using bar chart
question2_plot <- ggplot(data = question2_df) +
  geom_col(mapping = aes(x = country, y = number, fill = type), position = "dodge") +
  labs(title = "Number of Ramen Brands and Michelin Restaurants For Each Country") +
  xlab("Country") +
  ylab("Number") +
  scale_fill_discrete(name = "Type", labels = c("Michelin Restaurants", "Ramen Brands")) +
  theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))

#Q3

# simulate 1000 samples from wine_ratings
q3_wine_sample_1000 <- 
  na.omit(wine_ratings) %>% 
  sample_n(1000)

# plot for price against points for 1000 samples
q3_wine_price_points_plot <- 
  ggplot(data = q3_wine_sample_1000, aes(x = price, y = points)) +
  annotate("rect", xmin = 50, xmax = Inf, ymin = 90, ymax = Inf, fill = "aliceblue") +
  annotate("rect", xmin = -Inf, xmax = 50, ymin = -Inf, ymax = 90, fill = "aliceblue") +
  annotate("rect", xmin = -Inf, xmax = 50, ymin = 90, ymax = Inf, fill = "lightgreen") +
  annotate("rect", xmin = 50, xmax = Inf, ymin = -Inf, ymax = 90, fill = "lightcoral") +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 50) +
  geom_hline(yintercept = 90) +
  labs(title = "Wine price over points") +
  xlim(0, 100) +
  ylim(80, 100)

# Find average points for wine with price 0~50
q3_lower_price_average_points <- 
  filter(wine_ratings, price <= 50) %>% 
  summarise(average_points = mean(points)) %>% 
  pull() %>% 
  round(digits = 2)

# Find average points for wine with price 50~100
q3_higher_price_average_points <- 
  filter(wine_ratings, price > 50, price <= 100) %>% 
  summarise(average_points = mean(points)) %>% 
  pull() %>% 
  round(digits = 2)

# Omit na values in wine ratings
q3_new_wine_ratings <- 
  na.omit(wine_ratings) %>% 
  filter(price != "", points != "")

# Find the average price of wine
q3_average_price <- 
  mean(wine_ratings$price, na.rm = TRUE) %>% 
  round(digits = 2)

# Find the maximum price of wine
q3_max_price <- 
  max(wine_ratings$price, na.rm = TRUE)

# Find the correlation between points and price
q3_points_price_correlation <- 
  cor(q3_new_wine_ratings$points, q3_new_wine_ratings$price) %>% 
  round(digits = 2)

#Q4

# load and wrangle ramen data and save in a variable
q4_ramen <- ramen_ratings %>% 
  select(Country, Brand, Variety, Style) %>% 
  group_by(Country) %>% 
  summarize(ramen_count = length(Country))

# load and wrangle wine data and save in a variable
q4_wine <- wine_ratings %>% 
  select(country, winery, variety, region_1, region_2) %>% 
  group_by(country) %>% 
  summarize(wine_count = length(country))

# rename column name and some countries so that column and country names in both data sets match
q4_wine$country[q4_wine$country == "US"] <- "USA"
q4_wine$country[q4_wine$country == "England"] <- "UK"
names(q4_ramen)[1] <- "country"

# join data sets and filter out countries with counts for both ramen and wine
q4_variety_counts <- left_join(q4_wine, q4_ramen, by = "country")
q4_creative_countries <- q4_variety_counts %>% 
  filter(!is.na(ramen_count)) %>% 
  arrange(-wine_count)
q4_creative_countries_for_graph <- q4_creative_countries %>% 
  gather(key = category, value = count, c(wine_count, ramen_count))

# set the counts that are too large to the maximum of scale (3000) so that they can
# still be graphed
q4_creative_countries_for_graph$count[q4_creative_countries_for_graph$country == "USA" & q4_creative_countries_for_graph$category == "wine_count"] <- 3000
q4_creative_countries_for_graph$count[q4_creative_countries_for_graph$country == "Australia" & q4_creative_countries_for_graph$category == "wine_count"] <- 3000

# bar chart that shows ramen and wine counts for the most creative countries
q4_creative_countries_bar <- ggplot(data = q4_creative_countries_for_graph, 
                                 mapping = aes(reorder(country, count), y = count, color = category)) +
  geom_col(mapping = aes(fill = category), position = "dodge") +
  scale_y_continuous(limits = range(0, 3000)) +
  labs(title = "Countries that Make Both Wine and Ramen and Their Counts", 
       x = "Country", 
       y = "Variety Counts") +
  theme(axis.text.x = element_text(angle = 20))

# world map that shows specific countries that have great varieties of both ramen and wine
q4_countries <- map_data("world") %>% 
  mutate(country = region)
q4_map <- left_join(q4_countries, q4_creative_countries_for_graph, by = "country") %>% 
  mutate(creativity = count > 0)
q4_map$creativity[is.na(q4_map$creativity)] <- FALSE
q4_great_variety_countries <- ggplot(data = q4_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = creativity)) +
  scale_fill_brewer(palette = "Greens") +
  coord_quickmap() +
  theme_void() +
  labs(title = "Countries that Make Both Ramen and Wine", fill = "Creative")

