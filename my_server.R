library("shiny")
library("wbstats")
library("dplyr")
library("tidyr")
library("ggplot2")

# Source plots from report section 3 and subsection 2.2
source("analysis_plots.R")
source("subsection_2.2_final.R")

# load and wrangle income dataset
countries_income <- wb(country = "countries_only", indicator = c("NY.ADJ.NNTY.CD"), mrv = 1)
countries_income <- select(countries_income, country, value)

# wrangle ramen dataset
last_ramen_ratings <- ramen_ratings %>%
  mutate(country = Country) %>%
  select(Brand, country, Stars)
last_ramen_ratings$country[last_ramen_ratings$country == "USA"] <- "United States"
last_ramen_ratings$country[last_ramen_ratings$country == "Taiwan"] <- "China"
last_ramen_ratings$country[last_ramen_ratings$country == "Hong Kong"] <- "China"
last_ramen_ratings$Stars <- as.numeric(as.character(last_ramen_ratings$Stars))
last_ramen_ratings <-  last_ramen_ratings%>%
  drop_na(Stars) %>%
  group_by(country, Brand) %>%
  summarize(average_stars = mean(Stars))

# wrangle Michelin dataset
last_one_star <- one_star %>% 
  mutate(star = 1)
last_two_star <- two_star %>% 
  mutate(star = 2)
last_three_star <- three_star %>% 
  mutate(star = 3)
last_michelin_ratings <- 
  rbind(last_one_star, last_two_star, last_three_star) %>% 
  mutate(country = region) %>% 
  select(name, country, star)
last_michelin_ratings$country[last_michelin_ratings$country == "California"] <- "United States"
last_michelin_ratings$country[last_michelin_ratings$country == "Chicago"] <- "United States"
last_michelin_ratings$country[last_michelin_ratings$country == "New York City"] <- "United States"
last_michelin_ratings$country[last_michelin_ratings$country == "Washington DC"] <- "United States"
last_michelin_ratings$country[last_michelin_ratings$country == "Hong Kong"] <- "China"
last_michelin_ratings$country[last_michelin_ratings$country == "Macau"] <- "China"
last_michelin_ratings$country[last_michelin_ratings$country == "Taipei"] <- "China"

# make the result data frame
last_ramen <- left_join(last_ramen_ratings, countries_income, by = "country")
last_ramen <- drop_na(last_ramen)
last_ramen <- last_ramen %>%
  group_by(country) %>%
  summarize(brands = n(), average_stars = mean(average_stars), value = mean(value))
last_michelin <- last_michelin_ratings %>%
  group_by(country) %>%
  summarize(restaurants = n(), average_star = mean(star))
last_df <- left_join(last_ramen, last_michelin, by = "country")
last_df <- last_df %>%
  drop_na() %>%
  gather(key = type, value = number, -country, -value, -average_stars, -average_star)
last_ramen <- last_df %>%
  filter(type == "brands") %>%
  select(country, value, number, average_stars)
last_michelin <- last_df %>%
  filter(type == "restaurants") %>%
  select(country, value, number, average_star)

# data frame that shows countries with ramen and some other relevant information
ramen_ratings$Country[ramen_ratings$Country == "United States"] <- "USA"
ramen_countries <- ramen_ratings %>% 
  group_by(Country) %>% 
  summarize(ramen_count = length(Country), 
            avg_stars = mean(as.numeric(Stars), na.rm = TRUE)) %>% 
  mutate(average_rating_ramen = 80 + 4 * avg_stars)
# change the colname for country so that all data sets match
names(ramen_countries)[1] <- "country"


# data frame that shows countries with wine and some other relevant information
wine_countries <- wine_ratings %>% 
  group_by(country) %>% 
  summarize(wine_count = length(country),
            average_rating_wine = mean(points)) %>% 
  filter(country != "")
wine_countries$country[wine_countries$country == "US"] <- "USA"
wine_countries$country[wine_countries$country == "England"] <- "UK"

# data frame that shows countries with Michelin restaurants and some other relevant information
michelin_countries <- michelin_ratings %>%
  select(country, star) %>% 
  filter(country != "N/A")
# change scale of star rating
michelin_countries$star[michelin_countries$star == "one"] <- 80
michelin_countries$star[michelin_countries$star == "two"] <- 90
michelin_countries$star[michelin_countries$star == "three"] <- 100
michelin_countries <- mutate(michelin_countries, star = as.numeric(star))
michelin_countries <- michelin_countries %>% 
  group_by(country) %>% 
  summarize(michelin_count = length(country), average_rating_michelin = mean(star))
michelin_countries$country[michelin_countries$country == "Macau"] <- "China"
michelin_countries$country[michelin_countries$country == "Taipei"] <- "Taiwan"
michelin_countries$country[michelin_countries$country == "United Kingdom"] <- "UK"
michelin_countries$country[michelin_countries$country == "US"] <- "USA"

# define server
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(data = last_df) +
      geom_point(mapping = aes(
        x = value, y = country
      ), color = "red") +
      labs(x = "Income Range", y = "Country") +
      scale_x_continuous(limits = input$range)
  })
  
  output$plot2 <- renderPlot({
    ramen <- "Ramen Brands" %in% input$type
    michelin <- "Michelin Restaurants" %in% input$type
    if(ramen & michelin) {
      ggplot(data = last_df) +
        geom_col(mapping = aes(
          x = country, y = number, fill = type
        ), position = "dodge") +
        labs(x = "Country", y = "Number of Ramen Brands and Michelin Restaurants") +
        scale_fill_discrete(name = "Type", labels = c("Michelin Restaurants", "Ramen Brands")) +
        theme(axis.text.x = element_text(size = 9))
    } else if(ramen) {
      ggplot(data = last_ramen) +
        geom_col(mapping = aes(
          x = country, y = number
        ), fill = "#F0E442") +
        labs(x = "Country", y = "Number of Ramen Brands") +
        theme(axis.text.x = element_text(size = 9))
    } else if(michelin) {
      ggplot(data = last_michelin) +
        geom_col(mapping = aes(
          x = country, y = number
        ), fill = "#0072B2") +
        labs(x = "Country", y = "Number of Michelin Restaurants") +
        theme(axis.text.x = element_text(size = 9))
    } else {
      output$plot2 <- renderPlot({
        ggplot(data = last_df)
      })
    }
  })
  
  # Wine price over points plot
  output$wine_plot <- renderPlot({
    ggplot(data = wine_ratings, aes(x = price, y = points)) +
      annotate("rect", xmin = (input$price[1] + input$price[2]) / 2, xmax = Inf, ymin = (input$points[1] + input$points[2]) / 2, ymax = Inf, fill = "aliceblue") +
      annotate("rect", xmin = -Inf, xmax = (input$price[1] + input$price[2]) / 2, ymin = -Inf, ymax = (input$points[1] + input$points[2]) / 2, fill = "aliceblue") +
      annotate("rect", xmin = -Inf, xmax = (input$price[1] + input$price[2]) / 2, ymin = (input$points[1] + input$points[2]) / 2, ymax = Inf, fill = "lightgreen") +
      annotate("rect", xmin = (input$price[1] + input$price[2]) / 2, xmax = Inf, ymin = -Inf, ymax = (input$points[1] + input$points[2]) / 2, fill = "lightcoral") +
      geom_point() +
      geom_vline(xintercept = (input$price[1] + input$price[2]) / 2) +
      geom_hline(yintercept = (input$points[1] + input$points[2]) / 2) +
      labs(title = "Wine price over points") +
      scale_x_continuous(limits = input$price) +
      scale_y_continuous(limits = input$points)
  })
  
  # average points for wine
  output$points_average <- renderText({
    paste("The average points for this range is", 
          filter(wine_ratings, price >= input$price[1], price <= input$price[2], points >= input$points[1], points <= input$points[2]) %>% 
            summarise(average_points = mean(points)) %>% 
            pull() %>% 
            round(2), 
          "points")
    
  })
  
  # average price for wine
  output$price_average <- renderText({
    paste("The average price for this range is", 
          filter(wine_ratings, price >= input$price[1], price <= input$price[2], points >= input$points[1], points <= input$points[2]) %>% 
            summarise(average_price = mean(price)) %>% 
            pull() %>% 
            round(2), 
          "dollars")
  })
  
  # explanation for map
  output$map_message <- renderText({
    ramen <- "ramen" %in% input$food_data_set
    wine <- "wine" %in% input$food_data_set
    michelin <- "Michelin Restaurants" %in% input$food_data_set
    question <- "With the chosen data set(s), your question will then be: "
    explanation <- "The map below shows the countries that have data about "
    # select all data sets
    if (ramen & wine & michelin) {
      explanation <- paste0(explanation, "all ramen, wine, and Michelin restaurants")
    } 
    # select ramen and wine
    else if (ramen & wine) {
      explanation <- paste0(explanation, "both ramen and wine.")
    } 
    # select ramen and michelin restaurants
    else if (ramen & michelin) {
      explanation <- paste0(explanation, "both ramen and Michelin restaurants.")
    } 
    # select wine and michelin restaurants
    else if (wine & michelin) {
      explanation <- paste0(explanation, "both wine and Michelin restaurants.")
    } 
    # select ramen only
    else if (ramen) {
      explanation <- paste0(explanation, "ramen.")
    } 
    # select wine only
    else if (wine) {
      explanation <- paste0(explanation, "wine.")
    } 
    # select only michelin restaurants
    else {
      explanation <- paste0(explanation, "Michelin restaurants.")
    }
    return(explanation)
  })
  
  # shows the map of countries that have data for selected data set(s)
  output$map <- renderPlot({
    ramen <- "ramen" %in% input$food_data_set
    wine <- "wine" %in% input$food_data_set
    michelin <- "Michelin Restaurants" %in% input$food_data_set
    # select all data sets
    if (ramen & wine & michelin) {
      countries_with_all <- left_join(ramen_countries, wine_countries, by = "country") %>%
        left_join(michelin_countries, by = "country") %>% 
        drop_na()
      data_to_display <- left_join(q4_countries, countries_with_all, by = "country") %>% 
        mutate(has_data = ramen_count > 0)
      data_to_display$has_data[is.na(data_to_display$has_data)] <- FALSE
      ggplot(data = data_to_display) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries with All Ramen, Wine, and Michelin Restaurants", fill = "Has Data for All")
    } 
    # select ramen and wine
    else if (ramen & wine) {
      ggplot(data = q4_map) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = creativity)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries that Make Both Ramen and Wine", fill = "Makes Both Ramen and Wine")
    } 
    # select ramen and michelin restaurants
    else if (ramen & michelin) {
      data_to_display <- left_join(q4_countries, question2_df, by = "country") %>% 
        mutate(has_data = number > 0)
      data_to_display$has_data[is.na(data_to_display$has_data)] <- FALSE
      ggplot(data = data_to_display) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries with Both Ramen and Michelin Restaurants", fill = "Has Data for Both Ramen and Michelin Restaurants")
    } 
    # select wine and michelin restaurants
    else if (wine & michelin) {
      rwm_df$country[rwm_df$country == "US"] <- "USA"
      data_to_display <- left_join(q4_countries, rwm_df, by = "country") %>% 
        mutate(has_data = number > 0)
      data_to_display$has_data[is.na(data_to_display$has_data)] <- FALSE
      ggplot(data = data_to_display) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries with Both Wine and Michelin Restaurants", fill = "Has Data for Both Wine and Michelin Restaurants")
    } 
    # select ramen only
    else if (ramen) {
      data_to_display <- left_join(q4_countries, ramen_countries, by = "country") %>% 
        mutate(has_data = ramen_count > 0)
      data_to_display$has_data[is.na(data_to_display$has_data)] <- FALSE
      ggplot(data = data_to_display) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries that Make Top Ramens", fill = "Makes Ramen")
    } 
    # select wine only
    else if (wine) {
      data_to_display <- left_join(q4_countries, wine_countries, by = "country") %>% 
        mutate(has_data = wine_count > 0)
      data_to_display$has_data[is.na(data_to_display$has_data)] <- FALSE
      ggplot(data = data_to_display) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries that Make Top Wines", fill = "Makes Wine")
    } 
    # select only michelin restaurants
    else if (michelin) {
      data_to_display <- left_join(q4_countries, michelin_countries, by = "country") %>% 
        mutate(has_data = michelin_count > 0)
      data_to_display$has_data[is.na(data_to_display$has_data)] <- FALSE
      ggplot(data = data_to_display) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = has_data)) +
        scale_fill_brewer(palette = "Greens") +
        coord_quickmap() +
        theme_void() +
        labs(title = "Countries with Michelin Restaurants", fill = "Has Michelin Restaurants in data set")
    }
  })
  
  # question for analysis
  output$question <- renderText({
    ramen <- "ramen" %in% input$food_data_set
    wine <- "wine" %in% input$food_data_set
    michelin <- "Michelin Restaurants" %in% input$food_data_set
    question <- "With the chosen data sets, your question will then be: "
    if (input$attribute == "Counts") {
      # select all data sets
      if (ramen & wine & michelin) {
        question <- paste0(question, "Do countries with more ramen also have more wines and Michelin restaurants?")
      } 
      # select ramen and wine
      else if (ramen & wine) {
        question <- paste0(question, "Do countries with more ramen also have more wines?")
      } 
      # select ramen and michelin restaurants
      else if (ramen & michelin) {
        question <- paste0(question, "Do countries with more ramen also have more Michelin restaurants?")
      } 
      # select wine and michelin restaurants
      else if (wine & michelin) {
        question <- paste0(question, "Do countries with more wines also have more Michelin restaurants?")
      } 
      # select ramen only
      else if (ramen) {
        question <- "With only one data set chosen, we will just show you a general trend of the number of ramen brands for each country."
      } 
      # select wine only
      else if (wine) {
        question <- "With only one data set chosen, we will just show you a general trend of the number of wines for each country."
      } 
      # select only michelin restaurants
      else if (michelin) {
        question <- "With only one data set chosen, we will just show you a general trend of the number of Michelin restaurants for each country."
      }
    } 
    # attribute == Ratings
    else {
      # select all data sets
      if (ramen & wine & michelin) {
        question <- paste0(question, "Do countries with higher average ratings of ramen also have better average ratings for wines and more higher stars for Michelin restaurants?")
      } 
      # select ramen and wine
      else if (ramen & wine) {
        question <- paste0(question, "Do countries with higher average ratings of ramen also have better average ratings for wines?")
      } 
      # select ramen and michelin restaurants
      else if (ramen & michelin) {
        question <- paste0(question, "Do countries with more higher stars for Michelin restaurants also have higher average ratings of ramen?")
      } 
      # select wine and michelin restaurants
      else if (wine & michelin) {
        question <- paste0(question, "Do countries with more higher stars for Michelin restaurants also have better average ratings for wines?")
      } 
      # select ramen only
      else if (ramen) {
        question <- "With only one data set chosen, we will just show you a general trend of the average ratings of ramen brands for each country."
      } 
      # select wine only
      else if (wine) {
        question <- "With only one data set chosen, we will just show you a general trend of the average ratings of wines for each country."
      } 
      # select only michelin restaurants
      else if (michelin) {
        question <- "With only one data set chosen, we will just show you a general trend of the star ratings of Michelin restaurants for each country."
      }
    }
    return(question)
  })
  
  # shows the bar chart for specific analysis
  output$bar <- renderPlot({
    ramen <- "ramen" %in% input$food_data_set
    wine <- "wine" %in% input$food_data_set
    michelin <- "Michelin Restaurants" %in% input$food_data_set
    if (input$attribute == "Counts") {
      # select all data sets
      if (ramen & wine & michelin) {
        countries_with_all <- left_join(ramen_countries, wine_countries, by = "country") %>%
          left_join(michelin_countries, by = "country") %>% 
          drop_na()
        countries_with_all_for_graph <- countries_with_all %>% 
          gather(key = category, value = count, c(wine_count, ramen_count, michelin_count))
        ggplot(data = countries_with_all_for_graph) +
          geom_col(mapping = aes(x = reorder(country, count), y = count, fill = category), position = "dodge") +
          labs(title = "Number of Ramen Brands, Wines, and Michelin Restaurants For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Ramen", "Wines", "Michelin Restaurants")) +
          theme(axis.text.x = element_text(size = 9), legend.position = c(0.2,0.8)) +
          coord_cartesian(ylim = c(0, 1000))
      }
      # select ramen and wine
      else if(ramen & wine) {
        ggplot(data = q4_creative_countries_for_graph) +
          geom_col(mapping = aes(x = reorder(country, count), y = count, fill = category), position = "dodge") +
          labs(title = "Number of Ramen Brands and Wines For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Ramen", "Wines")) +
          theme(axis.text.x = element_text(size = 9), legend.position = c(0.2,0.8)) +
          coord_cartesian(ylim = c(0, 1000))
      }
      # select ramen and michelin
      else if(ramen & michelin) {
        ggplot(data = question2_df) +
          geom_col(mapping = aes(x = reorder(country, number), y = number, fill = type), position = "dodge") +
          labs(title = "Number of Ramen Brands and Michelin Restaurants For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Michelin Restaurants", "Ramen Brands")) +
          theme(axis.text.x = element_text(size = 9), legend.position = c(0.2,0.8))
      }
      # select michelin and wine
      else if(michelin & wine) {
        data_to_display <- left_join(wine_countries, michelin_countries, by = "country") %>% 
          drop_na() %>% 
          gather(key = category, value = count, c(wine_count, michelin_count))
        ggplot(data = data_to_display) +
          geom_col(mapping = aes(x = reorder(country, count), y = count, fill = category), position = "dodge") +
          labs(title = "Number of Wines and Michelin Restaurants For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Wines", "Michelin Restaurants")) +
          theme(axis.text.x = element_text(size = 9), legend.position = c(0.2,0.8)) +
          coord_cartesian(ylim = c(0, 500))
      }
      # select wine only
      else if(wine) {
        ggplot(data = wine_distribution_for_plot) +
          geom_col(mapping = aes(x = reorder(country, count), y = count, fill = "FF9999")) +
          labs(title = "Number of Wines For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Wines")) +
          theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))
      }
      # select ramen only
      else if(ramen) {
        ggplot(data = ramen_df_2.2_2) +
          geom_col(mapping = aes(x = reorder(Country, num_brand), y = num_brand, fill = "FF9999")) +
          labs(title = "Number of Ramen Brands For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Ramen")) +
          theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))
      }
      # select michelin only
      else if (michelin) {
        ggplot(data = michelin_countries) +
          geom_col(mapping = aes(x = reorder(country, michelin_count), y = michelin_count, fill = "FF9999")) +
          labs(title = "Number of Michelin Restaurants For Each Country") +
          xlab("Country") +
          ylab("Number") +
          scale_fill_discrete(name = "Type", labels = c("Michelin Restaurants")) +
          theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))
      }
    } 
    # attribute == Ratings
    else {
      # select all data sets
      if (ramen & wine & michelin) {
        all_ratings <- left_join(ramen_countries, wine_countries, by = "country") %>% 
          left_join(michelin_countries, by = "country") %>% 
          drop_na() %>% 
          select(country, average_rating_ramen, average_rating_wine, average_rating_michelin) %>% 
          gather(key = type, value = number, -country)
        ggplot(data = all_ratings) +
          geom_col(mapping = aes(x = reorder(country, number), y = number, fill = type), position = "dodge") +
          labs(title = "Ratings of Ramen Brands, Wines, and Michelin Restaurants For Each Country") +
          xlab("Country") +
          ylab("Ratings") +
          scale_fill_discrete(name = "Type", labels = c("Ramen", "Wines", "Michelin Restaurants")) +
          theme(axis.text.x = element_text(size = 9)) +
          coord_cartesian(ylim = c(75, 100))
      }
      # select ramen and wine
      else if(ramen & wine) {
        ramen_wine_ratings <- left_join(ramen_countries, wine_countries, by = "country") %>% 
          drop_na() %>% 
          select(country, average_rating_ramen, average_rating_wine) %>% 
          gather(key = type, value = number, -country)
        ggplot(data = ramen_wine_ratings) +
          geom_col(mapping = aes(x = reorder(country, number), y = number, fill = type), position = "dodge") +
          scale_fill_brewer(palette = "Paired") +
          coord_cartesian(ylim = c(75, 100)) +
          labs(title = "Ratings of Ramen and Wines For Each Country",
               x = "Country",
               y = "Ratings",
               fill = "Type")
      }
      # select ramen and michelin
      else if(ramen & michelin) {
        ramen_michelin_ratings <- left_join(ramen_countries, michelin_countries, by = "country") %>% 
          drop_na() %>% 
          select(country, average_rating_ramen, average_rating_michelin) %>% 
          gather(key = type, value = number, -country)
        ggplot(data = ramen_michelin_ratings) +
          geom_col(mapping = aes(x = reorder(country, number), y = number, fill = type), position = "dodge") +
          scale_fill_brewer(palette = "Paired") +
          coord_cartesian(ylim = c(75, 100)) +
          labs(title = "Ratings of Ramen and Michelin Restaurants For Each Country",
               x = "Country",
               y = "Ratings",
               fill = "Type")
      }
      # select michelin and wine
      else if(michelin & wine) {
        rwm_df$country[rwm_df$country == "US"] <- "USA"
        ggplot(data = rwm_df) +
          geom_col(mapping = aes(x = reorder(country, number), y = number, fill = type), position = "dodge") +
          scale_fill_brewer(palette = "Paired") +
          coord_cartesian(ylim = c(75, 90)) +
          labs(title = "Ratings of Wines and Michelin Restaurants For Each Country",
               x = "Country",
               y = "Ratings",
               fill = "Type")
      }
      # select wine only
      else if(wine) {
        ggplot(data = wine_countries) +
          geom_col(mapping = aes(x = reorder(country, average_rating_wine), y = average_rating_wine), fill = "#56B4E9") +
          coord_cartesian(ylim = c(75, 95)) +
          labs(title = "Ratings of Wines For Each Country",
               x = "Country",
               y = "Ratings") +
          theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))
      }
      # select ramen only
      else if(ramen) {
        ggplot(data = ramen_countries) +
          geom_col(mapping = aes(x = reorder(country, average_rating_ramen), y = average_rating_ramen), fill = "#56B4E9") +
          coord_cartesian(ylim = c(75, 100)) +
          labs(title = "Ratings of Ramen For Each Country",
               x = "Country",
               y = "Ratings") +
          theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))
      }
      # select michelin only
      else if (michelin) {
        ggplot(data = michelin_countries) +
          geom_col(mapping = aes(x = reorder(country, average_rating_michelin), y = average_rating_michelin), fill = "#56B4E9") +
          coord_cartesian(ylim = c(75, 90)) +
          labs(title = "Ratings of Michelin Restaurants For Each Country",
               x = "Country",
               y = "Ratings") +
          theme(axis.text.x = element_text(size = 9, angle = 90), legend.position = c(0.2,0.8))
      }
    }
  })
  
  # Part for the Last Summary Page
  output$summaryHTML <- renderUI({
    ra <- "Ramen" %in% input$summary_analyses
    wa <- "Wine" %in% input$summary_analyses
    ma <- "Michelin" %in% input$summary_analyses
    if (ra) {
      return(list(tags$h3("Summary for Ramen data set:"),
                  "The Stars column denotes the rating for this data set. The range for the rating is from minimum 0 to maximum 5 with the average number of 3.655. Other columns denotes the data set’s great diversity. We ignore the NA value.",
                  img(src = "ramen table.jpg"),
                  img(src = "ramen plot1.jpg"),
                  "For the first plot about ramen rating, the data for Nigeria(1.5) is an outlier that is much lower than the average value. In addition, Canada(2.2439024) and Netherlands(2.4833333) are also lower than average but just not that much. We won’t throw off these outliers since these can reflect some important facts about the relationship between countries’ overall development and culture and ramen culture development.",
                  img(src = "ramen plot2.jpg"),
                  "For the second plot about number of ramen brands, the data for USA(49), Thailand(25), Taiwan(47), South Korea(35), Malaysia(32), Japan(58), China(33) are outliers since they are much higher than the average number of ramen brands. Again, we won’t throw off these outliers since these can reflect some important facts about the relationship between countries’ overall development and culture and ramen culture development."))
    }
    if (wa) {
      return(list(tags$h3("Summary for Wine data set:"),
                  "The points column denotes the rating for this data set. The range for the rating is from minimum 80 to maximum 100 with the average number of 87.79. The price column denotes the direct price of the wines, with minimum 4 and maximum 2300. Other columns denotes the data set’s great diversity. We ignore the NA value.",
                  img(src = "wine table.jpg"),
                  img(src = "wine plot.jpg"),
                  "For the wine variety distribution over countries, there are quite some significant outliers with particularly low values: they don’t have that many of a variety in their wines. Since we are looking for countries with top varieties, we limit the range of distribution here to only show variety counts over 100. In this case, countries with variety counts under 100: Slovenia, Uruguay, Croatia, Bulgaria, Moldova, Mexico, Turkey, Georgia, Lebanon, Cyprus, Brazil, Macedonia, Serbia, Morocco, England, Luxembourg, India, Lithuania, Czech Republic, Ukraine, Bosnia and Herzegovina, South Korea, Switzerland, China, Egypt, Slovakia, Albania, Japan, Montenegro, Tunisia, US-France, will be our outliers."))
    }
    if (ma) {
      return(list(tags$h3("Summary for Michelin data set:"),
                  "The star column denotes the star level of the Michelin restarurant. The price denotes the relative price of cosuming in different restaruants. The range for the relative price is from 1 (31 restaurants) to 5 (73 restaurants), with 1 being the cheapest and 5 being the most expensive. Other columns denotes the data set’s great diversity. We ignore the NA value.",
                  img(src = "michelin table.jpg"),
                  img(src = "michelin plot1.jpg"),
                  "The first plot shows the numbers of different stars of Michelin Restaurants among countries. The outliers could be numbers of star_one restaurants in UK NA and US NA, but we want to keep all the datas to maintain the consistency.",
                  img(src = "michelin plot2.jpg"),
                  "The second plot shows the average consumption among different stars of Michelin restaurants. This plot has no outliers."))
    }
  })
}

