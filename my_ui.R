library("shiny")
library("shinythemes")

#################
### home page ###
#################
hp_intro_panel <- tabPanel(
  title = "Home",
  
  titlePanel("Food Analysis"),
  tags$h4("A2: Yunrui Shao, Sylvia Dou, Amanda Cheng, Steven Lu"),
  img(src = "food-policy.jpg", width = "500px", alt = "Food All Over the World"),
  
  tags$h2("About"),
  tags$h5("Food is one of the most important elements in human life. Our group is interested in food analysis for some specific food and restaurants. Ramen, Wine and Michelin restaurants are the 3 kinds we found the most attractive, and we collected 3 related data sets about their varieties and ratings among different countries. Through analyzing these 3 different types of data sets, we hope to find out the relationship between consumer needs and developments of food cultures around the world."), 
  
  tags$h2("Data Set Description"),
  tags$h5("We gathered 4 data sets to analyze the related questions on food. Below is the summuary for each:"),
  
  img(src = "ramen.jpg", width = "120px", align = "right"),
  tags$h4("Ramen Ratings"),
  p("\"Ramen Ratings\" data set collected over 2500 ramen ratings and was found on Kaggle contributed by the user Aleksey Bilogur. It was originally from \"The Ramen Rater\", a product review website for the hardcore ramen enthusiastan. The data set we have here is an export of \"The Big List\" (of reviews) converted into a CSV file."),
  p(a(href = "https://www.kaggle.com/residentmario/ramen-ratings/data", "Link to the Ramen Ratings data set")),
  
  img(src = "wine.jpg", width = "120px", align = "right"),
  tags$h4("Wine Reviews"),
  p("\"Wine Reviews\" data set contains 150k wine reviews with variety, location, winery, price and description. It was created by the user zackthoutt on Kaggle. The data was originally scraped from WineEnthusiast during the week of June 15th, 2017. The data set only contains reviews for wines with points >= 80."),
  p(a(href = "https://www.kaggle.com/zynicide/wine-reviews/data#winemag-data_first150k.csv", "Link to the Wine Reviews data set")),
  
  img(src = "michelin.jpg", width = "120px", align = "right"),
  tags$h4("Michelin Restaurants"),
  p("\"Michelin Restaurants\" data sets contains information for the Michelin restaurants all over the world, including their stars, locations, prices, etc. They automatically updates the information originated from the Michelin\'s Guide. Since there are three files separating different stars on Kaggle, where we found the data sets, the single data set we have here has already been combined together with an extra column indicating their stars. Note that the data set does not include restaurant information in the following countries: Belgium, France, Germany, Italy, Japan, Luxembourg, Netherlands, Portugal, China, Spain, and Switzerland."),
  p(a(href = "https://www.kaggle.com/jackywang529/michelin-restaurants", "Link to the Michelin Restaurants data set")),
  
  img(src = "us$.jpg", width = "120px", align = "right"),
  tags$h4("Adjusted Net National Income"),
  p("\"Adjusted Net National Income (current US$)\" is a data set collected from The World Bank. It represents the adjusted net national income as GNI minus cosumption of fixed capital and natural resources depletion. World Bank staff extimates based on sources and methods described in \"The Changing Wealth of Nations 2018: Building a Sustainable Future\" (Lange et al 2018)."),
  p(a(href = "https://data.worldbank.org/indicator/NY.ADJ.NNTY.CD?end=2017&start=1970&view=chart", "Link to the Adjusted Net National Income data set")),
  
  tags$h3("Data Report"),
  tags$h5("For more details, please click:"),
  p(a(href = "https://info201a-wi20.github.io/project-report-douw2/?nsukey=iHo24xPR2y66B12mXzs4Nio%2B5L5lQdcSXesn7nufNXJ73OcSXNLuVkAONXaymZ6VzmCp63GQlJKCAr5QZAHVyxdMnadU9Bqi0v4R2mn16Ht2FZCOdDLioO8S4Hej7xXR8XqK5CDGKDlOhgB9YWmo7oANvVVve3XceuUwz3QyBtTCv0UTQ5lcbmJUdhiYzI0vebfdOWPfsvXOLgCzXZ7LkA%3D%3D", "Link to Our Data Report"))
)

#########################
### Wine Price Points ###
#########################
sidebar_content_wine <- sidebarPanel(
  sliderInput(inputId = "price", label = "Price", value = c(0, 2300),
              min = 0, max = 2300),
  sliderInput(inputId = "points", label = "Points", value = c(80, 100),
                  min = 80, max = 100),
  textOutput("price_average"),
  textOutput("points_average"),
      
  h5("The green area represents the wines that has low price with high quality within the chosen range. The red area represents the opposite. Therefore, it is easy to predict what kind of wine you would most likely to get when choosing a certain range of price and points")
)
    
main_content_wine <- mainPanel(
  plotOutput("wine_plot"),
  p("Since there are some outliers in this data set, the best way to analyze this data set is to set the price
    of the wine below 500 dollars. Overall, we can see that the general trend between price and points is", 
    strong("positive"), "indicating that as price increases, the average points of the wine also increases. 
    This proves our theory that", strong("higher rating wines are more expensive."))
)


wine_panel <- tabPanel(
  title = "Prices vs Points Analysis",
  titlePanel("Are higher rating wines more expensive?"),
  sidebarLayout(
    sidebar_content_wine,
    main_content_wine
  )
)

#############################
### Comparison by Country ###
#############################
sidebar_content <- sidebarPanel(
  checkboxGroupInput(
    inputId = "food_data_set",
    label = "What data(s) of the countries do you want to know about?",
    choices = list("ramen", "wine", "Michelin Restaurants"),
    selected = "ramen"
  ),
  selectInput(
    inputId = "attribute",
    label = "Which attribute do you want to know about more?",
    choices = list("Ratings", "Counts")
  )
)

main_content <- mainPanel(
  textOutput(outputId = "map_message"),
  plotOutput(outputId = "map"),
  textOutput(outputId = "question"),
  plotOutput(outputId = "bar"),
  p("Based on the bar chart above and the data we have, we can see that there is not really a strong correlation
    between the foods or the foods and Michelin restaurants.  That is, countries with higher average ratings of 
    one food or Michelin restaurants do not necessarily have higher average ratings for another food/restaurant,
    and the same applies to the numbers of those data in each country.  Although there might be another problem
    that these data could be", strong("biased"), "or", strong("incomplete"), "as they are ratings from individual
    reviewers and there might exist many more possibilities that did not get reviewed, we can still conclude that",
    strong("countries with more better ratings/counts of one kind do not necessarily have more of the other"), "as 
    we believe each country has its specialties. However, we can also see that there are still some countries, such
    as the USA, that have great varieties or ratings of one while having various kinds of the other. We can say that
    they may be", strong("more creative"), "as they know how to make very different types of food, or they might just
    have", strong("greater resources"), "to create greater varieties of food. Anyhow, these countries not only provide
    us the chance to enjoy great food when going there, they also give us the direction of which countries to go for 
    first when we want to import those foods at the same time from a specific country.")
)

country_panel <- tabPanel(
  title = "Compare by Countries",
  titlePanel("Do countries with more ___ also have more ___?"),
  sidebarLayout(
    sidebar_content,
    main_content
  )
)


##########################
### last question page ###
##########################
sidebar_content_last <- sidebarPanel(
  sliderInput(inputId = "range", label = "Income Range", 
              min = 1500000000, max = 17000000000000, value = c(1500000000, 17000000000000)),
  checkboxGroupInput(inputId = "type", label = "Food Type", 
                     choices = c("Ramen" = "Ramen Brands", "Michelin" = "Michelin Restaurants"),
                     selected = c("Ramen" = "Ramen Brands", "Michelin" = "Michelin Restaurants"))
)

main_content_last <- mainPanel(
  plotOutput(outputId = "plot1"),
  plotOutput(outputId = "plot2"),
  p("Based on our analysis, there is a relationship between countries' food development, 
    specifically ramen and Michelin restaurants, and countries' economic development. 
    For example, China has year income 9.105743e+12 dollars, and Poland has year income 4.424217e+11 dollars.
    And China has 81 ramen brands and 106 Michelin restaurants, but Poland only has 2 ramen brands and 2 Michelin
    restaurants. In conclusion, with", strong("higher consumers' income"), "countries have", 
    strong("more ramen brands"), "and", strong("more Michelin restaurants."))
)

last_panel <- tabPanel(
  title = "Effect of Income",
  titlePanel("Countries' Ramen Brands and Michelin Restaurants Development With Countries' Income"),
  sidebarLayout(
    sidebar_content_last,
    main_content_last
  )
)

####################
### summary page ###
####################
last_summary_panel <- tabPanel(
  title = "Summary",
  titlePanel("Summary Analyses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("summary_analyses", "Choose a data set:",
                  choices = c("Ramen", "Wine", "Michelin")),
    ),
    mainPanel(
      htmlOutput("summaryHTML")
    )
  )
)

#################
### define ui ###
#################
ui <- navbarPage(
  title = img(src = "title_bar.jpg"),
  theme = "paper.css",
  collapsible = TRUE,
  inverse = TRUE,
  windowTitle = "Food Analysis",
  # position = "fixed-top",
  
  
  hp_intro_panel,
  wine_panel,
  country_panel,
  last_panel,
  last_summary_panel
)