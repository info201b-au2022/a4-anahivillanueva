library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
# test_query1 <- function() {
#  return ("Hello world")
#}

# Return a vector of numbers
#test_query2 <- function(num=6) {
#  v <- seq(1:num)
#  return(v)
#}

# Load data
incarceration_trends <- read.csv(
  file = '~/INFO201/Assignments/a4-anahivillanueva/source/incarceration_trends.csv'
)
View(incarceration_trends)

state_data <- read.csv(
 '~/INFO201/Assignments/a4-Anahivillanueva/source/state_names_and_codes.csv' 
)
View(state_data)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

    # Find the maximum year of the data
highest_year <- incarceration_trends %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  select(year)
View(highest_year)   #2018

    # County with the highest (max) black people in jail
max_black_pop_15to64 <- incarceration_trends %>%   # Data frame to start with
  filter(year == 2018) %>%   #Filter down to only 2018
  filter(black_pop_15to64 == max(black_pop_15to64, na.rm = TRUE)) %>% # Filter down to the highest
  select(county_name) # Select the name of the county
View(max_black_pop_15to64)
# The county with the highest Black Population in jail Ages 15 to 64 in 2018 is New York County

    # County with the highest (max) latinx people in jail
min_latinx_pop_15to64 <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(latinx_pop_15to64 == min(latinx_pop_15to64, na.rm = TRUE)) %>%
  select(county_name)
View(min_latinx_pop_15to64)
# The county with the minimum Latinx Population in jail Ages 15 to 64 in 2018 is Wade Hampton Census Area


    # Average female people in jail
average_female <- summarize(
  incarceration_trends,
  mean_female_jail = mean(female_jail_pop, na.rm = TRUE)
)
View(average_female)

    # Average male people in jail
average_male <- summarize(
  incarceration_trends,
  mean_male_jail = mean(male_jail_pop, na.rm = TRUE)
)
View(average_male)
# The mean of the male jail population is 143.6055, while the mean of the female
# jail population is 18.21043

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

    # Data frame that will only show the the total_jail_pop and year columns
get_year_jail_pop <- function() {
jail_pop_US <- incarceration_trends %>%
  select(total_jail_pop, year)
return(jail_pop_US)
}

    # Bar chart with the year and total jail population
plot_jail_pop_for_us <- function()  {
  data_frame <- get_year_jail_pop()
  graph <- ggplot(data = data_frame) +
    geom_col(
      mapping = aes(x = year, y = total_jail_pop)) +
    labs(x = "Year", y = "Total Jail Population") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return(graph)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

    # Choose states that are going to be used
states <- c("WA", "OR", "CA", "FL")

    # Function that selects the columns that are going to be used "year" and "total_jail_pop"
get_jail_pop_by_states <- function(states) {
  jail_pop_state <- incarceration_trends %>%
   filter(state %in% states) %>%
    replace(is.na(.), 0) %>%
    select(total_jail_pop, year)
  return(jail_pop_state)
}

    # Function that creates the chart
plot_jail_pop_by_states <- function(states) {
  graph_two <- ggplot(incarceration_trends, aes(x = year)) +
    geom_line(aes(y = "WA")) +
    geom_line(aes(y = "OR")) +
    geom_line(aes(y = "CA")) +
    geom_line(aes(y = "FL")) +
    labs(x = "Year", y = "Total Jail Population by State")
    ggtitle("Incarceration Rates by State")
  return(graph_two)
}

    # View the chart
plot_jail_pop_by_states(states)


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

   # average of latinx people in jail over time (1970-2018)
latinx_incarceration_avg <- incarceration_trends %>%
  group_by(year) %>%
  summarize(Latinx = mean(latinx_jail_pop, na.rm = TRUE))

   # average of white people in jail over time (1970-2018)
white_incarceration_avg <- incarceration_trends %>%
  group_by(year) %>%
  summarize(White = mean(white_jail_pop, na.rm = TRUE))

   # Data being combined
combine_latinx_whites <- left_join(
  latinx_incarceration_avg, white_incarceration_avg, by = "year") %>%
  pivot_longer(-c(year), names_to = "Race", values_to = "avg_incarceration")

    # Line chart that will show a pattern on inequality between whites and latinx
inequality_between_latinx_whites <- ggplot(data = combine_latinx_whites) +
  geom_line(mapping = aes(x = year, y = avg_incarceration, color = Race),
  size = 1) +
  labs(x = "Year", y = "Average Incarceration Rate") +
  ggtitle("Average Incarceration Rate of Racial Populations Over Time (1970-2018)")
  
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

    # Data frame of the current Latinx average incarceration rate for each state
latinx_incarceration_us_max <- incarceration_trends %>%
  group_by(state) %>%
  filter(year == max(year), na.rm = TRUE) %>%
  summarize(latinx_jail_pop_rate = mean(latinx_jail_pop_rate, na.rm = TRUE))

    # Load states data
state_shape <- map_data("state")

    # Load the state names and abbreviations that are in the data
state_names <- data.frame(state.abb, state.name)

    # Data frames joined together
    # Joining latinx_incarceration_us_max with state_names
latinx_state_data <- left_join(
  latinx_incarceration_us_max, state_names, by = c("state" = "state.abb"))

    # Create a new column "region" that will include a version of the state names
    # all in lowercase
lowercase_state <- latinx_state_data %>%
  mutate(region = tolower(state.name))

    # Data frames joined together
    # Joining state_shape with lowercase_state 
state_shape <- left_join(state_shape, lowercase_state)

    # map code in blank theme 
map_theme <- theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

    # Create Latinx Map
Latinx_map <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = latinx_jail_pop_rate),
               color = "white", size = 1) +
  coord_map() +
  scale_fill_continuous(low = "light blue", high = "dark blue") +
  labs(fill = "Incarceration Rate of Latinx Population") + 
  ggtitle("2018 U.S. Incarceration Rate of Latinx Population") +
  map_theme
