library(dplyr)
library(styler)
library(ggplot2)
library(plotly)

juvenile_df <- read.csv("data/juvenile_crime.csv", stringsAsFactors = FALSE)

# data set up for 1996/2016
crime_data_1996 <- juvenile_df %>%
  filter(year == 1996) %>% 
  group_by(offense_name) %>%
  summarise(total_individuals_1996 = round(sum(total_male + total_female), 2)) %>%
  arrange(-total_individuals_1996) %>%
  top_n(11, total_individuals_1996) %>% 
  arrange(offense_name)

crime_data_2016 <- juvenile_df %>%
  filter(year == 2016) %>% 
  group_by(offense_name) %>%
  summarise(total_individuals_2016 = round(sum(total_male + total_female), 2)) %>%
  arrange(-total_individuals_2016) %>%
  top_n(11, total_individuals_2016) %>% 
  arrange(offense_name)

# ignore the `all other offenses`
crime_data_1996 <- crime_data_1996[
  -match("All Other Offenses", crime_data_1996$offense_name), ]
crime_data_2016 <- crime_data_2016[
  -match("All Other Offenses", crime_data_2016$offense_name), ]

# join dfs and order
crime_joined <- full_join(crime_data_1996, crime_data_2016)
crime_joined <- mutate(crime_joined, 
                       average = (crime_joined$total_individuals_1996 
                                  + crime_joined$total_individuals_2016) / 2
)

# name columns
colnames(crime_joined) <- c(
  "Offense Category", 
  "Number of Arrests in 1996", 
  "Number of Arrests in 2016",
  "Average"
  )

# correct ordering of dumbbells
crime_joined$`Offense Category` <- factor(
  crime_joined$`Offense Category`, 
    levels = crime_joined$`Offense Category`[order(crime_joined$`Average`)]
)


# create dumbbell plot
theme_set(theme_classic())
crime_plot <- plot_ly(crime_joined, color = I("gray80"))
crime_plot <- crime_plot %>% add_segments(x = crime_joined$`Number of Arrests in 1996`, 
                                          xend = crime_joined$`Number of Arrests in 2016`,
                                          y = crime_joined$`Offense Category`, 
                                          yend = crime_joined$`Offense Category`, 
                                          showlegend = FALSE)
crime_plot <- crime_plot %>% add_markers(x = crime_joined$`Number of Arrests in 1996`, 
                                         y = crime_joined$`Offense Category`, 
                                         name = "1996", color = I("blue"))
crime_plot <- crime_plot %>% add_markers(x = crime_joined$`Number of Arrests in 2016`, 
                                         y = crime_joined$`Offense Category`, 
                                         name = "2016", color = I("red"))
crime_plot <- crime_plot %>% layout(
  title = "Comparison of Offense Category Frequencies Between 1996 and 2016",
  xaxis = list(title = "Number of Arrests"),
  margin = list(l = 65)
)

crime_plot
