# packages/files to import
library(dplyr)
library(styler)
crime_data <- read.csv("data/juvenile_crime.csv", stringsAsFactors = FALSE)

# creating and cleaning summary table
agg_table <- crime_data %>%
  group_by(offense_name) %>%
  summarise(
    total_individuals = round(sum(total_male + total_female), 2),
    percent_men = round(sum(total_male / total_individuals * 100), 2),
    percent_women = round(sum(total_female / total_individuals * 100), 2),
    percent_white = round(sum(white / total_individuals * 100), 2),
    percent_black = round(sum(black / total_individuals * 100), 2),
    percent_asian = round(sum(asian_pacific_islander / total_individuals
                              * 100), 2),
    percent_native_american = round(sum(american_indian / total_individuals
                                        * 100), 2)
  ) %>%
  arrange(-total_individuals)

# removing miscellaneous column
agg_table <- agg_table[-1, ]

# assigning column names
colnames(agg_table) <- c(
  "Offense",
  "Total cases",
  "Percentage of Men (%)",
  "Percentage of Women (%)",
  "Percentage of White (%)",
  "Percentage of Black (%)",
  "Percentage of Asian (%)",
  "Percentage of Native American (%)"
)
