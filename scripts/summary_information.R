# Summary information
juvenile_crime <- read.csv("data/juvenile_crime.csv",
                              stringsAsFactors = FALSE)
suppressMessages(library(dplyr))

#Delete Empty Column
juvenile_crime <- juvenile_crime %>%
  select(-state_abbr)

# A function that takes in a dataframe and returns a list of summary information about
# the dataframe

get_summary_info <- function(dataset) {
  ret <- list()
  ret$all_males <- dataset %>%
    summarise(total_males = sum(total_male, na.rm = TRUE)) %>%
    pull(total_males)
  ret$all_females <- dataset %>%
    summarise(total_females = sum(total_female, na.rm = TRUE)) %>%
    pull(total_females)
  ret$most_arrested_race <- dataset %>%
    summarise(
      total_african_american = sum(black, na.rm = TRUE),
      total_asian_pa = sum(asian_pacific_islander, na.rm = TRUE),
      total_native = sum(american_indian, na.rm = TRUE),
      total_caucasian = sum(white, na.rm = TRUE)) %>%
    max()
  ret$least_arrested_race <- dataset %>%
    summarise(
      total_african_american = sum(black, na.rm = TRUE),
      total_asian_pa = sum(asian_pacific_islander, na.rm = TRUE),
      total_native = sum(american_indian, na.rm = TRUE),
      total_caucasian = sum(white, na.rm = TRUE)) %>%
    min()
  ret$most_arrested_age <- dataset %>%
    summarise(
      zero_nine = sum(sum(m_0_9, na.rm = TRUE), sum(f_0_9, na.rm = TRUE)),
      ten_twelve = sum(sum(m_10_12, na.rm = TRUE), sum(f_10_12, na.rm = TRUE)),
      thirteen_fourteen = sum(sum(m_13_14, na.rm = TRUE), sum(f_13_14, na.rm = TRUE)),
      fifteen = sum(sum(m_15, na.rm = TRUE), sum(f_15, na.rm = TRUE)),
      sixeeen = sum(sum(m_16, na.rm = TRUE), sum(f_16, na.rm = TRUE)),
      seventeen = sum(m17 = sum(m_17, na.rm = TRUE), sum(f_17, na.rm = TRUE))) %>% 
    max()
  ret$least_arrested_age <- dataset %>%
    summarise(
      zero_nine = sum(sum(m_0_9, na.rm = TRUE), sum(f_0_9, na.rm = TRUE)),
      ten_twelve = sum(sum(m_10_12, na.rm = TRUE), sum(f_10_12, na.rm = TRUE)),
      thirteen_fourteen = sum(sum(m_13_14, na.rm = TRUE), sum(f_13_14, na.rm = TRUE)),
      fifteen = sum(sum(m_15, na.rm = TRUE), sum(f_15, na.rm = TRUE)),
      sixeeen = sum(sum(m_16, na.rm = TRUE), sum(f_16, na.rm = TRUE)),
      seventeen = sum(m17 = sum(m_17, na.rm = TRUE), sum(f_17, na.rm = TRUE))) %>% 
    min()
  return(ret)
}

info <- get_summary_info(juvenile_crime)

