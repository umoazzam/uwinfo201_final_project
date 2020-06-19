suppressMessages(library(dplyr))
suppressMessages(library(lintr))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))

juvenile_df <- read.csv("data/juvenile_crime.csv", stringsAsFactors = FALSE)

totals <- function(df, col_name) {
  age_gender_total <- df %>%
    select(all_of(col_name)) %>%
    filter(!is.na(!!as.name(col_name))) %>%
    mutate(total = cumsum(!!as.name(col_name))) %>%
    filter(total == max(total, na.rm = TRUE)) %>%
    pull(total)
}

total_by_gender <- data.frame(
  age_group = c("0-9", "10-12", "13-14", "15", "16", "17"),
  male = c(totals(juvenile_df, "m_0_9"), totals(juvenile_df, "m_10_12"),
             totals(juvenile_df, "m_13_14"), totals(juvenile_df, "m_15"),
             totals(juvenile_df, "m_16"), totals(juvenile_df, "m_17")),
  female = c(totals(juvenile_df, "f_0_9"), totals(juvenile_df, "f_10_12"),
             totals(juvenile_df, "f_13_14"), totals(juvenile_df, "f_15"),
             totals(juvenile_df, "f_16"), totals(juvenile_df, "f_17"))
)

bar <- total_by_gender %>%
  gather("Demographic", "Total", -age_group) %>%
  ggplot(aes(age_group, Total, fill = Demographic)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Juvenile Crime by Gender and Age Group in the US, 1994-2016",
       x = "Age Group", y = "Total Number of People")
