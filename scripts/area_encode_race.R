suppressMessages(library(dplyr))
suppressMessages(library(lintr))
suppressMessages(library(plotly))

juvenile_df <- read.csv("data/juvenile_crime.csv", stringsAsFactors = FALSE)

totals <- function(df, col_name) {
  race_total <- df %>%
  select(all_of(col_name)) %>%
  filter(!is.na(!!as.name(col_name))) %>%
  mutate(total = cumsum(!!as.name(col_name))) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(total)
}

total_by_race <- data.frame(
  race = c("White", "Black", "Asian Pacific Islander", "Native American"),
  total = c(totals(juvenile_df, "white"),
          totals(juvenile_df, "black"),
          totals(juvenile_df, "asian_pacific_islander"),
          totals(juvenile_df, "american_indian")
          )
)

pie_chart <- plot_ly(total_by_race, labels = ~race, values = ~total,
                     type = "pie") %>%
  layout(title = "Juvenile Crime by Race in the US, 1994-2016",
         xaxis = list(showgrid = FALSE, zeroline = FALSE,
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE,
                      showticklabels = FALSE))
