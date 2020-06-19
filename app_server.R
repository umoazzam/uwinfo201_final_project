suppressMessages(library("dplyr"))
suppressMessages(library("shiny"))
suppressMessages(library("ggplot2"))
suppressMessages(library("plotly"))
source("scripts/age_and_gender.R")
source("scripts/area_encode_race.R")

# load `juvenile crime dataframe`
juvenile_df <- read.csv("data/juvenile_crime.csv", stringsAsFactors = FALSE)

server <- function(input, output) {

  # table for "about" section
  output$tbl <- renderTable(head(agg_table, 10))

  # Age/Sex breakdown for Page 2
  output$distbar <- renderPlotly({
    validate(
      need(!is.na(input$age_select),
           "Please select an age group to display the visual.")
    )
    # filtering down `total_by_gender` dataset
    total_by_gender_2 <- total_by_gender %>%
      pivot_longer(cols = c(female, male),
                   names_to = "gender", values_to = "Total") %>%
      filter(age_group %in% input$age_select)

    # age and gender bar graph
    dist_bar <- ggplot(total_by_gender_2, aes(x = age_group, y = Total,
                                              text = paste("Quantity:",
                                                           Total))) +
      geom_bar(stat = "identity", width = 0.5, position = "dodge",
               aes(fill = gender)) +
      labs(title = "Juvenile Crime by Gender and Age Group in the US,
           1994-2016",
           x = "Age Group", y = "Total Number of People") +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 18))
    ggplotly(dist_bar, tooltip = c("text")) %>%
      layout(margin = list(t = 85))
  })

  # Race Pie Chart for Page 1
  output$pie_chart <- renderPlotly({

    # filter down the `juvenile_df` dataset
    race_data <- juvenile_df %>%
      filter(year > input$year_choice[1], year < input$year_choice[2])
    # creating new data frame
    new_total_by_race <- data.frame(
      race = c("White", "Black", "Asian Pacific Islander", "Native American"),
      total = c(totals(race_data, "white"),
                totals(race_data, "black"),
                totals(race_data, "asian_pacific_islander"),
                totals(race_data, "american_indian")
      )
    )

    # making pie chart for racial breakdown
    pie_chart <- plot_ly(new_total_by_race, labels = ~race,
                         values = ~total, type = "pie") %>%
      layout(title = "Juvenile Crime by Race in the US",
             xaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE))
    return(pie_chart)
  })

  # Dumbell Plot for Page 3
  output$crime_plot <- renderPlotly({

    # organizing data to filter out unwanted years
    crime_data_y1 <- juvenile_df %>%
      filter(year == input$slider[1]) %>%
      group_by(offense_name) %>%
        summarise(total_individuals_y1
                  = round(sum(total_male + total_female), 2)) %>%
      arrange(-total_individuals_y1) %>%
      top_n(11, total_individuals_y1) %>%
      arrange(offense_name)

    crime_data_y2 <- juvenile_df %>%
      filter(year == input$slider[2]) %>%
      group_by(offense_name) %>%
      summarise(total_individuals_y2
                = round(sum(total_male + total_female), 2)) %>%
      arrange(-total_individuals_y2) %>%
      top_n(11, total_individuals_y2) %>%
      arrange(offense_name)

    crime_data_y1 <- crime_data_y1[
      -match("All Other Offenses", crime_data_y1$offense_name), ]
    crime_data_y2 <- crime_data_y2[
      -match("All Other Offenses", crime_data_y2$offense_name), ]

    crime_joined_data <- full_join(crime_data_y1, crime_data_y2)
    crime_joined_data <- mutate(crime_joined_data,
                                average =
                                  (crime_joined_data$total_individuals_y1
                                           +
                                     crime_joined_data$total_individuals_y2)
                                / 2
    )

    colnames(crime_joined_data) <- c(
      "Offense Category",
      "Number of Arrests in Year 1",
      "Number of Arrests in Year 2",
      "Average"
    )

    crime_joined_data$`Offense Category` <- factor(
      crime_joined_data$`Offense Category`,
      levels = crime_joined_data$`Offense Category`
      [order(crime_joined_data$`Average`)]
    )

    theme_set(theme_classic())
    crime_dumbbell_plot <- plot_ly(crime_joined_data, color = I("gray80"))
    crime_dumbbell_plot <- crime_dumbbell_plot %>%
      add_segments(x = crime_joined_data$`Number of Arrests in Year 1`,
                   xend = crime_joined_data$`Number of Arrests in Year 2`,
                   y = crime_joined_data$`Offense Category`,
                   yend = crime_joined_data$`Offense Category`,
                   showlegend = FALSE)

    crime_dumbbell_plot <- crime_dumbbell_plot %>% add_markers(x =
                                                                 crime_joined_data$
                                                                 `Number of Arrests in Year 1`,
                                                               y = crime_joined_data$
                                                                 `Offense Category`,
                                                               name = input$slider[1],
                                                               color = I("blue"))
    crime_dumbbell_plot <- crime_dumbbell_plot %>% add_markers(x = crime_joined_data$
                                                                 `Number of Arrests in Year 2`,
                                                               y = crime_joined_data$
                                                                 `Offense Category`,
                                                               name = input$slider[2],
                                                               color = I("red"))
    crime_dumbbell_plot <- crime_dumbbell_plot %>% layout(
      title = "Comparison of Offense Category Frequencies Between
      1996 and 2016",
      xaxis = list(title = "Number of Arrests"),
      margin = list(l = 65)
    )
    return(crime_dumbbell_plot)
  })
}
