suppressMessages(library("dplyr"))
suppressMessages(library("shiny"))
suppressMessages(library("plotly"))
source("scripts/age_and_gender.R")
source("scripts/aggregate_table.R")
source("scripts/area_encode_race.R")
source("scripts/summary_information.R")

# load `juvenile crime dataframe`
juvenile_df <- read.csv("data/juvenile_crime.csv", stringsAsFactors = FALSE)

# First Page: Introduction
intro <- tabPanel(
  "About",
  img("", src =
        "https://journeyonline.com.au/wp-content/uploads/2014/03/Youth-crime-banner.jpg",
      height = "100%", width = "100%", allign = "center"),
  h1(id = "big heading", "Juvenile Arrests in the United States"),
  br(),
  p("No news network or media outlet is unbiased, and
    a team putting together a breaking news story
    is inevitably influenced by their parent company, their peers and superiors,
    as well as their own opinions.
    This can be seen in the way that criminals and victims are portrayed,
    expecially in instances of violent crime. Media is more likely to
    offer an excuse for a white criminal, such as childhood trauma or
    a history of poor mental health, while offering no motive or explanation
    for criminals of color. Unfortunately, this may even be seen in instances of
    juvenile crime, or crime commited by a person under the age of 18."),
  p("By analyzing raw data from juvenile crime arrests, the true nature of
    juvenile crime in America based on demographics will be revealed;
    including societal biases and discrimination among gender, age, racial
    and ethnic diasporas. The relationship between crime types and demographic
    characteristics will be explored, in reference to the Juvenile Crime
    dataset, that gathered pertinent arrest information of juvenile crime in
    the United States from 1994 to 2016, gathered and published by the Federal
    Bureau of Investigation and the University of California,
    Riverside. The data includes only", strong("arrests"), "of minors
    for crimes, excluding arrests made for the safety or well-being,
    of the juvenile, nor any other encounters with officials, regardless
    of reason."),
  h5("The source of the dataset can be found", a("here.", href =
                                                   "https://crime-data-explorer.fr.cloud.gov/downloads-and-docs")),
  p(strong("Note:"), "Not every state is required to report the race of a criminal, however,
    they are required to report the age and gender of the offender. This data reflects this practice.")
)

# Second Page: Summary Information
summary <- tabPanel(
  "Conclusions",
  h1(id = "big heading", "Findings in Investegating Juvenile Crime in
     the United States"),
  br(),
  img("", src = "https://publicpolicy.com/wp-content/uploads/2017/08/juvenile-justice-01-1600x400.jpg",
      height = "100%", width = "100%", allign = "center"),
  h2("General Findings"),
  h3("Gender"),
  p("Between the years 1994 and 2016,", sum(info$all_males + info$all_females),
    "juveniles were arrested, and was composed of", info$all_males,
    "boys and", info$all_females,
    "girls, alluding to the fact that youth males are arrested",
    round(info$all_males / info$all_females, 2),
    "times more frequently compared to youth females. This can be attributed
    to the historical connotations of female figures and their role in
    society, where they viewed as more 'dainty' or 'fragile', and that the only
    purpose that they serve is to be a good housewife. Male figures,
    historically, were described as 'tough', or 'strong', in comparison. Criminal
    activity can be characterized as such, and thus, unlawful activities may be
    motivated by these sentiments."
    ),
  h3("Age"),
  p("The seventeen year old age category had the highest number of arrests,
    with a total of",
    info$most_arrested_age, "arrests, which can be attributed to a seventeen
    year old's close proximity to adulthood. On the contrary, the age
    group with the fewest arrests was the zero through age nine age
    category, with", info$least_arrested_age, "arrests. These statistics
    fit into society's generalizations about crimes and age. Young children
    are seldom seen as criminals, justifying their actions through
    development deficiencies, or simply not knowing the rules. However,
    older teenagers that display this same behavioral problem are often
    referred to as 'dangerous' or 'violent', thus alluding to a higher
    likelihood of committing a crime. Alternatively, an adolescent's poor
    behavior is often conscious, as they have the cognitive capacity to
    comprehend their actions. In turn, they are seen as a greater threat
    to society, and consequently, arrested. Comparatively, young children
    do not know or understand the implications of their actions, and
    thus, are excused from these demonstrations of poor behaviors."),
  h3("Ethnicity & Race"),
  p("The ethnic group that had the greatest number of arrests were white minors,
    with a total of", info$most_arrested_race, "arrests throughout the 22 years,
    averaging", round(info$most_arrested_race / 22, 2), "arrests annually.
    The ethnic
    group with the least amount of arrests were Native people, with",
    info$least_arrested_race, "arrests total, and",
    round(info$least_arrested_race / 22, 2),
    "average annual arrests. This aligns with general (though not percentage)
    information from the U.S. Census, as
    Caucasians are the largest ethnic group in the U.S. population, and
    Native people is the smallest percentage of the U.S. population out of those
    represented in the dataset. However, black minors, who often fall victim
    to wrongful arrest or excessive violence, as seen in current events (more
    in the 'connection' tab), make up nearly 30% of juvenile arrests, despite
    the fact they makeup a mere 13% of the population. This prejudice persists,
    and has elevated today, as protests and riots storm across the United
    States demanding justice."),
  br(),
  h2("Table Findings"),
  tableOutput("tbl"),
  p("The table above shows juvenile crime arrests in the US from 1994-2016
    grouped by the offense category, and presents the sex/racial demographic
    breakdown for each category.
    The offense categories are sorted by the total number of cases,
    providing insight into which arrests are the most common among youth in
    America.",
    br(),
    br(),
    "The top 3 crimes committed by youth are", strong("larceny, simple
    assault, and basic drug abuse violations"), ", as seen in the 'crime
    category breakdown' page. The prominence of larceny, or simply put,
    petty theft, in juvenile crime can be attributed to the nature of the
    crime itself, as it is easy for a child, especially one that does not
    understand the concept of unlawfully taking something from a store,
    to commit. Further, unempathetic shopkeepers can simply call the
    police and have these children arrested. Simple assault can also easily
    be committed by a juvenile or even a young child. Demonstrations of
    this behavior can include punching a classmate, threatening to
    physically hurt an individual or other instances of physical harm.
    Basic drug-abuse violations are more often violated by older
    juveniles, who desire to become under the influence of an unnatural
    substance, such as alcohol. Its presence in juvenile crime can be
    attributed to this desire, as well as the simplicity of obtaining
    over the counter drugs and misusing them.",
    br(),
    br(),
    "As seen in previous data, those that are arrested for criminal activity
    overwhelmingly white and male, however, are disproportionate to their
    representation in the U.S. population, as presented by the Census,
    Caucasians having a lower rate of juvenile arrests in comparison to
    their population composition, 76.5% , and males composing
    of 50.2% of the population.",
    br(),
    br(),
    strong("Note:"), "The table only shows the top 10 offense
    categories of juveniles, as smaller categories were removed in
    the creation of this summary table."),
  br(),
  h5("Source:", a("U.S. Census", href =
      "https://www.census.gov/quickfacts/fact/table/US/PST045219"))
)

# Third Page: Racial Breakdown (Pie Chart)
year_range <- unique(juvenile_df$year)

year_input <- sliderInput(
  inputId = "year_choice",
  label = h3("Racial Breakdown by Year"),
  min = min(year_range, na.rm = TRUE), max = max(year_range, na.rm = TRUE),
  value = c(1994, 2016), step = 1
)

race_breakdown <- tabPanel("Racial Breakdown",
                           h1(id = "big heading", "Juvenile Crime by Race"),
                           # This content uses a sidebar layout
                           sidebarLayout(
                             sidebarPanel(
                               year_input
                             ),

                             mainPanel(
                               p("The pie chart reveals the racial breakdown for
                               juvenile crime from 1994 to 2016."),
                               br(),
                               p("Across the crimes documented, white minors are
                               the most arrested group compared to others. Asian
                               Pacific Islander and Native American minors
                               account for smaller fractions of the total
                               amount of juvenile arrests. While black minors
                               are the second largest group for juvenile
                               arrests. The total arrests still being
                               less than half of the total arrests that
                               are documented for white minors."),
                               br(),
                               p("Additionally, comparing the racial breakdown
                               of arrests to the general population, black
                               minors are more likely to be arrested compared
                               to their white counterparts. According to the
                               US Census, those identifying as black take up ",
                                 a("13.4% of the population", href =
                                            "https://www.census.gov/quickfacts/fact/table/US/PST045219"),
                                 ". Yet, in juvenile crime alone, they hold
                                 almost 30% of arrests. For other groups, the
                                 percentage of arrests are still fewer than
                                 the overall population percentage."),
                               br(),
                               plotlyOutput("pie_chart")
                             ),

                             position = c("left", "right"),
                             fluid = TRUE
                           ),
)


# To help keep the code organized, we'll store some UI elements in variables
# _before_ defining the UI.

# Define a variable `age_input` that is a `selectInput()` with the following
# properties:
# - an inputId of `age_input`
# - a label of "Gender and Age Details for Juvenile Crime"
# - This dropdown should let the user pick one of the age group of the
# juvenile crime dataset.
# - use all age groups as a default

age_input <- checkboxGroupInput(
  inputId = "age_select",
  label = h3("Age Groups for Juvenile Crime"),
  choices = c("0-9", "10-12", "13-14", "15", "16", "17"),
  selected = c("0-9", "10-12", "13-14", "15", "16", "17")
)


# Define the fourth page content; uses `tabPanel()` and `sidebarLayout()`
# as well as add CSS styles for the entire app
page_four <- tabPanel(
  "Age and Gender Distribution", # label for the tab in the navbar
  # the following 5 CSS styles are:
  # - change the title's color and font in the tabPanel
  # - change the backgound's color
  # - change the pargraph's color, font and size
  # - change the title's color and font in the mainPanel
  # - Make it bold for the text in the sidebarPanel
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 {
        font-family: 'Arial Black';
        font-weight: 500;
        line-height: 1.1;
        color: #FFFFFF;
      }
      body {
      background-color: #FFD700;
      }
      p {
        font-family:'Arial';
        color: #000000;
        font-size: 18px;
      }
      h2 {
      font-family: 'Arial';
      color:#1E90FF;
      }
      h3 {
      font-weight: bold;
      }
    "))
  ),
  # show with a displayed title
  h1(id = "big heading", "Juvenile Crime by Age and Gender"),
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      age_input
    ),
    mainPanel(
      p("The bar chart below shows the discrepancies in gender and
        age groups in juvenile crime between the years 1994 to 2016.
        The interactive bar graph shows that boys in any age group
        are arrested more frequently when compared to their female
        counterparts of the same age. Additionally, there is a
        positive correlation between age and arrests of boys,
        whereas a negative correlation is seen amongst girls."),
      br(),
      p(plotlyOutput(outputId = "distbar"))
    )
  )
)

# Fifth Page: Crime Breakdown (Tree Map)
year_range <- 1996:2016

year_slider <- sliderInput(
  inputId = "slider",
  label = h3("Selected Year(s) for Crime Breakdown"),
  min = min(year_range), max = max(year_range),
  value = c(2000, 2010), step = 1
)

crime_breakdown <- tabPanel(
  "Crime Category Breakdown",
  h1(id = "big heading", "Top 10 Most Common Crime Categories
     by US Youth Arrests"),

  sidebarLayout(
    sidebarPanel(
      year_slider
    ),

    mainPanel(
      p("The dumbbell plot above answers the question of which
      offense is most common among youth in the US from 1996
      to 2016 by comparing the number of arrests for each
      category between 1996 and 2016. The true top offense,
      which was a miscellaneous category that grouped smaller
      categories into one, was removed to display a more
      accurate picture of the most common offenses. In
      addition, the removes observations from 1994 and
      1995 due to some issues with the cateogry
      data during those years.
"),
      p("This plot reveals a key insight. As observed in
      the aggregate table, the most common offense by number
      of arrests is Larceny, followed by Simple Assault and
      Drug Abuse Violations. It is also noted that Larceny
      has near twice the arrests of the next category,
      heavily outweighing other categories. Finally, it appears
      that juvenile arrests have been declining in every category 
      since 1996."),
      br(),
      p(plotlyOutput(outputId = "crime_plot"))
    )
  )
)

#Resources for race based biases- a connection of the project to current events
current_events <- tabPanel(
  "Connections",
  h1(id = "big heading", "Findings and Connections to BLM and Current Events"),
  img("", src = "https://assets.weforum.org/article/image/large_8YNNp6UMvy8t-yc08e1IFQcuK6O9gfTUKtK1kmrGV8o.jpg",
      height = "75%", width = "100%", allign = "center"),
  sidebarLayout(
    sidebarPanel(h2("Links"),
                 h3("Resources"),
                 a("How to Be an Ally", href =
                     "https://docs.google.com/document/d/1vIDs_NkAM66EMVZw7NXPpRxtwJDPZY1Z_LsgzMI88Tk/edit"),
                 br(),
                 a("National Resource Google Doc", href =
                     "https://docs.google.com/document/u/1/d/1CjZMORRVuv-I-qo4B0YfmOTqIOa3GUS207t5iuLZmyA/mobilebasic?fbclid=IwAR1e9ZLLdC9anI2wibzgsunAIGP5nDWyor9nulL4kU3NWVvl2B8HBe2pTXk"),
                 h3("Donate"),
                 a("Black Lives Matter", href =
                     "https://secure.actblue.com/donate/ms_blm_homepage_2019"),
                 br(),
                 a("WATCH TO DONATE", href =
                     "https://youtu.be/bCgLa25fDHM"),
                 br(),
                 a("NAACP Legal Defense Fund", href =
                     "https://www.naacpldf.org/"),
                 h4("Seattle Organizations"),
                 a("Northwest Community Bail Fund", href =
                     "https://www.nwcombailfund.org"),
                 br(),
                 a("Black Lives Matter Seattle", href =
                     "https://blacklivesseattle.org/bail-fund/"),
                 h3("Petitions"),
                 a("Justice for George Floyd", href =
                     "https://t.co/VwvycVsUJv"),
                 br(),
                 a("Justice for Ahmaud Arbery", href =
                     "https://www.runwithmaud.com/"),
                 br(),
                 a("Justice for Breonna Taylor", href =
                     "https://www.change.org/p/andy-beshear-justice-for-breonna-taylor"),
                 br(),
                 a("Campaign Zero", href =
                     "https://www.joincampaignzero.org/")
    ),
    mainPanel(
      h2("BLACK LIVES MATTER"),
      p("On February 23rd,", strong("Ahmaud Arbery"), "was
      shot by a former police officer and investigator while
      he jogging. On March 13th,", strong("Breonna Taylor"),
      "was killed inside her own home by police during a
      no-knock entry into her apartment. On May 25th,",
      strong("George Floyd"), "died after a police
      officer kneeled on his neck for eight minutes during
      an arrest."),
      p("While this project began prior to the civil unrest
      that our country currently faces, it is a bitter
      reminder, this is not a new incident. Young, black
      boys are disporportionately arrested compared to
      other demographics. With that, the biases that were
      discovered in the exploration of this dataset are often
      seen beyond youth."),
      p("With the Black Lives Matter Movement sparking
      conversations and spurring change around the US about
      decades of unjustified police violence against the black
      community, we as a team decided that its extremely
      important to donate to support the movement, sign
      petitions to help achieve civic justice for the innocent
      lives taken, and continue to educate ourselves and have
      conversations about racial injustice in America with the
      goal of one day ending it. We hope that anyone viewing
      this app will take time to donate to the BLM movement,
      sign the petitions, and help fight for not just the black
      community, but for the betterment of the future of America.")
    )
  )
)


#UI
ui <- navbarPage(
  "Juvenile Crime in the U.S.",
  intro,
  race_breakdown,
  page_four,
  crime_breakdown,
  summary,
  current_events
)
