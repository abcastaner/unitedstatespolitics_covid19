#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)
library(plotly)
library(rstanarm)
library(tidycensus)
library(readxl)
library(tidymodels)
library(sf)
library(rgdal)
library(gt)
library(broom.mixed)
library(gtsummary)
library(shinythemes)

# Data read-in for the maps plotting covid-19 cases

for_plots1_2 <- tibble(st_read("data_forapp/for_plots1_2.shp")) %>%
  rename("combined_fips" = cmbnd_f,
         "state_abbr" = stt_bbr,
         "favor_dem" = favr_dm,
         "pop_estimate" = POPESTI,
         "March" = X03, "April" = X04, "May" = X05, "June" = X06, "July" = X07,
         "August" = X08, "September" = X09, "October" = X10, "November" = X11) 

# Setting up the choice of states/months for the plots on my first tab

plot1_states <- for_plots1_2$state_abbr %>% 
    unique()

plot1_months <- c("March", "April", "May", "June", "July", "August",
                  "September", "October", "November")

# Reading in the data for the Flattening the Curve tab

data_curve <- read_csv("data_forapp/data_curve.csv") %>% 
  select(-X1)

flattencurve_usplot <- read_csv("data_forapp/flattencurve_usplot.csv") %>% 
  select(-X1)

# Read in data for Deaths tab

deaths_usplot <- read_csv("data_forapp/deaths_usplot.csv") %>% 
  select(-X1)

# Read in the data for the Tracking Testing tab

testing <- read.csv("data_forapp/testing.csv") %>% 
  select(-X)

# Reading in data needed for the Model tab

data_formodel <- read_csv("data_forapp/data_formodel.csv") %>% 
  select(-X1)

# Building my linear model, excluding ~336 cases in the US that did not have
# any confirmed cases on 12/04/20

f1 <- stan_glm(log_normalized_casesdec4 ~ favor_dem + favor_dem*pop_density_per1k,
               data = (data_formodel %>% 
                         filter(log_normalized_casesdec4 != -Inf)),
               refresh = 0)

# creating new observation for posterior_predict()

new_obs <- tibble(favor_dem = c(1, 0),
                  pop_density_per1k = 274844) 

# running posterior_predict()

pp <- posterior_predict(f1, 
                        newdata = new_obs) %>%
  as_tibble() %>% 
  mutate_all(as.numeric)

# Building the table for the linear regression

gt_tbl <- tbl_regression(f1, intercept = TRUE,
                         estimate_fun = function(x) 
                           style_sigfig(x, digits = 10)) %>%
      as_gt() %>%
      tab_header(title = "Linear Regression of COVID-19 Cases by County",
                 subtitle = "The Effect of Political Affiliation on COVID-19
                 Infections in The United States")

# Define UI for application
ui <- navbarPage(
    "COVID-19 & U.S. Political Affiliation",
    theme = shinytheme("lumen"),
    tabPanel("Mapping the Outbreak", 
             h3("Overview"),
             p("This project attempts to visualize and model the relationship
               between COVID-19 confirmed cases and majority political 
               affiliation for each county in the United States. Each of the
               tabs above focuses on a different area, from how well counties
               were able to flatten the curve to building a predictive model
               using the data. Feel free to explore as you like!"),
             h3("Mapping New COVID-19 Cases: A Month by Month View, Divided by
                Political Affiliation"),
             p("The first visualization, seen below, shows the number of
               new COVID-19 cases by county, for the selected state and month
               of 2020. The map on the left shows the number of new COVID-19 
               cases
               only for Democratic counties in the selected state, while the map
               on the right shows the number of new COVID-19 cases only for
               Republican counties. The party for which a county predominantly
               voted for president in the 2016 U.S. General Election is used
               as a proxy for determining whether a county is Democratic or
               Republican. The months of January and February 2020 
               are excluded from the visualization due to poor
               quality data and concentration of COVID-19 cases in only 
               a few states. We can observe that in some states,
               it appears that the Democratic counties report a greater number
               of new COVID-19 cases per month than the Republican counties."),
             fluidRow(
               selectInput("state_plot1", "State",
                           choices = plot1_states),
               selectInput("month_plot1", "Month",
                           choices = plot1_months),
                      column(6,plotOutput("distPlot1")),
                      column(6,plotOutput("distPlot2")),
             ),
             h2("  "),
             h3("Why Population is Important"),
             p("The second visualization, seen below, shows the number of
               new COVID-19 cases by county, for the selected state and month
               of 2020. As in the visualization above, the counties are 
               segregated by political affiliation. This visualization, however,
               differs in that it takes into account each county's population:
               the scale for the number of new COVID-19 cases per month is 
               presented per 100,000 residents of each county. We can observe
               that in some states, like AR and CO, the Democratic counties 
               seem to have the largest outbreaks early on or in the summer 
               months, whereas into late fall the Republican counties surpass
               them."),
             fluidRow(
               selectInput("state_plot2", "State",
                           choices = plot1_states),
               selectInput("month_plot2", "Month",
                           choices = plot1_months),
               column(6,plotOutput("distPlot3")),
               column(6,plotOutput("distPlot4")),
               ),
             ),
    tabPanel("Flattening the Curve", 
             h2("Flattening the Curve"),
             h4("By state"),
             p("From the visualizations in the Mapping the Outbreak tab,
                we could observe that there seems to be some relationship 
                between the number of new COVID-19 cases per county and 
                time (months). In order to better quantify this relationship,
                this visualization came about. It depicts the number of new
                COVID-19 cases per month for a county, accounting for population, from Mar.
                - Nov. 2020 for a specified state. The Democratic line represents the average
                number of new COVID-19 cases every month for Democratic counties.
                The Republican line represents the average
                number of new COVID-19 cases every month for Republican counties.
                As a general trend, it seems that for many states, on average
                the Democratic counties within it had the largest number of 
                cases per 100,000 people first, while towards fall there is a 
                shift and Republican counties seem to on average surpass
                Democratic counts."),
             fluidPage(
                 selectInput("state_plot5", "State",
                             choices = plot1_states),
                 plotOutput("distPlot5")
                 ),
             h4("For the United States"),
             p("In order to quantify and better visualize the trends on a 
                country-level scale, I found the average of new COVID-19 
                cases for all counties in the United States. Here, we can see
                that, on average, Democratic counties had a greater
                number of new confirmed COVID-19 cases per 100,000 people
                per month until mid-August. In mid-August, Republican counties
                on average surpass the Democratic counties in number of 
                confirmed COVID-19 cases per 100,000 people."),
             fluidPage(
               plotOutput("distPlot6")
               )
             ),
    tabPanel("Data on Deaths", 
             h2("Data on Deaths"),
             h4("For the United States"),
             p("The visualization below is analogous to the one in the
             Flattening the Curve tab, but for the number of confirmed COVID-19
             deaths. It depicts the number of new
                COVID-19 deaths per month,
                accounting for population, from Mar.
                - Nov. 2020, on average for a county in the United States.
                The Democratic line 
                represents the average
                number of new COVID-19 deaths every month for Democratic
                counties.
                The Republican line represents the average
                number of new COVID-19 deaths every month for Republican
                counties.
                As a general trend, it seems that for many states, on average
                the Democratic counties within it had the largest number of 
                deaths per 100,000 people first, while towards fall there is a 
                shift and Republican counties seem to on average surpass
                Democratic counts."),
             fluidPage(
               plotOutput("distPlot12")
             )
    ),
    tabPanel("Tracking Testing",
             h2("Tracking Testing and COVID-19 by State"),
             p("Ideally, in my project I would have been able to visualize
               the relationship between COVID-19 cases, political affiliation,
               and testing for each county in the U.S. However, there does not
               exist data for testing at the county level. Regardless, it is
               still useful to see general trends at the state level, 
               as will be shown
               on this page. There are three plots in the tabs below which
               visualize testing density, political afffiliation, and COVID-19
               cases/deaths for each state. Political affiliation is defined
               here as which party garnished a plurality of a state's votes
               during the 2016 Presidential Election. It is important
               to note that
               15 states are left out of these visualizations because they do
               not report data on the total amount of PCR test specimens
               processed. Of these 15 states, 10 are Democratic and 5 are
               Republican."),
             tabsetPanel(type = "tabs",
                         tabPanel("Testing Density & Political Affiliation",
                                  h3("Testing Density & Political Affiliation"),
                                  p("Below we have a visualization of the 
                                    average number of PCR tests per 1,000 people
                                    for a state in the United States, depending
                                    on the political affiliation of that 
                                    state. Please note that the y-axis
                                    is counting the total number of PCR test
                                    specimens taken in each state and does
                                    not represent the number of unique
                                    individuals tested."),
                                  plotOutput("distPlot9")
                                  ),
                         tabPanel("Testing Density & Confirmed Cases",
                                  h3("Testing Density & Confirmed Cases"),
                                  p("Below we have a visualization of the 
                                    confirmed COVID-19 cases per 1,000 people
                                    in each
                                    state versus the total number of PCR tests
                                    administered per 1,000 people in that state,
                                    divided by political affiliation.
                                    Each blue dot is a Democratic state, while
                                    each red triangle is a Republican state.
                                    Please note that the y-axis
                                    is counting the total number of PCR test
                                    specimens taken in each state and does
                                    not represent the number of unique
                                    individuals tested."),
                                  plotOutput("distPlot10")
                                  ),
                         tabPanel("Testing Density & Deaths",
                                  h3("Testing Density & Deaths"),
                                  p("Below we have a visualization of the 
                                    confirmed and probable COVID-19 deaths
                                    per 10,000 people in each
                                    state versus the total number of PCR tests
                                    administered per 1,000 people in each state,
                                    divided by political affiliation.
                                    Each blue dot is a Democratic state, while
                                    each red triangle is a Republican state.
                                    Please note that the y-axis
                                    is counting the total number of PCR test
                                    specimens taken in each state and does
                                    not represent the number of unique
                                    individuals tested."),
                                  plotOutput("distPlot11")
                                  )
                         )
             ),
    tabPanel("Model",
             tabsetPanel(type = "tabs",
                         tabPanel("Building a Linear Model",
                                  h3("Building a Linear Model"),
                                  p("I constructed a linear model that regresses
                                  the total number of confirmed
                                  COVID-19 cases per 1,000 people 
                                  for each county in the United
                                  States on December 4th, 2020 with
                                  the predominant political party
                                  of that county and their population density.
                                  For simplicity, my model excludes around 
                                  336 counties in the United States which had
                                  0 confirmed COVID-19 cases on December 4th.
                                  Below is the equation describing this
                                  linear model. The y-variable in this equation,
                                  log(normalizedcasesdec4), is the natural log
                                  of the number of confirmed COVID-19
                                  cases a county had on 
                                  December 4th for every 1,000 residents.  
                                  The favor_dem variable is a measure of 
                                  whether the county leans Republican or 
                                  Democratic. It has a value of 1 if
                                  the county voted Democratic in 
                                  the 2016 general election,
                                  and 0 if they voted Republican.
                                  Pop_density_per1k is a variable
                                  that describes the population density for
                                  each county, in units of 1,000 people
                                  per square mile.
                                  It is worth noting that the model
                                  has a term, whose coefficient is Beta 3,
                                  for the interaction between the
                                  favor_dem and pop_density_per1k variables.
                                  The epsilon in the equation represents the 
                                  error caused by real-world variation and
                                  is outside the scope of my model."),
                        imageOutput("Image1"),
                        align = "center"
                                  ),
                         tabPanel("Regression Results",
                                  h3("Regression Results"),
                                  p("We can see that the (Intercept) value 
                                  is 3.80.
               This represents the average natural log of the number of COVID-19
               cases per 1000 people for a county in the United States on
               December 4th,
               2020 that is Republican and has a 0 pop_density_per1k.
               Next, we have our favor_dem variable, which is 1 if
               the county voted Democratic in the 2016 general election,
               and 0 if they voted Republican. It has a value of -0.19,
               meaning a county that is Democratic with a 0 pop_density_per1k
               score will have slightly less cases than a Republican one
               with 0 pop_density. Pop_density_per1k is a variable that describes
               population density for each county, in units of 1,000 people
               per square mile. Its coefficient has a value here of
               -0.0000001289, which seems
               very small, but we must remember that this has to be
               multiplied by the value of pop_density_per1k for the county in order
               to arrive at the decrease in the log of cases for the county.
               The last parameter is our interaction term between favor_dem
               and population_density, which is 0.0000001262.
               It is the slope in the log number of COVID-19 cases for
               a county that is Democratic and has a non-0 population density."),
                                  gt_output(outputId = "table"),
                                  align = "center"
                         ),
                        tabPanel("Coefficients",
                                 h3("Posterior Distribution of the 
                                    Coefficients"),
                                 p("Below we have the posterior
                                  distribution for each of the coefficients in
                                  the model. Use the buttons on the left
                                  to select a coefficient and
                                  visualize its posterior distribution."),
                                 sidebarPanel(
                                   radioButtons("coeff", "Coefficient for:",
                                                c("Intercept" = "intercept",
                                                  "favor_dem" = "favor_dem",
                                                  "pop_density_per1k" = "pop_density_per1k",
                                                  "favor_dem*pop_density_per1k" = "favor_dem:pop_density_per1k")),
                                   # br() element to introduce extra vertical spacing
                                   br()
                                 ),
                                 mainPanel(
                                   plotOutput("distPlot8")
                                 )
                        ),
                         tabPanel("Posterior Predictive Distribution",
                                  h3("Posterior Predictive Distribution"),
                                  p("Below we have the posterior predictive
                                  distribution for
                                  a Democratic and a Republican county in the
                                  United States,
                                  with average population densities,
                                  given by the model."),
                                  plotOutput("distPlot7")
                                  ),
                        tabPanel("Limitations",
                                 h3("Limitations"),
                                 p("It is important to recognize the limitations
                                 of my linear modeling of a very complex 
                                 disease situation within the United States. 
                                 My predictive
                                 model is built on relating confirmed COVID-19
                                 cases in every county to their political
                                 affiliation and population density. The model
                                 does not account for the testing volume of 
                                 each county, which varies widely. My model 
                                 also, in using only confirmed COVID-19 cases,
                                 does not account for the probable cases not
                                 captured in the confirmed count. Additionally,
                                 my model does not account for time in relation
                                 to the number of COVID-19 cases - it does not
                                 account for the fact that COVID-19 arrived in
                                 some counties much earlier than in others.
                                 Finally, it is important to recognize that
                                 my model, in only using 2016 general
                                 election data as a proxy for political
                                 affiliation,
                                 might not fully capture the current or 
                                 majority political 
                                 affiliation of every county in the United
                                 States.")
                        )
                         
             )
    ),
    tabPanel("About", 
             h3("About"),
             p("This is my final project for Harvard University's 
               Government 50: Data, Fall 2020."),
             h3("Methods"),
             p("For my project I wanted to investigate the relationship between
               political ideology and COVID-19 case density by county in the
               United States. My COVID-19 confirmed case data was read in from
               the Johns Hopkins COVID-19 Database, which compiles data from
               state and local governments in the U.S. I was unable to find
               an online table or data frame that reported the new cases of 
               COVID-19 at the county-level per day or per month, so I used 
               this JHU data to create one myself. The dataset I created 
               reports the total of new confirmed COVID-19 cases at the 
               county-level for each month of 2020. In order to determine 
               the political ideology of each county, I defined it as their
               voting preference in the 2016 Presidential Election.
               My model takes
               into account the population density of every county in the United
               States as well, using land size (2010 Census) and population 
               estimates (for 2019) published by the United States
               Census Bureau. In order to calculate population density in
               units of 1,000 people per square miles, I divided the 2019
               population estimate for each county by the 2010 land estimate
               and then multiplied by 1,000. Throughout this project, I use
               the data aforementioned to create a variety of visualizations
               and to build a predictive linear model."),
             h3("Data Sources"),
             p("2016 County-Level U.S. Presidential Election Results compiled
             by Tony McGovern, Stephen Larson, Bill Morris, & Matt Hodges.
             Found",
               a("here",
                 href = "https://github.com/tonmcg/US_County_Level_Election_Results_08-20")
               ),
             p("2016 State-Level U.S. Presidential Election Data from the",
               a("MIT Election Data and Science Lab",
                 href = "https://electionlab.mit.edu/data")
             ),
             p("COVID-19 Confirmed Cases/Deaths Time-Series Data from",
               a("JHU CSSE COVID-19 Data",
                 href = "https://github.com/CSSEGISandData/COVID-19")
             ),
             p("Geometry for maps from the Census API, specifically from
               the 2018 American Community Survey"),
             p("County-Level Population Estimates (2019) from the",
               a("Census",
                 href = "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902")
             ),
             p("County Land Area data (2010) from the",
               a("Census",
                 href = "https://www.census.gov/library/publications/2011/compendia/usa-counties-2011.html#LND")
             ),
             p("State-Level Testing Data from",
               a("The COVID Tracking Project",
                 href = "https://covidtracking.com/data")
             ),
             h3("Github"),
             a("Here is the link to view my Github repository
               for this project.",
               href = "https://github.com/abcastaner/unitedstatespolitics_covid19"),
             h3("About Me"),
             p("My name is Ana Castaner and I study Human Developmental and
             Regenerative Biology at Harvard College. You can reach me at 
             anacastaner@college.harvard.edu.")
    )
)

# Define server
server <- function(input, output, session) {       
    dataInput_1 <- reactive({
      for_plots1_2 %>%
            filter(state_abbr == input$state_plot1) %>% 
            mutate(cases_selectedmonth = ifelse(favor_dem == 0, NA, get(input$month_plot1)))
    })
    dataInput_2 <- reactive({
      for_plots1_2 %>%
          filter(state_abbr == input$state_plot1) %>% 
          mutate(cases_selectedmonth = ifelse(favor_dem == 1, NA, get(input$month_plot1)))
    })
    dataInput_3 <- reactive({
      for_plots1_2 %>%
        filter(state_abbr == input$state_plot2) %>% 
        mutate(cases_selectedmonth = ifelse(favor_dem == 0, NA, (get(input$month_plot2)/pop_estimate)*100000))
    })
    dataInput_4 <- reactive({
      for_plots1_2 %>%
        filter(state_abbr == input$state_plot2) %>% 
        mutate(cases_selectedmonth = ifelse(favor_dem == 1, NA, (get(input$month_plot2)/pop_estimate)*100000))
    })
    dataInput_5 <- reactive({
      data_curve %>% 
        filter(state_abbr == input$state_plot5)
    })
    
    
    
    output$distPlot1 <- renderPlot({
        # data_1 <- st_sf(tibble(dataInput_1()))
        ggplot(st_sf(tibble(dataInput_1())), aes(fill = cases_selectedmonth)) +
          geom_sf() +
          scale_fill_gradient(low = "lightblue2",
                              high =  "mediumblue",
                              na.value = "white") +
            theme_bw() +
          labs(title = "Visualization of new COVID-19 cases per month in Democratic counties",
                 fill = "Total number of confirmed cases") +
          theme(axis.text = element_text(size = 8))
    })
    output$distPlot2 <- renderPlot({
      ggplot(st_sf(tibble(dataInput_2())), aes(fill = cases_selectedmonth)) +
                geom_sf() +
          scale_fill_gradient(low = "yellow",
                              high =  "red",
                              na.value = "white") +
                theme_bw() +
                labs(title = "Visualization of new COVID-19 cases per month in Republican counties",
                 fill = "Total number of confirmed cases") +
                theme(axis.text = element_text(size = 8))
    })
    output$distPlot3 <- renderPlot({
      ggplot(st_sf(tibble(dataInput_3())), aes(fill = cases_selectedmonth)) +
        geom_sf() +
        scale_fill_gradient(low = "lightblue2",
                            high =  "mediumblue",
                            na.value = "white") +
        theme_bw() +
        labs(title = "Visualization of new COVID-19 cases per month in Democratic counties",
             fill = "Confirmed cases per 100,000 people") +
        theme(axis.text = element_text(size = 8))
    })
    output$distPlot4 <- renderPlot({
      ggplot(st_sf(tibble(dataInput_4())), aes(fill = cases_selectedmonth)) +
        geom_sf() +
        scale_fill_gradient(low = "yellow",
                            high =  "red",
                            na.value = "white") +
        theme_bw() +
        labs(title = "Visualization of new COVID-19 cases per month in Republican counties",
             fill = "Confirmed cases per 100,000 people") +
        theme(axis.text = element_text(size = 8))
    })
    output$distPlot5 <- renderPlot({
      ggplot(tibble(dataInput_5()), aes(month, newcases_per100k_avg,
                                        color = as.factor(favor_dem))) +
        geom_line(aes(group = favor_dem)) +
        scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9, 10, 11),
                           labels = c("Mar.", "Apr.", "May", "Jun.", "Jul.",
                                      "Aug.", "Sept.", "Oct.", "Nov.")) +
        scale_color_manual(values = c("tomato2", "skyblue1"),
                           labels = c("Republican", "Democrat")) +
        theme_bw() +
        labs(title = "Average monthly COVID-19 cases for counties in the selected state by political party",
             x = "Month of 2020",
             y = "Average COVID-19 cases per 100,000 people",
             color = "Party")
    })
    output$distPlot6 <- renderPlot({
        ggplot(flattencurve_usplot, aes(month, newcases_per100k_avg,
                                        color = as.factor(favor_dem))) +
        geom_line(aes(group = favor_dem)) +
        scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9, 10, 11),
                           labels = c("Mar.", "Apr.", "May", "Jun.", "Jul.",
                                      "Aug.", "Sept.", "Oct.", "Nov.")) +
        scale_color_manual(values = c("tomato2", "skyblue1"),
                           labels = c("Republican", "Democrat")) +
        theme_bw() +
        labs(title = "Average monthly COVID-19 cases for counties in the U.S. by predominant political party",
             x = "Month of 2020",
             y = "Average COVID-19 cases per 100,000 people",
             color = "Party")
    })
    output$table <- render_gt(
      expr = gt_tbl,
      height = px(600),
      width = px(600)
    )
    output$distPlot7 <- renderPlot({
      pp %>% 
        rename(Democratic = `1`,
               Republican = `2`) %>% 
        pivot_longer(cols = Democratic:Republican, 
                     names_to = "Party",
                     values_to = "log_normalized_casesdec4") %>% 
        ggplot(aes(log_normalized_casesdec4, fill = Party)) +
        geom_histogram(aes(y = after_stat(count/sum(count))),
                       alpha = 0.4, 
                       bins = 100, 
                       position = "identity") +
        labs(title = "Posterior Predictive Distribution",
             subtitle = "For Democratic and Republican counties with average population densities",
             x = "COVID-19 Cases per 1000 people",
             y = "Probability") + 
        scale_fill_manual(values = c("skyblue1", "tomato2"),
                          labels = c("Democratic", "Republican")) +
        scale_x_continuous(labels = scales::number_format()) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_classic()
    })
    output$distPlot8 <- renderPlot({
      f1 %>% 
        as_tibble() %>% 
        rename("intercept" = `(Intercept)`) %>% 
        ggplot(aes(x = (get(input$coeff)))) + 
        geom_histogram(aes(y = after_stat(count/sum(count))),
                       bins = 100,
                       fill = "cornflowerblue") +
        labs(title = paste("Posterior Distribution for the Coefficient of", input$coeff),
             y = "Probability",
             x = paste("Coefficient of", input$coeff)) + 
        scale_y_continuous(labels = scales::percent_format()) +
        theme_bw()
    })
    output$Image1 <- renderImage({ 
      list(src = "data_forapp/equation.jpg",
           width = 783,
           height = 46)
    },
    deleteFile = FALSE)
    
    output$distPlot9 <- renderPlot({
      testing %>% 
        group_by(state_lean) %>% 
        mutate(us_avgtesting = mean(totaltestsviral_per1k)) %>% 
        select(us_avgtesting, state_lean) %>% 
        unique() %>% 
        ggplot(aes(state_lean, us_avgtesting, fill = state_lean)) +
        geom_col() +
        scale_fill_manual(values = c("skyblue1", "tomato2"),
                          labels = c("Democratic", "Republican")) +
        labs(title = "Average tests per 1,000 people in each state by political affiliation",
             x = "Political Affiliation of Each State",
             y = "Average Number of PCR Tests per 1,000 People") +
        theme_bw() +
        theme(legend.position = "none") 
    })
    output$distPlot10 <- renderPlot({
      ggplot(testing, aes(state_casesdec4_per1k,totaltestsviral_per1k,
                          color = state_lean,
                          shape = state_lean)) +
        geom_point() +
        geom_smooth(aes(color = state_lean, fill = state_lean), 
                    method = "lm") +
        scale_fill_manual(values = c("skyblue1", "tomato2"),
                          labels = c("Democrat", "Republican")) +
        scale_color_manual(values = c("skyblue1", "tomato2")) +
        labs(title = "Confirmed cases vs. total PCR tests by state",
             fill = "State Political Leaning",
             shape = "State Political Leaning",
             color = "State Political Leaning",
             x = "Confirmed Cases Per 1,000 People for Each State",
             y = "Total PCR Tests Per 1,000 People for Each State") +
        theme_bw() 
    })
    output$distPlot11 <- renderPlot({
      ggplot(testing, aes(deaths_per10k,totaltestsviral_per1k,
                          color = state_lean,
                          shape = state_lean)) +
        geom_point() +
        geom_smooth(aes(color = state_lean, fill = state_lean), 
                    method = "lm") +
        scale_fill_manual(values = c("skyblue1", "tomato2"),
                          labels = c("Democrat", "Republican")) +
        scale_color_manual(values = c("skyblue1", "tomato2")) +
        labs(title = "Deaths vs. total PCR tests by state",
             fill = "State Political Leaning",
             shape = "State Political Leaning",
             color = "State Political Leaning",
             x = "Deaths Per 10,000 People for Each State",
             y = "Total PCR Tests Per 1,000 People for Each State") +
        theme_bw() 
    })
    output$distPlot12 <- renderPlot({
      deaths_usplot %>% 
        ggplot(aes(month, newdeaths_per100k_avg,
                   color = as.factor(favor_dem))) +
          geom_line(aes(group = favor_dem)) +
          scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9, 10, 11),
                             labels = c("Mar.", "Apr.", "May", "Jun.",
                                        "Jul.", "Aug.",
                                        "Sept.", "Oct.", "Nov.")) +
          scale_color_manual(values = c("tomato2", "skyblue1"),
                             labels = c("Republican", "Democrat")) +
        theme_bw() +
        labs(title = "Average monthly COVID-19 deaths for counties in the U.S. by predominant political party",
             x = "Month of 2020",
             y = "Average confirmed COVID-19 deaths per 100,000 people",
             color = "Party")
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)



