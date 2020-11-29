library(boot)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(MASS)
library(pscl)
library(readr)
library(shiny)
library(shinythemes)
library(tidyverse)

combo <- read_csv("ec_ter.csv")

data_map <- read_csv("map.csv")

ui <- navbarPage(
  
  theme = shinytheme("flatly"),
  "Terrorism and Inequality",
  
  tabPanel("Attack Map", 
           fluidPage(
             fluidRow(column(5,
                             h1("Global Terror"),
                             p("In October 2020, the US Department of Homeland 
                               Security named terrorism, both from within and 
                               outside the United States, as one of the gravest 
                               threats to national security. This makes sense, 
                               especially coming from an agency that was created 
                               in direct response to the deadliest terror attack 
                               ever carried out against the US. However, 
                               terrorist organizations have been carrying out 
                               attacks since long before 2001, and only a small 
                               portion of those attacks have been committed on 
                               US soil. The map to the left displays terror 
                               attacks from around the globe since 1970, and 
                               clearly, the threats posed by terrorism are not 
                               going away. The goal of this project is to 
                               evaluate why that is.")),
                      column(7,
                             sliderInput(inputId = "yearInput",
                                         label = "Year",
                                         min = 1970, max = 2018, value = 2018, 
                                         step = 1, ticks = FALSE, 
                                         sep = "", animate = TRUE),
                             leafletOutput(outputId = "mymap")
                             )
                      )
             )
           ),
    
     tabPanel("Economic Model",
              fluidPage(
                fluidRow(column(12, 
                                h3("Economic Inequality"),
                                p(a("Fearon and Laitin", 
                                    href = "https://bit.ly/32WwSAV"), 
                                  "argue that it is not ethnic or religious 
                                  characteristics that explain why insurgencies 
                                  grow - rather, a number of other conditions, 
                                  including the distribution of resources, can 
                                  be used to determine the likelihood of civil 
                                  war. I wanted to see if that logic held true 
                                  when applied to terrorist events. In other 
                                  words, I want to determine whether there is a 
                                  correlation between economic inequality and
                                  the frequency of terrorist events."),
                                h3("Methods"),
                                p("To test my question, I used variables from 
                                  two different data sets. The first, from the
                                  University of Maryland's Global Terrorism
                                  Database, meausres how many terror attacks 
                                  have occurred in every country since 1970.
                                  I had to manipulate this data to show that 
                                  variable, as the raw data simply displayed a
                                  detailed list of terrorist incidents. The
                                  second variable is from the Variants of
                                  Democracy data set, and is called the Equal 
                                  Distribution of Resources Index. This 
                                  measures the extent to which resources are
                                  distributed across countries - the higher a 
                                  country's score, the more economic equality
                                  its citizens experience. To create my model,
                                  I merged the data sets (I had to change
                                  country names that did not match across the 
                                  two), manipulated the terror variable to 
                                  account for population, and ran a 
                                  zero-inflated negative binomal regression on
                                  my two variables. The result is displayed 
                                  below."))),
                fluidRow(column(12, 
                                h3("Model"),
                                h4("Zero-Inflated Negative Binomial Regression 
                                   Between Economic Equality and Terror Attack 
                                   Frequency"),
                                plotOutput("Plot1")),
                         ),
                fluidRow(column(12,
                                h3("Results"))
                         ),
                fluidRow(column(7,
                                p("I found that there is a correlation between 
                                   economic equality and the frequency of terror 
                                   attacks. As the plot demonstrates, countries
                                   with a more equal distribution of resources 
                                   experience less terrorist incidents. The blue
                                   line represents the predicted frequency of 
                                   terror attacks based on economic inequality,
                                   and has a negative slope. The table to the 
                                   right displays some of the key values of my
                                   model. The beta value of -1.8 for econ_equal
                                   means that on average, a country with no
                                   economic inequality would have 1.8 less 
                                   terror attacks per million people than a 
                                   coutnry with complete economic inequality. 
                                   The p-values of my regression are less than
                                   0.001, which means that I can be extremely
                                   confident in my model and the correlation it 
                                   displays.")),
                         column(5,
                                img(src = "Rplot.png", 
                                    height = 300, 
                                    width = 400))
                         )
                )
              ),
  
    tabPanel("Models by Country",
             fluidPage(
               fluidRow(column(12,
                               h3("Results Across Countries"),
                               p("To take my analysis on step further, I 
                                 ran a regression on each country by filtering
                                 my data set. The graph below displayed the
                                 country-specific results of my regression,
                                 along with each model's confidence interval.
                                 Overall, most countries show a negative
                                 trend between economic equality and terror
                                 attack frequency, but the confidence intervals
                                 across countries varies."))),
               fluidRow(column(12,
                               h4("Zero-Inflated Negative Binomial Regression 
                                  Between Economic Equality and Terror Attack 
                                  Frequency by Country"),
                                selectizeInput(inputId = "countryInput",
                                               label = "Country",
                                               choices = unique(combo$country),
                                               selected = "Sudan"),
                                plotOutput("Plot2")
                               )
                        )
                )
              ),
  
     tabPanel("About", 
              fluidPage(
                fluidRow(column(12,
                                h3("Project Background"),
                                p("I decided to choose this topic because I'm 
                                  interested in counterterrorism and what causes 
                                  terrorist activity. I've never studied these 
                                  topics on a big-picture scale, and thought
                                  that this line of inquiry would give me a 
                                  better picture of terrorist activity over 
                                  time and around the globe."),
                                h3("Source Information"),
                                p("I used data from two databases: Variants of 
                                  Democracy and the Global Terrorism Database.
                                  The specific variables that I used are 
                                  outlined on my Models page."),
                                h3("About Me"),
                                p("My name is James Joyce, and I'm a junior 
                                  studying Government. I can be reached at 
                                  james_joyce@college.harvard.edu. Code for this 
                                  project can be found ", 
                                  a("here.", href = "https://github.com/jjoyce915/Final"))
                                )
                         )
                )
              )
  )
                

server <- function(input, output) {
    
    map <- reactive({
        filtered <- data_map %>%
            filter(iyear == input$yearInput)
        })
    
    output$mymap <- renderLeaflet({
        leaflet(map()) %>% 
            setView(lng = -99, lat = 45, zoom = 2)  %>% 
            addTiles() %>% 
            addCircles(data = map(), 
                       lat = ~ latitude, 
                       lng = ~ longitude, 
                       weight = 1, 
                       radius = ~sqrt(nkill)*25000, 
                       label = ~as.character(paste0(attacktype1_txt, " by ", 
                                                    gname, " (", nkill, 
                                                    " deaths)")), 
                       color = "red", 
                       fillOpacity = 0.3)
    })
    
    output$Plot1 <- renderPlot({
        combo %>% 
            ggplot(aes(x = econ_equal, y = attacks_per_mil)) +
            geom_point() +
            geom_smooth(method = MASS::glm.nb, color = "blue", se = TRUE) +
            labs(x = "EDR Index",
                 y = "Number of Terror Attacks (per million people)",
                 caption = "This includes data from 1990 to 2018.") +
            theme_classic()
    })
    
    comboCountry <- reactive({
      filtered <- combo %>%
        filter(country == input$countryInput)
    })
    
    output$Plot2 <- renderPlot({
        ggplot(data = comboCountry(), 
               aes(x = econ_equal, y = attacks_per_mil)) +
        geom_point() +
        geom_smooth(method = MASS::glm.nb, color = "blue", se = TRUE) +
        labs(x = "EDR Index",
             y = "Number of Terror Attacks (per million people)",
             caption = "This includes data from 1990 to 2018.") +
        theme_classic()
    })
    
}

shinyApp(ui = ui, server = server)