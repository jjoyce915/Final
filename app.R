library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(ggplot2)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(rstanarm)

combo <- read_csv("ec_ter.csv")

data_map <- read_csv("map.csv")

stanglam <- stan_glm(attacks_per_mil ~ econ_equal,
                     data = combo,
                     refresh = 0,
                     family = gaussian())
    
ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Terror Attack Frequency",
  
  tabPanel("Map of Past Incidents", 
           fluidPage(
             fluidRow(column(5,
                             h1("Global Terror"),
                             p("In October 2020, the US Department of Homeland Security
                               named terrorism, both from within and outside the United
                               States, as one of the gravest threats to national security.
                               This makes sense, especially coming from an agency that
                               was created in direct response to the deadliest terror
                               attack ever carried out against the US. However, terrorist
                               organizations have been carrying out attacks since long
                               before 2001, and only a small portion of those attacks have
                               been committed on US soil. The map to the left displays
                               terror attacks from around the globe since 1970, and clearly, 
                               the threats posed by terrorism are not going away. The goal of 
                               this project is to evaluate why that is.")),
                      column(7,
                             selectizeInput("yearInput", "Year",
                                            choices = unique(data_map$iyear),
                                            selected = "2018", 
                                            multiple = FALSE),
                             leafletOutput(outputId = "mymap")
                             )
                      )
             )
           ),
    
     tabPanel("Regression",
              h1("An Economic Correlation?"),
              p(""),
              mainPanel(plotOutput("Plot1")),
              h3("Economic Inequality"),
              p(a("Fearon and Laitin", href = "https://www.cambridge.org/core/journals/american-political-science-review/article/ethnicity-insurgency-and-civil-war/B1D5D0E7C782483C5D7E102A61AD6605"), 
                "argue that it is not ethnic or religious characteristics that
                explain why insurgencies grow - rather, a number of other 
                conditions, including poverty, can be used to determine the 
                likelihood of civil war. I wanted to see if that logic held
                true when applied to terrorist events."),
              h3("Methods"),
              p("Because it is difficult to directly measure poverty, I decided
                to test whether the frequency of terror attacks is correlated 
                with economic equality. To do this, I used a variable from the 
                Variants of Demcoracy dataset called the Equal Distribution of 
                Resources Index, which measures the extent to which resources
                are distributed across countries. The higher a country's score, 
                the more economic equality its citizens experience."),
              h3("Data"),
              p("To create this model, I merged the VDem dataset - which contained
                the Index score and population information, with the University of
                Maryland's Global Terrorism Database data. The counntries did not
                always neatly match up between the two, so I had to eliminate
                states that didn't have an exact equivalent, and merge those that 
                did but had different names."),
              h3("Results"),
              p("I found that there is a correlation between economic equality and
                the frequency of terror attacks. As the plot demonstrates, countries
                with a more equal distribution of resources experience less 
                terrorist incidents.")
              ),
    
     tabPanel("About", 
             h3("Project Background"),
             p("I decided to choose this topic because I'm interested in 
               counterterrorism and what causes terrorist activity. I've never
               studied these topics on a big-picture scale, and thought
               that this line of inquiry would give me a better picture of 
               global terrorism activity."),
             h3("Source Information"),
             p("I used data from two databases: Variants of Democracy and the
               Global Terrorism Database."),
             h3("About Me"),
             p("My name is James Joyce, and I'm a junior studying Government. I
               can be reached at james_joyce@college.harvard.edu. Code for this 
               project can be found ", a("here.", href = "https://github.com/jjoyce915/Final")))
  
  
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
                       popup = ~as.character(gname, attacktype1_txt, nkill), 
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
            geom_line(aes(y = fitted(stanglam)), color = "blue") +
            labs(title = "Regression Between Economic Equality and Terror Attack Frequency",
                 subtitle = "There is a correlation between economic equality and terror attacks", 
                 x = "Equal Distribution of Resources Index Score",
                 y = "Number of Terror Attacks (per million people)",
                 caption = "This includes data from 1990 to 2018.") +
            ylim(0, 10) +
            theme_classic()
    })
}

shinyApp(ui = ui, server = server)