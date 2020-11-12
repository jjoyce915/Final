library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)


d1 <- read_csv("ireland.csv")

combo <- read_csv("ec_ter.csv")
combo$terror_attacks %>% 
    replace_na(0)
    

ui <- navbarPage(
    "Final Project Title",
    
    
    
    tabPanel("Home", 
             titlePanel("Welcome to my Page!"),
             h3("Navigating This Page"),
             p("oof, the new model page needs a lotta work, but its a start")),
    
    
    
     tabPanel("Northern Ireland Model",
              
             fluidPage(
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectizeInput("yearInput", "Year",
                                        choices = unique(d1$iyear),
                                        selected = "1970", multiple = FALSE)
                         
                         
                         
                     ),
                 
                 mainPanel(
                     h3("Terror Attacks Committed by Paramilitary Organizations
                 in Northern Ireland"),
                     plotOutput("myplot")
                     
                 )
             )
           )
        ),
    
    
    tabPanel("NEW MODEL",
             
             fluidPage(
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectizeInput("countryInput", "Country",
                                        choices = unique(combo$country),
                                        selected = "Saudi Arabia", multiple = FALSE)
                         
                         
                         
                     ),
                     
                     mainPanel(
                         h3("Terror Attacks and Economic Inequality by Year"),
                         plotOutput("myplot2")
                         
                     )
                 )
             )
    ),
             
             
             tabPanel("About", 
                      titlePanel("About"),
                      h3("Project Background"),
                      p("Changed my dataset to one that details EVERY (!) terror attack committed in the world since like 1970."),
                      h3("About Me"),
                      p("My name is James and I study Government.  
             You can reach me at james_joyce@college.harvard.edu. I accept Venmo.")
                      )
     )
                 


server <- function(input, output) {
    
    x <- reactive({
        
        filtered <- d1 %>%
            filter(iyear == input$yearInput)
        
    })
    
    output$myplot <- renderPlot({
        
        ggplot(x(), aes(x = gname,
                       y = count,
                       fill = attacktype1_txt)) +
            geom_col() +
            labs(x = "Organization",
                 y = "Amount of Terror Attacks") +
            
            scale_fill_discrete(name = "Type of Attack") +
            
            theme(axis.text.x = element_text(angle = 90))
    
        
        
    })
    
    y <- reactive({
        
        filtered <- combo %>%
            filter(country == input$countryInput)
        
    })
    
    output$myplot2 <- renderPlot({
        
        ggplot(y(), aes(x = year)) +
            geom_line(aes(y = eq_dist, color = "red")) +
            geom_line(aes(y = terror_attacks, color = "blue")) +
            labs(x = "Year") +
            
            theme(axis.text.x = element_text(angle = 90))
        
        
        
    })
    
    
}

shinyApp(ui = ui, server = server)