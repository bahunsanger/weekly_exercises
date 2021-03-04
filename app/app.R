

library(shiny)
library(tidyverse)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

covid19_states <- covid19 %>% 
    select(state) %>% 
    distinct(state) %>% 
    arrange(state) %>% 
    pull(state)
    
    
ui <- fluidPage("US Covid19 Cases",
                selectInput(inputId = "state",
                            label = "Choose a State",
                            choices = covid19_states,
                            multiple = TRUE),
                plotOutput(outputId = "covid19"))

   
server <- function(input, output) 
    {output$covid19 <- renderPlot(covid19 %>% 
                                filter(state %in% input$state) %>% 
                                mutate(cases_20 = cases > 20) %>% 
                                ggplot(aes(x = date, y = cases_20, color = state)) +
                                geom_line() +
                                scale_y_log10() +
                                theme_tufte() +
                                labs(title = "Cumulative Covid 19 Cases", x = "", y = ""))}

  
shinyApp(ui = ui, server = server)
