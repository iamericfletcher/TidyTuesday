#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Libraries
library(DT)
library(dplyr)
library(stringr)
library(rsconnect)
library(tidytuesdayR)
library(shiny)
library(readr)

#Data
#tuesdata <- tidytuesdayR::tt_load(2020, week = 39)
peaks <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

peaks <- peaks %>% 
    filter(!is.na(first_ascent_country),
           climbing_status != "Unclimbed") %>% 
    #first_ascent_year == 201) %>% 
    mutate(first_ascent_year = case_when(
        first_ascent_year == 201 ~ 2018,
        TRUE ~ as.numeric(.$first_ascent_year)
    ))

country_names <- sort(
    unique(trimws(unlist(strsplit(peaks$first_ascent_country, ','))))
)


#Shiny
ui <- fluidPage(
    titlePanel(HTML(paste("Himalayan Climbing Expeditions: 1909 - 2019", "Which Country Was the First to the Summit?", sep = "<br/>"))),
    
    fluidPage(
        fluidRow(
            column(4,
                   selectInput("first" ,
                               label = "Select Country or Countries:",
                               choices = c("All", country_names),
                               selected = "All",
                               multiple = TRUE)))
                                        
            ),

        DT::dataTableOutput("table"),
    span(textOutput("message"), style="color:black")
)

server <- function(input, output) {
    output$message <- renderText({"Data: The Himalayan Database | Author: Eric Fletcher"})
    # Filter data based on selections
    output$table <- renderDataTable(datatable({
        data <- peaks
        
        if (any(input$first != "All")) {
            #data <- data[data$first_ascent_country == input$first,]
            data <- data %>% 
                filter(str_detect(first_ascent_country,
                                  str_c(input$first, collapse = "|")))
        }
        data
    }
    ))
    
}
shinyApp(ui, server)