#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)

# If you want to update the data, set it as TRUE. 
# Or read csv directly from the link of the data
update_data <- FALSE


cov_data <- read.csv("../epi_gov_data_cleaned.csv")

cov_data$date <- as.Date(cov_data$date)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("COVID-19 visualisation"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the country ----
            # Here choices include all the location in the covid_data; and we will set
            # Australia as default country
            selectInput("countries", "Countries/Regions:", 
                        choices = unique(cov_data$country_name),
                        selected = "Australia", multiple = TRUE),
            
            # Add widget to specify date range
            dateRangeInput("daterange", "Date range:",
                           start = min(cov_data$date),
                           end = max(cov_data$date),
                           min = min(cov_data$date),
                           max = max(cov_data$date),
                           format = "mm/dd/yy", separator = "-"),
            
            "Type of Government Response",
            checkboxInput("facial", "Facial Coverings", value = TRUE),
            checkboxInput("school", "School Closures", value = TRUE),
            checkboxInput("work", "Workplace Closures", value = TRUE),
            checkboxInput("work", "Public Transport Closing", value = FALSE),
            checkboxInput("trace", "Contact Tracing", value = FALSE),
            checkboxInput("test", "Testing Policy", value = FALSE),
            checkboxInput("events", "Cancel Public Events", value = FALSE),
            checkboxInput("gather", "Restrictions on Gatherings", value = FALSE),
            checkboxInput("home", "Stay at Home Requirements", value = FALSE),
            checkboxInput("internal", "Restrictions on Internal Movements", value = FALSE),
            checkboxInput("campaign", "Public Information Campaign", value = FALSE),
            checkboxInput("international", "International Travel Controls", value = FALSE),
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Title of the plot ----
            h4("New cases"),
            
            # Output: plot of new cases ----
            plotlyOutput(outputId = "new_cases_plot"),
            
            h4("Government Response: Facial Coverings"),
            plotlyOutput(outputId = "gov_res_mask"),
            
            h4("Government Response: School Closures"),
            plotlyOutput(outputId = "gov_res_school"),
            
            h4("Government Response: Workplace Closures"),
            plotlyOutput(outputId = "gov_res_work"),
            
            h4("Government Response: Public Transport Closing"),
            plotlyOutput(outputId = "gov_res_transport"),
            
            h4("Government Response: Contact Tracing"),
            plotlyOutput(outputId = "gov_res_trace"),
            
            h4("Government Response: Testing Policy"),
            plotlyOutput(outputId = "gov_res_test"),
            
            h4("Government Response: Cancel Public Events"),
            plotlyOutput(outputId = "gov_res_events"),
            
            h4("Government Response: Restrictions on Gatherings"),
            plotlyOutput(outputId = "gov_res_gather"),
            
            h4("Government Response: Stay at Home Requirements"),
            plotlyOutput(outputId = "gov_res_home"),
            
            h4("Government Response: Restrictions on Internal Movements"),
            plotlyOutput(outputId = "gov_res_internal"),
            
            h4("Government Response: Public Information Campaign"),
            plotlyOutput(outputId = "gov_res_campaign"),
            
            h4("Government Response: International Travel Controls"),
            plotlyOutput(outputId = "gov_res_int"),
            
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    
    # Plot of the new cases with selected countries ----
    # This expression that generates a plot is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$new_cases_plot <- renderPlotly({
        
        countries <- input$countries

        ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = new_confirmed, group = country_name, color = country_name)) +
            geom_line(lwd = 1) +
            theme_bw() +
            ylab("Number of new cases") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(color = "Country/Region") +
            xlab("")
        
    })
    
    output$gov_res_mask <- renderPlotly({
        
        countries <- input$countries
        
        if (input$facial) {
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = facial_coverings, group = country_name, color = country_name)) +
                geom_line(lwd = 1)+ 
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })    
    
    output$gov_res_school <- renderPlotly({
        
        countries <- input$countries
        
        if (input$school) {
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = school_closing, group = country_name, color = country_name)) +
                geom_line(lwd = 1) + 
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
        
    })   
    
    output$gov_res_work <- renderPlotly({
        
        countries <- input$countries
        
        if (input$school) {
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = workplace_closing, group = country_name, color = country_name)) +
                geom_line(lwd = 1) + 
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
        
    })   
    
    output$gov_res_trace <- renderPlotly({

        countries <- input$countries
        
        if (input$trace) {
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = contact_tracing, group = country_name, color = country_name)) +
                geom_line(lwd = 1) + 
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }


    })
    
    output$gov_res_test <- renderPlotly({
        
        countries <- input$countries
        
        if (input$test) {
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = testing_policy, group = country_name, color = country_name)) +
                geom_line(lwd = 1) + 
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
        
    })

    output$gov_res_home <- renderPlotly({

        countries <- input$countries
        
        if (input$home) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = stay_at_home_requirements, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }


    })
    
    output$gov_res_int <- renderPlotly({
        
        countries <- input$countries
        
        if (input$international) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = international_travel_controls, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })
    
    output$gov_res_events <- renderPlotly({
        
        countries <- input$countries
        
        if (input$international) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = cancel_public_events, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })
    
    output$gov_res_gather <- renderPlotly({
        
        countries <- input$countries
        
        if (input$international) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = restrictions_on_gatherings, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })
    
    
    output$gov_res_internal <- renderPlotly({
        
        countries <- input$countries
        
        if (input$international) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = restrictions_on_internal_movement, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })
    
    
    output$gov_res_campaign <- renderPlotly({
        
        countries <- input$countries
        
        if (input$international) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = public_information_campaigns, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })
    
    
    output$gov_res_transport <- renderPlotly({
        
        countries <- input$countries
        
        if (input$international) {
            ggplot(cov_data %>% filter(country_name %in% countries), aes(x = date, y = public_transport_closing, group = country_name, color = country_name)) +
                geom_line(lwd = 1) +
                theme_bw() +
                ylab("Value") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(color = "Country") +
                xlab("")
        }
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
