library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("COVID-19 visualisation 22222"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the country ----
            # Here choices include all the location in the covid_data; and we will set
            # Australia as default countryulation
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
            
            "Standardisation",
            checkboxInput("standardise_pop", "Standardise New Cases/Deaths Against Population", value = FALSE),
            
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
        
            numericInput(
                "ndays_rolling", label="Rolling Window (days)", 
                value = 30, 
                min = 0
            )
        
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
            
            h4("CCF Lag"),
            plotlyOutput(outputId="ccf_lag"),
            
            h4("Rolling Correlation"),
            plotlyOutput(outputId="rolling_corr"),

            #h4("New Confirmed Cases"),
            #plotlyOutput(outputId="new_confirmed_cases"),

            #h4("New Confirmed Deaths"),
            #plotlyOutput(outputId="new_confirmed_deaths"),

            #h4("Stringency Difference"),
            #plotlyOutput(outputId="stringency_difference"),

            #h4("Stringency Index"),
            #plotlyOutput(outputId="stringency_index")

        )
    )
)

