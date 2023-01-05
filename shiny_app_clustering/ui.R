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

            # Add widget to specify date range
            dateRangeInput("daterange", "Date range:",
                           start = min(cov_data$date),
                           end = max(cov_data$date),
                           min = min(cov_data$date),
                           max = max(cov_data$date),
                           format = "dd/mm/yy", separator = "-"),
            numericInput(
                "num_clusters", label="Number of Clusters", 
                value = 10, 
                min = 2,
                max = nrow(cov_data)
            ),
            
            numericInput(
                "display_cluster", label="Cluster Number to Display",
                value=1, min=2, max=nrow(cov_data)
            ),
            
            selectInput("countries", "Countries/Regions:", 
                        choices = unique(colnames(new_cases_scaled)),
                        selected = unique(colnames(new_cases_scaled)), multiple = TRUE),
            
        
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Title of the plot ----
            h4("Cluster Cases"),
            
            # Output: plot of new cases ----
            plotlyOutput(outputId = "cluster_case_plot"),
            
        )
    )
)

