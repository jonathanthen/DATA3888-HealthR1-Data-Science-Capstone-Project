library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


set.seed(123)

header <- dashboardHeader(title = strong("COVID-19"))


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Statistics", icon = icon("chart-area"), tabName = "statistics"),
        menuItem("Clusters", icon = icon("shapes"), tabName = "clusters"),
        menuItem("Cluster Visualisation", icon = icon("eye"), tabName = "cluster_vis"),
        menuItem("PSE Factors", icon = icon("file", lib = "glyphicon"), tabName = "PSE"),
        menuItem("Interactive Map", icon = icon("globe"), tabName = "map"),
        menuItem("About", icon = icon("info-circle"), tabName = "about")
    )
)

body <- dashboardBody(
    shinyjs::useShinyjs(),
  
    tabItems(
        tabItem(tabName = "dashboard",
                
                tags$head(tags$style(
                    type="text/css",
                    "#image img {max-width: 100%; width: 100%;}")),
                
                
                imageOutput("image", height = "auto"),

                h3("Global Statistics"),

                fluidRow(
                    valueBoxOutput("newCaseBox"),

                   valueBoxOutput("newDeathBox"),

                   valueBoxOutput("dateBox")
               ),
               
               fluidRow(
                   valueBoxOutput("totalCaseBox", width = 6),
                   valueBoxOutput("totalDeathBox", width = 6)
               )
        ),
        
        tabItem(tabName = "statistics",
                
                fluidRow(
                    box(title = "New Cases", width = 6, plotlyOutput(outputId = "new_cases_plot")),
                    box(title = "Government Stringency", width = 6, plotlyOutput(outputId = "stringency_plot"))
                ),
                
                fluidRow(
                    
                    box(title = "Data Filters", status = "primary", width = 8,
                        
                        column(width=6, 
                               selectInput("countries", "Countries/Regions",
                                           choices = unique(cov_data$country_name),
                                           selected = "Australia", multiple = TRUE),
                               
                               # Add widget to specify date range
                               dateRangeInput("daterange", "Date range",
                                              start = min(cov_data$date),
                                              end = max(cov_data$date),
                                              min = min(cov_data$date),
                                              max = max(cov_data$date),
                                              format = "mm/dd/yy", separator = "to")
                               ),
                        
                        column(width=6,
                              
                               
                               
                               strong("Standardisation"),
                               checkboxInput("standardise_pop", "Standardise New Cases Against Population", value = FALSE),
                               
                               strong("Smoothing"),
                               checkboxInput("smooth", "Smooth New Cases", value = FALSE))
                               )
                    )
        ),
        
        tabItem(tabName = "clusters",
                
                fluidRow(
                    box(title = "SOM Cluster Cases", width = 12, plotlyOutput("som_cluster_confirmed_plot"))
                    
                ),
                
                fluidRow(
                    box(title = "Data Filters", status = "primary", width = 4, 

                        radioButtons(inputId = "cluster_factor_som",
                            label = "Factor to Cluster On",
                            choices = c("New Cases", "Stringency Index"),
                            selected = "New Cases",
                            inline = TRUE),
                        
                        strong("Standardisation"),
                        checkboxInput("standardise_pop_som", "Standardise New Cases and Deaths Against Population", value = FALSE),
                        
                        strong("Smoothing"),
                        checkboxInput("smooth_som", "Smooth New Cases and Deaths", value = FALSE),
                        
                        # sliderInput("num_som_clusters", "Number of SOM Clusters", 1, 12, 6),
                        sliderInput("cluster_to_plot", "Number of the Cluster to Plot", value=1, min=1, max=6)   
                    ),
                    
                    box(title = "Cluster Members", width = 4, 
                        shiny::dataTableOutput("som_cluster_members")
                    )
                    ,
                
                box(title="Country Filter", status="primary", width=4,
                    strong("Subset of Countries to view in Cluster Overview"),
                    selectInput("countries_cluster_view", "Countries to Display",
                                choices = unique(cov_data$country_name),
                                selected = c(
                                  "Australia", "South Korea", "China", "New Zealand",
                                  "United States of America", "United Kingdom", "Iran", "Japan", "France",
                                  "Germany", "India", "Canada", "Sweden"
                                ), multiple = TRUE),
                    
                )),
                
                fluidRow(
                    box(width = 12, title = "Cluster Overview",
                        plotOutput("som_clusters"),
                        
                        tags$a(icon("question-circle")), "The Silhouette Score recommended 6 clusters."
                    )
                )
            ),
        
        tabItem(tabName = "cluster_vis",

                fluidRow(
                    box(title = "SOM Cluster World Map", width = 6, height = 500, plotlyOutput("som_cluster_world_map"),
                        tags$a(icon("question-circle")), "This is a world map based on the time series data of new confirmed cases. "),
                    box(title = "SOM Cluster Subgroup Map", width = 6, plotlyOutput("som_cluster_subgroup_map"),
                        tags$a(icon("question-circle")), "This is a map of the countries in the selected cluster grouped into subgroups by their GDP per capita ($USD), population (in millions) or political system.")
                ),
            
                fluidRow(
                    box(title = "Data Filters", status = "primary", width = 8,
                        
                        column(width=6, 
                               
                               radioButtons(inputId = "cluster_vis_datatype",
                                            label = "Data Type",
                                            choices = c("New Cases", "Stringency Index"),
                                            selected = "New Cases",
                                            inline = TRUE),
                               
                               strong("Standardisation"),
                               checkboxInput("standardise_pop_cluster_vis", "Standardise New Cases and Deaths Against Population", value = FALSE),
                               
                               strong("Smoothing"),
                               checkboxInput("smooth_cluster_vis", "Smooth New Cases and Deaths", value = FALSE)
                        ),
                        
                        column(width=6,
                               # sliderInput("num_som_clusters", "Number of SOM Clusters", 1, 12, 6),
                               sliderInput("cluster_to_plot_subgroup", "Number of the Cluster to Plot", value=1, min=1, max=6),
                               selectInput("subgroup_option", "Cluster Subgroup Parameter", 
                                           choices = c("GDP per capita", "Population", "Political system"),
                                           selected = "GDP per capita", multiple = FALSE)
                               
                        )

                    ),

                   )
        ),
        
        tabItem(tabName = "PSE",
                
                fluidRow(
                    box(title = "Plotting Clusters Against PSE Factors", width = 9, plotlyOutput("pse_graph"),
                        tags$a(icon("question-circle")), "This graph shows the countries in each cluster group plotted against political, social and economic factors."),
                    box(width = 3, imageOutput("gdpimage", height = "auto"))

                ),
                
                
                fluidRow(
                    box(title = "Data Filters", status = "primary", width = 12,
                        column(width = 5,
                               selectInput("choose_cluster_pse", "Choose Cluster Variable to Plot:", c("New Confirmed Cases", "Stringency Index"), multiple = FALSE, selected = "New Confirmed Cases"),
                               conditionalPanel(
                                 condition = "input.choose_cluster_pse == 'New Confirmed Cases'",
                                 strong("Additional Options:"),
                                 checkboxInput("standardise_pse", "Standardise New Cases Against Population"),
                                 checkboxInput("smooth_pse", "Apply Smoothing")
                                               )
                               ),
                        column(width = 5, offset = 1,
                               sliderInput("choose_cluster_number_pse", "Choose Cluster Number:", value=1, min=1, max=6)
                              )
                          
                        ),
                )
                
                ),
        
        tabItem(tabName = "map", leafletjs,
                
                tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),

                         leafletOutput("map", width = "100%", height = "100%"),
                         absolutePanel(id = "controls", class = "panel panel-default",
                                       style = "background-color: white;
                      	                        opacity: 0.85;
                      	                        padding: 20px 20px 20px 20px;
                      	                        margin: auto;
                      	                        border-radius: 5pt;
                      	                        box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                padding-bottom: 2mm;
                                                padding-top: 1mm;",
                                       h4("Data Filter"),
                                       
                                       prettyRadioButtons(inputId = "dataType",
                                                          label = "Data Type",
                                                          choices = c("New Cases", "Stringency Index"),
                                                          selected = "New Cases",
                                                          inline = TRUE),
                                       
                                       prettyRadioButtons(inputId = "mapType",
                                                    label = "Map Type",
                                                    choices = c("Bubble", "Choropleth"),
                                                    selected = "Bubble",
                                                    inline = TRUE),

                                       top = 100, left = 300, width = 350, fixed=TRUE,
                                       draggable = TRUE, height = "auto",
                                       sliderInput("dateSel", "Date",
                                                min = min(cov_data$date),
                                                max = max(cov_data$date),
                                                value = min(cov_data$date),
                                                step = 7,
                                                timeFormat = "%d %b %y",
                                                animate = animationOptions(interval = 500, loop = FALSE)),

                                       prettyCheckbox("clusterSel", "Show Clusters", value = FALSE),
                                       
                                       tags$a(icon("question-circle")), "This is an interactive world map which displays the following as either a bubble or choropleth plot (depending on the data filter): new cases over time, 
                                                stringency over time, and cluster groups. ")


                
                
                ),
        
        tabItem(tabName = "about",
                
                "This Shiny App was curated by group HealthR_1 of the DATA3888 cohort.", tags$br(),
                a("Project Github Page", href="https://github.sydney.edu.au/dwan8443/DATA3888_HealthR_1"),
                
                h3("Purpose/Problem"),

                "\"What is the relationship between new cases or government response with various political, social and economic (PSE) factors?\" The specific PSE factors investigated for each country include government system, population and GDP per capita.",
                
                h3("Motivation"),
                "The motivation of this project is to understand the impact of PSE factors on how countries respond to the COVID pandemic, in order to understand how these countries will respond to crises in the future.",
                
                h3("Target Audience"),
                "Social and Political scientists in industry and academia:",

                tags$div(tags$ul(
                  tags$li("Social and political science researchers"),
                  tags$li("Social epidemiologists"),
                  tags$li("The United Nations"),
                  tags$li("The World Bank"),
                  tags$li("The Institute for Health Metrics and Evaluation (IHME)"),
                  tags$li("The Sydney Social Sciences and Humanities Advanced Research Centre")
                  )),
                
                
                h3("Data Sources"),
                tags$div(tags$ul(
                  tags$li(a("COVID-19 open data collected by Google Cloud Platform", href="https://github.com/GoogleCloudPlatform/covid-19-open-data"), " on Github (COVID new cases and stringency data, country GDP and population data)"),
                  tags$li(a("Historical Atlas of the Twentieth Century", href="http://users.erols.com/mwhite28/20centry.htm"),"(government system data)")
                )),
                

                h3("Methodology/Analysis"),

                h4("Data Cleaning"),

                "Due to the high variability in Covid cases data, we add the optionality to standardise by population and smooth out daily data to get a more representative picture of trends in cases. ",
                
                br(), br(),

                "Stringency Index has not been processed further, as it is already standardised.",
                "Due to the inelastic nature of government policy",
                "(i.e. we don’t expect it to change every day), the data has also been left unsmoothed. ",

                br(), br(),

                h4("Clustering"),


                "The data was reshaped into a timeseries for each country, and clustered using Self-Organising Maps (SOM). ",
                "SOMs are a form of artificial neural network which identifies similarity in features and groups data points by distance. ",
                "The final SOM model used hexagonal topography and a 4x4 grid, which is the hyperparameter set that enabled roughly 5-10 observations in each node. ",

                br(), br(),

                "Using various clustering distance scores such as Silhouette coefficient, WSS, and the GAP statistic, ",
                "we found 6 clusters to be the next best candidate, excluding the uninformative k=2. ",
                "Based on a hierarchical clustering model generated from the SOM, we combined the 16 nodes into 6 clusters to give the final groupings. "

                ))
                    
                
                
    
)

ui <- dashboardPage(title="COVID-19 Visualisation",header, sidebar, body, skin = "green")
