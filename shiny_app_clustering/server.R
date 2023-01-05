# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    
    # Plot of the new cases with selected countries ----
    # This expression that generates a plot is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$cluster_case_plot <- renderPlotly({
        
        countries <- input$countries
        df_covid <- cov_data[colnames(cov_data) %in% countries,] %>% filter( 
                                        date <= input$daterange[2], 
                                        date >= input$daterange[1]
                                        )
        
        scaled_covid <- df_covid %>% dplyr::select(-date) %>% apply(2, min_max_scale)
        
        
        new_cases_scaled_dist <- dist(t(scaled_covid), method = "manhattan")
        hclust_scaled_res <- hclust(new_cases_scaled_dist, method="ward.D2")
        hclust_scaled_cluster <- cutree(hclust_res, k = input$num_clusters) %>% as.factor %>% as.data.frame 
        colnames(hclust_scaled_cluster) = "clust_num"
        
        
        # Dataframe for ggplot visualisation
        df_covid_scaled <- reshape2::melt(scaled_covid)
        colnames(df_covid_scaled) <- c("date", "country_name", "new_confirmed_scaled")
        df_covid_scaled$date <- as.Date(df_covid$date)
        df_covid_scaled$cluster <-  hclust_scaled_cluster[as.character(df_covid_scaled$country_name), 1]
        
        
        ggplot() +
          geom_line(data = df_covid_scaled[df_covid_scaled$cluster == input$display_cluster,], 
                    aes(x = date, y = new_confirmed_scaled, color = country_name)) +
          facet_wrap(~cluster, ncol = 1) +  ## argument scale allow you to adjust whether to have the same y-axis or not
          theme_bw() + ylab("Normalised values") + labs(title = "COVID new cases (scaled)")
        
        
    })
    

    
    
}