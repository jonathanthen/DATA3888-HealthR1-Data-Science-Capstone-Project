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
        
        if (input$standardise_pop) {
          ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = new_confirmed_pop, group = country_name, color = country_name)) +
            geom_line(lwd = 1) +
            theme_bw() +
            ylab("Number of new cases") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(color = "Country/Region") +
            xlab("")
        } else {
          ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = new_confirmed, group = country_name, color = country_name)) +
            geom_line(lwd = 1) +
            theme_bw() +
            ylab("Number of new cases") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(color = "Country/Region") +
            xlab("")
        }

        
        
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
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = international_travel_controls, group = country_name, color = country_name)) +
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
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = cancel_public_events, group = country_name, color = country_name)) +
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
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = restrictions_on_gatherings, group = country_name, color = country_name)) +
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
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = restrictions_on_internal_movement, group = country_name, color = country_name)) +
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
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = public_information_campaigns, group = country_name, color = country_name)) +
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
            ggplot(cov_data %>% filter(country_name %in% countries, date <= input$daterange[2], date >= input$daterange[1]), aes(x = date, y = public_transport_closing, group = country_name, color = country_name)) +
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

    ##Jonathan: New & Deaths, Stringency Difference, Stringency Index

      output$new_confirmed_cases <- renderPlot({
    countries <- input$countries
    
    ggplot(ntd_data %>% filter(country_name %in% countries,
                              date <= input$daterange[2],
                              date >= input$daterange[1]),
      aes(x = date, y = new_confirmed, 
      group = country_name, color = country_name)) +
      geom_line(lwd = 1) +
      theme_bw() +
      ylab("Number of New Confirmed Cases") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(color = "Country/Region") +
      xlab("Date") +
      geom_vline(xintercept=as.numeric(final_data$date[indnew]), linetype=4, colour = "red")
    
  })










    ## Serena:  Lag time
    # CCF plots
    output$ccf_lag <- renderPlotly({
        countries <- input$countries
        
        subset_cov_data = cov_data %>% filter(
                country_name %in% countries, 
                date <= input$daterange[2], 
                date >= input$daterange[1]
            )

        cases <- ts(subset_cov_data$new_confirmed)
        stringency <- ts(subset_cov_data$stringency_index)

        ggCcf(
            cases, stringency,
            main="Cases vs Stringency ACF Lag", 
            xlab="Lag (days)"
            )
        
    })
    

    output$rolling_corr <- renderPlotly({
        subset_cov_data = cov_data %>% filter(
            country_name %in% countries[0], 
            date <= input$daterange[2], 
            date >= input$daterange[1]
        )

        plot_rolling_corr(input$ndays_rolling, subset_cov_data)

    })


    
    
}