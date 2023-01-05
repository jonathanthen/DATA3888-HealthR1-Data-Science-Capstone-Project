library(kohonen)
library(DT)
library(leaflet)
library(spData)
library(dplyr)
library(leaflet.extras)
library(shinydashboard)
library(shinyjs)

set.seed(123)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    # Code to keep:

    observeEvent(input$cluster_vis_datatype, {
      if (input$cluster_vis_datatype == "Stringency Index") {
        shinyjs::disable("standardise_pop_cluster_vis")
        shinyjs::disable("smooth_cluster_vis")
      } else {
        shinyjs::enable("standardise_pop_cluster_vis")
        shinyjs::enable("smooth_cluster_vis")
      }
    })
  
  observeEvent(input$cluster_factor_som, {
    if (input$cluster_factor_som == "Stringency Index") {
      shinyjs::disable("standardise_pop_som")
      shinyjs::disable("smooth_som")
    } else {
        shinyjs::enable("standardise_pop_som")
        shinyjs::enable("smooth_som")
    }
  })

    to_plot_stats <- reactive({
        to_plot <- ""
        if (input$standardise_pop & input$smooth) {
        to_plot <- "new_confirmed_pop_smooth"

        } else if (input$standardise_pop) {
            to_plot <- "new_confirmed_pop"
        } else if (input$smooth) {
            to_plot <- "new_confirmed_smooth"
        } else {
          to_plot <- "new_confirmed"
        }
        to_plot
    })

    output$new_cases_plot <- renderPlotly({
        
        countries <- input$countries

        ggplot(cov_data %>% filter(
          country_name %in% countries, 
          date <= input$daterange[2], 
          date >= input$daterange[1]
          ), 
          aes_string(x = "date", y = to_plot_stats(), group = "country_name", color = "country_name")) +
            geom_line(lwd = 1) +
            theme_bw() +
            ylab("Number of new cases") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(color = "Country/Region") +
            xlab("")
    })

    output$stringency_plot <- renderPlotly({
        
        countries <- input$countries
         ggplot(cov_data %>% filter(
           country_name %in% countries, 
           date <= input$daterange[2], 
           date >= input$daterange[1]
           ), 
           aes(x = date, y = stringency_index, group = country_name, color = country_name)) +
            geom_line(lwd = 1) +
            theme_bw() +
            ylab("Government Stringency Index") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(color = "Country/Region") +
            xlab("")
    })
    

    to_plot <- reactive({
        to_plot <- ""
        if (input$cluster_factor_som == "Stringency Index") {
          to_plot <- "stringency_index"
        }
        else if (input$standardise_pop_som & input$smooth_som) {
          to_plot <- "new_confirmed_pop_smooth"
        } else if (input$standardise_pop_som) {
            to_plot <- "new_confirmed_pop"
        } else if (input$smooth_som) {
            to_plot <- "new_confirmed_smooth"
        } else {
          to_plot <- "new_confirmed"
        }

        to_plot
    })
    
    to_plot_cluster_vis <- reactive({
      to_plot <- ""
      if (input$cluster_vis_datatype == "Stringency Index") {
        to_plot <- "stringency_index"
      } else if (input$standardise_pop_cluster_vis & input$smooth_cluster_vis) {
        to_plot <- "new_confirmed_pop_smooth"
      } else if (input$standardise_pop_cluster_vis) {
        to_plot <- "new_confirmed_pop"
      } else if (input$smooth_cluster_vis) {
        to_plot <- "new_confirmed_smooth"
      } else {
        to_plot <- "new_confirmed"
      }
      
      to_plot
    })

    output$som_clusters <- renderPlot({
      som_model_to_use <- som_model[[to_plot()]]
      som_cluster <- cutree(hclust(dist(som_model_to_use$model$codes[[1]])), 6)

      ## TODO make an adjustemnt for countries_cluster_view
      filtered_labels <- c()
      X <- as.vector(sort(unique(som_model_to_use$mappings$country_name)))
      for (i in 1:length(X)) {
        if (X[i] %in% input$countries_cluster_view) { filtered_labels[i] <- X[i] }
        else { filtered_labels[i] <-"." }
      }
      
      plot(som_model_to_use$model, type="mapping", 
        main="Cluster Overview",
          bgcol = rainbow(6)[som_cluster],
          labels=filtered_labels)
      add.cluster.boundaries(som_model_to_use$model, som_cluster)
      


        # # Visualising cluster results
        # som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 6)

        # plot(som_model, type="mapping",  main = "Clusters",
        #     bgcol = rainbow(6)[som_cluster]) 
        # add.cluster.boundaries(som_model, som_cluster)

    })

    output$som_cluster_members <- shiny::renderDataTable({
      ## Read input from cluster6_cache

      cluster_df <- cluster6_cache[cluster6_cache$value_col == to_plot(),
      c("som_cluster", "country_name", "som_node")]
        cluster_df %>% arrange(som_cluster)

    },
    options = list(
      autoWidth = TRUE, scrollX = TRUE, pageLength = 5)
    )

    output$som_cluster_confirmed_plot <- renderPlotly({
        cluster_df <- cluster6_cache[cluster6_cache$value_col == to_plot(), ]

        CLUST_NUM = input$cluster_to_plot
        country_list <- cluster_df[cluster_df$som_cluster == CLUST_NUM, "country_name"]

        df_to_use <- cov_data %>% 
            filter(country_name %in% country_list, date <= input$daterange[2], date >= input$daterange[1])
        
        # TODO bug somewhere in geom_line. Warning: Error in order: argument 1 is not a vector
        ggplot(df_to_use,
            aes_string(x = "date", y = to_plot(), group = "country_name", color = "country_name")) +
            geom_line(lwd = 1) +
            theme_bw() +
            ylab("Number of new cases") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(color = "Country/Region") +
            xlab("")
    })
    
    output$som_cluster_subgroup_map <- renderPlotly({
      
      cluster_df <- cluster6_cache[cluster6_cache$value_col == to_plot_cluster_vis(), ]
      
      cluster_df <- merge(cluster_df, index_subgroups, by.x="country_name", by.y="country_name")
      
      CLUST_NUM = input$cluster_to_plot_subgroup
      country_list <- cluster_df[cluster_df$som_cluster == CLUST_NUM, "country_name"]
      
      df_to_use <- cluster_df %>%
        filter(country_name %in% country_list)
      
      world_map <- map_data("world")
      
      # setdiff(unique(cov_data$country_name), unique(world_map$region))
      world_map <- world_map %>%
        mutate(region = replace(region, region == "UK","United Kingdom")) %>%
        mutate(region = replace(region, region == "USA","United States of America"))
      
      
      SUBGROUP = input$subgroup_option
      
      if(SUBGROUP=="GDP per capita") {
        breaks <- c(0, 10^c(1:8))
        ## max(covid_full$total_cases, na.rm=TRUE) < max(breaks) ## Make sure this return TRUE
        
        options(scipen=999) ## Force R not to use exponential notation.
        world_map_with_data <- merge(world_map, df_to_use,
                                     by.x = "region", by.y = "country_name",
                                     all.x = TRUE)

        world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]
        # 
        # reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Paired")
        # names(reds_col) <- levels(world_map_with_data$som_cluster)
        
        world_map_with_data$gdp_per_capita_category <- 
          cut(as.numeric(world_map_with_data$gdp_per_capita),
              breaks,include.lowest = TRUE, right = FALSE, dig.lab=10)
        
        reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Greens")
        names(reds_col) <- levels(world_map_with_data$gdp_per_capita_category)
        
        p <- ggplot(world_map_with_data,
                    aes(x = long, y = lat, group = group, fill = gdp_per_capita_category,
                        text = paste('Country: ', region,
                                     '<br>GDP per capita:', gdp_per_capita)
                    )) +
          theme(legend.position = "bottom") +
          guides(fill=guide_legend(title="")) +
          geom_polygon() +
          scale_fill_manual(values = reds_col, na.value="gray93") +
          theme_void() +
          xlab("") + ylab("")
        
        ggplotly(p, tooltip = "text", height = 400) %>%
          layout(xaxis = list(visible=FALSE), 
                 yaxis = list(visible=FALSE), 
                 legend = list(orientation = "h", x = 0.1, y = -0.01))
      } else if(SUBGROUP=="Political system") {
        breaks <- c(0, 1^c(1:12))
        ## max(covid_full$total_cases, na.rm=TRUE) < max(breaks) ## Make sure this return TRUE
        
        options(scipen=999) ## Force R not to use exponential notation.
        world_map_with_data <- merge(world_map, df_to_use,
                                     by.x = "region", by.y = "country_name",
                                     all.x = TRUE)
        world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]
        
        reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Paired")
        names(reds_col) <- levels(world_map_with_data$som_cluster)
        
        p <- ggplot(world_map_with_data,
                    aes(x = long, y = lat, group = group, fill = as.factor(constitutional_form),
                        text = paste('Country: ', region,
                                     '<br>Constitutional_form:', str_wrap(constitutional_form,10))
                    )) +
          theme(legend.position = "bottom") +
          guides(fill=guide_legend(title="")) +
          geom_polygon() +
          scale_fill_manual(values = reds_col, na.value="gray93") +
          theme_void() +
          xlab("") + ylab("")
        
        ggplotly(p, tooltip = "text", height = 400) %>%
          layout(xaxis = list(visible=FALSE), 
                 yaxis = list(visible=FALSE), 
                 legend = list(orientation = "h", x = 0.1, y = -0.01))
      } else if(SUBGROUP=="Population") {
        breaks <- c(1, 5, 10, 50, 100, 500, 1000, 1500)
        ## max(covid_full$total_cases, na.rm=TRUE) < max(breaks) ## Make sure this return TRUE
        
        options(scipen=999) ## Force R not to use exponential notation.
        world_map_with_data <- merge(world_map, df_to_use,
                                     by.x = "region", by.y = "country_name",
                                     all.x = TRUE)
        
        world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]
        # 
        # reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Paired")
        # names(reds_col) <- levels(world_map_with_data$som_cluster)
        
        world_map_with_data$population_million<- world_map_with_data$population/1000000
        
        world_map_with_data$population_million_category <- 
          cut(as.numeric(world_map_with_data$population_million),
              breaks,include.lowest = TRUE, right = FALSE, dig.lab=10)
        
        reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Greens")
        names(reds_col) <- levels(world_map_with_data$population_million_category)
        
        p <- ggplot(world_map_with_data,
                    aes(x = long, y = lat, group = group, fill = population_million_category,
                        text = paste('Country: ', region,
                                     '<br>Population:', population,
                                     '<br>Population (million):', population_million)
                    )) +
          theme(legend.position = "bottom") +
          guides(fill=guide_legend(title="")) +
          geom_polygon() +
          scale_fill_manual(values = reds_col, na.value="gray93") +
          theme_void() +
          xlab("") + ylab("")
        
        ggplotly(p, tooltip = "text", height = 400) %>%
          layout(xaxis = list(visible=FALSE), 
                 yaxis = list(visible=FALSE), 
                 legend = list(orientation = "h", x = 0.1, y = -0.01))
      }
      
      
    })
    
    output$som_cluster_world_map <- renderPlotly({

      cluster_df <- cluster6_cache[cluster6_cache$value_col == to_plot_cluster_vis(), ]

      CLUST_NUM = input$cluster_to_plot
      country_list <- cluster_df[cluster_df$som_cluster == CLUST_NUM, "country_name"]

      df_to_use <- cov_data %>%
        filter(country_name %in% country_list, date <= input$daterange[2], date >= input$daterange[1])

      world_map <- map_data("world")

      # setdiff(unique(cov_data$country_name), unique(world_map$region))
      world_map <- world_map %>%
        mutate(region = replace(region, region == "UK","United Kingdom")) %>%
        mutate(region = replace(region, region == "USA","United States of America"))

      breaks <- c(0, 1^c(1:12))
      ## max(covid_full$total_cases, na.rm=TRUE) < max(breaks) ## Make sure this return TRUE

      options(scipen=999) ## Force R not to use exponential notation.
      world_map_with_data <- merge(world_map, cluster_df,
                                   by.x = "region", by.y = "country_name",
                                   all.x = TRUE)
      world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]

      reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Paired")
      names(reds_col) <- levels(world_map_with_data$som_cluster)

      p <- ggplot(world_map_with_data,
             aes(x = long, y = lat, fill = as.factor(som_cluster), group = group, 
                 text = paste('Country: ', region,
                 '<br>SOM cluster number:', som_cluster)
                 )) +
        theme(legend.position = "bottom") +
        geom_polygon() +
        scale_fill_manual(values = reds_col, na.value="gray93") +
        theme_void() +
        xlab("") + ylab("")

      ggplotly(p, tooltip = "text", height = 400) %>%
        layout(xaxis = list(visible=FALSE), 
               yaxis = list(visible=FALSE), 
               legend = list(orientation = "h", x = 0.1, y = -0.01))
    })
    
    #filter data for new cases depending on selected date
    filteredData <- reactive({
        cov_data_filtered <- cov_data[cov_data$date == input$dateSel, ]
        world_spdf$Cases <- cov_data_filtered$new_confirmed[match(world_spdf$ISO2, cov_data_filtered$key)]
        
        cov_data_filtered_1 <- cov_data[cov_data$date == input$dateSel, ]
        world_spdf$Stringency <- cov_data_filtered_1$stringency_index[match(world_spdf$ISO2, cov_data_filtered_1$key)]
        
        # TODO integrate -- not super clear on how to do this
        ##  cluster_df <- cluster6_cache[cluster6_cache$value_col == to_plot(), ]
        
        cluster_df <- cluster6_cache[cluster6_cache$value_col == "new_confirmed", ]
        
        CLUST_NUM = input$cluster_to_plot
        country_list <- cluster_df[cluster_df$som_cluster == CLUST_NUM, "country_name"]
        
        world_spdf$Cluster <- cluster_df$som_cluster[match(world_spdf$ISO2, cluster_df$key)]
        
        world_spdf
    })
    
    
    #create the base leaflet map
    output$map <- renderLeaflet({
      #define colorpalette for chart legend
      
      leaflet(world_spdf) %>% 
        addTiles() %>%
        addFullscreenControl() %>%
        setView(lat = 0, lng = 0, zoom = 2) %>%
        
        addPolygons( 
          layerId = ~ISO2,
          fillColor = "lightgray",
          stroke = TRUE, 
          fillOpacity = 1, 
          color = "white", 
          weight = 1
        ) %>% 
        
        #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
        leaflet::addLegend(pal = colorPalette, values = cov_data$new_confirmed, opacity = 0.9, title = "New Cases", position = "bottomleft") %>% 
        leaflet::addLegend(pal = colorPalette2, values = cov_data$stringency_index, opacity = 0.9, title = "Stringency Index", position = "bottomleft") %>%
        leaflet::addLegend(pal = colorPalette1, values = world_spdf$Cluster, opacity = 0.9, title = "Cluster", position = "bottomright", labFormat = function(type, cuts, p) {  # Here's the trick
          paste0(paletteBins1)}) 
      
    })
    
    observeEvent(input$dataType, {
      if (input$dataType == "Stringency Index") {
        shinyjs::disable("standardise_pop_cluster_vis")
        shinyjs::disable("smooth_cluster_vis")
      } else {
        shinyjs::enable("standardise_pop_cluster_vis")
        shinyjs::enable("smooth_cluster_vis")
      }
      
      
    })
    
    # prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
    observe({
      
      world_spdf$Cases <- filteredData()$Cases
      world_spdf$Cluster <- filteredData()$Cluster
      world_spdf$Stringency <- filteredData()$Stringency
      
      world_spdf@data$LabelText <- paste0(
        "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
        "<b>Population:</b> ", format(world_spdf@data$Population, nsmall=0, big.mark=","), "<br>",
        "<b>Constitution Form:</b> ", format(world_spdf@data$Constitution, nsmall=0, big.mark=","), "<br>",
        # "<b>Total Confirmed Cases:</b> ", format(world_spdf@data$TotalCase, nsmall=0, big.mark=","), "<br>",
        "<b>New Confirmed Cases:</b> ", format(world_spdf@data$Cases, nsmall=0, big.mark=","), "<br>",
        "<b>Stringency:</b> ", format(world_spdf@data$Stringency, nsmall=0, big.mark=","),"<br>",
        "<b>Cluster Group:</b> ", format(world_spdf@data$Cluster, nsmall=0, big.mark=","))

      if (input$dataType == "New Cases") {
        
        if(input$mapType == "Bubble"){
          
          leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            setShapeStyle(layerId = ~ISO2, fillColor = "lightgray", label = world_spdf$LabelText) %>%
            addCircleMarkers(lng = ~LON,
                             lat = ~LAT,
                             radius = ~log(Cases) * 2,
                             weight = 1,
                             opacity = 1,
                             color = ~ifelse(Cases > 0, "black", "transparent"),
                             fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "transparent"),
                             fillOpacity = 0.8)
        }
        
        if (input$mapType == "Choropleth") {
          
            leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "lightgray"), label = world_spdf$LabelText)
        }  
        
      } else if (input$dataType == "Stringency Index") {
        
        if(input$mapType == "Bubble"){
          
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>%
          setShapeStyle(layerId = ~ISO2, fillColor = "lightgray", label = world_spdf$LabelText) %>%
          addCircleMarkers(lng = ~LON,
                           lat = ~LAT,
                           radius = ~log(Stringency) * 2,
                           weight = 1,
                           opacity = 1,
                           color = ~ifelse(Stringency > 0, "black", "transparent"),
                           fillColor = ~ifelse(Stringency > 0, colorPalette2(Stringency), "transparent"),
                           fillOpacity = 0.8)
        
        } else if (input$mapType == "Choropleth") {
          
            leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(Stringency > 0, colorPalette2(Stringency), "lightgray"), label = world_spdf$LabelText)
        } 
      }  
      

      if (input$clusterSel == TRUE) {
        
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>%
          setShapeStyle(layerId = ~ISO2, fillColor = "lightgray") %>%
          addPolygons(
            layerId = ~ISO2,
            fillColor = ~colorPalette1(Cluster),
            stroke = TRUE,
            fillOpacity = 1,
            color = "white",
            weight = 1, label = ~lapply(LabelText, htmltools::HTML))
      }

      # if (input$clusterSel == FALSE) {
      #   leafletProxy("map", data = filteredData()) %>%
      #     clearMarkers() %>%
      #     addPolygons(
      #       layerId = ~ISO2,
      #       fillColor = "lightgray",
      #       stroke = TRUE,
      #       fillOpacity = 1,
      #       color = "white",
      #       weight = 1)
      # }

    })
    
    #--------------------------------------------------------TO BE USED IF SILHOUETTE METHOD IS IN SHINY APP
    # NEW CASES - ELBOW METHOD
    # output$cluster_cases_elbow <- renderPlot({
    #   fviz_nbclust(new_cases_matrix_norm, FUN = hcut, method = "wss")
    # })
    # 
    # #NEW CASES - SILHOUETTE METHOD
    # output$cluster_cases_silhouette <- renderPlot({
    #   fviz_nbclust(new_cases_matrix_norm, FUN = hcut, method = "silhouette")
    # })
    # 
    # #STRINGENCY INDEX - ELBOW METHOD
    # output$cluster_stringency_elbow <- renderPlot({
    #   fviz_nbclust(stringency_matrix_norm, FUN = hcut, method = "wss")
    # })
    # 
    # #STRINGENCY INDEX - SILHOUETTE METHOD
    # output$cluster_stringency_silhouette <- renderPlot({
    #   fviz_nbclust(stringency_matrix_norm, FUN = hcut, method = "silhouette")
    # })
    #--------------------------------------------------------
    
    
    #REACTIVE - CLUSTER NUMBER CONFIRMED CASES & STRINGENCY INDEX
    choose_cluster_number_pse <- reactive({
      input$choose_cluster_number_pse
    })
    
    to_plot_cluster_pse <- reactive({
      to_plot <- ""
      if (input$choose_cluster_pse == "Stringency Index") {
        to_plot <- "stringency_index"
      } else if (input$standardise_pse & input$smooth_pse) {
        to_plot <- "new_confirmed_pop_smooth"
      } else if (input$standardise_pse) {
        to_plot <- "new_confirmed_pop"
      } else if (input$smooth_pse) {
        to_plot <- "new_confirmed_smooth"
      } else {
        to_plot <- "new_confirmed"
      }
      
      to_plot
    })
    
    output$gdpimage <- renderImage({
      return(list(
        src = "www/gdplegend.jpg",
        contentType = "image/jpg"
      ))
    }, deleteFile = FALSE)
    
    #POLITICAL, SOCIAL, ECONOMIC - NEW CASES OR STRINGENCY INDEX
    output$pse_graph <- renderPlotly({
    
      if (input$choose_cluster_pse == "New Confirmed Cases") {
        pse_graph = cov_clusters %>%
                    filter(value_col == to_plot_cluster_pse()) %>%
                    filter(som_cluster == choose_cluster_number_pse()) %>%
                    group_by(country_name) %>%
                    mutate(max_total = max(total_confirmed),
                           max_stringency = max(stringency_index),
                           perc_infected_pop = max_total/population) %>%
                    arrange(desc(gdp_per_capita)) %>%
                    ggplot(aes(country_name = country_name, x=perc_infected_pop, y=max_stringency, size=gdp_per_capita, color=constitutional_form)) +
                    geom_point(alpha=1) +
                    scale_size(range = c(.5, 12), name="Constitutional Form") + 
                    scale_color_brewer(palette = "Paired") + 
                    xlab("Percentage of Infected Population") +
                    ylab("Maximum Stringency") +
                    labs(color = "Constitutional Form") +
                    scale_x_continuous(limits = c(0, 0.2)) + 
                    scale_y_continuous(limits = c(0, 100))
        
      } else {
        pse_graph = cov_clusters %>%
                    filter(value_col == "stringency_index") %>%
                    filter(som_cluster == choose_cluster_number_pse()) %>%
                    group_by(country_name) %>%
                    mutate(max_total = max(total_confirmed),
                           max_deceased = max(total_deceased),
                           perc_infected_pop = max_total/population,
                           perc_death_pop = max_deceased/population) %>%
                    arrange(desc(gdp_per_capita)) %>%
                    ggplot(aes(country_name = country_name, x=perc_death_pop, y=perc_infected_pop, size=gdp_per_capita, color=constitutional_form)) +
                    geom_point(alpha=1) +
                    scale_size(range = c(.5, 12), name="Constitutional Form") + 
                    scale_color_brewer(palette = "Paired") + 
                    xlab("Percentage of Deceased Population") +
                    ylab("Percentage of Infected Population") +
                    labs(color = "Constitutional Form")
                }
  
      ggplotly(pse_graph)
    })
      
      ## MAIN DASHBOARD PAGE
      
      output$newCaseBox <- renderValueBox({
        cov_data_1 <- cov_data %>% filter(date == max(date))
        valueBox(
          paste0(prettyNum(sum(cov_data_1$new_confirmed), big.mark = ",", scientific = FALSE)), "Daily New Confirmed Cases", icon = icon("virus"), color = "orange"
        )
      })
      
      output$newDeathBox <- renderValueBox({
        cov_data_1 <- cov_data %>% filter(date == max(date))
        valueBox(
          paste0(prettyNum(sum(cov_data_1$new_deceased), big.mark = ",", scientific = FALSE)), "Daily New Deaths", icon = icon("skull-crossbones"), color = "red"
        )
      })
      
      
      output$dateBox <- renderValueBox({
        valueBox(
          paste0(max(cov_data$date)), "Date Last Updated", icon = icon("calendar-day"), color = "purple"
        )
      })
      
      # source: https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
      
      hc1 <- hchart(cov_data_new, "area", hcaes(date, total_confirmed), name = "total cases")  %>% 
        hc_size(height = 100) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      hc2 <- hchart(cov_data_new, "area", hcaes(date, total_deceased), name = "total deceased")  %>% 
        hc_size(height = 100) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      tcBox <- valueBoxSpark(
        value = prettyNum(max(cov_data_new$total_confirmed), big.mark = ",", scientific = FALSE),
        title = toupper("Total Confirmed Cases"),
        sparkobj = hc1,
        subtitle = tagList(HTML("&uarr;"), "0.57% Since last day"),
   #     info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
        icon = icon("virus"),
        width = 4,
        color = "blue",
        href = NULL
      )
      
      tdBox <- valueBoxSpark(
        value = prettyNum(max(cov_data_new$total_deceased), big.mark = ",", scientific = FALSE),
        title = toupper("Total Confirmed Deaths"),
        sparkobj = hc2,
        subtitle = tagList(HTML("&uarr;"), "0.39% Since last day"),
  #      info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
        icon = icon("skull-crossbones"),
        width = 4,
        color = "navy",
        href = NULL
      )
      
      output$totalCaseBox <- renderValueBox(tcBox)
      
      output$totalDeathBox <- renderValueBox(tdBox)
      
      # PRELOAD TABS
      outputOptions(output, "new_cases_plot", suspendWhenHidden = FALSE)
      outputOptions(output, "stringency_plot", suspendWhenHidden = FALSE)
      outputOptions(output, "som_clusters", suspendWhenHidden = FALSE)
      outputOptions(output, "som_cluster_members", suspendWhenHidden = FALSE)
      outputOptions(output, "som_cluster_confirmed_plot", suspendWhenHidden = FALSE)
      outputOptions(output, "som_cluster_subgroup_map", suspendWhenHidden = FALSE)
      outputOptions(output, "som_cluster_world_map", suspendWhenHidden = FALSE)
      outputOptions(output, "map", suspendWhenHidden = FALSE)
      
      output$image <- renderImage({
        return(list(
          src = "www/banner.png",
          contentType = "image/png",
          alt = "Banner"
        ))
      }, deleteFile = FALSE)
    
}

