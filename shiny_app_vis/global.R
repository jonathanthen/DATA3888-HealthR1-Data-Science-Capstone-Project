library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(shiny)
library(forecast)
# library(maditr)
library(reshape2)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(highcharter)
library(maps)

# """

# Plan
# - Import data (cases, stringency, PSE factors)
# - Apply smoothing (col in ^)
# - Cluster picking? -- vis silhouette coeffs 
# - SOM on cases + stringency
# - WOrld map SOM clusters
# - Bubble chart SOM clusters + GDP


# """
set.seed(123)

cov_data <- read.csv("data/data_smoothed_standardised.csv")
# cov_data <- readRDS("../data/cov_data.rds")
cov_data$date <- as.Date(cov_data$date)

cluster6_cache <- read.csv("data/c6_country_clusters_cache.csv")

index_subgroups <- read.csv("data/index_subgroups.csv")

cov_clusters <- read.csv("data/merged_covid_clusters.csv")

new_confirmed_rds <- readRDS("data/c6_RERUN_new_confirmed.rds")
new_confirmed_pop_rds <- readRDS("data/c6_RERUN_new_confirmed_pop.rds")
new_confirmed_pop_smooth_rds <- readRDS("data/c6_RERUN_new_confirmed_pop_smooth.rds")
new_confirmed_smooth_rds <- readRDS("data/c6_RERUN_new_confirmed_smooth.rds")
stringency_index_rds <- readRDS("data/c6_RERUN_stringency_index.rds")


som_model <- list(
  new_confirmed = new_confirmed_rds,
  new_confirmed_pop = new_confirmed_pop_rds,
  new_confirmed_pop_smooth = new_confirmed_pop_smooth_rds,
  new_confirmed_smooth = new_confirmed_smooth_rds,
  stringency_index = stringency_index_rds
)


cov_data_new <- cov_data %>%
  select(date, total_confirmed, total_deceased) %>%
  group_by(date) %>%
  summarise_each(funs(sum))

#---------------------

## TODO : remove
# key_matrix <- cov_data[c("date", "key", value_col)] %>% 
#   dcast(key ~ date, value.var=value_col)
# key_matrix[is.na(key_matrix)] <- 0
# rownames(key_matrix) <- key_matrix$key
# key_matrix <- subset(key_matrix, select = -key)

#---------------------------------------- TO BE USED IF SILHOUETTE INSIDE SHINY APP
# #STRINGENCY MATRIX (JON)
# stringency_matrix <- cov_data %>% 
#   dplyr::select(country_name, date, stringency_index) %>%
#   pivot_wider(names_from = country_name, values_from = stringency_index) %>%
#   arrange(date) %>% 
#   replace(is.na(.), 0) %>%
#   as.data.frame()
# 
# rownames(stringency_matrix) <- stringency_matrix$date
# stringency_matrix <- stringency_matrix %>% dplyr::select(-date)
# stringency_matrix_norm <- apply(stringency_matrix, 2, function(x) x/sum(x))
# 
# #NEW CASES MATRIX (JON)
# new_cases_matrix <- cov_data %>% 
#   dplyr::select(country_name, date, new_confirmed) %>% #SILHOUETTE METHOD WAS BASED ON NEW_CONFIRMED
#   pivot_wider(names_from = country_name, values_from = new_confirmed) %>%
#   arrange(date) %>% 
#   replace(is.na(.), 0) %>%
#   as.data.frame()
# 
# rownames(new_cases_matrix) <- new_cases_matrix$date
# new_cases_matrix <- new_cases_matrix %>% dplyr::select(-date)
# new_cases_matrix_norm <- apply(new_cases_matrix, 2, function(x) x/sum(x))
#----------------------------------------

### FOR INTERACTIVE PLOT - source / credits: https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1


world_spdf <- readOGR( 
  dsn = getwd() , 
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)


world_spdf[is.na(world_spdf$Cases)] <- 0

world_spdf$Population <- cov_data$population[match(world_spdf$ISO2, cov_data$key)]
world_spdf$Constitution <- cov_data$constitutional_form[match(world_spdf$ISO2, cov_data$key)]
world_spdf$TotalCase <- cov_data$total_confirmed[match(world_spdf$ISO2, cov_data$key)]

world_spdf@data$LabelText <- paste0(
  "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
  "<b>Population:</b> ", format(world_spdf@data$Population, nsmall=0, big.mark=","), "<br>",
  "<b>Constitution Form:</b> ", format(world_spdf@data$Constitution, nsmall=0, big.mark=","), "<br>",
  # "<b>Total Confirmed Cases:</b> ", format(world_spdf@data$TotalCase, nsmall=0, big.mark=","), "<br>",
  "<b>New Confirmed Cases:</b> ", format(world_spdf@data$Cases, nsmall=0, big.mark=","), "<br>",
  "<b>Stringency:</b> ", format(world_spdf@data$Stringency, nsmall=0, big.mark=","),"<br>",
  "<b>Cluster Group:</b> ", format(world_spdf@data$Cluster, nsmall=0, big.mark=","))

paletteBins <- c(0, 10, 100, 1000, 10000, 100000, 1000000)
colorPalette <- colorBin(palette = "YlOrBr", domain = cov_data$new_confirmed, na.color = "transparent", bins = paletteBins)

paletteBins1 <- c(1, 2, 3, 4, 5, 6, 7)
colorPalette1 <- colorBin(palette = "Spectral", domain = cov_data$new_confirmed, na.color = "transparent", bins = paletteBins1)

paletteBins2 <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
colorPalette2 <- colorBin(palette = "YlOrBr", domain = cov_data$stringency_index, na.color = "transparent", bins = paletteBins2)


#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)

### FOR MAIN DASHBOARD

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  # info_icon <- tags$small(
  #   tags$i(
  #     class = "fa fa-info-circle fa-lg",
  #     title = info,
  #     `data-toggle` = "tooltip",
  #     style = "color: rgba(255, 255, 255, 0.75);"
  #   ),
  #   # bs3 pull-right 
  #   # bs4 float-right
  #   class = "pull-right float-right"
  # )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      # if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}
