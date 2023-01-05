library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(shiny)
library(forecast)



## Apply min-max scaling to the data
min_max_scale = function(series) {
  return ((series - min(series))/(max(series) - min(series)))
}


cov_data = read.csv("../new_cases_matrix.csv")%>% 
  dplyr::select(-Turkmenistan, -Kiribati, -Tonga)

cov_data$date = as.Date(cov_data$date)


