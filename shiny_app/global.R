library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(shiny)
library(forecast)


update_data <- FALSE

cov_data <- read.csv("../epi_gov_data_cleaned.csv")
cov_data$date <- as.Date(cov_data$date)

demog_data <- read.csv("../demographics.csv")

cov_data <- merge(cov_data, demog_data[, c("key", "population")], by=c("key","key"))

cov_data$new_confirmed_pop <- cov_data$new_confirmed/(cov_data$population/1000000)
cov_data$new_deceased_pop <- cov_data$new_deceased/(cov_data$population/1000000)

cov_data[is.na(cov_data)] <- 0 #Fill NAs with 0s
cov_data$new_confirmed <- replace(cov_data$new_confirmed, cov_data$new_confirmed < 0, 0) #Replace negative values with 0s
cov_data$new_deceased <- replace(cov_data$new_deceased, cov_data$new_deceased < 0, 0) #Replace negative values with 0s

ntd_data = cov_data %>% select(date, country_name, new_confirmed, total_confirmed, new_deceased, total_deceased, stringency_index) %>%
    mutate(stringency_diff = stringency_index - lag(stringency_index, default = first(stringency_index)))

#Replace NaN and Infinity to 0
ntd_data[sapply(ntd_data, simplify = 'matrix', is.nan)] <- 0
ntd_data[sapply(ntd_data, simplify = 'matrix', is.infinite)] <- 0

#New Cases and New Deaths
maxnew = max(ntd_data$new_confirmed) 
indnew = which(ntd_data$new_confirmed == maxnew)

maxdeath = max(ntd_data$new_deceased)
inddeath = which(ntd_data$new_deceased == maxdeath)

plot_rolling_corr = function(ndays=30, df) {
  df$date = as.Date(df$date)
  roll_corr_calc = rollapply(df[c("new_confirmed", "stringency_index")],
          width=ndays, 
          function(x) cor(x[,1], x[,2]), 
          by.column=FALSE)
  date_ndays = df$date[ndays:nrow(df)]
  
  roll_corr_df = data.frame(list(date=date_ndays, roll_corr = roll_corr_calc))
  
  ggplot() +
  geom_point(data = roll_corr_df, aes(x=date_ndays, y=roll_corr_calc), color="blue") +
      geom_hline(yintercept=0, color='lightgrey') + 
  theme_minimal() + 
  ggtitle(paste("Rolling correlation over past", ndays, "days"))

}
