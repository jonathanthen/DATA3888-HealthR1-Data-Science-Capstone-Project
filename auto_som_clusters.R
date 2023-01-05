## Build and generate SOM models + clusters for each value_col

set.seed(123)


generate_som <- function(VALUE_COL) {
  
  # Read in smoothed standardised data
  data <- read.csv("data/data_smoothed_standardised.csv")
  data$date <- as.Date(data$date)
  
  # Filter for the value col and turn into a matrix
  data2 <- data[c("date", "country_name", VALUE_COL)]
  country_matrix <- dcast(data2, country_name ~ date, value.var=VALUE_COL)
  country_matrix[is.na(country_matrix)] <- 0
  
  rownames(country_matrix) <- country_matrix$country_name
  country_matrix <- subset(country_matrix, select=-country_name)
  
  ## Run an som model on the country matrix 
  som_model <- som(
    as.matrix(country_matrix), 
    grid=somgrid(xdim = 4, ydim=4, 
                 topo="hexagonal"), 
    rlen=100, 
    alpha=c(0.05,0.01), 
    keep.data = TRUE,)
  
  # Cut at 6 neighbours and join with og names
  som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 6)
  country_clusters <- data.frame(
    rownames(country_matrix), som_model$unit.classif
  )
  ab <- data.frame(som_cluster)
  ab$node_name = sapply(rownames(ab), substring, 2,) 
  c6_cases_norm <- merge(country_clusters, ab, by.x="som_model.unit.classif", by.y="node_name")
  colnames(c6_cases_norm) <- c("som_node", "country_name", "som_cluster")
  c6_cases_norm$country_name <- gsub("\\.", " ",
                                     c6_cases_norm$country_name)
  
  # Write mappings to a csv
  write.csv(c6_cases_norm,   paste("data/c6_clusters_RERUN_", VALUE_COL, ".csv", sep=""))
  
  plot(som_model, type="mapping",  main = "Clusters",
       bgcol = rainbow(6)[som_cluster],
       cex=0.6
       , labels=rownames(country_matrix)
  )
  add.cluster.boundaries(som_model, som_cluster)
  
  # Write model to an rds
  final_results <- list(
    mappings = c6_cases_norm,
    model = som_model
  )
  saveRDS(final_results,  paste("data/c6_RERUN_", VALUE_COL, ".rds", sep=""))
  
}


VALUE_COL = "stringency_index"
generate_som(VALUE_COL)
