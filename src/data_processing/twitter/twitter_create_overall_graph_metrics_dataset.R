library(dplyr)
library(arrow)

rm(list = ls())
gc()


topics <- c("football", "elections")
output_filename <- "./overall_tree_data/twitter_overall_graph_metrics.parquet"

df_total <- tibble()

for (t in topics)
{
  print(paste("Topic:", t))
  
  folder_path <-
    paste("./", 
          t,
          "/",
          "twitter",
          "/trees/graph_analysis_for_each_comment/",
          sep = "")
  
  cat("Working with", folder_path, "folder.\n", sep = " ")
  
  list_files <- list.files(folder_path,
                           full.names = T,
                           "generative_graph_features_for_comment*")
  
  cat("Found", length(list_files), "files.\n", sep = " ")
  
  file_counter <- 1
  for(f in list_files)
  {
    if(file_counter %% 100 == 0)
    {
      print(paste(file_counter, "/", length(list_files)))
    }
    df <- read.csv(f)
    overall_graph_metrics <- df %>% tail(1)
    overall_graph_metrics$topic = t
    overall_graph_metrics$social = "Twitter"
    
    df_total <- plyr::rbind.fill(df_total, overall_graph_metrics)
    
    file_counter = file_counter + 1
  }
}

df_total <- as.data.frame(df_total)
write_parquet(df_total, output_filename)
