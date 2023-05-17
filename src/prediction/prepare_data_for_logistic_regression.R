library(dplyr)
library(lubridate)
library(arrow)
# Reset
rm(list = ls())
gc()

set.seed(42)
args = commandArgs(trailingOnly=TRUE)
args = c("youtube", "elections", "shuffled")

if (length(args) < 2) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==2) {
  mode = "original"
} else {
  social_name = args[1]
  topic_name = args[2]
  mode = args[3]

  if(mode == "shuffled")
  {
    shuffled_label = "shuffled_"
  } else if(mode == "original") {
    shuffled_label = ""
  } else {
    stop("mode parameter must be valorized with original or shuffled values")
  }
}

if(social_name == "youtube")
{
  graph_folder <-
  paste("/media/gabett/Volume/data-repository/panconesi-football-elections", 
        topic_name,
        social_name,
  paste("/trees/", shuffled_label, "graph_analysis_for_each_comment", sep = ""),
  sep = "/")
  
  output_folder <- 
    paste("/media/gabett/Volume/data-repository/panconesi-football-elections", 
          topic_name,
          social_name,
          "trees",
          sep = "/")
  
  classes = c(
    "video_id" = "character",
    "id" = "character",
    "parent_id" = "character",
    "created_at" = "character",
    "root" = "character",
    "tree_size" = "numeric",
    "max_width" = "numeric",
    "max_depth" = "numeric",
    "number_of_unique_users" = "numeric",
    "toxicity_ratio" = "numeric",
    "assortativity" = "numeric",
    "avg_toxicity_distance" = "numeric",
    "wiener_index" = "numeric",
    "toxicity_score" = "numeric"
  )
  
  text_pattern = "*generative_graph_features_for_video*"
} else {
graph_folder <-
  paste("/media/gabett/Volume/data-repository/panconesi-football-elections/", 
        topic_name,
        "/",
        social_name,
  paste("/trees/", shuffled_label, "graph_analysis_for_each_comment/", sep = ""),
  sep = "")

output_folder <- 
  paste("/media/gabett/Volume/data-repository/panconesi-football-elections/", 
        topic_name,
        social_name,
        "trees",
        sep = "/")

classes = c("video_id" = "character",
  "id" = "character",
  "parent_id" = "character",
  "created_at" = "character",
  "root" = "character",
  "tree_size" = "numeric",
  "max_width" = "numeric",
  "max_depth" = "numeric",
  "number_of_unique_users" = "numeric",
  "toxicity_ratio" = "numeric",
  "assortativity" = "numeric",
  "avg_toxicity_distance" = "numeric",
  "wiener_index" = "numeric",
  "toxicity_score" = "numeric"
  )

text_pattern = "*generative_graph_features_for_conversation*"
}

graph_files <- list.files(graph_folder,
                          full.names = T, 
                          pattern = text_pattern)

dt_all_graph_comments = tibble()
dt_temp = tibble()

print(graph_folder)
print(output_folder)

# Unify graph iterative data ####
i = 1 #which(file == graph_files) + 1
for (file in graph_files[i: length(graph_files)])
{
  dt_graph <- read.csv(
    file,
    colClasses = classes
  ) %>%
    mutate(created_at = as_datetime(created_at),
           is_toxic = ifelse(toxicity_score > 0.6, TRUE, FALSE),
           social = social_name,
           topic = topic_name)
  
  dt_temp = plyr::rbind.fill(dt_temp, dt_graph)
  
  if(i == 100)
  {
    print(which(file == graph_files))
    
    dt_all_graph_comments = plyr::rbind.fill(dt_all_graph_comments, dt_temp)
    i = 0
    dt_temp = tibble()
  }

    i = i + 1
}

dt_all_graph_comments = plyr::rbind.fill(dt_all_graph_comments, dt_temp) %>% 
  distinct(id, .keep_all = T)

output_filename <-
  paste(output_folder, "/", shuffled_label, social_name, "_", topic_name, "_all_graphs_unified.parquet", sep = "")
write_parquet(dt_all_graph_comments, output_filename)
