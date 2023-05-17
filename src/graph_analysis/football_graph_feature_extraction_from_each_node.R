# Libraries and reset ####
library(tibble)
library(dplyr)
library(igraph)
library(jsonlite)
library(stringr)
library(lubridate)

rm(list = ls())
gc()
set.seed(42)

# Functions ####
create_graph <- function(edge_list)
{
  g = graph_from_data_frame(edge_list[c("parent_id", "id")])
  V(g)$created_at = edge_list$created_at
  V(g)$toxicity_score = edge_list$toxicity_score
  V(g)$is_toxic = ifelse(edge_list$toxicity_score >= threshold, 1, 2) # 1 true, 2 false
  V(g)$author_id = edge_list$author_id
  V(g)$text = edge_list$text
  g = delete_vertices(g, "NA")
  
  if (is.igraph(g) == FALSE)
  {
    stop("The graph is not a proper one.")
  }
  
  return(simplify(g))
}

tree_size <- function(g)
{
  return(gorder(g))
}

tree_depth <- function(g, root)
{
  df_eccentricity = as.data.frame(eccentricity(g)) %>%
    filter(rownames(.) == root)
  tree_depth_from_root = df_eccentricity$`eccentricity(g)`
  
  return(tree_depth_from_root)
}

get_wiener_index <- function(tree)
{
  wiener_index = mean_distance(tree)
  return(wiener_index)
}

get_shortest_path_from_root_to_node <-
  function(g, root_node, target_node) {
    shortest_path_from_root = get.shortest.paths(
      g,
      from = as.character(target_node),
      to = as.character(root_node),
      weights = NULL,
      mode = "in",
      output = "vpath"
    )[[1]]
    
    length = length(shortest_path_from_root[[1]]) - 1
    return(length)
  }

get_avg_toxicity_distance <-
  function(graph, root_node) {
    avg_distances = c()
    toxic_ids = c()
    
    
    nodes_to_keep = V(graph)$name[is.na(V(graph)$is_toxic) == F]
    graph = induced_subgraph(graph, nodes_to_keep)
    
    node_ids = V(graph)$name
    toxic_ids = V(graph)$is_toxic
    
    for (node in node_ids)
    {
      distance =
        get_shortest_path_from_root_to_node(graph, root_node, target_node = node)
      
      avg_distances = append(distance,
                             avg_distances)
    }
    
    toxic_distances = avg_distances[toxic_ids == 1]
    
    if (length(toxic_distances) > 0) {
      avg_toxic_distances = mean(toxic_distances)
      cat("Average distance toxicity distance: ",
          avg_toxic_distances,
          "\n")
      return(avg_toxic_distances)
    } else
    {
      return(NA)
    }
  }

get_assortativity <-
  function(g, root, is_directed = FALSE)
  {
    g = delete.vertices(g, is.na(V(g)$is_toxic))
    property_attributes = get.vertex.attribute(g, "is_toxic")
    
    assortativity_score = assortativity(g, types1 = property_attributes, directed = F)
    
    return(l = assortativity_score)
  }

tree_max_width <- function(tree, c_id)
{
  bfs_result = bfs(
    tree,
    c_id,
    order = TRUE,
    rank = TRUE,
    father = TRUE,
    pred = TRUE,
    succ = TRUE,
    dist = TRUE
  )
  dist_table = tibble(id = names(bfs_result$dist),
                      distance_from_root = bfs_result$dist)
  
  degree_nodes = degree(tree, v = dist_table$id, mode = "out")
  
  degree_table = tibble(id = names(degree_nodes),
                        degree = degree_nodes)
  
  max_width = degree_table %>%
    inner_join(dist_table, by = "id") %>%
    group_by(distance_from_root) %>%
    summarise(total_degree_by_level = sum(degree)) %>%
    summarise(max_width = max(total_degree_by_level)) %>%
    pull(1)
  
  return(max_width)
  
}

# Global variables ####
topic_name = "football"
load_comments = TRUE
is_toxicity_shuffling_enabled = F
threshold = 0.6

folder <- "/media/gabett/Volume/data-repository/panconesi-football-elections/football/youtube/"
posts_filename = paste(folder, "youtube_football_original_videos_entire_dataset_scored.parquet", sep = "")
comments_filename = paste(folder, "youtube_football_entire_dataset.parquet", sep = "")
output_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/youtube/trees/graph_analysis_for_each_comment"

# Load data ####
# Posts
df_posts <- read_parquet(posts_filename)
df_posts <- as.data.frame(df_posts)

df_posts <- df_posts %>% 
  mutate(topic = topic_name,
         created_at = as_datetime(created_at),
         downloaded_at = as_datetime(downloaded_at)) %>% 
  distinct(video_id,
           .keep_all = T) %>%
  filter(created_at >= "2022-08-25" &
           created_at <= "2022-12-25") 

# Comments
df_comments <- read_parquet(comments_filename)
df_comments <- as.data.frame(df_comments)

df_comments <- df_comments %>%
  filter(topic == topic_name) %>% 
  mutate(
    created_at = as_datetime(created_at_comment),
    can_rate = ifelse(can_rate == "t", T, F),
    parent_id = ifelse(parent_id == "", NA, parent_id),
    moderation_status = ifelse(moderation_status == "", NA, moderation_status)
  ) %>%
  distinct(comment_id, .keep_all = T)

video_ids_for_search = df_comments %>%
  group_by(video_id) %>%
  dplyr::summarise(n = n()) %>%
  ungroup()

# Graph analysis ####

# Create local variables
i = 1

unique_video_ids = unique(video_ids_for_search$video_id)

"
Idea: given a dataset of comments for a specific thread - identified by its
video_id and sorted by creation time - we add each node to the graph
and compute the according metrics, iteratively.
"

existing_video_ids = tibble(filename = list.files(output_folder))
existing_video_ids$video_ids = sapply(existing_video_ids$filename,
                                      function(x) str_split(x,
                                                            "generative_graph_features_for_video-id_*")[[1]][2],
                                      simplify = T)
existing_video_ids$video_ids = sapply(existing_video_ids$video_ids,
                                      function(x) str_split(x,
                                                            ".csv")[[1]][1],
                                      simplify = T)

from_id = which("2q4TbVVwa0M" == unique_video_ids)

for (v_id in unique_video_ids[from_id:length(unique_video_ids)])
{
  cat("Working with video ID", v_id, "\n", sep = " ")
  output_filename = paste(
    output_folder,
    "generative_graph_features_for_video-id_",
    v_id,
    ".csv",
    sep = ""
  )
  
  if(length(grep(v_id, existing_video_ids$filename)) > 0)
  {
    print("Skipping")
    next
  }
  
  df_metrics = tibble()
  df_metrics_result = tibble()
  
  # Dataframe of the comments for the current video_id
  df_tree = tibble()
  
  # This dataframe will be iteratively populated
  thread_edge_list = tibble()
  
  # Obtain the original information of the post
  original_post_info = df_posts %>%
    filter(video_id == v_id) %>%
    select(
      id = video_id,
      parent_id = NULL,
      toxicity_score,
      created_at,
      author_id = channel_id,
      text = description) %>%
    mutate(created_at = as_datetime(created_at))
  thread_edge_list = rbind(thread_edge_list, original_post_info)
  
  if(dim(original_post_info)[1] == 0)
  {
    print("Skipping due to missing original post info")
    next
  }
  
  # Populate the tree dataframe with root info
  df_tree = rbind(df_tree, original_post_info)
  
  # Obtain the comments for the corresponding conversation id
  df_comments_by_video_id <- df_comments %>%
    filter(video_id == v_id) %>%
    select(id = comment_id,
           author_id = user_id,
           parent_id,
           toxicity_score,
           video_id,
           user_name, 
           text = comment_text_original,
           created_at) %>%
    mutate(
      id = as.character(id),
      parent_id = as.character(parent_id),
      author_id = as.character(author_id)
    ) %>%
    arrange(created_at)
  
  "
  We prepare the tree by unfolding the mentions and assigning the latest 
  comment from the mentioned user as the parent of the mentioning comment
  "
  mention_pattern <- '@([a-zA-Z0-9\\_\\.]+)'
  df_mentioning_comments <- df_comments_by_video_id %>% 
    filter(str_starts(text, "@"))
  
  mentioning_index <- 1
  while (mentioning_index < dim(df_mentioning_comments)[1])
  {
    node <- df_mentioning_comments[mentioning_index, ]
    old_parent_id <- node$id
    
    mentioned_user <- str_extract(node$text, mention_pattern)[1]
    
    if(length(mentioned_user) > 0)
    {
      mentioned_user <- str_replace(mentioned_user, "@", "")
      df_latest_mentioned_user_comment <- df_mentioning_comments %>% 
        filter(user_name == mentioned_user) %>% 
        arrange(desc(created_at)) %>% 
        top_n(1)
    }
    
    df_comments_by_video_id  <- df_comments_by_video_id %>% 
      mutate(parent_id = ifelse(id == node$id, 
                                df_latest_mentioned_user_comment$id,
                                parent_id))
    
    cat(mentioning_index,
        "Parent ID of node with id", 
        node$id, 
        "changed from",
        old_parent_id,
        "to",
        node$parent_id, 
        "\n",
        sep = " ")
    
    mentioning_index <- mentioning_index + 1
  }
  
  
  # The list of edges is initially populated with the children of the
  # root (the post)
  df_children = df_comments_by_video_id %>%
    filter(is.na(parent_id) == T) %>% 
    mutate(parent_id = v_id) %>% 
    select(
      id,
      parent_id,
      toxicity_score,
      created_at,
      author_id,
      user_name,
      text,
      video_id
    )
  
  number_of_children_left = dim(df_children)[1]
  
  # For each children
  children_index = 1
  while (number_of_children_left > 0)
  {
    cat("\tNumber of children left ",
        number_of_children_left,
        "\n",
        sep = "")
    
    # Take the top of the dataset since the it is always sorted
    df_child = df_children[1, ]
    
    # Add the children to the tree dataframe
    df_tree = plyr::rbind.fill(df_tree, df_child)
    
    # Remove that row from the children dataset
    df_children = df_children[-1,]
    
    # Add the parent-child relationship to the current tree edge list
    thread_edge_list = plyr::rbind.fill(thread_edge_list, df_child)
    thread_edge_list <- thread_edge_list %>% 
      distinct(id, .keep_all = T)
    
    # Create an updated version of the graph with the newer node
    tree = create_graph(thread_edge_list)
    
    "
    Computation of several metrics of the tree at each comment insertion

    - Size
    - Toxicity Ratio
    - Number of Unique Users
    - Wiener Index
    - Avg. Toxic Distance
    - Max width
    - Max Depth
    - Assortativity
    "
    
    # Size
    ts = tree_size(tree)
    
    # Toxicity Ratio
    number_of_toxic_elements = df_tree %>%
      filter(toxicity_score >= threshold) %>%
      count(n()) %>%
      pull(1)
    
    number_of_toxic_elements = ifelse(length(number_of_toxic_elements) == 0,
                                      0,
                                      number_of_toxic_elements)
    
    number_of_non_toxic_elements = df_tree %>%
      filter(toxicity_score < threshold) %>%
      count(n()) %>%
      pull(1)
    
    number_of_non_toxic_elements = ifelse(length(number_of_non_toxic_elements) == 0,
                                          0,
                                          number_of_non_toxic_elements)
    
    toxicity_ratio = number_of_toxic_elements / (number_of_toxic_elements + number_of_non_toxic_elements)
    
    # Number of Unique Users
    number_of_unique_users = df_tree %>%
      distinct(author_id) %>%
      count(n()) %>%
      pull(1)
    
    # Wiener Index
    wiener_index = get_wiener_index(tree)
    
    # Max Depth
    max_depth = tree_depth(tree, v_id)
    
    # Max Width
    max_width = tree_max_width(tree, v_id)
    
    # Assortativity
    assortativity = NA
    tryCatch({
      assortativity = get_assortativity(tree, v_id)
    },
    error = function(e) {
      assortativity = NA
    })
    
    # Average Toxicity distance from root
    avg_toxicity_distance = NA
    tryCatch({
      avg_toxicity_distance = get_avg_toxicity_distance(tree, v_id)
    },
    error = function(e) {
      avg_toxicity_distance = NA
    })
    
    # Create result
    df_metrics = data.frame(
      "children_index" = children_index,
      "video_id" = v_id,
      "id" = df_child$id,
      "parent_id" = df_child$parent_id,
      "created_at" = df_child$created_at,
      "root" = v_id,
      "toxicity_score" = df_child$toxicity_score,
      "tree_size" = ts,
      "max_width" = max_width,
      "max_depth" = max_depth,
      "number_of_unique_users" = number_of_unique_users,
      "toxicity_ratio" = toxicity_ratio,
      "assortativity" = assortativity,
      "avg_toxicity_distance" = avg_toxicity_distance,
      "wiener_index" = wiener_index
    )
    
    head(df_metrics)
    
    # Append new row
    df_metrics_result = plyr::rbind.fill(df_metrics_result, df_metrics)
    write.csv(df_metrics_result, output_filename, row.names = FALSE)
    
    # Add new children
    new_children <- df_comments_by_video_id %>%
      inner_join(df_child, by = c("parent_id" = "id")) %>%
      arrange(created_at.x)  %>%
      mutate(video_id = v_id) %>%
      select(
        id = id,
        parent_id = parent_id,
        toxicity_score = toxicity_score.x,
        created_at = created_at.x,
        author_id = author_id.x,
        video_id
      )
    
    if (dim(new_children)[1] > 0)
    {
      cat(df_child$id,
          "has the following children:",
          new_children$id,
          "\n")
    }
    
    df_children = plyr::rbind.fill(df_children, new_children) %>%
      arrange(created_at)
    
    # Update the number of children to consider
    number_of_children_left = dim(df_children)[1]
    
    # Update the children index
    children_index = children_index + 1
  }
}
