# Libraries and reset ####
library(tibble)
library(dplyr)
library(igraph)
library(jsonlite)
library(stringr)
library(data.table)
library(lubridate)
library(arrow)
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
topic = "elections"
threshold = 0.6

posts_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/twitter/elections_comments_without_in_reply_to_id_filled.RData"
comments_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/twitter/elections_comments_without_in_reply_to_id_filled.RData"
output_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/twitter/trees/graph_analysis_for_each_comment/"

load(posts_filename)
elections_original_posts <- df 
rm(df)

elections_original_posts <- elections_original_posts %>% 
  filter(is.na(in_reply_to_id) == T) %>% 
  distinct(id, .keep_all = T)

elections_original_posts = elections_original_posts %>%
  distinct(id, .keep_all = T) %>%
  mutate(created_at = as_datetime(created_at)) %>% 
  filter(created_at >= "2022-08-25" &
           created_at <= "2022-12-25") %>% 
  mutate(in_reply_to_id = NULL)

load(posts_filename)
elections_comments <- df 

elections_comments <- elections_comments %>% 
  filter(is.na(in_reply_to_id) == F) %>% 
  mutate(created_at = as_datetime(created_at)) %>%
  distinct(id, .keep_all = T)

conversation_ids_for_search = elections_original_posts %>%
  group_by(conversation_id) %>%
  dplyr::summarise(n = n()) %>%
  ungroup()

conversation_ids_for_search <- conversation_ids_for_search %>% 
  filter(conversation_id %in% unique(elections_comments$conversation_id))

# Graph analysis ####
# Create local variables
unique_conversation_ids = unique(conversation_ids_for_search$conversation_id)

"
Idea: given a dataset of comments for a specific thread - identified by its
conversation_id and sorted by creation time - we add each node to the graph
and compute the according metrics, iteratively.
"
existing_conversation_ids = tibble(filename = list.files(output_folder))
existing_conversation_ids$conversation_ids = sapply(existing_conversation_ids$filename,
                                                    function(x) str_split(x,
                                                              "generative_graph_features_for_conversation-id_*")[[1]][2],
                                                    simplify = T)
existing_conversation_ids$conversation_ids = sapply(existing_conversation_ids$conversation_ids,
                                                    function(x) str_split(x,
                                                                          ".csv")[[1]][1],
                                                    simplify = T)
# 
# unique_conversation_ids = unique_conversation_ids[!unique_conversation_ids %in% existing_conversation_ids$conversation_ids]
# unique_conversation_ids = unique_conversation_ids[unique_conversation_ids %in% elections_comments$conversation_id]

i = 1
for (c_id in unique_conversation_ids[i: length(unique_conversation_ids)])
{
  cat(i, "/", length(unique_conversation_ids), "Working with conversation ID", c_id, "\n", sep = " ")
  i = i + 1
  
  output_filename = paste(
    output_folder,
    "generative_graph_features_for_conversation-id_",
    c_id,
    ".csv",
    sep = ""
  )
  
  if(length(grep(c_id, existing_conversation_ids$filename)) > 0)
  {
    print("Skipping")
    next
  }
  
  df_metrics = tibble()
  df_metrics_result = tibble()
  
  # Dataframe of the comments for the current conversation_id
  df_tree = tibble()
  
  # This dataframe will be iteratively populated
  thread_edge_list = tibble()
  
  # Obtain the original information of the post
  original_post_info = elections_original_posts %>%
    filter(id == c_id) %>%
    select(conversation_id,
           id,
           in_reply_to_id = NULL,
           toxicity_score,
           created_at,
           author_id,
           text) %>%
    mutate(created_at = as_datetime(created_at))
  
  thread_edge_list = rbind(thread_edge_list, original_post_info)
  # Populate the tree dataframe with root info
  df_tree = rbind(df_tree, original_post_info)
  
  # Obtain the comments for the corresponding conversation id
  elections_comments_by_conversation_id <- elections_comments %>%
    filter(conversation_id == c_id) %>%
    select(conversation_id,
           id,
           in_reply_to_id,
           toxicity_score,
           text,
           created_at,
           author_id) %>%
    mutate(
      id = as.character(id),
      in_reply_to_id = as.character(in_reply_to_id),
      author_id = as.character(author_id)
    ) %>%
    arrange(created_at)
  
  # The list of edges is initially populated with the children of the
  # root (the post)
  df_children = elections_comments_by_conversation_id %>%
    inner_join(original_post_info, by = c("in_reply_to_id" = "id")) %>%
    arrange(created_at.x) %>%
    mutate(conversation_id = c_id) %>% 
    select(
      id = id,
      parent_id = in_reply_to_id,
      toxicity_score = toxicity_score.x,
      created_at = created_at.x,
      author_id = author_id.x,
      text = text.x,
      conversation_id
    )
  
  number_of_children_left = dim(df_children)[1]
  
  # For each children
  children_index = 1
  while (number_of_children_left > 0)
  {
    cat("\tNumber of children left ", number_of_children_left, "\n", sep = "")
    
    # Take the top of the dataset since the it is always sorted
    df_child = df_children[1,]
    
    # Add the children to the tree dataframe
    df_tree = plyr::rbind.fill(df_tree, df_child)
    
    # Remove that row from the children dataset
    df_children = df_children[-1, ]
    
    # Add the parent-child relationship to the current tree edge list
    thread_edge_list = plyr::rbind.fill(thread_edge_list, df_child)
    
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
    
    number_of_toxic_elements = ifelse(length(number_of_toxic_elements) == 0, 0, number_of_toxic_elements)
    
    number_of_non_toxic_elements = df_tree %>%
      filter(toxicity_score < threshold) %>%
      count(n()) %>%
      pull(1)
    
    number_of_non_toxic_elements = ifelse(length(number_of_non_toxic_elements) == 0, 0, number_of_non_toxic_elements)
    
    toxicity_ratio = number_of_toxic_elements / (number_of_toxic_elements + number_of_non_toxic_elements)
    
    # Number of Unique Users
    number_of_unique_users = df_tree %>%
      distinct(author_id) %>%
      count(n()) %>%
      pull(1)
    
    # Wiener Index
    wiener_index = get_wiener_index(tree)
    
    # Max Depth
    max_depth = tree_depth(tree, c_id)
    
    # Max Width
    max_width = tree_max_width(tree, c_id)
    
    # Assortativity
    assortativity = NA
    tryCatch({
      assortativity = get_assortativity(tree, c_id)
    },
    error = function(e) {
      assortativity = NA
    })
    
    # Average Toxicity distance from root
    avg_toxicity_distance = NA
    tryCatch({
      avg_toxicity_distance = get_avg_toxicity_distance(tree, c_id)
    },
    error = function(e) {
      avg_toxicity_distance = NA
    })
    
    # Create result
    df_metrics = data.frame(
      "children_index" = children_index,
      "conversation_id" = c_id,
      "id" = df_child$id,
      "parent_id" = df_child$parent_id,
      "created_at" = df_child$created_at,
      "root" = c_id,
      "toxicity_score" = df_child$toxicity_score,
      "tree_size" = ts,
      "max_width" = max_width,
      "max_depth" = max_depth,
      "number_of_unique_users" = number_of_unique_users,
      "toxicity_ratio" =toxicity_ratio,
      "assortativity" = assortativity,
      "avg_toxicity_distance" = avg_toxicity_distance,
      "wiener_index" = wiener_index
    )
    
    head(df_metrics)
    
    # Append new row
    df_metrics_result = plyr::rbind.fill(df_metrics_result, df_metrics)
    write.csv(df_metrics_result, output_filename, row.names = FALSE)
    
    # Add new children
    new_children = elections_comments_by_conversation_id %>%
      inner_join(df_child, by = c("in_reply_to_id" = "id")) %>%
      arrange(created_at.x)  %>%
      mutate(conversation_id = c_id) %>% 
      select(
        id = id,
        parent_id = in_reply_to_id,
        toxicity_score = toxicity_score.x,
        created_at = created_at.x,
        author_id = author_id.x,
        text = text.x,
        conversation_id
      )
    
    if(dim(new_children)[1] > 0)
    {
      cat(df_child$id, "has the following children:", new_children$id, "\n")
    }
    
    df_children = plyr::rbind.fill(df_children, new_children) %>% 
      arrange(created_at)
    
    # Update the number of children to consider
    number_of_children_left = dim(df_children)[1]
    
    # Update the children index
    children_index = children_index + 1
  }
  
  
}

