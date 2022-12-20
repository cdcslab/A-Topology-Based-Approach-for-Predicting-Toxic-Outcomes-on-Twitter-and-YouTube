library(tibble)
library(dplyr)
library(igraph)
library(jsonlite)
library(stringr)

rm(list = ls())
gc()
set.seed(42)

topic = "football"
load_comments = TRUE
is_toxicity_shuffling_enabled = T
threshold = 0.6

if(is_toxicity_shuffling_enabled)
{
  tree_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/trees/igraph_data/shuffled"
} else
{
  tree_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/trees/igraph_data/" 
}
  
football_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/"

# Functions ####
create_tree_edge_list <- function(root, comment_df)
{
  tree_edge_list = comment_df %>%
    select(
      children = id,
      parent = in_reply_to_id,
      toxicity_score,
      conversation_id,
      created_at
    )
  
  return(tree_edge_list)
}

create_graph <- function(edge_list)
{
  g = graph_from_data_frame(edge_list)
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

get_mean_root_distance <-
  function(graph, root_node) {
    avg_distances = c()
    toxic_ids = c()
    
    id_root = which(V(graph)$name == root_node)
    id_NA = which(V(graph)$name == "NA")
    node_ids = V(graph)$name[-c(id_root, id_NA)]
    
    toxic_ids = V(graph)$is_toxic[-c(id_root, id_NA)]
    
    for (node in node_ids)
    {
      distance =
        get_shortest_path_from_root_to_node(graph, as.character(root_node), target_node = as.character(node))
      
      avg_distances = append(distance,
                             avg_distances)
    }
    
    toxic_distances = avg_distances[toxic_ids == T]
    
    if (length(toxic_distances) > 0) {
      avg_toxic_distances = mean(toxic_distances)
      return(avg_toxic_distances)
    }
  }

get_toxicity_ratio <-
  function (df_comments,
            df_with_toxic_comments,
            root) {
    n_comments = df_comments %>%
      filter(conversation_id == root)  %>%
      count()
    
    n_toxic_comments = df_with_toxic_comments %>%
      filter(conversation_id == root) %>%
      count()
    
    if (n_comments$n == 0)
    {
      return(toxicity_ratio = 0)
    }
    else
    {
      toxicity_ratio = n_comments$n / n_toxic_comments$n
      return(toxicity_ratio = toxicity_ratio)
    }
    
  }
get_assortativity <-
  function(g, root, is_directed = FALSE)
  {
    g = delete.vertices(g, root)
    property_attributes = get.vertex.attribute(g, "is_toxic")
    
    assortativity_score = assortativity_nominal(g,
                                          types = V(g)$is_toxic,
                                        directed = F)
    
    return(l = assortativity_score)
  }

# Data loading ####
# submission_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/cleaned_footbal_twitter_timeline_with_original_tweets.csv"
# df_submission = read.csv(submission_filename)
# df_submission$conversation_id = as.character(df_submission$conversation_id)
# df_submission$created_at = lubridate::as_date(df_submission$created_at)

comments_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/comments_unified/football_comments_unified_labelled.RData"
load(
  comments_filename
  )

df_comments = total_comments_df
rm(total_comments_df)

# Data processing ####

# Graph analysis ###
unique_conversation_ids = unique(df_comments$conversation_id)
i = 1
df_metrics_results = tibble()
df_metrics = tibble()
for (c_id in unique_conversation_ids)
{
  df_comments_by_conversation_id <- df_comments %>%
    filter(conversation_id == c_id) %>%
    filter(is.na(toxicity_score) == F)
  
  if (is_toxicity_shuffling_enabled == TRUE)
  {
    print("Shuffling Toxicity Score.")
    df_comments_by_conversation_id$toxicity_score = sample(df_comments_by_conversation_id$toxicity_score)
  }
  
  df_comments_by_conversation_id$id = as.character(df_comments_by_conversation_id$id)
  
  df_toxic_comments_by_conversation_id <-
    df_comments_by_conversation_id %>%
    filter(toxicity_score >= threshold)
  
  cat("i:", i, "root:", c_id, "\n")
  i = i + 1
  
  edge_list = create_tree_edge_list(root = c_id,
                                    comment_df = df_comments_by_conversation_id)
  
  dim_edge_list = as.numeric(dim(edge_list)[1])
  if (dim_edge_list == 0)
    # If the graph is composed only by the root (conversation) id
  {
    next
  }
  else
  {
    cat("Found an existing network.\n")
    tree = create_graph(edge_list[c("parent", "children")])
    
    tree_lcc <-
      induced_subgraph(tree, which(
        components(tree)$membership == which.max(components(tree)$csize)
      ))
    
    if (c_id %in% V(tree_lcc)$name == F)
    {
      cat("Root is not in LCC. Skipping to the next network")
      next
    }
    
    lcc_size_percentage = gorder(tree_lcc) / gorder(tree)
    
    lcc_metadata <-
      tibble(id = V(tree_lcc)$name) %>%
      left_join(df_comments_by_conversation_id,
                by = c("id" = "id"))
    
    V(tree_lcc)$created_at = lcc_metadata$created_at
    V(tree_lcc)$toxicity_score = lcc_metadata$toxicity_score
    V(tree_lcc)$is_toxic = ifelse(lcc_metadata$toxicity_score >= threshold, 1, 2)
    
    if (is_toxicity_shuffling_enabled == TRUE)
    {
      tree_filename = paste(
        tree_folder,
        "shuffled_",
        "tree_",
        c_id,
        ".csv",
        sep = ""
      )
    }
    else
    {
      tree_filename = paste(
        tree_folder,
        "tree_",
        c_id,
        ".csv",
        sep = ""
      )
    }

    write.csv(as_long_data_frame(tree),
              tree_filename)
    
    # computing the size of the tree
    ts = tree_size(tree)
    ts_lcc = tree_size(tree_lcc)
    
    #compute the depth of the tree considering the root node only
    gtree_depth = tree_depth(tree_lcc, c_id)
    
    # Max Width
    bfs_result = bfs(tree_lcc, c_id, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE,
                     succ=TRUE, dist=TRUE)
    dist_table = tibble(id = names(bfs_result$dist),
                        distance_from_root = bfs_result$dist)
    
    degree_nodes = degree(tree_lcc, v = dist_table$id, mode = "out")
    
    degree_table = tibble(id = names(degree_nodes),
                          degree = degree_nodes)
    
    max_width = degree_table %>% 
      inner_join(dist_table, by = "id") %>% 
      group_by(distance_from_root) %>% 
      summarise(total_degree_by_level = sum(degree)) %>% 
      summarise(max_width = max(total_degree_by_level))
    
    # We compute metrics involving toxicity only for LCC with at least 10 nodes
    if(gorder(tree_lcc) >= 10)
    {
      # Toxicity ratio
      toxicity_ratio = get_toxicity_ratio(
        df_toxic_comments_by_conversation_id,
        df_comments_by_conversation_id,
        c_id
      )
      
      # Assortativity
      assortativity = NA
      tryCatch({
        assortativity = get_assortativity(tree_lcc, c_id)
      },
      error = function(e) {
        assortativity = NA
      })
      
      # Mean root distance
      mean_root_distance = NA
      tryCatch({
        mean_root_distance = get_mean_root_distance(tree_lcc, c_id)
      },
      error = function(e) {
        mean_root_distance = NA
      })
    }
    else
    {
      toxicity_ratio = NA
      assortativity = NA
      mean_root_distance = NA
    }
    
    # Wiener Index
    wiener_index = get_wiener_index(tree_lcc)
    
    # Save tree metrics for the following subverse
    df_metrics = tibble(
      "topic" = topic,
      "root" = c_id,
      "tree_size" = ts,
      "tree_size_lcc" = ts_lcc,
      "lcc_size_percentage" = lcc_size_percentage,
      "max_width" = max_width$max_width,
      "depth" = gtree_depth,
      "toxicity_ratio" = toxicity_ratio,
      "assortativity" = assortativity,
      "avg_toxicity_distance" = mean_root_distance,
      "wiener_index" = wiener_index
    )
    
    df_metrics_results = plyr::rbind.fill(df_metrics_results, df_metrics)
  }
}

if (is_toxicity_shuffling_enabled)
{
  output_filename = paste(
    football_folder,
    topic,
    "/",
    "shuffled_",
    topic,
    "_graph_metrics.csv",
    sep = ""
  )
} else
{
  output_filename = paste(
    football_folder,
    topic,
    "/",
    topic,
    "_graph_metrics.csv",
    sep = ""
  )
}

write.csv(df_metrics_results,
          output_filename)
