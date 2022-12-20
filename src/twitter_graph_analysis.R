library(tibble)
library(dplyr)
library(igraph)
library(jsonlite)
library(stringr)

rm(list = ls())
gc()

topic = "elections"
load_comments = TRUE
threshold = 0.6

# Functions ####
create_tree_edge_list <- function(root, comment_df)
{
  #cat("The root of this tree is at node", root, "\n")
  parent_edge_list = tibble(
    "parent" = NA,
    "children" = root,
    "toxicity_score" = NA,
    "conversation_id" = NA,
    "created_at" = NA
  )
  
  edge_list = comment_df %>%
    select(
      children = id,
      parent = in_reply_to_id,
      toxicity_score,
      conversation_id,
      created_at
    )
  
  tree_edge_list = rbind(parent_edge_list, edge_list)
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
  df_eccentricity = as.integer(eccentricity(g, root))
  
  return(df_eccentricity)
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
    
    if (n_comments == 0)
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
  function(g, root, property, is_directed = FALSE)
  {
    g = delete.vertices(g, root)
    property_attributes = get.vertex.attribute(g, property)
    
    assortativity_score = assortativity(g,
                                        types1 = property_attributes,
                                        directed = is_directed)
    
    return(l = assortativity_score)
  }

extract_metrics <- function(row)
{
  row["referenced_tweets"] = str_replace_all(row["referenced_tweets"],
                                             fixed('\''),
                                             "\"")
  row["referenced_tweets"] = str_replace_all(row["referenced_tweets"],
                                             fixed("\\"),
                                             "")
  
  metrics = fromJSON(row["referenced_tweets"])
  
  if ("replied_to" %in% metrics$type)
  {
    return(metrics$id)
  }
  else
  {
    return(NA)
  }
}

# Data loading ####
submission_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/original_tweets/tweets_unified.csv"
df_submission = read.csv(submission_filename)
df_submission$conversation_id = as.character(df_submission$conversation_id)
df_submission$created_at = lubridate::as_date(df_submission$created_at)

df_submission = df_submission %>%
  filter(created_at >= "2022-08-25" &
           created_at <= "2022-10-25")

if (load_comments == FALSE)
{
  df_comments = read.csv("/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments_unified/elections_comment_labelled_unified.csv")
  
  df_comments = df_comments %>%
    distinct(id, .keep_all = T)
  
  df_comments$id = as.character(df_comments$id)
  df_comments$conversation_id = as.character(df_comments$conversation_id)
  
  df_comments$result <- apply(df_comments, 1, extract_metrics)
  df_comments$in_reply_to_id = apply(df_comments, 1, function(x) {
   x["result"][[1]]
  })
  
  
  saveRDS(
    df_comments,
    "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments_unified/elections_comments_unified_preprocessed.RData",
    version = 2
  )
} else {
  df_comments = readRDS(
    "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments_unified/elections_comments_unified_preprocessed.RData"
  )
  df_comments$id = as.character(df_comments$id)
  
}


# Graph analysis ###
unique_conversation_ids = unique(df_comments$conversation_id)
i = 1
df_metrics_results = tibble()
df_metrics = tibble()
  
compute_tree_metrics <- function(c_id)
{
  df_comments_by_conversation_id <- df_comments %>%
    filter(conversation_id == c_id) %>%
    filter(is.na(toxicity_score) == F)
  df_comments_by_conversation_id$id = as.character(df_comments_by_conversation_id$id)
  
  df_toxic_comments_by_conversation_id <-
    df_comments_by_conversation_id %>%
    filter(toxicity_score >= threshold)
  
  cat("i:", i, "root:", conversation_id, "\n")
  
  edge_list = create_tree_edge_list(root = conversation_id,
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
    V(tree_lcc)$is_toxic = ifelse(lcc_metadata$toxicity_score >= threshold, T, F)
    
    tree_filename = paste(
      "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/trees/igraph_data/",
      "tree_",
      c_id,
      ".csv",
      sep = ""
    )
    
    write.csv(as_long_data_frame(tree),
              tree_filename)
    
    #computing the size of the tree
    ts = tree_size(tree)
    ts_lcc = tree_size(tree_lcc)
    
    #compute the depth of the tree considering the root node only
    gtree_depth = tree_depth(tree_lcc, c_id)
    
    # Toxicity ratio
    toxicity_ratio = get_toxicity_ratio(
      df_toxic_comments_by_conversation_id,
      df_comments_by_conversation_id,
      c_id
    )
    
    assortativity = NaN
    # Assortativity
    tryCatch({
      assortativity = get_assortativity(tree_lcc, conversation_id, "toxicity_score")
    },
    error = function(e) {
      assortativity = NaN
    })
    
    # Mean root distance
    mean_root_distance = NaN
    tryCatch({
      mean_root_distance = get_mean_root_distance(tree_lcc, conversation_id)
    },
    error = function(e) {
      mean_root_distance = NaN
    })
    
    # Wiener Index
    wiener_index = get_wiener_index(tree_lcc)
    
    # Save tree metrics for the following subverse
    df_metrics = tibble(
      "topic" = topic,
      "root" = conversation_id,
      "tree_size" = ts,
      "tree_size_lcc" = ts_lcc,
      "lcc_size_percentage" = lcc_size_percentage,
      "eccentrity_from_root" = gtree_depth,
      "toxicity_ratio" = toxicity_ratio,
      "assortativity" = assortativity,
      "avg_toxicity_distance" = mean_root_distance,
      "wiener_index" = wiener_index
    )
    
    df_metrics_results = plyr::rbind.fill(df_metrics_results, df_metrics)
  }
}

output_filename = paste(
  "/media/gabett/Volume/data-repository/panconesi-football-elections/",
  topic,
  "/",
  topic,
  "_graph_metrics.csv",
  sep = ""
)

write.csv(df_metrics_results,
          output_filename)
