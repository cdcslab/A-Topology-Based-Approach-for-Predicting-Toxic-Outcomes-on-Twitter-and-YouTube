library(dplyr)
library(arrow)

rm(list = ls())
gc()

df_filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter/football_twitter_data_scored.parquet"
df_output_filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter/football_comments_without_in_reply_to_id_filled.parquet"
df <- read_parquet(df_filename)
df$created_at <- lubridate::as_datetime(df$created_at)

# Finding comments without in_reply_to_id

# I post prima di questa data sono tutti original posts
comments_that_actually_are_original_posts <- df %>%
  filter(is.na(in_reply_to_id) == T &
           is.na(in_reply_to_user_id) == F & 
           id != conversation_id & 
           created_at < "2022-10-26")

df$in_reply_to_user_id <- ifelse(df$id %in% comments_that_actually_are_original_posts$id,
                                 NA,
                                 df$in_reply_to_user_id)

comments_without_in_reply_to_id <- df %>%
  filter(is.na(in_reply_to_id) == T &
           is.na(in_reply_to_user_id) == F & 
           id != conversation_id & 
           created_at >= "2022-10-26" &
           created_at < "2022-12-31")

# For each comment, we find the latest comment posted by the user the comment is 
# replying to. The found id is assigned to the in_reply_to_id field.

df_with_conversation_ids_of_missing_replies <- df %>% 
  filter(conversation_id %in% comments_without_in_reply_to_id$conversation_id)

for(i in 1:dim(comments_without_in_reply_to_id)[1])
{
  print(i)
  
  if(i %% 100 == 0)
  {
    write_parquet(comments_without_in_reply_to_id, df_output_filename)
  }
  
  latest_comment_related <- df_with_conversation_ids_of_missing_replies %>% 
    filter(conversation_id == comments_without_in_reply_to_id[i, ]$conversation_id &
             author_id == comments_without_in_reply_to_id[i, ]$in_reply_to_user_id & 
             created_at < comments_without_in_reply_to_id[i, ]$created_at)
  
  if(dim(latest_comment_related)[1] == 0)
  {
    comments_without_in_reply_to_id[i, ]$in_reply_to_id <- df[i, ]$conversation_id
  }
  else
  {
    print("Found a tweet")
    latest_comment_related <- latest_comment_related %>% 
      arrange(desc(created_at))
    
    latest_comment_related <- latest_comment_related[1, ]
    comments_without_in_reply_to_id[i, ]$in_reply_to_id <- latest_comment_related$id
  }
}
