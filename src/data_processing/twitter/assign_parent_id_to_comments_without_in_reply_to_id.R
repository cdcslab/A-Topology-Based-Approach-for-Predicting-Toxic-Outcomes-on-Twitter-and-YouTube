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
           created_at < "2022-12-31") %>% 
  select(id, 
         created_at,
         conversation_id,
         text,
         author_id,
         in_reply_to_user_id,
         in_reply_to_id)

# For each comment, we find the latest comment posted by the user the comment is 
# replying to. The found id is assigned to the in_reply_to_id field.
df_with_conversation_ids_of_missing_replies <- df %>% 
  select(id, 
         created_at,
         conversation_id,
         text,
         author_id,
         in_reply_to_user_id,
         in_reply_to_id)

comments_with_a_parent <- comments_without_in_reply_to_id %>% 
  inner_join(df_with_conversation_ids_of_missing_replies, 
             by = c("conversation_id" = "conversation_id",
                    "in_reply_to_user_id" = "author_id"),
             multiple = "all",
             suffix = c("_comment", "_referenced_tweet")) %>% 
  filter(created_at_comment > created_at_referenced_tweet &
           id_comment != id_referenced_tweet) %>% 
  group_by(id_comment) %>% 
  arrange(desc(created_at_referenced_tweet)) %>% 
  distinct(id_comment, .keep_all = T)

df$in_reply_to_id <- ifelse(df$id %in% comments_with_a_parent$id_comment,
                            comments_with_a_parent$id_referenced_tweet,
                            df$in_reply_to_id)

comments_without_a_parent <- comments_without_in_reply_to_id %>% 
  anti_join(df_with_conversation_ids_of_missing_replies, 
            by = c("conversation_id" = "conversation_id",
                   "in_reply_to_user_id" = "author_id"))

df$in_reply_to_id <- ifelse(df$id %in% comments_without_a_parent$id,
                            comments_without_a_parent$conversation_id,
                            df$in_reply_to_id)

comments_without_in_reply_to_id <- df %>%
  filter(is.na(in_reply_to_id) == T &
           is.na(in_reply_to_user_id) == F & 
           id != conversation_id & 
           created_at >= "2022-10-26" &
           created_at < "2022-12-31")

comments_with_a_parent <- comments_without_in_reply_to_id %>% 
  inner_join(df, 
             by = c("conversation_id" = "conversation_id",
                    "in_reply_to_user_id" = "author_id"),
             multiple = "all",
             suffix = c("_referenced_tweet", "_comment")) %>% 
  filter(created_at_comment > created_at_referenced_tweet &
           id_comment != id_referenced_tweet) %>% 
  group_by(id_comment) %>% 
  arrange(desc(created_at_referenced_tweet)) %>% 
  distinct(id_comment, .keep_all = T) %>% 
  select(id_comment, 
         created_at_comment,
         conversation_id,
         text_comment,
         author_id,
         in_reply_to_user_id,
         in_reply_to_id_comment,
         id_referenced_tweet, 
         created_at_referenced_tweet,
         conversation_id,
         text_referenced_tweet,
         author_id,
         in_reply_to_user_id,
         in_reply_to_id_referenced_tweet)

df$in_reply_to_id <- ifelse(df$id %in% comments_with_a_parent$id_comment,
                            comments_with_a_parent$id_referenced_tweet,
                            df$in_reply_to_id)

comments_without_in_reply_to_id <- df %>%
  filter(is.na(in_reply_to_id) == T &
           is.na(in_reply_to_user_id) == F & 
           id != conversation_id & 
           created_at >= "2022-10-26" &
           created_at < "2022-12-31")

df$in_reply_to_id <- ifelse(df$id %in% comments_without_in_reply_to_id$id,
                            comments_with_a_parent$conversation_id,
                            df$in_reply_to_id)


comments_without_in_reply_to_id <- df %>%
  filter(is.na(in_reply_to_id) == T &
           is.na(in_reply_to_user_id) == F & 
           id != conversation_id & 
           created_at >= "2022-10-26" &
           created_at < "2022-12-31")

output_filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter/football_comments_without_in_reply_to_id_filled.RData"

save(df, file = output_filename)
