library(dplyr)
library(arrow)

df_filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/twitter/elections_twitter_data_scored.parquet"
df <- read_parquet(df_filename)
df$created_at <- lubridate::as_date(df$created_at)


auth_setup_default()

# Finding comments without in_reply_to_id
comments_without_in_reply_to_id <- df %>%
  filter(is.na(in_reply_to_id) == F |
           is.na(in_reply_to_user_id) == F &
           id != conversation_id) %>% 
  filter(is.na(in_reply_to_id) ) %>% 
  select(id, conversation_id, created_at, in_reply_to_id, in_reply_to_user_id, referenced_tweets, text)


# For each comment, we find the latest comment posted by the user the comment is 
# replying to. The found id is assigned to the in_reply_to_id field.
for(i in 1:dim(comments_without_in_reply_to_id)[1])
{
  latest_comment <- df %>% 
    filter(conversation_id == comments_without_in_reply_to_id[i, ]$conversation_id &
             created_at < comments_without_in_reply_to_id[i, ]$created_at &
             author_id == comments_without_in_reply_to_id[i, ]$in_reply_to_user_id)
}

latest_comment <- df %>% 
  filter(conversation_id == comments_without_in_reply_to_id[i, ]$conversation_id)
