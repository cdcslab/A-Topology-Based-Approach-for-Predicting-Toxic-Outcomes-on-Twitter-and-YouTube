library(dplyr)
library(plyr)
library(foreach)
library(doParallel)

rm(list = ls())
gc()

# Global variables ####
comments_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter-conversations/"
csv_result_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/comments_unified/comments_conversations-folder_unified"

# Read conversations folder ####
conversation_list = list.files(comments_folder, full.names = T, pattern = "*.csv")
conversation_list = unique(conversation_list)

# Unify conversations ####
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
total_comments_df <- foreach (i = 1:length(conversation_list),
         .combine = rbind.fill) %dopar%
  {
    comment_filename = conversation_list[i]
    is_error = FALSE
    tryCatch({
      comments_df = read.csv(
        comment_filename,
        colClasses = c(
          "id" = "character",
          "author_id" = "character",
          "conversation_id" = "character",
          "in_reply_to_user_id" = "character"
        )
      )
      
      comments_df
    },
    error = function(x)
    {
      is_error = TRUE
    })
    
  }

total_comments_df = total_comments_df %>%
  distinct(id, .keep_all = T)

output_filename = paste(csv_result_filename, ".csv", sep = "")
write.csv(total_comments_df, output_filename)

output_filename = paste(csv_result_filename, ".RData", sep = "")
total_comments_df_old = total_comments_df
save(total_comments_df,
     file = output_filename,
     version = 2)
load(output_filename)
