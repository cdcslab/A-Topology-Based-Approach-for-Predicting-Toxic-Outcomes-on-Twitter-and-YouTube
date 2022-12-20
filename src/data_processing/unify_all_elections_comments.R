library(dplyr)
library(plyr)

rm(list = ls())
gc()

# Global variables ####
comments_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/conversations/"
csv_result_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/comments_conversations-folder_unified"

# Read conversations folder ####
conversation_list = list.files(comments_folder, full.names = T, pattern = "*.csv")
conversation_list = unique(conversation_list)

# Unify conversations ####
total_comments_df = tibble()
tmp_comments_df = tibble()

for(i in 1:length(conversation_list))
  {
    comment_filename = conversation_list[i]
    is_error = FALSE
    #print(comment_filename)
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
      
      tmp_comments_df = rbind.fill(tmp_comments_df, comments_df)
      if(i%%100 == 0)
      {
        print("Adding 100 new datasets to the final dataframe.")
        total_comments_df = plyr::rbind.fill(total_comments_df, tmp_comments_df)
        cat("i =", i, ".The dataframe has now", dim(total_comments_df)[1], "rows.\n")
        tmp_comments_df = tibble()
      }
      
    },
    error = function(x)
    {
      is_error = TRUE
    })
    
}

output_filename = 
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/comments_conversations-folder_unified_1.RData"

save(total_comments_df,
     file = output_filename,
     version = 2)

comments_folder = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/conversations-backward/"
conversation_list = list.files(comments_folder, full.names = T, pattern = "*.csv")
conversation_list = unique(conversation_list)

rm(total_comments_df)
gc()


total_comments_df = tibble()
tmp_comments_df = tibble()

for(i in 1:length(conversation_list))
{
  comment_filename = conversation_list[i]
  is_error = FALSE
  #print(comment_filename)
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
    
    tmp_comments_df = rbind.fill(tmp_comments_df, comments_df)
    if(i%%100 == 0)
    {
      print("Adding 100 new datasets to the final dataframe.")
      total_comments_df = plyr::rbind.fill(total_comments_df, tmp_comments_df)
      cat("i =", i, ".The dataframe has now", dim(total_comments_df)[1], "rows.\n")
      tmp_comments_df = tibble()
    }
    
  },
  error = function(x)
  {
    is_error = TRUE
  })
  
}

output_filename = 
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/comments_conversations-folder_unified_2.RData"

total_comments_df_2 = total_comments_df

save(total_comments_df,
     file = output_filename,
     version = 2)

input_filename = 
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/comments_conversations-folder_unified_1.RData"

load(input_filename)

total_comments_df = rbind(total_comments_df, total_comments_df_2)

total_comments_df = total_comments_df %>%
  distinct(id, .keep_all = T)

output_filename = 
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/comments_conversations-folder_unified.RData"
save(total_comments_df,
     file = output_filename,
     version = 2)

write.csv(total_comments_df, output_filename)

output_filename = paste(csv_result_filename, ".RData", sep = "")
save(total_comments_df,
     file = output_filename,
     version = 2)
