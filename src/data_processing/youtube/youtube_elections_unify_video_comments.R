library(dplyr)
library(cld3)
library(tibble)
library(arrow)

existing_comments_folder <- "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/youtube/current/Download_Video_Comments/"
video_comments_folder <- "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/youtube/new/Download_Video_Comments/"

files_in_existing_video_comments_folder <- list.files(existing_comments_folder)
files_in_video_comments_folder <- list.files(video_comments_folder)

files_in_video_comments_folder <- setdiff(files_in_video_comments_folder, files_in_existing_video_comments_folder)
files_in_video_comments_folder <- paste(video_comments_folder, files_in_video_comments_folder, sep = "")

youtube_comments <- tibble()
i = 1
for(file in files_in_video_comments_folder)
{
  if(i %% 1000 == 0)
  {
    print(i)
  }
  tmp <- readRDS(file)
  youtube_comments <- rbind(youtube_comments, tmp)
  i = i + 1
}

youtube_comments$language <- sapply(youtube_comments$comment_text_display, detect_language)

youtube_comments <- youtube_comments %>% 
  filter(language == "it")

output_filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/youtube/new/elections_italian_comments_unified.parquet"
write_parquet(youtube_comments, output_filename)
