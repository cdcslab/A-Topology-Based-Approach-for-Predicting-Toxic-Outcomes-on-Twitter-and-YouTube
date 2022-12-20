library(ggplot2)
library(dplyr)

rm(list = ls())

# Global variables ###
data_folder = "./data/"
figures_folder = "./figures"

elections_comments_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments_unified/elections_comments_unified_preprocessed.RData"
football_comments_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/comments_unified/comments_unified.csv"

# Data Processing ####
elections_comments = readRDS(elections_comments_filename)
football_comments = read.csv(football_comments_filename)

# Plot ####