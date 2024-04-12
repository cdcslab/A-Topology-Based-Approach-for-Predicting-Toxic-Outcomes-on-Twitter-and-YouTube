library(arrow)
library(dplyr)
library(arrow)
library(ggplot2)
library(data.table)

set.seed(42)
# Reset ####
rm(list = ls())
gc()

# Variables ####
social = "twitter"

filename <-
  paste(social,
        "overall_graph_metrics.parquet",
        sep = "_")

overall_graph_metrics_filename <-
  paste(
    "overall_tree_data",
    filename,
    sep = "/"
  )

output_filename <- paste(
  "robustness_check_data",
  paste(social, "permutation_results.parquet", sep = "_"),
  sep = "/"
)

# Load Data ####
print(paste("Reading", overall_graph_metrics_filename))

tw_metrics <- read_parquet(overall_graph_metrics_filename) %>%
  setDT() 

# Select only one index
tw_metrics = select(tw_metrics,"toxicity_score","topic") %>%
  na.omit()

# Permutation
use = tw_metrics
N = 1000 # Number of permutation
aux = tw_metrics[,.(m = mean(toxicity_score)), by = .(topic)]
m_obs = aux$m[2] - aux$m[1]

m_rand = rep(0,N)

for (i in 1:N) {
  
  use[, toxicity_score := sample(toxicity_score)] 
  aux = use[,.(m = mean(toxicity_score)), by = .(topic)]
  m_rand[i] = aux$m[2] - aux$m[1]
  
}

p = (sum(abs(m_rand) >= abs(m_obs)))/N

dt = data.table(m = m_rand)

ggplot(dt, aes(m)) + geom_histogram(col = "black",
                                    fill = "lightblue") +
  geom_vline(xintercept = m_obs, col = "red") +
  geom_vline(xintercept = -m_obs, col = "red") +
  theme_bw()
