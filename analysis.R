# Michelle Cleary (s1979093, michellecleary)
# Place analysis code that may take too long to run every time the report.Rmd
# document is run.
# Run the code in this file with
#   source("analysis.R")
# in a fresh R session to ensure consistency of the results.

# Load function definitions
source("functions.R")

# Set the number of permutations
N <- 10000
# Initialise a matrix to store the random test statistics for the average rainfall
random_test_stats_average <- matrix(0, N, 8)

# Iterate over the number of permutations
for (i in seq_len(N)) {
  # Iterate over the number of stations
  for (j in seq_len(8)) {
    # Compute the random test statistic for each station for the current permutation
    random_test_stats_average[i, j] <- randomisation(ghcnd, station_names[j], "average")
  }
}

# Store the results in a dataframe
random_test_stats_average <- data.frame(random_test_stats_average)
colnames(random_test_stats_average) <- station_names
saveRDS(random_test_stats_average, file = "data/random_test_stats_average.rds")

# Initialise a matrix to store the random test statistics for the empirical
# non-zero proportion of rainfall
random_test_stats_prob <- matrix(0, N, 8)

# Iterate over the number of permutations
for (i in seq_len(N)) {
  # Iterate over the number of stations
  for (j in seq_len(8)) {
    # Compute the random test statistic for each station for the current permutation
    random_test_stats_prob[i, j] <- randomisation(ghcnd, station_names[j], "prob")
  }
}

# Store the results in a dataframe
random_test_stats_prob <- data.frame(random_test_stats_prob)
colnames(random_test_stats_prob) <- station_names
saveRDS(random_test_stats_prob, file = "data/random_test_stats_prob.rds")

