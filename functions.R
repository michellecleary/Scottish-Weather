# Michelle Cleary (s1979093, michellecleary)

# Place your function definitions that may be needed in analysis.R and report.Rmd
# in this file, including documentation.
# You can also include any needed library() calls here

#' Dataframe
#'
#' @param rows number of rows
#' @param cols number of columns
#'
#' @return a dataframe of size rows x cols with all entries equal to zero

df <- function(rows, cols) {
  # Create a zero matrix of size rows x cols
  M <- matrix(0, nrow = rows, ncol = cols)
  # Convert the matrix to a dataframe
  df <- data.frame(M)
  df
}

#' Test statistic
#'
#' @param data a dataset
#' @param station string consisting of the name of the station for which the test
#'                statistic is to be computed
#' @param type string; if "average", the average test statistic is computed;
#'                     if "prob", the empirical non-zero proportion test statistic
#'                     is computed
#'
#' @return the given type of test statistic for the given station in the given dataset

test_statistic <- function(data, station, type) {
  # Compute the winter and summer values for the average test statistic
  if (type == "average") {
    average_data <- data %>%
      # Only given station
      filter(Name == station) %>%
      group_by(ID, Name, Element, Season) %>%
      # Compute average precipitation
      summarise(Value = mean(Value), .groups = "drop")
    # Compute the winter and summer values
    winter_value <- filter(average_data, Season == "winter")$Value
    summer_value <- filter(average_data, Season == "summer")$Value
  }

  # Compute the winter and summer values for the empirical non-zero proportion
  # test statistic
  if (type == "prob") {
    prob_data <- data %>%
      # Only given station
      filter(Name == station) %>%
      group_by(Season) %>%
      # Count the number of days with rain
      count(Value > 0) %>%
      # Create a new column showing the proportion of rainy days
      mutate(Prob = ifelse(`Value > 0` == TRUE, yes = n / sum(n), no = 0)) %>%
      # Compute the total proportion of rainy days per season
      summarise(Prob = sum(Prob))
    # Compute the winter and summer values
    winter_value <- filter(prob_data, Season == "winter")$Prob
    summer_value <- filter(prob_data, Season == "summer")$Prob
  }

  # Compute the test statistic
  test_stat <- abs(winter_value - summer_value)
  test_stat
}

#' Test statistics dataframe
#'
#' @param data a dataset
#' @param station_names vector of strings of names of stations for which the test
#'                      statistics are to computed
#' @param type string; if "average", the average test statistic is computed;
#'                     if "prob", the empirical non-zero proportion test statistic
#'                     is computed
#'
#' @return a dataframe containing the appropriate test statistic for each station given

original_test_statistics <- function(data, station_names, type) {
  # Create a dataframe to store the results
  original_test_stats <- df(1, length(station_names))
  colnames(original_test_stats) <- station_names
  rownames(original_test_stats) <- "T"

  # Iterate over each station
  for (i in seq_len(length(station_names))) {
    # Compute the appropriate test statistic for the station
    T <- test_statistic(data, station_names[i], type)
    original_test_stats[i] <- T
  }
  # Return the test statistics
  original_test_stats
}

#' Test statistics for random permutations
#'
#' @param data a dataset
#' @param station string consisting of the name of the station for which the random test
#'                statistic is to be computed
#' @param type string; if "average", the average test statistic is computed;
#'                     if "prob", the empirical non-zero proportion test statistic
#'                     is computed
#'
#' @return the given type of test statistic for the given station in  the given dataset,
#'         with a random permutation of the seasons

randomisation <- function(data, station, type) {
  # Create a version of the dataset with a random permutation of the seasons
  randomisation_data <- data %>%
    filter(Name == station) %>%
    mutate(Season = sample(Season, length(Season), replace = FALSE)) %>%
    group_by(Season)

  # Compute the winter and summer values for the average test statistic
  if (type == "average") {
    randomisation_data <- randomisation_data %>%
      summarise(Value = mean(Value), .groups = "drop")

    # Compute the winter and summer values
    random_winter_value <- filter(randomisation_data, Season == "winter")$Value
    random_summer_value <- filter(randomisation_data, Season == "summer")$Value
  }

  # Compute the winter and summer values for the empirical non-zero proportion
  # test statistic
  if (type == "prob") {
    randomisation_data <- randomisation_data %>%
      # Count the number of days with rain
      count(Value > 0) %>%
      # Create a new column showing the proportion of rainy days
      mutate(Prob = ifelse(`Value > 0` == TRUE, yes = n / sum(n), no = 0)) %>%
      # Compute the total proportion of rainy days per season
      summarise(Prob = sum(Prob))

    # Compute the winter and summer values
    random_winter_value <- filter(randomisation_data, Season == "winter")$Prob
    random_summer_value <- filter(randomisation_data, Season == "summer")$Prob
  }

  # Compute the test statistic
  random_test_statistic <- abs(random_winter_value - random_summer_value)
  random_test_statistic
}

#' Confidence interval
#'
#' @param p_value the estimated p-value
#' @param N number of samples in the random permutation test
#'
#' @return if p_value = 0, an exact 95% confidence interval for the true p-value;
#'         if p_value > 0, an approximate 95% confidence interval for the true
#'         p-value

p_value_conf_int <- function(p_value, N = 10000) {
  # Compute an exact interval if p_value = 0
  if (p_value == 0) {
    CI <- c(0, 1 - 0.025^(1 / N))
  }

  # Compute an approximate interval otherwise
  else {
    z_alpha <- qnorm(0.975, mean = 0, sd = 1)
    lower <- p_value - z_alpha * sqrt((N * p_value * (N - N * p_value)) / (N^3))
    upper <- p_value + z_alpha * sqrt((N * p_value * (N - N * p_value)) / (N^3))
    CI <- c(lower, upper)
  }

  # Return the confidence interval
  CI
}

#' Leave-station-out cross validation
#'
#' @param dataset a dataset
#' @param stations vector of station names for which to carry out leave-station-out
#'                 cross-validation
#'
#' @return the original dataset with new columns containing the mean and standard
#'         deviations of the predictions, and the Squared Error and Dawid-Sebastiani
#'         scores

leave_station_out <- function(dataset, stations) {
  # Add columns to store the means and standard deviations
  data <- dataset %>%
    mutate(Mean = NA_real_, SD = NA_real_)

  # Iterate over the stations
  for (station in stations) {
    # Leave the station out
    model_data <- data %>%
      filter(Name != station)

    # Estimate a model using the remaining stations
    fit <- lm(sqrt(Value) ~ Latitude + Longitude + Elevation + AverageDecYear
      + cos(2 * pi * AverageDecYear) + sin(2 * pi * AverageDecYear),
    data = model_data
    )

    # Use the left out station as data for prediction
    pred_data <- data %>%
      filter(Name == station)

    # Predict the values for the left-out station
    pred <- predict(fit, newdata = pred_data, se.fit = TRUE)

    # Store the means and standard deviations
    data$Mean[which(data$Name == station)] <- pred$fit
    data$SD[which(data$Name == station)] <- sqrt(pred$se.fit^2 + pred$residual.scale^2)
  }

  # Compute and store the Squared Error and Dawid-Sebastiani scores
  data <- data %>%
    mutate(
      se = proper_score("se", Value, mean = Mean),
      ds = proper_score("ds", Value, mean = Mean, sd = SD)
    )
  data
}

