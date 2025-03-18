

# Compute NSE values
compute_nse <- function(df, scenario1, scenario2) {
  observed <- df[scenario == scenario1, Mean]
  simulated <- df[scenario == scenario2, Mean]
  
  if (length(observed) == length(simulated) && length(observed) > 0) {
    mean_obs <- mean(observed, na.rm = TRUE)
    numerator <- sum((observed - simulated)^2, na.rm = TRUE)
    denominator <- sum((observed - mean_obs)^2, na.rm = TRUE)
    return(1 - (numerator / denominator))
  } else {
    return(NA)
  }
}
