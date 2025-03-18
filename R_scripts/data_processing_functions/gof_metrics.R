library(data.table)
library(stats)

# Compute Mean Error (ME)
# ME is the average of the difference between the simulated and observed data. A positive value indicates overestimation.
compute_me <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    me <- mean(simulated_data - observed_data, na.rm = TRUE)
    return(me)
  } else {
    return(NA)
  }
}

# Compute Percent Bias (PBIAS)
# PBIAS quantifies the percentage of bias between the simulated and observed data.
compute_pbias <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    pbias <- 100 * sum(simulated_data - observed_data, na.rm = TRUE) / sum(observed_data, na.rm = TRUE)
    return(pbias)
  } else {
    return(NA)
  }
}

# Compute Mean Absolute Error (MAE)
# MAE is the average of the absolute differences between the simulated and observed data.
compute_mae <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    mae <- mean(abs(simulated_data - observed_data), na.rm = TRUE)
    return(mae)
  } else {
    return(NA)
  }
}

# Compute Root Mean Squared Error (RMSE)
# RMSE is the square root of the average of the squared differences between the simulated and observed data.
compute_rmse <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    rmse <- sqrt(mean((simulated_data - observed_data)^2, na.rm = TRUE))
    return(rmse)
  } else {
    return(NA)
  }
}

# Compute R-squared (R²)
# R² indicates the proportion of variance in the observed data explained by the simulated data.
compute_r_squared <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    ss_total <- sum((observed_data - mean(observed_data, na.rm = TRUE))^2, na.rm = TRUE)
    ss_residual <- sum((observed_data - simulated_data)^2, na.rm = TRUE)
    r_squared <- 1 - (ss_residual / ss_total)
    return(r_squared)
  } else {
    return(NA)
  }
}

# Compute Pearson Correlation Coefficient
# Pearson correlation indicates the strength and direction of the linear relationship between observed and simulated data.
compute_pearson_corr <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    cor_value <- cor(observed_data, simulated_data, use = "complete.obs")
    return(cor_value)
  } else {
    return(NA)
  }
}

# Compute Nash-Sutcliffe Efficiency (NSE)
# NSE evaluates how well the simulated data predicts the observed data, with a perfect model yielding an NSE of 1.
compute_nse <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    mean_obs <- mean(observed_data, na.rm = TRUE)
    numerator <- sum((observed_data - simulated_data)^2, na.rm = TRUE)
    denominator <- sum((observed_data - mean_obs)^2, na.rm = TRUE)
    return(1 - (numerator / denominator))
  } else {
    return(NA)
  }
}

# Compute Kling-Gupta Efficiency (KGE)
# KGE quantifies the balance between correlation, bias, and variability in the model prediction.
compute_kge <- function(dt, scenario_variant1, scenario_variant2) {
  observed_data <- dt[scenario_variant == scenario_variant1, Mean]
  simulated_data <- dt[scenario_variant == scenario_variant2, Mean]
  
  if (length(observed_data) == length(simulated_data) && length(observed_data) > 0) {
    # Correlation
    r <- cor(observed_data, simulated_data, use = "complete.obs")
    
    # Bias
    mean_obs <- mean(observed_data, na.rm = TRUE)
    mean_sim <- mean(simulated_data, na.rm = TRUE)
    bias <- mean_sim / mean_obs
    
    # Variability
    sd_obs <- sd(observed_data, na.rm = TRUE)
    sd_sim <- sd(simulated_data, na.rm = TRUE)
    variability <- sd_sim / sd_obs
    
    # KGE calculation
    kge <- 1 - sqrt((r - 1)^2 + (bias - 1)^2 + (variability - 1)^2)
    return(kge)
  } else {
    return(NA)
  }
}