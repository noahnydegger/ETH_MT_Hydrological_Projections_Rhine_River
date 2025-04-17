library(terra)

#' Apply logit transformation to a raster stack
#' Clamps values to (eps, 1 - eps) to avoid infinite values
logit_transform <- function(r_stack, eps = 1e-6) {
  # Ensure values are in (0,1) interval
  r_stack_clipped <- clamp(r_stack, lower = eps, upper = 1 - eps)
  
  # Apply logit transformation
  logit_r <- log(r_stack_clipped / (1 - r_stack_clipped))
  
  return(logit_r)
}

#' Apply inverse logit transformation to a raster stack
#' Converts log-odds back to probability space (0–1)
inv_logit_transform <- function(logit_r_stack) {
  # Apply back transformation
  backtransformed <- 1 / (1 + exp(-logit_r_stack))
  
  return(backtransformed)
}

#' Bias-correct a relative sunshine raster stack in logit space
#' Shifts logit-transformed values by a constant and back-transforms
bias_correct_sund_rel <- function(r_stack, logit_diff = 1.077914) {
  # transform relative values [0, 1] to logit space
  r_stack_logit <- logit_transform(r_stack)
  
  # subtract difference (of reference - hindcast) in logit space
  r_stack_logit_bc <- r_stack_logit - logit_diff
  
  # backtransform to relative values [0, 1]
  r_stack_bc <- inv_logit_transform(r_stack_logit_bc)
  return(r_stack_bc)
}