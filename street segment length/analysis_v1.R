## Load the data

# Read the CSV file
df <- read.csv("Barcelona_street_lengths.csv")

# Rename the first column to "length"
names(df)[1] <- "length"

# Inspect
head(df)

## Estimate the density

# Estimate the kernel density of the street-length data
dens <- density(df$length, bw = "SJ")

# Convert the 'density' object to a data frame
dens_df <- data.frame(x = dens$x, y = dens$y)

# Load ggplot2
library(ggplot2)

ggplot() +
  # 1) Scatterplot of the raw data at its estimated density
  geom_point(
    data = df, aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +

  # 2) Add a thin line connecting the points
  geom_line(
    data = df, aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +

  # 3) Set log-log scales
  scale_x_log10() +
  scale_y_log10() +

  # 4) Add titles and labels
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +

  # 5) Minimalist theme
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

############################################# Modelling

library(stats4)

# Function to compute the normalization constant
compute_normalization_constant <- function(alpha, beta, gamma, l_min = 1e-6, l_max = Inf) {
  f_unnormalized <- function(l, alpha, beta, gamma) {
    exp(-alpha / l - beta * l) * l^(-gamma)
  }
  
  # Attempt integration and catch errors
  result <- try(integrate(f_unnormalized,
                          lower = l_min, upper = l_max,
                          alpha = alpha, beta = beta, gamma = gamma),
                silent = TRUE)
  
  # Check if integration failed or produced non-finite results
  if (inherits(result, "try-error") || !is.finite(result$value) || result$message != "OK") {
    return(NA)  # Indicate failure
  }
  
  return(1 / result$value)
}

minus_log_likelihood <- function(alpha, beta, gamma) {
  # Attempt to compute the normalization constant
  C <- compute_normalization_constant(alpha, beta, gamma)
  
  # If integration failed, return a large penalty value
  if (is.na(C) || !is.finite(C)) {
    return(1e12)  # Large penalty to discourage this parameter set
  }
  
  # Compute the negative log-likelihood using valid normalization constant
  value <- -sum(log(C * exp(-alpha / df$length - beta * df$length) * df$length^(-gamma)))

  # If computation yields non-finite result, also penalize
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

# Parameter estimation via MLE


result_1 <- mle(
  minus_log_likelihood,
  start = c(alpha = 145, beta = 1/2000, gamma = 4),  # Named numeric vector for start
  method = "L-BFGS-B",
  lower = c(alpha = 1e-6, beta = 1e-6, gamma = 1e-6),      # Named numeric vector for lower
  upper = c(alpha = Inf, beta = Inf, gamma = Inf)         # Named numeric vector for upper
)

result_1

result_2 <- mle(
  minus_log_likelihood,
  start = c(alpha = 100, beta = 0.001, gamma = 3),  # Named numeric vector for start
  method = "L-BFGS-B",
  lower = c(alpha = 1e-6, beta = 1e-6, gamma = 1e-6),      # Named numeric vector for lower
  upper = c(alpha = Inf, beta = Inf, gamma = Inf)         # Named numeric vector for upper
)

result_2

result_test <- mle(
  minus_log_likelihood,
  start = c(alpha = 500, beta = 0.001, gamma = 5),  # Named numeric vector for start
  method = "L-BFGS-B",
  lower = c(alpha = 1e-6, beta = 1e-6, gamma = 1e-6),      # Named numeric vector for lower
  upper = c(alpha = Inf, beta = Inf, gamma = Inf)         # Named numeric vector for upper
)

result_test

print(minus_log_likelihood(alpha =118.38138631, beta =0.00579913 , gamma =2.27288330))
print(minus_log_likelihood(alpha =1.449748e+02, beta =1.000000e-10 , gamma =3.617356e+00))
print(minus_log_likelihood(alpha =145, beta =1/2000 , gamma =3.36))
print(minus_log_likelihood(alpha =2.500000e+02, beta =1.841261e-03 , gamma =4.000074e+00))
print(minus_log_likelihood(alpha =2.000000e+02, beta =6.405599e-03 , gamma =3.000040e+00))
print(minus_log_likelihood(alpha =5.856739691, beta =0.011443168 , gamma =0.007618103))

print(minus_log_likelihood(alpha =6.15611022   , beta =0.01154206    , gamma =0.01154206  ))

get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

get_AIC(minus_log_likelihood(alpha =118.38138631, beta =0.00579913 , gamma =2.27288330), 3, length(df$length))

get_AIC(minus_log_likelihood(alpha =5.856739691, beta =0.011443168 , gamma =0.007618103), 3, length(df$length))

############################## Parameter tests ####################################

# Define a list of starting parameter vectors
start_grid <- list(
  c(alpha = 500, beta = 0.001, gamma = 5),
  c(alpha = 400, beta = 0.0005, gamma = 4.5),
  c(alpha = 600, beta = 0.002, gamma = 5.5),
  c(alpha = 300, beta = 0.0015, gamma = 4),
  c(alpha = 700, beta = 0.0008, gamma = 6)
  # Add as many starting points as needed
)

results_list <- vector("list", length(start_grid))

for (i in seq_along(start_grid)) {
  init <- start_grid[[i]]
  cat("Trying initialization:", init, "\n")
  # Use tryCatch to handle potential errors during estimation
  est <- tryCatch({
    mle(
      minus_log_likelihood,
      start = init,
      method = "L-BFGS-B",
      lower = c(alpha = 1e-6, beta = 1e-6, gamma = 1e-6),
      upper = c(alpha = Inf, beta = Inf, gamma = Inf)
    )
  }, error = function(e) {
    message("Error with initialization ", i, ": ", e$message)
    return(NULL)  # return NULL on failure
  })
  
  results_list[[i]] <- est
}

# Filter out NULL entries from failed fits
valid_results <- Filter(Negate(is.null), results_list)


# Extract minus log-likelihood values from each valid result
mll_values <- sapply(valid_results, function(res) {
  # -logLik gives the minus log-likelihood since mle maximizes log-likelihood
  -as.numeric(logLik(res))
})

# Find the index of the minimum minus log-likelihood
best_index <- which.min(mll_values)
best_result <- valid_results[[best_index]]

cat("Best initialization resulted in:\n")
print(best_result)
cat("Minimum minus log-likelihood:", mll_values[best_index], "\n")


######################### Plotting ########################################

params <- c(alpha = 5.856739691, beta = 0.011443168, gamma = 0.007618103)

l_seq <- seq(min(df$length), max(df$length), length.out = 300)

# Compute normalization constants
C <- compute_normalization_constant(params["alpha"], params["beta"], params["gamma"])

# Compute densities for each model at the sequence of lengths
density <- C * exp(-params["alpha"] / l_seq - params["beta"] * l_seq) * l_seq^(-params["gamma"])



model_df <- data.frame(length = l_seq, density = density, model = "Best Model")



library(ggplot2)

ggplot() +
  # Original data density plot
  geom_point(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +
  geom_line(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +
  
  # Overlay model predictions
  geom_line(
    data = model_df,
    aes(x = length, y = density, color = model),
    size = 1
  ) +
  
  # Log-log scales and labels
  scale_x_log10() +
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom"  # Show legend to distinguish models
  )

#############################################
###################### Model 1: the simplest
#############################################

# 1) Compute the normalization constant
compute_normalization_constant_model_1 <- function(gamma, l_min = 1e-6, l_max = Inf) {
  f_unnormalized <- function(l, gamma) {
    l^(-gamma)
  }
  
  # Attempt integration and catch errors
  result <- try(integrate(f_unnormalized,
                          lower = l_min, upper = l_max,
                          gamma = gamma),
                silent = TRUE)
  
  # Check if integration failed or produced non-finite results
  if (inherits(result, "try-error") || !is.finite(result$value) || result$message != "OK") {
    return(NA)  # Indicate failure
  }
  
  return(1 / result$value)
}

# 2) Compute the minus the log-likelihood

minus_log_likelihood_model_1 <- function(gamma) {
  # Attempt to compute the normalization constant
  C <- compute_normalization_constant_model_1(gamma)
  
  # If integration failed, return a large penalty value
  if (is.na(C) || !is.finite(C)) {
    return(1e12)  # Large penalty to discourage this parameter set
  }
  
  # Compute the negative log-likelihood using valid normalization constant
  value <- -sum(log(C * df$length^(-gamma)))
  
  # If computation yields non-finite result, also penalize
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

# 3) Parameter estimation via MLE
# Define a list of starting parameter vectors
start_grid <- list(
  c(gamma = 1),
  c(gamma = 2),
  c(gamma = 3),
  c(gamma = 4),
  c(gamma = 5),
  c(gamma = 6),
  c(gamma = 7),
  c(gamma = 8)
)

results_list <- vector("list", length(start_grid))

for (i in seq_along(start_grid)) {
  init <- start_grid[[i]]
  cat("Trying initialization:", init, "\n")
  # Use tryCatch to handle potential errors during estimation
  est <- tryCatch({
    mle(
      minus_log_likelihood_model_1,
      start = init,  # Named numeric vector for start
      method = "L-BFGS-B",
      lower = c(gamma = 0),      # Named numeric vector for lower
      upper = c(gamma = Inf)         # Named numeric vector for upper
    )
  }, error = function(e) {
    message("Error with initialization ", i, ": ", e$message)
    return(NULL)  # return NULL on failure
  })
  
  results_list[[i]] <- est
}

# Filter out NULL entries from failed fits
valid_results <- Filter(Negate(is.null), results_list)


# Extract minus log-likelihood values from each valid result
mll_values <- sapply(valid_results, function(res) {
  # -logLik gives the minus log-likelihood since mle maximizes log-likelihood
  -as.numeric(logLik(res))
})

# Find the index of the minimum minus log-likelihood
best_index <- which.min(mll_values)
best_result <- valid_results[[best_index]]

cat("Best initialization resulted in:\n")
print(best_result)
cat("Minimum minus log-likelihood:", mll_values[best_index], "\n")

# 4) Obtain AIC value
get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

get_AIC(minus_log_likelihood_model_1(1.05548),1, length(df$length))

########################################################
# 5) Plot on top of the empirical distribution

params <- c(gamma = 1.05548)

l_seq <- seq(min(df$length), max(df$length), length.out = 300)

# Compute normalization constants
C <- compute_normalization_constant_model_1(params["gamma"])

# Compute densities for each model at the sequence of lengths
density <- C * l_seq^(-params["gamma"])



model_df <- data.frame(length = l_seq, density = density, model = "Best Model")


ggplot() +
  # Original data density plot
  geom_point(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +
  geom_line(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +
  
  # Overlay model predictions
  geom_line(
    data = model_df,
    aes(x = length, y = density, color = model),
    size = 1
  ) +
  
  # Log-log scales and labels
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom"  # Show legend to distinguish models
  )


##################### Model 2: a bit more complex

# 1) Compute the normalization constant
compute_normalization_constant_model_2 <- function(beta, gamma, l_min = 1e-6, l_max = Inf) {
  f_unnormalized <- function(l, beta, gamma) {
    l^(-gamma) * exp(-beta * l)
  }
  
  # Attempt integration and catch errors
  result <- try(integrate(f_unnormalized,
                          lower = l_min, upper = l_max,
                          beta = beta, gamma = gamma),
                silent = TRUE)
  
  # Check if integration failed or produced non-finite results
  if (inherits(result, "try-error") || !is.finite(result$value) || result$message != "OK") {
    return(NA)  # Indicate failure
  }
  
  return(1 / result$value)
}

# 2) Compute the minus the log-likelihood

minus_log_likelihood_model_2 <- function(beta, gamma) {
  # Attempt to compute the normalization constant
  C <- compute_normalization_constant_model_2(beta, gamma)
  
  # If integration failed, return a large penalty value
  if (is.na(C) || !is.finite(C)) {
    return(1e12)  # Large penalty to discourage this parameter set
  }
  
  # Compute the negative log-likelihood using valid normalization constant
  value <- -sum(log(C * df$length^(-gamma) * exp(-beta * df$length)))
  
  # If computation yields non-finite result, also penalize
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

# 3) Parameter estimation via MLE
# Define a list of starting parameter vectors
start_grid <- list(
  c(beta = 0.001, gamma = 5),
  c(beta = 0.0005, gamma = 4.5),
  c(beta = 0.002, gamma = 5.5),
  c(beta = 0.0015, gamma = 4),
  c(beta = 0.0008, gamma = 6),
  c(beta = 0.0008, gamma = 3),
  c(beta = 0.0008, gamma = 2),
  c(beta = 0.0008, gamma = 1),
  c(beta = 0.0008, gamma = 3),
  c(beta = 0.001, gamma = 3),
  c(beta = 0.0005, gamma = 2),
  c(beta = 0.002, gamma = 3),
  c(beta = 0.01, gamma = 5),
  c(beta = 0.01, gamma = 4.5),
  c(beta = 0.01, gamma = 5.5)
)

results_list <- vector("list", length(start_grid))

for (i in seq_along(start_grid)) {
  init <- start_grid[[i]]
  cat("Trying initialization:", init, "\n")
  # Use tryCatch to handle potential errors during estimation
  est <- tryCatch({
    mle(
      minus_log_likelihood_model_2,
      start = init,  # Named numeric vector for start
      method = "L-BFGS-B",
      lower = c(beta = 1e-6, gamma = 0),      # Named numeric vector for lower
      upper = c(beta = Inf, gamma = Inf)         # Named numeric vector for upper
    )
  }, error = function(e) {
    message("Error with initialization ", i, ": ", e$message)
    return(NULL)  # return NULL on failure
  })
  
  results_list[[i]] <- est
}

# Filter out NULL entries from failed fits
valid_results <- Filter(Negate(is.null), results_list)


# Extract minus log-likelihood values from each valid result
mll_values <- sapply(valid_results, function(res) {
  # -logLik gives the minus log-likelihood since mle maximizes log-likelihood
  -as.numeric(logLik(res))
})

# Find the index of the minimum minus log-likelihood
best_index <- which.min(mll_values)
best_result <- valid_results[[best_index]]

cat("Best initialization resulted in:\n")
print(best_result)
cat("Minimum minus log-likelihood:", mll_values[best_index], "\n")

# 4) Obtain AIC value
get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

get_AIC(minus_log_likelihood_model_2(beta = 0.01001578, gamma = 0),2, length(df$length))

########################################################
# 5) Plot on top of the empirical distribution

params <- c(beta = 0.01001578 , gamma = 0)

l_seq <- seq(min(df$length), max(df$length), length.out = 300)

# Compute normalization constants
C <- compute_normalization_constant_model_2(params["beta"], params["gamma"])

# Compute densities for each model at the sequence of lengths
density <- C * l_seq^(-params["gamma"]) * exp(-params["beta"] * l_seq)



model_df <- data.frame(length = l_seq, density = density, model = "Best Model")


ggplot() +
  # Original data density plot
  geom_point(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +
  geom_line(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +
  
  # Overlay model predictions
  geom_line(
    data = model_df,
    aes(x = length, y = density, color = model),
    size = 1
  ) +
  
  # Log-log scales and labels
  scale_x_log10() +
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom"  # Show legend to distinguish models
  )

##################### Model 3

# 1) Compute normalization constant for Model 3
compute_normalization_constant_model_3 <- function(alpha, gamma, l_min = 1e-6, l_max = Inf) {
  f_unnormalized <- function(l, alpha, gamma) {
    exp(-alpha / l) * l^(-gamma)
  }
  
  result <- try(integrate(f_unnormalized,
                          lower = l_min, upper = l_max,
                          alpha = alpha, gamma = gamma),
                silent = TRUE)
  
  if (inherits(result, "try-error") || !is.finite(result$value) || result$message != "OK") {
    return(NA)
  }
  
  return(1 / result$value)
}

# 2) Compute the minus log-likelihood for Model 3
minus_log_likelihood_model_3 <- function(alpha, gamma) {
  C <- compute_normalization_constant_model_3(alpha, gamma)
  if (is.na(C) || !is.finite(C)) {
    return(1e12)
  }
  
  value <- -sum(log(C * exp(-alpha / df$length) * df$length^(-gamma)))
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

# 3) Parameter estimation via MLE
# Define a list of starting parameter vectors
start_grid <- list(
  c(alpha = 150, gamma = 5),
  c(alpha = 200, gamma = 4.5),
  c(alpha = 250, gamma = 5.5),
  c(alpha = 300, gamma = 4),
  c(alpha = 400, gamma = 6),
  c(alpha = 500, gamma = 3),
  c(alpha = 150, gamma = 2),
  c(alpha = 200, gamma = 1),
  c(alpha = 250, gamma = 5.5),
  c(alpha = 300, gamma = 4),
  c(alpha = 400, gamma = 6),
  c(alpha = 500, gamma = 3)
)

results_list <- vector("list", length(start_grid))

for (i in seq_along(start_grid)) {
  init <- start_grid[[i]]
  cat("Trying initialization:", init, "\n")
  # Use tryCatch to handle potential errors during estimation
  est <- tryCatch({
    mle(
      minus_log_likelihood_model_3,
      start = init,  # Named numeric vector for start
      method = "L-BFGS-B",
      lower = c(alpha = 0, gamma = 0),      # Named numeric vector for lower
      upper = c(alpha = Inf, gamma = Inf)         # Named numeric vector for upper
    )
  }, error = function(e) {
    message("Error with initialization ", i, ": ", e$message)
    return(NULL)  # return NULL on failure
  })
  
  results_list[[i]] <- est
}

# Filter out NULL entries from failed fits
valid_results <- Filter(Negate(is.null), results_list)


# Extract minus log-likelihood values from each valid result
mll_values <- sapply(valid_results, function(res) {
  # -logLik gives the minus log-likelihood since mle maximizes log-likelihood
  -as.numeric(logLik(res))
})

# Find the index of the minimum minus log-likelihood
best_index <- which.min(mll_values)
best_result <- valid_results[[best_index]]

cat("Best initialization resulted in:\n")
print(best_result)
cat("Minimum minus log-likelihood:", mll_values[best_index], "\n")

# 4) Obtain AIC value
get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

get_AIC(minus_log_likelihood_model_3(alpha = 42.89366, gamma = 2.08045),2, length(df$length))

########################################################
# 5) Plot on top of the empirical distribution

params <- c(alpha = 42.89366 , gamma = 2.08045)

l_seq <- seq(min(df$length), max(df$length), length.out = 300)

# Compute normalization constants
C <- compute_normalization_constant_model_3(params["alpha"], params["gamma"])

# Compute densities for each model at the sequence of lengths
density <- C * exp(-params["alpha"] / l_seq) * l_seq^(-params["gamma"])



model_df <- data.frame(length = l_seq, density = density, model = "Best Model")


ggplot() +
  # Original data density plot
  geom_point(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +
  geom_line(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +
  
  # Overlay model predictions
  geom_line(
    data = model_df,
    aes(x = length, y = density, color = model),
    size = 1
  ) +
  
  # Log-log scales and labels
  scale_x_log10() +
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom"  # Show legend to distinguish models
  )


#################### Model 4

# 1) Compute normalization constant for Model 4
compute_normalization_constant_model_4 <- function(alpha, beta, l_min = 1e-6, l_max = Inf) {
  f_unnormalized <- function(l, alpha, beta) {
    exp(-alpha / l - beta * l)
  }
  
  result <- try(integrate(f_unnormalized,
                          lower = l_min, upper = l_max,
                          alpha = alpha, beta = beta),
                silent = TRUE)
  
  if (inherits(result, "try-error") || !is.finite(result$value) || result$message != "OK") {
    return(NA)
  }
  
  return(1 / result$value)
}

# 2) Compute the minus log-likelihood for Model 4
minus_log_likelihood_model_4 <- function(alpha, beta) {
  C <- compute_normalization_constant_model_4(alpha, beta)
  if (is.na(C) || !is.finite(C)) {
    return(1e12)
  }
  
  value <- -sum(log(C * exp(-alpha / df$length - beta * df$length)))
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

# 3) Parameter estimation via MLE
# Define a list of starting parameter vectors
start_grid <- list(
  c(alpha = 150, beta = 0.001),
  c(alpha = 200, beta = 0.01),
  c(alpha = 250, beta = 0.0001),
  c(alpha = 300, beta = 0.00001),
  c(alpha = 400, beta = 0.0005),
  c(alpha = 500, beta = 0.1),
  c(alpha = 150, beta = 0.1),
  c(alpha = 200, beta = 0.0001),
  c(alpha = 250, beta = 0.01),
  c(alpha = 300, beta = 0.00001),
  c(alpha = 400, beta = 0.01),
  c(alpha = 500, beta = 0.01)
)

results_list <- vector("list", length(start_grid))

for (i in seq_along(start_grid)) {
  init <- start_grid[[i]]
  cat("Trying initialization:", init, "\n")
  # Use tryCatch to handle potential errors during estimation
  est <- tryCatch({
    mle(
      minus_log_likelihood_model_4,
      start = init,  # Named numeric vector for start
      method = "L-BFGS-B",
      lower = c(alpha = 0, beta = 0),      # Named numeric vector for lower
      upper = c(alpha = Inf, gamma = Inf)         # Named numeric vector for upper
    )
  }, error = function(e) {
    message("Error with initialization ", i, ": ", e$message)
    return(NULL)  # return NULL on failure
  })
  
  results_list[[i]] <- est
}

# Filter out NULL entries from failed fits
valid_results <- Filter(Negate(is.null), results_list)


# Extract minus log-likelihood values from each valid result
mll_values <- sapply(valid_results, function(res) {
  # -logLik gives the minus log-likelihood since mle maximizes log-likelihood
  -as.numeric(logLik(res))
})

# Find the index of the minimum minus log-likelihood
best_index <- which.min(mll_values)
best_result <- valid_results[[best_index]]

cat("Best initialization resulted in:\n")
print(best_result)
cat("Minimum minus log-likelihood:", mll_values[best_index], "\n")

# 4) Obtain AIC value
get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

get_AIC(minus_log_likelihood_model_4(alpha = 5.77615942 , beta = 0.01149134 ), 2, length(df$length))

########################################################
# 5) Plot on top of the empirical distribution

params <- c(alpha = 5.77615942 , beta = 0.01149134 )

l_seq <- seq(min(df$length), max(df$length), length.out = 300)

# Compute normalization constants
C <- compute_normalization_constant_model_4(params["alpha"], params["beta"])

# Compute densities for each model at the sequence of lengths
density <- C * exp(-params["alpha"] / l_seq - params["beta"] * l_seq)




model_df <- data.frame(length = l_seq, density = density, model = "Best Model")


ggplot() +
  # Original data density plot
  geom_point(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +
  geom_line(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +
  
  # Overlay model predictions
  geom_line(
    data = model_df,
    aes(x = length, y = density, color = model),
    size = 1
  ) +
  
  # Log-log scales and labels
  scale_x_log10() +
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom"  # Show legend to distinguish models
  )

###################### Model 6
# 1) Compute normalization constant for Model 6
compute_normalization_constant_model_6 <- function(beta, l_min = 1e-6, l_max = Inf) {
  f_unnormalized <- function(l, beta) {
    exp(-beta * l)
  }
  
  result <- try(integrate(f_unnormalized,
                          lower = l_min, upper = l_max,
                          beta = beta),
                silent = TRUE)
  
  if (inherits(result, "try-error") || !is.finite(result$value) || result$message != "OK") {
    return(NA)
  }
  
  return(1 / result$value)
}

# 2) Compute the minus log-likelihood for Model 6
minus_log_likelihood_model_6 <- function(beta) {
  C <- compute_normalization_constant_model_6(beta)
  if (is.na(C) || !is.finite(C)) {
    return(1e12)
  }
  
  value <- -sum(log(C * exp(-beta * df$length)))
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

# 3) Parameter estimation via MLE
# Define a list of starting parameter vectors
start_grid <- list(
  c(beta = 0.1),
  c(beta = 0.01),
  c(beta = 0.001),
  c(beta = 0.0001),
  c(beta = 0.00001),
  c(beta = 0.000001)
)

results_list <- vector("list", length(start_grid))

for (i in seq_along(start_grid)) {
  init <- start_grid[[i]]
  cat("Trying initialization:", init, "\n")
  # Use tryCatch to handle potential errors during estimation
  est <- tryCatch({
    mle(
      minus_log_likelihood_model_6,
      start = init,  
      method = "L-BFGS-B",
      lower = c(beta = 0),      
      upper = c(beta = Inf)         
    )
  }, error = function(e) {
    message("Error with initialization ", i, ": ", e$message)
    return(NULL)  # return NULL on failure
  })
  
  results_list[[i]] <- est
}

# Filter out NULL entries from failed fits
valid_results <- Filter(Negate(is.null), results_list)


# Extract minus log-likelihood values from each valid result
mll_values <- sapply(valid_results, function(res) {
  # -logLik gives the minus log-likelihood since mle maximizes log-likelihood
  -as.numeric(logLik(res))
})

# Find the index of the minimum minus log-likelihood
best_index <- which.min(mll_values)
best_result <- valid_results[[best_index]]

cat("Best initialization resulted in:\n")
print(best_result)
cat("Minimum minus log-likelihood:", mll_values[best_index], "\n")

# 4) Obtain AIC value
get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

get_AIC(minus_log_likelihood_model_6(beta = 0.01001878), 1, length(df$length))

########################################################
# 5) Plot on top of the empirical distribution

params <- c(beta = 0.01001878)

l_seq <- seq(min(df$length), max(df$length), length.out = 300)

# Compute normalization constants
C <- compute_normalization_constant_model_6(params["beta"])

# Compute densities for each model at the sequence of lengths
density <- C * exp(-params["beta"] * l_seq)



model_df <- data.frame(length = l_seq, density = density, model = "Best Model")


ggplot() +
  # Original data density plot
  geom_point(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", alpha = 0.6
  ) +
  geom_line(
    data = df, 
    aes(x = length, y = approx(dens$x, dens$y, xout = length)$y),
    color = "steelblue", size = 0.5
  ) +
  
  # Overlay model predictions
  geom_line(
    data = model_df,
    aes(x = length, y = density, color = model),
    size = 1
  ) +
  
  # Log-log scales and labels
  scale_x_log10() +
  labs(
    title = "Barcelona's Street Length Distribution (Log-Log)",
    x = "Street Length (m)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom"  # Show legend to distinguish models
  )
