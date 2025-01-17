############ Load required packages ############

library(ggplot2)
library(stats4)
library(xtable)

############ Define city list and initialize summary data frame ############

cities <- c("Barcelona", "Girona", "Tarragona", "Lleida")

combined_model_summary <- data.frame(
  City = character(),
  Model = character(),
  Parameters = character(),
  AIC = numeric(),
  AIC_diff = numeric(),
  stringsAsFactors = FALSE
)

# Create 'figures' directory if it doesn't exist
if(!dir.exists("figures")) {
  dir.create("figures")
}

################# Define minus log-likelihood for each model #################

########## Model 1 ##########

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

########## Model 2 ##########

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

########## Model 3 ##########

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

########## Model 4 ##########

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

########## Model 5 ##########

# 1) Compute normalization constant for Model 5
compute_normalization_constant_model_5 <- function(beta, l_min = 1e-6, l_max = Inf) {
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

# 2) Compute the minus log-likelihood for Model 5
minus_log_likelihood_model_5 <- function(beta) {
  C <- compute_normalization_constant_model_5(beta)
  if (is.na(C) || !is.finite(C)) {
    return(1e12)
  }
  
  value <- -sum(log(C * exp(-beta * df$length)))
  if (!is.finite(value)) {
    return(1e12)
  }
  
  return(value)
}

########## Model 6 ##########

compute_normalization_constant_model_6 <- function(alpha, beta, gamma, l_min = 1e-6, l_max = Inf) {
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

minus_log_likelihood_model_6 <- function(alpha, beta, gamma) {
  # Attempt to compute the normalization constant
  C <- compute_normalization_constant_model_6(alpha, beta, gamma)
  
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


################# Helper functions for estimation and plotting #################

get_AIC <- function(min_log_lik, K, N) {
  2* min_log_lik + 2 * K * N / (N - K - 1)  # AIC with correction for sample size
}

run_model_estimation <- function(model_spec, df) {
  results_list <- vector("list", length(model_spec$starting_grid))
  
  for (i in seq_along(model_spec$starting_grid)) {
    init <- model_spec$starting_grid[[i]]
    cat(model_spec$id, "- Trying initialization:", init, "\n")
    est <- tryCatch({
      mle(
        model_spec$minus_log_likelihood,
        start = init,
        method = "L-BFGS-B",
        lower = model_spec$lower,
        upper = model_spec$upper
      )
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    results_list[[i]] <- est
  }
  
  valid_results <- Filter(Negate(is.null), results_list)
  
  # Handle case with no valid results
  if(length(valid_results) == 0) {
    warning(paste("No valid results found for", model_spec$id))
    return(list(best_result = NULL, best_params = NULL, min_neg_loglik = NA))
  }
  
  mll_values <- sapply(valid_results, function(res) -as.numeric(logLik(res)))
  best_index <- which.min(mll_values)
  best_result <- valid_results[[best_index]]
  best_params <- coef(best_result)
  
  list(
    best_result = best_result,
    best_params = best_params,
    min_neg_loglik = mll_values[best_index]
  )
}


plot_model <- function(model_spec, params, df, dens, city, log_y = FALSE) {
  l_seq <- seq(min(df$length), max(df$length), length.out = 300)
  density <- model_spec$compute_density(params, l_seq)
  model_df <- data.frame(length = l_seq, density = density, model = model_spec$id)
  
  p <- ggplot() +
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
    geom_line(
      data = model_df,
      aes(x = length, y = density, color = model),
      size = 1
    ) +
    scale_x_log10() +
    labs(
      title = paste(city, "'s Street Length Distribution -", model_spec$id),
      x = "Street Length (m)",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "bottom"
    )
  
  # Conditionally add log10 scale for y-axis
  if(log_y) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}


################### Define model specifications in a list ###################

# Model 1 Specification
model1_spec <- list(
  id = "Model 1",
  param_names = c("gamma"),
  lower = c(gamma = 0),
  upper = c(gamma = Inf),
  starting_grid = list(
    c(gamma = 1), c(gamma = 2), c(gamma = 3), c(gamma = 4),
    c(gamma = 5), c(gamma = 6), c(gamma = 7), c(gamma = 8)
  ),
  minus_log_likelihood = minus_log_likelihood_model_1,
  compute_density = function(params, l_seq) {
    C <- compute_normalization_constant_model_1(params["gamma"])
    C * l_seq^(-params["gamma"])
  }
)

# Model 2 Specification
model2_spec <- list(
  id = "Model 2",
  param_names = c("beta", "gamma"),
  lower = c(beta = 0, gamma = 0),
  upper = c(beta = Inf, gamma = Inf),
  starting_grid = list(
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
  ),
  minus_log_likelihood = minus_log_likelihood_model_2,
  compute_density = function(params, l_seq) {
    C <- compute_normalization_constant_model_2(params["beta"], params["gamma"])
    C * l_seq^(-params["gamma"]) * exp(-params["beta"] * l_seq)
  }
)

# Model 3 Specification
model3_spec <- list(
  id = "Model 3",
  param_names = c("alpha", "gamma"),
  lower = c(alpha = 0, gamma = 0),
  upper = c(alpha = Inf, gamma = Inf),
  starting_grid = list(
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
  ),
  minus_log_likelihood = minus_log_likelihood_model_3,
  compute_density = function(params, l_seq) {
    C <- compute_normalization_constant_model_3(params["alpha"], params["gamma"])
    C * exp(-params["alpha"] / l_seq) * l_seq^(-params["gamma"])
  }
)

# Model 4 Specification
model4_spec <- list(
  id = "Model 4",
  param_names = c("alpha", "beta"),
  lower = c(alpha = 0, beta = 0),
  upper = c(alpha = Inf, beta = Inf),
  starting_grid = list(
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
  ),
  minus_log_likelihood = minus_log_likelihood_model_4,
  compute_density = function(params, l_seq) {
    C <- compute_normalization_constant_model_4(params["alpha"], params["beta"])
    C * exp(-params["alpha"] / l_seq - params["beta"] * l_seq)
  }
)

# Model 5 Specification
model5_spec <- list(
  id = "Model 5",
  param_names = c("beta"),
  lower = c(beta = 0),
  upper = c(beta = Inf),
  starting_grid = list(
    c(beta = 0.1),
    c(beta = 0.01),
    c(beta = 0.001),
    c(beta = 0.0001),
    c(beta = 0.00001),
    c(beta = 0.000001)
  ),
  minus_log_likelihood = minus_log_likelihood_model_5,
  compute_density = function(params, l_seq) {
    C <- compute_normalization_constant_model_5(params["beta"])
    C * exp(-params["beta"] * l_seq)
  }
)

# Model 6 Specification
model6_spec <- list(
  id = "Model 6",
  param_names = c("alpha", "beta", "gamma"),
  lower = c(alpha = 0, beta = 0, gamma = 0),
  upper = c(alpha = Inf, beta = Inf, gamma = Inf),
  starting_grid = list(
    c(alpha = 1, beta = 0.01, gamma = 1),
    c(alpha = 10, beta = 0.001, gamma = 2),
    c(alpha = 100, beta = 0.0001, gamma = 3),
    c(alpha = 1000, beta = 0.00001, gamma = 4),
    c(alpha = 10000, beta = 0.000001, gamma = 5),
    c(alpha = 0.1, beta = 0.1, gamma = 6)
  ),
  minus_log_likelihood = minus_log_likelihood_model_6,
  compute_density = function(params, l_seq) {
    C <- compute_normalization_constant_model_6(params["alpha"], params["beta"], params["gamma"])
    C * exp(-params["alpha"] / l_seq - params["beta"] * l_seq) * l_seq^(-params["gamma"])
  }
)

# Combine all model specifications into a list
model_specs <- list(model1_spec, model2_spec, model3_spec, model4_spec, model5_spec, model6_spec)

################### Run estimations and plot results ###################

for(city in cities) {
  cat("Processing city:", city, "\n")
  
  # Load city-specific data
  df <- read.csv(paste0(city, "_street_lengths.csv"))
  names(df)[1] <- "length"
  dens <- density(df$length, bw = "SJ")
  
  # Update sample size for the current city
  N <- length(df$length)
  
  # Run estimations for each model for the current city
  results <- list()
  for(model_spec in model_specs) {
    cat("Estimating", model_spec$id, "for", city, "\n")
    res <- run_model_estimation(model_spec, df)
    results[[model_spec$id]] <- res
    
    # Check if estimation was successful before plotting
    if(is.null(res$best_params)) {
      cat("No valid result for", model_spec$id, "in", city, "\n")
    } else {
      # Generate and save plots without y-axis log scale
      p1 <- plot_model(model_spec, res$best_params, df, dens, city = city, log_y = FALSE)
      filename1 <- paste0("figures/", city, "_", model_spec$id, "_no_logy.png")
      ggsave(filename = filename1, plot = p1, width = 6, height = 4, dpi = 300)
      
      # Generate and save plots with y-axis log scale
      p2 <- plot_model(model_spec, res$best_params, df, dens, city = city, log_y = TRUE)
      filename2 <- paste0("figures/", city, "_", model_spec$id, "_logy.png")
      ggsave(filename = filename2, plot = p2, width = 6, height = 4, dpi = 300)
    }
  }
  
  # Create a summary for the current city
  city_summary <- data.frame(
    Model = character(),
    Parameters = character(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(model_spec in model_specs) {
    res <- results[[model_spec$id]]
    if(!is.null(res)) {
      params <- res$best_params
      param_str <- paste(names(params), sprintf("%.4g", params), collapse = ", ")
      K <- length(model_spec$param_names)
      aic_val <- get_AIC(res$min_neg_loglik, K, N)
      
      city_summary <- rbind(city_summary, data.frame(
        Model = model_spec$id,
        Parameters = param_str,
        AIC = aic_val,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Compute best AIC for the current city and AIC differences
  AIC_best_city <- min(city_summary$AIC, na.rm = TRUE)
  city_summary$AIC_diff <- city_summary$AIC - AIC_best_city
  city_summary$City <- city  # Add city column
  
  # Append the current city's summary to the combined summary
  combined_model_summary <- rbind(combined_model_summary, city_summary)
}


################ Generate Latex tables ################ 

# Separate tables for parameters and AIC differences, including city info
parameters_table <- combined_model_summary[, c("City", "Model", "Parameters")]
aic_diff_table <- combined_model_summary[, c("City", "Model", "AIC_diff")]

# Convert tables to LaTeX using xtable
latex_parameters <- xtable(parameters_table, caption = "Estimated Parameters by City and Model")
latex_aic_diff   <- xtable(aic_diff_table, caption = "AIC Differences by City and Model")

# Export LaTeX code to files
print(latex_parameters, include.rownames = FALSE, file = "parameters_table.tex")
print(latex_aic_diff, include.rownames = FALSE, file = "aic_diff_table.tex")

