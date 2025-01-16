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

## Modelling

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

result <- mle(
  minus_log_likelihood,
  start = c(alpha = 145, beta = 1 / 2000, gamma = 20),  # Named numeric vector for start
  method = "L-BFGS-B",
  lower = c(alpha = 0, beta = 1e-6, gamma = -Inf),      # Named numeric vector for lower
  upper = c(alpha = Inf, beta = Inf, gamma = Inf)         # Named numeric vector for upper
)

result
