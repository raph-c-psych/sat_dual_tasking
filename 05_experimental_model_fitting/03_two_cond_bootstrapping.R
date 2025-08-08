# ---- loading packages ----

library(rtdists)
library(brms)
library(tidyverse)


# ========= testing with simulated data set =========
# ---- simulating data set ----
# set.seed(123)

# Parameters
n_subj <- 20
n_trials <- 40

# Simulate fixed DDM parameters for all subjects
v <- 2.0       # drift rate (toward correct decision) {1.0}
a <- 3.5       # boundary separation {1.5}
t0 <- 0.4      # non-decision time {0.3}
z <- a * 0.5       # unbiased starting point (centered) {0.5}
s_0 <- 0.755       # high noise (default value) {1}
s_1 <- 1.25     # low noise (reduced value) {0.5}

# Generate data frame
sim_data <- do.call(rbind, lapply(1:n_subj, function(id) {
  if (id %% 2 == 0) {
    d <- rdiffusion(n = n_trials, a = a, v = v, t0 = t0, z = z, s = s_0)
    d$condition <- "high"
  } else {
    d <- rdiffusion(n = n_trials, a = a, v = v, t0 = t0, z = z, s = s_1)
    d$condition <- "low"
  }
  d$subject <- id
  return(d)
}))
?rdiffusion

# Make sure response is a character
sim_data$response <- as.character(sim_data$response)

# splitting data set
group_low <- subset(sim_data, condition == "low")
group_high <- subset(sim_data, condition == "high")

# removing excess data
rm(a, n_subj, n_trials, s_0, s_1, t0, v, z)


# ------ estimate all parameters other than noise s -----
# Negative log-likelihood for joint estimation of v, a, t0, z (fix s = 1)
nll_common <- function(par, data) {
  v <- par[1]
  a <- par[2]
  t0 <- par[3]
  z <- par[4]
  
  # Bounds
  if (a <= 0 || t0 <= 0 || z <= 0 || z >= 1) return(1e6)
  
  probs <- dwiener(
    data$rt,
    data$response,
    alpha = a,
    tau = t0,
    beta = z,
    delta = v
  )
  
  probs[probs <= 1e-10] <- 1e-10
  -sum(log(probs))
}

# Starting values for parameters: v, a, t0, z
start_vals <- c(v = 1.0, a = 1.5, t0 = 0.3, z = 0.5)

# Fit to full dataset (s = 1 is assumed)
fit_common <- optim(par = start_vals, fn = nll_common, data = sim_data,
                    method = "Nelder-Mead")

# Extract estimated parameters
est_v <- fit_common$par[1]
est_a <- fit_common$par[2]
est_t0 <- fit_common$par[3]
est_z <- fit_common$par[4]

cat("Estimated v:", est_v, "\n")
cat("Estimated a:", est_a, "\n")
cat("Estimated t0:", est_t0, "\n")
cat("Estimated z:", est_z, "\n")



# ----- esimate noise by group -----
nll_s <- function(s, data, v, a, t0, z) {
  if (s <= 0) return(1e6)
  
  a_scaled <- a / s
  v_scaled <- v / s

  probs <- dwiener(
    data$rt,
    data$response,
    alpha = a_scaled,
    tau = t0,
    beta = z,
    delta = v_scaled
  )

  probs[probs <= 1e-10] <- 1e-10
  -sum(log(probs))
}

# Estimate s per group using fixed parameters
opt_s_low <- optim(par = 1, fn = nll_s, data = group_low,
                   v = est_v, a = est_a, t0 = est_t0, z = est_z,
                   method = "Brent", lower = 0.1, upper = 3)
?optim

opt_s_high <- optim(par = 1, fn = nll_s, data = group_high,
                    v = est_v, a = est_a, t0 = est_t0, z = est_z,
                    method = "Brent", lower = 0.1, upper = 3)

cat("Estimated s for Group low :", opt_s_low$par, "\n")
cat("Estimated s for Group high:", opt_s_high$par, "\n")


# ---- bootstrapping confidence intervalls ----
bootstrap_s <- function(data, v, a, t0, z, 
                        n_boot = 1000, 
                        lower = 0.1, 
                        upper = 3,
                        verbose = TRUE) {
  # Ensure response is in the right format
  data$response <- as.character(data$response)
  
  # Storage for bootstrap estimates
  s_boot <- numeric(n_boot)
  
  for (i in 1:n_boot) {
    # Resample with replacement
    resample <- data[sample(nrow(data), replace = TRUE), ]
    
    # Fit s for this resample
    fit <- tryCatch({
      optim(par = 1, fn = nll_s, data = resample,
            v = v, a = a, t0 = t0, z = z,
            method = "Brent", lower = lower, upper = upper)
    }, error = function(e) {
      list(par = NA)
    })
    
    s_boot[i] <- fit$par
  }
  
  # Remove failed fits
  s_boot <- s_boot[!is.na(s_boot)]
  
  # Compute summary
  mean_s <- mean(s_boot)
  ci <- quantile(s_boot, probs = c(0.025, 0.975))
  
  if (verbose) {
    cat("Bootstrap mean s:", round(mean_s, 4), "\n")
    cat("95% CI:", round(ci[1], 4), "-", round(ci[2], 4), "\n")
  }
  
  return(list(mean = mean_s, ci = ci, samples = s_boot))
}

# First estimate v, a, t0, z using the full dataset
# (Assume you already did this and stored them in est_v, est_a, est_t0, est_z)

# Now bootstrap s for each group:
result_low  <- bootstrap_s(group_low, est_v, est_a, est_t0, est_z)
result_high <- bootstrap_s(group_high, est_v, est_a, est_t0, est_z)

hist(result_low$samples, breaks = 30, col = "skyblue", 
     main = "Bootstrap Distribution: s (low noise)", xlab = "s")

hist(result_high$samples, breaks = 30, col = "salmon", 
     main = "Bootstrap Distribution: s (high noise)", xlab = "s")



# ========= fitting with real data set =========

# ---- loading data set -----
real_data <- read_csv("/Users/raphaelchristmann/Documents/Psychologie/4. Methodik/EmPra/data_sets_empra/data_aleks/dataRTFehler_wide.csv")
