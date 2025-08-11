library(rtdists)
library(brms)


# ----- simulating data set -----
set.seed(123)

# Parameters
n_subj <- 20
n_trials <- 40

# Simulate fixed DDM parameters for all subjects
a <- 1.5       # boundary separation
v <- 1.0       # drift rate (toward correct decision)
t0 <- 0.3      # non-decision time
z <- 0.5       # unbiased starting point (centered)
s_0 <- 1       # high noise (default value)
s_1 <- 0.5     # low noise (reduced value)

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
# ?rdiffusion

# Convert response to factor
sim_data$response <- as.factor(sim_data$response)


# ---- fit data ----

# Fit the model
ddm_fit <- brm(
  formula = rt | dec(response) ~ 1 + (1 | subject),
  data = sim_data,
  family = wiener(),   # Drift Diffusion model
  chains = 2,          # Use fewer chains for speed (increase for real use)
  iter = 1000,         # Fewer iterations for demo (increase to 2000+ for real fits)
  cores = 2
)
# check fit
summary(ddm_fit)
plot(ddm_fit)



# ---- model with condition -----
fit_cond <- brm(
  formula = rt | dec(response) ~ condition + (1 | subject),
  data = sim_data,
  family = wiener(),
  chains = 2, iter = 1000, cores = 2
)


