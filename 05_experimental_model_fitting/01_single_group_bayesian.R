library(rtdists)
library(brms)


# ----- simulating data set -----
set.seed(123)

# Parameters
n_subj <- 20
n_trials <- 30

# Simulate fixed DDM parameters for all subjects
a <- 1.5       # boundary separation
v <- 1.0       # drift rate (toward correct decision)
t0 <- 0.3      # non-decision time
z <- 0.5       # unbiased starting point (centered)

# Generate data frame
sim_data <- do.call(rbind, lapply(1:n_subj, function(id) {
  d <- rdiffusion(n = n_trials, a = a, v = v, t0 = t0, z = z)
  d$subject <- id
  return(d)
}))
# ?rdiffusion

# Convert response to factor
sim_data$response <- as.factor(sim_data$response)

# add condition
# sim_data$condition <- rep(c("easy", "hard"), length.out = nrow(sim_data))


# Preview
head(sim_data)


# ---- fit data ----

# Fit the model
ddm_fit <- brm(
  formula = rt | dec(response) ~ 1 + (1 | subject),
  data = sim_data,
  family = wiener(),   # Drift Diffusion model
  chains = 2,          # Use fewer chains for speed (increase for real use)
  iter = 1000,         # Fewer iterations for demo (increase to 2000+ for real fits)
  cores = 2,
  silent = TRUE,       # suppress output
  refresh = 0          # suppress console updates
)
# check fit
summary(ddm_fit)
plot(ddm_fit)
?brm


# ---- model with condition -----
fit_cond <- brm(
  formula = rt | dec(response) ~ condition + (1 | subject),
  data = sim_data,
  family = wiener(),
  chains = 2, iter = 1000, cores = 2
)


