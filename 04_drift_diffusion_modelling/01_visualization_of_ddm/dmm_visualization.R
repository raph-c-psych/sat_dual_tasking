# ---- loading packages ----

library(ggplot2)



# ---- DDM simulation function ----

simulate_ddm <- function(v = 0.3, a = 1.0, z = 0.5, t0 = 0.3,
                         s = 0.1, dt = 0.01, max_time = 2.0) {
  z <- z * a
  time_steps <- ceiling((max_time - t0) / dt)
  x <- z
  trajectory <- numeric(time_steps)
  trajectory[1] <- x

  for (t in 2:time_steps) {
    dx <- v * dt + s * sqrt(dt) * rnorm(1)
    x <- x + dx
    trajectory[t] <- x

    if (x >= a || x <= 0) {
      rt <- (t - 1) * dt + t0
      choice <- ifelse(x >= a, 1, 0)

      # Prepend NA values for t0 duration
      pre_t0_steps <- ceiling(t0 / dt)
      pre_t0_traj <- rep(NA, pre_t0_steps)

      full_traj <- c(pre_t0_traj, trajectory[1:t])
      return(list(rt = rt, choice = choice, traj = full_traj))
    }
  }

  # If no boundary hit, still prepend NA values
  pre_t0_steps <- ceiling(t0 / dt)
  pre_t0_traj <- rep(NA, pre_t0_steps)
  full_traj <- c(pre_t0_traj, trajectory)

  return(list(rt = max_time, choice = NA, traj = full_traj))
}



# ---- function for simulating multiple DDMs at once ----

simulate_multiple_ddm <- function(noise_level, num_sim = 10, v = 0.45, a = 1, z = 0.5, t0 = 0.3, dt = 0.01, max_time = 2) {
  trials <- list()
  rts <- numeric(num_sim)
  choices <- numeric(num_sim)

  for (i in 1:num_sim) {
    sim <- simulate_ddm(v, a, z, t0, s = noise_level, dt, max_time)
    traj <- sim$traj
    time <- seq(0, by = dt, length.out = length(traj))
    df <- data.frame(
      time = time,
      evidence = traj,
      trial = i,
      noise = as.factor(noise_level)
    )
    trials[[i]] <- df
    rts[i] <- sim$rt
    choices[i] <- sim$choice
  }

  trial_data <- do.call(rbind, trials)
  mean_rt <- mean(rts)
  error_rate <- mean(choices == 0, na.rm = TRUE)

  attr(trial_data, "mean_rt") <- mean_rt
  attr(trial_data, "error_rate") <- error_rate
  return(trial_data)
}


# ---- create plot for visualization ----

# set seed for reproducability
set.seed(24)

# simulate 5 trials
ddm_data <- simulate_multiple_ddm(noise_level = 0.6, num_sim = 5)


# function for plot
plot_ddm_trajectories <- function(data, a = 1.0) {

  # Plot
  p <- ggplot(data, aes(x = time, y = evidence, group = trial)) +
    geom_line(alpha = 0.3, linewidth = 0.6) +
    annotate("segment", x = 0, xend = 2, y = 0, yend = 0,
         linetype = "dashed", linewidth = 1) +
    annotate("segment", x = 0, xend = 2, y = a, yend = a,
            linetype = "dashed", linewidth = 1) +
    # geom_hline(yintercept = c(0, a), linetype = "dashed", linewidth = 1) +
    annotate("text", x = 1.56, y = a + 0.06, label = "a (richtige Antwort)", size = 8, hjust = 0) +
    annotate("text", x = 1.56, y = -0.06, label = "0 (falsche Antwort)", size = 8, hjust = 0) +
    # Drift rate arrow
    annotate("segment", x = 0.31, xend = 1.31, y = 0.5, yend = 1,
            arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
    annotate("text", x = 0.73, y = 0.75, label = "d", size = 7, fontface = "bold") +
    # Non-decision time Ter
    annotate("rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 1,
         fill = "grey90", alpha = 0.7) +
    annotate("segment", x = 0.01, xend = 0.29, y = 0.6, yend = 0.6, size = 1.2) +
    annotate("segment", x = 0.01, xend = 0.01, y = 0.575, yend = 0.625, size = 1.2) + # left tick
    annotate("segment", x = 0.29, xend = 0.29, y = 0.575, yend = 0.625, size = 1.2) + # right tick
    annotate("text", x = 0.15, y = 0.65, label = "Ter", size = 7, fontface = "bold") + # label Ter
    # Noise s (double-headed vertical arrow)
    annotate("segment", x = 1.3, xend = 1.3, y = 0.5 - 0.15, yend = 0.5 + 0.15,
            arrow = arrow(ends = "both", length = unit(0.25, "cm")), size = 1.2) +
    annotate("text", x = 1.32, y = 0.5, label = "dc", size = 7, fontface = "bold", hjust = 0) +
    # Starting point z at y-axis
    annotate("segment", x = 0.275, xend = 0.275, y = 0, yend = 0.5, size = 1.2) +
    annotate("segment", x = 0.25, xend = 0.3, y = 0.5, yend = 0.5, size = 1.2) + # upper tick
    annotate("segment", x = 0.25, xend = 0.3, y = 0, yend = 0, size = 1.2) + # lower tick
    annotate("text", x = 0.27, y = 0.25, label = "z * a", size = 7, fontface = "bold", hjust = 1) + 
    # labels
    labs(x = "Zeit (Sekunden)",
         y = "Angehäufte Evidenz") +
    scale_color_viridis_d(option = "viridis", guide = "none") +
    scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 2.1)) +
    scale_y_continuous(limits = c(-0.1, a + 0.1), breaks = seq(0, a, by = 0.2)) +
    theme_minimal(base_size = 14) +
    theme(
      # background
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.grid        = element_blank(),

      # keep standard axis
      axis.line         = element_line(color = "black", size = 1),
      
      # sizing of elements
      axis.title.x      = element_text(size = 20, face = "bold", color = "black"),
      axis.title.y      = element_text(size = 20, face = "bold", color = "black"),
      axis.text         = element_text(size = 20, color = "black"),
      
      axis.line.x = element_line(
        color = "black",
        arrow = arrow(length = unit(0.7, "cm"))
      ),
      axis.line.y = element_line(
        color = "black",
        arrow = arrow(length = unit(0.7, "cm"))
      ),
      panel.border = element_blank(),
    )
  
  return(p)
}

# create plot
visualization_plot <- plot_ddm_trajectories(ddm_data)

# export plot
ggsave(
  filename = "04_drift_diffusion_modelling/01_visualization_of_ddm/visualization_ddm.jpg",
  plot = visualization_plot,
  width = 16,
  height = 10,
  dpi = 300
)

# open plot
system("open 04_drift_diffusion_modelling/01_visualization_of_ddm/visualization_ddm.jpg")