# ========== Speed-Accuracy Trade-Off in Dual-Tasking  ==========

# ---- author ----

# Raphael Christmann (raphael.christmann@rwth-aachen.de)
# plus ChatGPT <3

# ---- purpose ----

# -- general
# data analysis of experiemnt of the research internship seminar SS2025 under Dr. Aleks Pieczykolan
# topic: dual-tasking with rating of effort and confidence
# focus of this analysis on speed-accuracy trade-off (SAT) and balanced integration score (BIS)

# -- independent variables
# 1) congruency (congruent / incongruent) [not in focus of this analysis]
# 2) strategy (serial / parallel)
# 3) instruction (inhibiting / activating)

# -- dependent variables
# 1) Reaction Time (RT)
# 2) Error Rates (ER)
# 3) Balanced Integration Score (BIS)
# 4) Speed-Accuracy Trade-Off / Accuracy-Bias (SAT)



# ---- structure ----
# part 1: processing data set
# part 2: checking and removing outliers
# part 3: calculating new measures (SAT & BIS)
# part 4: exporting final data set in wide
# part 5: visualizing resulsts
# part 6: testing with inference statistics
# part 7: post hoc power analysis & partial eta square



# ========== Basics ==========

# ----- load packages -----

# for general processing
library(tidyverse)
# for exporting sav
library(haven)
# standard import
library(psych)
# for anova
library(afex)
# for arranging plots
library(patchwork)
# for calculating post hoc power
library(effectsize)
library(pwr)
# for outputting tables in console
library(knitr)
# for exporting tables
library(gt)
library(webshot2)


# ---- loading data frames ----

# error rates and reaction times (trials with errors included)
# data_set_rt_er <- read_csv("../data_sets_empra/data_raph/data_RT_ER_all_wide.csv")

# data frame with rt only from correct trials
data_set_rt_er <- read_csv("../data_sets_empra/data_raph/dataRTFehler_wide.csv")





# ========== part 1: processing data set ==========

# selecting columns and renaming variables to personal taste
data_set_rt_er <- data_set_rt_er |>
  select(-matches("FehlerR1|FehlerR2")) |> # exclude absolute error count
  select(-any_of(c("...1", "UV.Instruk.y", "Reihenfolge.y"))) |> # remove access variables
  rename( # renaming variables
    Instruktion = any_of(c("UV.Instruk.x", "UV.Instruk")),
    Reihenfolge = any_of(c("Reihenfolge.x", "Reihenfolge")),
    ID          = any_of(c("participantID"))
  ) |>
  mutate(Instruktion = case_when( # renaming values of instruction
    Instruktion == "ACT" ~ "aktivierend",
    Instruktion == "INH" ~ "inhibierend",
    TRUE ~ Instruktion
  )) |>
  mutate(Reihenfolge = case_when( # renaming values of sequence
    Reihenfolge == "SerPar" ~ "seriell>parallel",
    Reihenfolge == "parSer" ~ "parallel>seriell",
    TRUE ~ Reihenfolge
  )) |>
  mutate(ID = str_sub(ID, 2)) |> # removing letter in ID Variable
  arrange(ID) |> # order by ID
  mutate( # bring RT to ms and ER to %
    across(contains("RT"), ~ . * 1000),
    across(contains("Fehler"), ~ . * 100)
  )

# converting to long format
data_set_rt_er_long <- data_set_rt_er %>%
  pivot_longer(
    cols = 4:ncol(.),
    names_to = "Variable",
    values_to = "Wert"
  ) %>%
  separate(
    Variable,
    into = c("Typ", "Strategie", "Kongruenz"),
    sep = "\\.",
    fill = "right"
  ) %>%
  mutate(Typ = case_when(
    Typ == "FehlerQuote1" ~ "ER1",
    Typ == "FehlerQuote2" ~ "ER2",
    TRUE ~ Typ
  )) |>
  pivot_wider(
    names_from = Typ,
    values_from = Wert
  ) |>
  mutate(Kongruenz = case_when(
    Kongruenz == "komp" ~ "kongruent",
    Kongruenz == "inkomp" ~ "inkongruent",
    TRUE ~ Kongruenz
  )) |>
  mutate(Instru_Strat = paste(Instruktion, Strategie, sep = "-")) # new condition var for convinience



# ---- collapsing congruency -----

# copy pre collapsing 
data_set_rt_er_long_pre_coll <- data_set_rt_er_long
# data_set_rt_er_long <- data_set_rt_er_long_pre_coll # if needed

# collapsing congruency
data_set_rt_er_long <- data_set_rt_er_long |>
  group_by(ID, Instruktion, Reihenfolge, Strategie, Instru_Strat) |>
  summarise(
    RT1 = mean(RT1, na.rm = TRUE),
    RT2 = mean(RT2, na.rm = TRUE),
    ER1 = mean(ER1, na.rm = TRUE),
    ER2 = mean(ER2, na.rm = TRUE),
    .groups = "drop"
  )





# ========== part 2: checking and removing outliers ==========

# ---- structure -----

# -- 2 statistical criteria:
# 1. checking outliers by values beyond 2 SD from mean (analysis 1 & 2)
# 2. checking outliers by values beyong 1,5 interquartile range away from first/third quartile (3 & 4)

# -- 2 types differentiation of conditions
# 1. checking inside each group: deviation from in EACH condition  (1 & 3)
# 2. checking over all groups: deviation OVER ALL conditions (2 & 4)

# -- 2 types of differentiation of congruency
# 1. differed by congruency (first result row)
# 2. congruency collapsed (second result row)
# (first one achieved by skipping collapsind in previous part)



# ---- optional pre-selection of participants ----

# removing 09 from the start
# used for data set with rts only from correct trials; 09 has missing values
# data_set_rt_er_long <- data_set_rt_er_long |>
#   filter(!ID %in% c("09"))



# ---- 1) checking outliers (inside each group): 2 SD for each measure ----

# Check for any value > 2 SD
outliers_table_sd_inside <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Instru_Strat, Variable) |>
  mutate(z_score = as.numeric(scale(Original_Wert))) |>
  ungroup() |>
  filter(abs(z_score) > 2)

# output
print(outliers_table_sd_inside, n = Inf, width = Inf)

# congruency differed:  9, 12, 14
# congruency collapsed: 9, 12, 13



# ---- 2) checking outliers (over all groups): 2 SD for each measure ----

# Check for any value > 2 SD
outliers_table_sd_over <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Variable) |>
  mutate(z_score = as.numeric(scale(Original_Wert))) |>
  ungroup() |>
  filter(abs(z_score) > 2)

# output
print(outliers_table_sd_over, n = Inf, width = Inf)

# congruency differed:  9, 12, 14
# congruency collapsed: 9, 12, 14



# ---- 3) checking outliers (inside groups): 1,5 interquartile range from upper and lower quartile for each measure ----

# Check for outliers based on IQR
outliers_table_iqr_inside <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Instru_Strat, Variable) |>
  mutate(
    Q1 = quantile(Original_Wert, 0.25, na.rm = TRUE),
    Q3 = quantile(Original_Wert, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    outlier_flag = case_when(
      Original_Wert < Lower_Bound ~ "Lower",
      Original_Wert > Upper_Bound ~ "Upper",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(outlier_flag)) %>%
  select(ID, Variable, Original_Wert, outlier_flag)

# output
print(outliers_table_iqr_inside, n = Inf, width = Inf)

# congruency differed:  9, 12, 16
# congruency collapsed: 9, 12, 13

# new analysis: 9, 12, 16 (congruency differed)

# ---- 4) checking outliers (over all groups): 1,5 interquartile range from upper and lower quartile for each measure ----

# Check for outliers based on IQR
outliers_table_iqr_over <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Variable) |>
  mutate(
    Q1 = quantile(Original_Wert, 0.25, na.rm = TRUE),
    Q3 = quantile(Original_Wert, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    outlier_flag = case_when(
      Original_Wert < Lower_Bound ~ "Lower",
      Original_Wert > Upper_Bound ~ "Upper",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(outlier_flag)) %>%
  select(ID, Variable, Original_Wert, outlier_flag)

# output
print(outliers_table_iqr_over, n = Inf, width = Inf)

# congruency differed:  9, 12, 13, 16
# congruency collapsed: 9,12, 14, 16 

# new analysis: 09, 12, 13, 16 congruency differed

# ---- decision of outliers ----

# decision based upon third analysis (IQR, inside groups) congruency differed
# reason: no systematic discrimination of stronger speed-accuracy trade-offs
# for further elaboration see research paper
# -> exclusion of 9, 12, 16



# ---- excluding outlier ----

# saving copy before removing outliers
data_set_rt_er_long_pre_exl <- data_set_rt_er_long
# data_set_rt_er_long <- data_set_rt_er_long_pre_exl # if necessary

# removing outliers
# data_set_rt_er_long <- data_set_rt_er_long |> # from data set with false response rts
#   filter(!ID %in% c("09", "12", "16"))
data_set_rt_er_long <- data_set_rt_er_long |>
  filter(!ID %in% c("09", "12", "16")) # 13?




# ========== part 3: calculating new measures (SAT & BIS) ==========

# calculating z-standardised RT & ER plus SAT & BIS
data_set_rt_er_long <- data_set_rt_er_long |>
  mutate(
    z_RT_1 = as.numeric(scale(RT1)),
    z_RT_2 = as.numeric(scale(RT2)),
    z_ER_1 = as.numeric(scale(ER1)),
    z_ER_2 = as.numeric(scale(ER2))
  ) |>
  mutate(
    BIS_1 = - z_RT_1 - z_ER_1, # Balanced Integration Score Task 1
    BIS_2 = - z_RT_2 - z_ER_2,
    Genauigkeitstendenz_1 = + z_RT_1 - z_ER_1, # SAT 1: accuracy bias Task 1
    Genauigkeitstendenz_2 = + z_RT_2 - z_ER_2
  )

# checking descriptive values (divieded by strategy & instruction)
data_set_rt_er_long |>
  group_by(Instruktion, Strategie) |>
  summarise(
    Genauigkeitstendenz_1 = round(mean(Genauigkeitstendenz_1), 2),
    Genauigkeitstendenz_2 = round(mean(Genauigkeitstendenz_2), 2)
  ) |>
  kable()

# checking descriptive values (divieded by strategy)
data_set_rt_er_long |>
  group_by(Strategie) |>
  summarise(
    Genauigkeitstendenz_1 = round(mean(Genauigkeitstendenz_1), 2),
    Genauigkeitstendenz_2 = round(mean(Genauigkeitstendenz_2), 2)
  ) |>
  kable()

# checking descriptive values (divieded by instruction)
data_set_rt_er_long |>
  group_by(Instruktion) |>
  summarise(
    Genauigkeitstendenz_1 = round(mean(Genauigkeitstendenz_1), 2),
    Genauigkeitstendenz_2 = round(mean(Genauigkeitstendenz_2), 2)
  ) |>
  kable()




# ========== part 4: exporting final data set in wide ==========

# exporting data set with BIS & SAT in wide format
data_set_rt_er_long |>
  pivot_wider(
    id_cols = c(ID, Instruktion, Reihenfolge),
    names_from = Strategie,
    values_from = c(BIS_1, BIS_2, Genauigkeitstendenz_1, Genauigkeitstendenz_2)
  ) |>
  write.csv("../data_sets_empra/data_raph/data_SAT.csv")




# ========== part 5: visualizing resulsts ==========

# ---- visualizing SAT and BIS over z-standardised variables ----

# Balanced Integration Score (BIS) over z-standardised variables
ggplot(data_set_rt_er_long) +
  geom_point(aes(x = z_RT_1, y = z_ER_1, color = BIS_1, shape = Instru_Strat)) +
  labs(
    title = "BIS in Relation zu RT & ER",
    x = "Reaktionszeit (z-standardisiert)",
    y = "Fehlerrate (z-standardisiert)",
    shape = "Bedingung",
    color = "BIS"
  )

# Spped-Accuracy Trade-Off (SAT) as accuracy-bias over z-standardised variables
ggplot(data_set_rt_er_long) +
  geom_point(aes(x = z_RT_1, y = z_ER_1, color = Genauigkeitstendenz_1, shape = Instru_Strat)) +
  labs(
    title = "Genauigkeitstendenz in Relation zu RT & ER",
    x = "Reaktionszeit (z-standardisiert)",
    y = "Fehlerrate (z-standardisiert)",
    shape = "Bedingung",
    color = "Genauigkeitstendenz"
  )



# ---- visualizing RT, ER, BIS & SAT across groups of strategy & instruction -----

# selecting columns / variables to summarize
vars_to_summarize <- c(
  "RT1", "RT2", "ER1", "ER2", "BIS_1", "BIS_2", "Genauigkeitstendenz_1", "Genauigkeitstendenz_2"
)

# summarizing data
summary_data <- data_set_rt_er_long |>
  select(Strategie, Instruktion, all_of(vars_to_summarize)) |>
  pivot_longer(cols = all_of(vars_to_summarize), 
               names_to = "Variable", 
               values_to = "Wert") |>
  group_by(Strategie, Instruktion, Variable) |>
  summarise(
    mean_wert = mean(Wert, na.rm = TRUE),
    sd_wert = sd(Wert, na.rm = TRUE),
    n = n(),
    se_wert = sd_wert / sqrt(n),
    ci_lower = mean_wert - qt(0.975, df = n-1) * se_wert, # 95 %-iges confidende intervall
    ci_upper = mean_wert + qt(0.975, df = n-1) * se_wert, # 95 %-iges Konfidenzintervall
    .groups = "drop"
  )


# function for creating individual plots
make_metric_plot <- function(data, var, title, ylab, ylim_vec, legend_pos) {
  # basic plot
  p <- data |> 
    filter(Variable == var) |>
    ggplot(aes(x = Strategie,
                         y = mean_wert,
                         shape = Instruktion,
                         group = Instruktion)) +
    geom_point(position = position_dodge(width = 0.1), size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.1),
                width = 0.1) +
    geom_line(position = position_dodge(width = 0.1), size = 0.8)
  
  # formatting y-scale
  if (ylim_vec[1] == 0) {
    p <- p + scale_y_continuous(
      limits = ylim_vec,
      expand = expansion(mult = c(0, 0), add = c(0, 1))
    )
  } else {
    p <- p + scale_y_continuous(
      limits = ylim_vec
    )
  }
  
  # rest of plot
  p +
    labs(
      title = title,
      x     = "Strategie",
      y     = ylab,
      shape = "Instruktion"
    ) +
    theme_minimal() +
    theme(
      # background
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.grid        = element_blank(),
      
      # keep standard axis
      axis.line         = element_line(color = "black", size = 1),
      
      # sizing of elements
      plot.title        = element_text(size = 17, face = "bold", color = "black", hjust = 0.5),
      axis.title.x      = element_text(size = 17, face = "bold", color = "black"),
      axis.title.y      = element_text(size = 17, face = "bold", color = "black"),
      axis.text         = element_text(size = 17, color = "black"),
      axis.ticks        = element_line(size = 1, color = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      legend.position   = legend_pos,
      legend.title      = element_text(size = 17, face = "bold", color = "black"),
      legend.text       = element_text(size = 17, color = "black"),
      legend.key.size   = unit(1.2, "lines"), # spacing, not shape size!

      # add arrow heads to the axis lines
      axis.line.y = element_line(
        color = "black",
        arrow = arrow(length = unit(0.7, "cm"))
      ),
      panel.border = element_blank()
    )
}


# specifying the plots
plot_specs <- tribble(
  ~var, ~title, ~ylab, ~ylim_vec, ~legend_pos, 
  "RT1", "A) Reaktionszeit (Auf. 1)", "RZ (ms)", c(700, 1650), "none",
  "RT2", "B) Reaktionszeit (Auf. 2)", "RZ (ms)", c(700, 1650), "right",
  "ER1", "C) Fehlerrate (Auf. 1)", "FR (%)", c(0, 13), "none",
  "ER2", "D) Fehlerrate (Auf. 2)", "FR (%)", c(0, 13), "right",
  "BIS_1", "E) Balanced Integration Score (Auf. 1)", "BIS", c(-3.4, 3.4), "none",
  "BIS_2", "F) Balanced Integration Score (Auf. 2)", "BIS", c(-3.4, 3.4), "right",
  "Genauigkeitstendenz_1", "G) Genauigkeitstendenz (Auf. 1)", "rSATS", c(-2.6, 2.6), "none",
  "Genauigkeitstendenz_2", "H) Genauigkeitstendenz (Auf. 2)", "rSATS", c(-2.6, 2.6), "right"
)

# creating all the plots
all_plots <- purrr::pmap(
  plot_specs,
  function(var, title, ylab, ylim_vec, legend_pos) {
    make_metric_plot(summary_data, var, title, ylab, ylim_vec, legend_pos)
  }
)

# arranging all the plots in overview plot
overview_plot <- 
  (all_plots[[1]] | all_plots[[2]]) /
  (all_plots[[3]] | all_plots[[4]]) /
  (all_plots[[5]] | all_plots[[6]]) /
  (all_plots[[7]] | all_plots[[8]])

# saving overview plot
plot_path_overview <- "03_data_analysis/overview_plot.jpg"
ggsave(
  filename = plot_path_overview,
  plot = overview_plot,
  width = 14,    # in inches 
  height = 20,    
  dpi = 300      # resolution
)

# Open the plot (macOS command)
system2("open", args = shQuote(plot_path_overview))


# ---- visualizing SAT in task 1 & 2 -----

# specifying the plots
plot_specs_sat <- tribble(
  ~var, ~title, ~ylab, ~ylim_vec, ~legend_pos, 
  "Genauigkeitstendenz_1", "A) Genauigkeitstendenz (Auf. 1)", "rSATS", c(-2.6, 2.6), "none",
  "Genauigkeitstendenz_2", "B) Genauigkeitstendenz (Auf. 2)", "rSATS", c(-2.6, 2.6), "right"
)

# creating all the plots
plots_sat <- purrr::pmap(
  plot_specs_sat,
  function(var, title, ylab, ylim_vec, legend_pos) {
    make_metric_plot(summary_data, var, title, ylab, ylim_vec, legend_pos)
  }
)

# arranging all the plots in overview plot
overview_sat_plot <- 
  (plots_sat[[1]] | plots_sat[[2]])

# saving overview plot
sat_plot_path_overview <- "03_data_analysis/results/sat_results_plot.jpg"
ggsave(
  filename = sat_plot_path_overview,
  plot = overview_sat_plot,
  width = 12,    # in inches 
  height = 5,    
  dpi = 300      # resolution
)

# Open the plot (macOS command)
system2("open", args = shQuote(sat_plot_path_overview))





# ========= part 6: testing with inference statistics =========

# ---- ANOVAs for SAT ----

# ANOVA Accuracy-Bias for Task 1
aov_accuracy_1 <- aov_ez(
  id = "ID",
  dv = "Genauigkeitstendenz_1",
  between = "Instruktion",
  within = "Strategie",
  data = data_set_rt_er_long
)

# ANOVA Accuracy-Bias for Task 2
aov_accuracy_2 <- aov_ez(
  id = "ID",
  dv = "Genauigkeitstendenz_2",
  between = "Instruktion",
  within = "Strategie",
  data = data_set_rt_er_long
)

# output results of both ANOVAs of SAT
aov_accuracy_1
aov_accuracy_2

# saving results as tables
aov_accuracy_1$anova_table |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Effekt") |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  gt() |>
  gtsave("03_data_analysis/results/Genauigkeitstendenz_1.png")
aov_accuracy_2$anova_table |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "Effekt") |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  gt() |>
  gtsave("03_data_analysis/results/Genauigkeitstendenz_2.png")





# ========== part 7: post hoc power analysis & partial eta square ==========

# function to output effectsize, power and beta error for alpha-level
power_from_aov_ez <- function(aov_result, title_of_table, sig_level = 0.05) {
  # convert anova resulsts
  aov_table <- as.data.frame(aov_result$anova_table)

  # get relevant values
  F_value    <- aov_table[, "F"]
  df_effect  <- aov_table[, "num Df"]
  df_error   <- aov_table[, "den Df"]

  # calculate partial eta2 and f2
  eta2_p <- (F_value * df_effect) / (F_value * df_effect + df_error)
  f2     <- eta2_p / (1 - eta2_p)

  # calculate power
  pwr_result <- pwr::pwr.f2.test(u = df_effect,
                                  v = df_error,
                                  f2 = f2,
                                  sig.level = sig_level)

  # create table for output
  result_table <- data.frame(
    Effect     = rownames(aov_table),
    Power      = round(pwr_result$power * 100, 1),
    Beta       = round((1 - pwr_result$power) * 100, 1),
    Part_eta2  = round(eta2_p, 3)
  )
  
  # output results in knitr table
  knitr::kable(result_table,
               col.names = c("Effekt", "Power (%)", "Beta (%)", "Part. eta²"),
               align = "lrrr",
               caption = paste0("Powerberechnung von ", title_of_table, " bei Alpha-Niveau ", sig_level * 100, " %"))
}

# checking values for SAT & BIS
power_from_aov_ez(aov_accuracy_1, "Genauigkeitstendenz (Aufg. 1)", sig_level = 0.10)
power_from_aov_ez(aov_accuracy_2, "Genauigkeitstendenz (Aufg. 2)", sig_level = 0.10)

