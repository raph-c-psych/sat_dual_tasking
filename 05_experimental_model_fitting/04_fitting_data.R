# ---- loading packages ----

library(rtdists)
library(brms)
library(tidyverse)



# ---- loading and processing data set ----

# importing data set
raw_data <- read_csv("../data_sets_empra/data_raph/raw_trial_data.csv")

# recoding error to lower boundary and correct to upper
raw_data <- raw_data |>
  mutate(
    FehlerR1 = ifelse(raw_data$FehlerR1 == 0, "upper", "lower"),
    FehlerR2 = ifelse(raw_data$FehlerR2 == 0, "upper", "lower")
  )

# removing unnecessary column
raw_data <- raw_data |>
  select(-c(...1, Reihenfolge, UV_komp))

# cleaning participant names
raw_data <- raw_data |>
  mutate(
    participantID = as.integer(substr(participantID, 2, 3))
  )

# renaming to taste
raw_data <- raw_data |>
  rename(
    participant = participantID,
    instruction = UV_Instruk,
    strategy = UV_Strategie,
    rt_1 = RT1,
    rt_2 = RT2,
    error_1 = FehlerR1,
    error_2 = FehlerR2
  ) |>
  mutate(
    instruction = recode(
      instruction,
      ACT = "activating",
      INH = "inhibiting"
    ),
    strategy = recode(
      strategy,
      seriell = "serial",
      parallel = "parallel"
    )
  )


