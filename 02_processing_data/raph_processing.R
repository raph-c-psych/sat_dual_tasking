# This file is licenced under the MIT licence

# Copyright 2025 Aleks Pieczykolan
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#    
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


# ========== Verarbeitung der Daten aus EmPra Pieczykolan ==========

# ---- Autoren ----

# Grundlage von Dr. Aleks Pieczykolan
# angepasst von Raphael Christmann (raphael.christmann@rwth-aachen.de)
# Untersützung durch ChatGPT <3

# ---- Zweck ----

# Verarbeitung der Daten des Experiments aus dem Empirischen Praktikum SS2025 
# unter Dr. Aleks Pieczykolan
# Inhalt: Dual-Tasking mit Bewertung von Effort und Confidence
# UVs: Kongruenz, Strategie (seriell/parallel), Instruktion (inhibierend/aktivierend)

# ---- Struktur ----
# Teil 1: Grundlegende Verarbeitung des Datensatzes
# Teil 2: Datensatz generieren mit Effeort- und Confidence-Rating
# Teil 3: Datensätze generieren mit Reaktionszeit & Fehlerraten




# ========== Grundlagen ==========

# ---- Pakete laden ----

library(tidyverse)
library(haven) # für SPSS sav-Datein



# ---- Einlesen des aggregierten Datensatzes  -----

dataExp <- read_csv("../data_sets_empra/data_aleks/all_raw_data.csv")




# ========== Teil 1: Grundlegende Verarbeitung des Datensatzes  ==========

# ---- Säubern des Datensatzes -----

# Löschen von unnötigen Variablen
dataExp <- dataExp |>
    select(!c(Instruktion, Reminder, expName, X, psychopyVersion, date, expStart, thisRow.t, notes, 
        Fingerpositions_image.started, sampleRandomTrials.started, sampleRandomTrials.stopped, 
        trial_response_T1.duration, trial_response_T2.duration, trials_exp.trial_response_T2.duration, 
        trial_response_T1.started, trial_response_T2.started, trial_response_T1.stopped, trial_response_T2.stopped
    ))

# Weitere Spalten löschen
dataExp <- dataExp |>
  select(-(S1_Center:S2_feat))

# Weitere Spalten entfernen
dataExp <- dataExp |>
    select(!c(strategie_block.ratingConfidence.rt, strategie_block.ratingConfidence_confirm.keys,
        strategie_block.ratingConfidence_confirm.duration, strategie_block.ratingConfidence_confirm.rt,
        strategie_block.ratingEffort.rt, strategie_block.ratingEffort_confirm.keys, 
        strategie_block.ratingEffort_confirm.duration, strategie_block.ratingEffort_confirm.rt, pause.started,
        pause.stopped, ITI.started, ITI.stopped, rating_textbox.started, trials_exp.thisIndex, thisRepN
    ))

# Löschen der Trainingsvariablen
dataExp <- dataExp |>
    select(-(training_allgemein.thisRepN:trials_training.thisIndex)) |>
    select(-(training_allgemein_block.ratingEffort_confirm.keys:training_allgemein_block.ratingConfidence_confirm.duration))

# Effort & Confidence von 1. Zeile des nächsten Blocks auf letzte des aktuellen verschieben 
# dataExp$strategie_block.ratingEffort.response, dataExp$strategie_block.ratingConfidence.response
dataExp$effort <- ifelse(dataExp$trials_exp.thisTrialN == 31 ,lead(dataExp$strategie_block.ratingEffort.response), NA)
dataExp$confidence <- ifelse(dataExp$trials_exp.thisTrialN == 31 ,lead(dataExp$strategie_block.ratingConfidence.response), NA)

# löschen weiterer Spalten
dataExp <- dataExp |>
    select(!c("strategie_block.ratingEffort.response", "strategie_block.ratingConfidence.response"))

# Löschen der Trainingstrials und dann Löschen der Variable (Zeilen-Selektion)
dataExp <- subset(dataExp, strategie.thisRepN == 0)
dataExp <- dataExp |>
    select(!("strategie.thisRepN"))

# Löschen weiterer Trainingsvariablen
dataExp <- dataExp |>
    select(-(trials_training.trial_response_T1.keys:trials_training.trial_response_T2.duration))

# Umbenennen von UV (Instruktion) in UV_Instruk #
names(dataExp)[names(dataExp) == "UV"] <- "UV_Instruk"
names(dataExp)[names(dataExp) == "strategie_block.thisN"] <- "Block_Nr"


# Identifizieren der Reihenfolgenvariable
# dataExp$strategie.thisIndex = 0 bedeutet "parallel"
# dataExp$strategie.thisIndex = 1 bedeutet "seriell"
dataExp$Reihenfolge <- ifelse((dataExp$strategie.thisIndex == 0 & dataExp$strategie.thisN == 0) | (dataExp$strategie.thisIndex == 1 & dataExp$strategie.thisN == 1) ,  "parSer", 
                              "SerPar")



# ---- Anlegen einer Kopie von dataExp ----

dataExp_c <- dataExp
# dataExp <- dataExp_c # falls nötig





# ========== Teil 2: Datensatz generieren mit Effeort- und Confidence-Rating  ==========

# relevante Zeilen auswählen
dataRating <- dataExp |>
    filter(thisN == 31)

# relevante Spalten auswählen
dataRating <- dataRating |>
    select(participantID, UV_Instruktion, UV_Instruk, UV_Strategie, Block_Nr, Reihenfolge, effort, confidence)

# effort rating runden
dataRating$effort <- round(as.numeric(dataRating$effort), 0)

#  Trainingsblock (Block 0) entfernen
dataRating <- subset(dataRating, Block_Nr != 0) 

# aus long wide machen
dataRating_wide <- dataRating |>
    unite("Strategie_Block_Nr", UV_Strategie, Block_Nr) |> # # rep meas vars
    pivot_wider(names_from = Strategie_Block_Nr,
                values_from = c(effort, confidence), # Beide Wertespalten angeben
                names_sep = ".") # Separator für die neuen Spaltennamen

# in Spaltennamen Unterstriche durch Punkte ersetzen
colnames(dataRating_wide) <- gsub("_", ".", colnames(dataRating_wide))

# Datensätze schreiben
write_sav(dataRating_wide, "../data_sets_empra/data_raph/dataRating_wide.sav")
write.csv(dataRating_wide, "../data_sets_empra/data_raph/dataRating_wide.csv")




# ========== Teil 3: Datensätze generieren mit Reaktionszeit & Fehlerraten ==========

# ---- Grundlegendes Verarbeiten ----

# Block 0 (Trainingsblock entfernen)
dataRT <- dataExp |>
    filter(Block_Nr != 0)

# Umbenennen und verständlich machen, Fehlervariable soll darstellen, dass Fehler gezählt werden. 1 = Fehler
dataRT <- dataRT |>
  mutate(
    RT1 = trial_response_T1.rt,
    RT2 = trial_response_T2.rt,
    FehlerR1 = 1 - trial_response_T1.corr,
    FehlerR2 = 1 - trial_response_T2.corr,
    .keep = "unused"
  )

# relevante Spalten auswählen
dataRT <- dataRT |>
    select(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp, RT1, RT2, FehlerR1, FehlerR2)

# Datensatz exportieren mit rohen Trial-Daten für DDM
write.csv(dataRT, "../data_sets_empra/data_raph/raw_trial_data.csv")



# ---- Teildatensatz für Reaktionszeiten korrekter Trials generieren ----

# Subdatensatz der Trials mit zwei korrekten Antworten und zusammenfassen
RT_agg_corr <- dataRT |>
    filter(FehlerR1 == 0 & FehlerR2 == 0) |>
    select(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp, RT1, RT2) |>
    group_by(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp) |>
    summarise(
        RT1 = mean(RT1, na.rm = TRUE),
        RT2 = mean(RT2, na.rm = TRUE),
        .groups = "drop"
    )

# ins wide-Format bringen
dataRT_wide <- RT_agg_corr |>
    unite("Strategie_Komp", UV_Strategie, UV_komp) |> # rep meas vars
    pivot_wider(names_from = Strategie_Komp,
                values_from = c(RT1, RT2), # Beide Wertespalten angeben
                names_sep = ".") # Separator für die neuen Spaltennamen

# in Spaltennamen Unterstriche durch Punkte ersetzen
colnames(dataRT_wide) <- gsub("_", ".", colnames(dataRT_wide))



# ---- Teildatensatz für Fehlerraten generieren -----

# Berechnung der Fehleranzahl und Fehlerquote aus ursprünglichen Datensatz
Fehler_agg <- dataRT |>
    group_by(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp) |>
    summarise(
      FehlerR1 = sum(FehlerR1, na.rm = TRUE),
      FehlerR2 = sum(FehlerR2, na.rm = TRUE),
       .groups = "drop"
    ) |>
    mutate(
        FehlerQuote1 = FehlerR1 / 64,
        FehlerQuote2 = FehlerR2 / 64
    )

# ins wide-Format bringen
dataFehler_wide <- Fehler_agg |>
    unite("Strategie_Komp", UV_Strategie, UV_komp) |> # rep meas vars
    pivot_wider(names_from = Strategie_Komp,
                values_from = c(FehlerQuote1, FehlerQuote2, FehlerR1, FehlerR2), # Die Messwerte in neue Spalten umwandeln
                names_sep = ".") # Separator für neue Spaltennamen

# in Spaltennamen Unterstriche durch Punkte ersetzen
colnames(dataFehler_wide) <- gsub("_", ".", colnames(dataFehler_wide))



# ----- Zusammenführen der Datensätze & Ausgabe -----

# Zusammenfügen von Reaktionszeiten und Fehlerquoten
dataRTFehler_wide <- merge(dataRT_wide, dataFehler_wide, by = "participantID", all = TRUE)

# Datensätze ins working directory schreiben als sav-Datei und csv-Datei
write_sav(dataRTFehler_wide, "../data_sets_empra/data_raph/dataRTFehler_wide.sav")
write.csv(dataRTFehler_wide, "../data_sets_empra/data_raph/dataRTFehler_wide.csv")



# ----- Datensatz generieren mit RTs fehlerhafter Trials eingschlossen -----

data_RT_ER_all <- dataRT |>
    group_by(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp) |>
    summarise(
        FehlerR1 = sum(FehlerR1, na.rm = TRUE),
        FehlerR2 = sum(FehlerR2, na.rm = TRUE),
        RT1 = mean(RT1, na.rm = TRUE),
        RT2 = mean(RT2, na.rm = TRUE),
       .groups = "drop"
    ) |>
    mutate(
        FehlerQuote1 = FehlerR1 / 64,
        FehlerQuote2 = FehlerR2 / 64
    )

# ins wide-Format bringen
data_RT_ER_all_wide <- data_RT_ER_all |>
    unite("Strategie_Komp", UV_Strategie, UV_komp) |> # rep meas vars
    pivot_wider(names_from = Strategie_Komp,
                values_from = c(RT1, RT2, FehlerQuote1, FehlerQuote2, FehlerR1, FehlerR2), # Die Messwerte in neue Spalten umwandeln
                names_sep = ".") # Separator für neue Spaltennamen

# in Spaltennamen Unterstriche durch Punkte ersetzen
colnames(data_RT_ER_all_wide) <- gsub("_", ".", colnames(data_RT_ER_all_wide))

# Datensätze ins working directory schreiben als sav-Datei und csv-Datei
write_sav(data_RT_ER_all_wide, "../data_sets_empra/data_raph/data_RT_ER_all_wide.sav")
write.csv(data_RT_ER_all_wide, "../data_sets_empra/data_raph/data_RT_ER_all_wide.csv")
