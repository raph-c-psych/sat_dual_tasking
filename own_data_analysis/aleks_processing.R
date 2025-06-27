# quick and dirty-Version #

# setwd("/ ") # Verzeichnis, wo alle csvs drinliegen


library(tidyr)
library(dplyr)
#### Einlesen aller Dateien und erzeugen einer Datei ####

temp <- list.files(pattern ="*.csv")
myfiles <- lapply(temp, read.csv, header = TRUE, sep = "\t")

for (index in rev(seq_along(myfiles))) {
    myfiles[[index]]$Danke.started <- NULL
    myfiles[[index]]$Danke.stopped <- NULL
    myfiles[[index]]$trial_response.stopped <- NULL
   myfiles[[index]]$trials_exp.trial_response_T1.duration <- NULL
   myfiles[[index]]$trials_training.trial_response_T1.duration <- NULL
   myfiles[[index]]$training_allgemein_block.ratingConfidence.response <- NULL
   myfiles[[index]]$training_allgemein_block.ratingConfidence.rt <- NULL
   myfiles[[index]]$training_allgemein_block.ratingEffort.response <- NULL
   myfiles[[index]]$training_allgemein_block.ratingEffort.rt <- NULL
    myfiles[[index]]$frameRate <- NULL
    myfiles[[index]]$strategie_block.ratingEffort.response <- as.character(myfiles[[index]]$strategie_block.ratingEffort.response)
    myfiles[[index]]$strategie_block.ratingEffort.rt <- as.character(myfiles[[index]]$strategie_block.ratingEffort.rt)
    
}

myfiles[[1]] <- NULL

data <- bind_rows(myfiles)


# library("dplyr")
# dataAll <- bind_rows(myfiles)
# dataAll <- do.call(rbind, myfiles)


# Anlegen von hard copies des gesamtdatendatzes
library("haven")
write_sav(data, "../dataAll.sav")
write.csv(data, "../dataAll.csv")

# Arbeitskopie anlegen
dataExp <- data
# -> mein Datensatz
dataExp <- read_csv("own_data_analysis/data_sets/all_raw_data.csv")

### Aufraeumen: Variablen löschen ###
# per Hand, um einen Überblick über das Experiment zu behalten und ggf. was in ZUkunft bei psychopy verbessern zu können ;-)


# Löschen von unnötigen Variablen
# 12
dataExp$Instruktion <- NULL
dataExp$Reminder <- NULL
dataExp$expName <- NULL
dataExp$X <- NULL
dataExp$psychopyVersion <- NULL
dataExp$date <- NULL
dataExp$expStart <- NULL
dataExp$thisRow.t <- NULL
dataExp$notes <- NULL
dataExp$Fingerpositions_image.started <- NULL
dataExp$sampleRandomTrials.started <- NULL
dataExp$sampleRandomTrials.stopped <- NULL
# 3
dataExp$trial_response_T1.duration <- NULL
dataExp$trial_response_T2.duration <- NULL
dataExp$trials_exp.trial_response_T2.duration <- NULL
# 4
dataExp$trial_response_T1.started <- NULL
dataExp$trial_response_T2.started <- NULL
dataExp$trial_response_T1.stopped <- NULL
dataExp$trial_response_T2.stopped <- NULL
# -> 19

whichStart <- which(colnames(dataExp)=="S1_Center")
whichEnd <-which(colnames(dataExp)=="S2_feat")
dataExp <- dataExp[,-whichEnd:-whichStart]

dataExp$strategie_block.ratingConfidence.rt <- NULL
dataExp$strategie_block.ratingConfidence_confirm.keys <- NULL
dataExp$strategie_block.ratingConfidence_confirm.duration <- NULL
dataExp$strategie_block.ratingConfidence_confirm.rt <- NULL

dataExp$strategie_block.ratingEffort.rt <- NULL
dataExp$strategie_block.ratingEffort_confirm.keys <- NULL
dataExp$strategie_block.ratingEffort_confirm.duration <- NULL
dataExp$strategie_block.ratingEffort_confirm.rt <- NULL

dataExp$pause.started <- NULL
dataExp$pause.stopped <- NULL

dataExp$ITI.started <- NULL
dataExp$ITI.stopped<- NULL

dataExp$rating_textbox.started <- NULL

dataExp$trials_exp.thisIndex <- NULL 
dataExp$thisRepN <- NULL # immer 0 also uninformativ

# Löschen der Trainingsvariablen
# Finden der Spalten-Indizes der Trainings-Variablen
whichStart <- which(colnames(dataExp)=="training_allgemein.thisRepN")
whichEnd <-which(colnames(dataExp)=="trials_training.thisIndex")
dataExp <- dataExp[,-whichEnd:-whichStart]

whichStart <- which(colnames(dataExp)=="training_allgemein_block.ratingEffort_confirm.keys")
whichEnd <-which(colnames(dataExp)=="training_allgemein_block.ratingConfidence_confirm.duration")
dataExp <- dataExp[,-whichEnd:-whichStart]


# benoetigt werden:
# dataExp$strategie_block.ratingConfidence.response
# dataExp$strategie_block.ratingEffort.response
# Setzen der beiden Vars in die richtige Zeile, d.h. verschieben um 1 Zeile nach oben, damit es auf dem letzten Trial des Blocks liegt (aktuell auf dem ersten trial des neuen Blocks bzw. auf keinem nach dem letzten Block )  

dataExp$effort <- ifelse(dataExp$trials_exp.thisTrialN == 31 ,lead(dataExp$strategie_block.ratingEffort.response), NA)
dataExp$confidence <- ifelse(dataExp$trials_exp.thisTrialN == 31 ,lead(dataExp$strategie_block.ratingConfidence.response), NA)

dataExp$strategie_block.ratingEffort.response <- NULL
dataExp$strategie_block.ratingConfidence.response <- NULL


# Löschen der Trainingstrials und dann Löschen der Variable (Zeilen-Selektion)
dataExp <- subset(dataExp, strategie.thisRepN == 0)

dataExp$strategie.thisRepN <- NULL
whichStart <- which(colnames(dataExp)=="trials_training.trial_response_T1.keys")
whichEnd <-which(colnames(dataExp)=="trials_training.trial_response_T2.duration")
# Löschen der Trainingsvariablen
dataExp <- dataExp[,-whichEnd:-whichStart]


# Umbenennen von UV (Instruktion) in UV_Instruk #
names(dataExp)[names(dataExp) == "UV"] <- "UV_Instruk"
names(dataExp)[names(dataExp) == "strategie_block.thisN"] <- "BlockNr"


# Identifizieren der Reihenfolgenvariable
# dataExp$strategie.thisIndex = 0 bedeutet "parallel"
# dataExp$strategie.thisIndex = 1 bedeutet "seriell"

dataExp$Reihenfolge <- ifelse((dataExp$strategie.thisIndex == 0 & dataExp$strategie.thisN == 0) | (dataExp$strategie.thisIndex == 1 & dataExp$strategie.thisN == 1) ,  "parSer", 
                              "SerPar")

# -> Ergebnis 6080 rows x 36 columns -> gleich in meinem Skript

#### Anlegen einer Kopie von dataExp ####
dataExp_c <- dataExp
# dataExp <- dataExp_c # falls nötig


#### effort_Rating-Tabelle erzeugen ####

dataRating <- subset(dataExp, thisN == 31)
dataRating <- subset(dataRating, select = c(participantID, UV_Instruktion, UV_Instruk, UV_Strategie, BlockNr, Reihenfolge, effort, confidence))

dataRating$effort <- round(as.numeric(dataRating$effort), 0)

dataRating <- subset(dataRating, BlockNr != 0) # Block 0 ist der "Trainingsblock", den brauhcne wir also nicht

# aus long wide machen
dataRating_wide <- dataRating %>%
    unite("Strategie_BlockNr", UV_Strategie, BlockNr) %>% # # rep meas vars
    pivot_wider(names_from = Strategie_BlockNr,
                values_from = c(effort, confidence), # Beide Wertespalten angeben
                names_sep = ".") # Separator für die neuen Spaltennamen
colnames(dataRating_wide) <- gsub("_", ".", colnames(dataRating_wide))

# -> dataRating_wide 19 rows x 20 columns -> gleich bei mir

write_sav(dataRating_wide, "../dataRating_wide.sav")
write.csv(dataRating_wide, "../dataRating_wide.csv")

#### RT_Daten ####
# dataExp <- dataExp_c # falls nötig

dataRT <- subset(dataExp, BlockNr != 0)

# Umbenennen und verständlich machen, Fehlervariable soll darstellen, dass Fehler gezählt werden. 1 = Fehler
names(dataRT)[names(dataRT) == "trial_response_T1.rt"] <- "RT1"
names(dataRT)[names(dataRT) == "trial_response_T2.rt"] <- "RT2"
names(dataRT)[names(dataRT) == "trial_response_T1.corr"] <- "FehlerR1"
dataRT$FehlerR1  <- 1 - dataRT$FehlerR1
names(dataRT)[names(dataExp) == "trial_response_T2.corr"] <- "FehlerR2"
dataRT$FehlerR2  <- 1 - dataRT$FehlerR2

dataRT <- subset(dataRT, select = c(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp, RT1, RT2, FehlerR1, FehlerR2))

write_sav(dataRT, "../dataRT.sav")
# write.csv(dataRT, "../dataRating_wide.csv")



#### Fehler aus RTs entfernen ####

dataRT_corr <- subset(dataRT, 
                      FehlerR1 == 0 & FehlerR2 == 0,
                      select = c(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp, RT1, RT2))

RT_agg_corr <- aggregate(cbind(RT1, RT2) ~ participantID + UV_Instruk + Reihenfolge + UV_Strategie + UV_komp,
                             data = dataRT_corr,
                             mean, na.rm = TRUE)

Fehler_agg <- aggregate(cbind(FehlerR1, FehlerR2) ~ participantID + UV_Instruk + Reihenfolge + UV_Strategie + UV_komp,
                         data = dataRT,
                         FUN = sum)
# Fehlerquoten berechnen
Fehler_agg$FehlerQuote1 <- Fehler_agg$FehlerR1/ 64  
Fehler_agg$FehlerQuote2 <- Fehler_agg$FehlerR2/ 64

# RTs
dataRT_wide <- RT_agg_corr %>%
    unite("Strategie_Komp", UV_Strategie, UV_komp) %>% # rep meas vars
    pivot_wider(names_from = Strategie_Komp,
                values_from = c(RT1, RT2), # Beide Wertespalten angeben
                names_sep = ".") # Separator für die neuen Spaltennamen
colnames(dataRT_wide) <- gsub("_", ".", colnames(dataRT_wide))

# Fehler #

dataFehler_wide <- Fehler_agg %>%
    unite("Strategie_Komp", UV_Strategie, UV_komp) %>% # rep meas vars
    pivot_wider(names_from = Strategie_Komp,
                values_from = c(FehlerQuote1, FehlerQuote2, FehlerR1, FehlerR2), # Die Messwerte in neue Spalten umwandeln
                names_sep = ".") # Separator für neue Spaltennamen
colnames(dataFehler_wide) <- gsub("_", ".", colnames(dataFehler_wide))

# Zusammenführen
dataRTFehler_wide <- merge(dataRT_wide, dataFehler_wide, by = "participantID", all = TRUE)

write_sav(dataRTFehler_wide, "../dataRTFehler_wide.sav")
write.csv(dataRTFehler_wide, "../dataRTFehler_wide.csv")

# -> dataRTFehler_wide 19 rows x 29 columns -> bei miru auch 19 x 29


# # mit dyplr
# final_result <- dataRT %>%
#     group_by(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp) %>%
#     summarise(
#         FehlerR1_Summe = sum(FehlerR1, na.rm = TRUE),
#         FehlerR2_Summe = sum(FehlerR2, na.rm = TRUE),
#         N = n()  # Zählt die Anzahl der Zeilen
#     )
# 
# final_result_RT <- dataRT %>%
#     group_by(participantID, UV_Instruk, Reihenfolge, UV_Strategie, UV_komp) %>%
#     summarise(
#         RT1 = mean(RT1, na.rm = TRUE),
#         RT2 = mean(RT2, na.rm = TRUE),
#         N = n()  # Zählt die Anzahl der Zeilen
#     )