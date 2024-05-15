# -------------------------------------------------------------------------
# FOR THE CLASSROOM                                                       -
# EXPLORATORY DATA ANALYSIS OF FUEL ECONOMY DATA FROM THE EPA             -
# Matthew C. Vanderbilt, MSBA                                             -
# -------------------------------------------------------------------------

# INSTALL & LOAD NECESSARY PACKAGES ---------------------------------------

if (!require("ggplot2", character.only = TRUE)) {install.packages("ggplot2")}
if (!require("ggcorrplot", character.only = TRUE)) {install.packages("ggcorrplot")}
if (!require("car", character.only = TRUE)) {install.packages("car")}
if (!require("dplyr", character.only = TRUE)) {install.packages("dplyr")}

library(ggplot2)     #Enables visualization of data
library(ggcorrplot)  #Creates correlation matrix heatmap
library(car)         #Used for calculation of Variance Inflation Factor (VIF)
library(dplyr)       #Used for combining relational data

# REMOVE DATA & VALUES FROM PREVIOUS RUN-----------------------------------

removeMe <- c(
  "data_vehicles",
  "data_emissions",
  "data_study",
  "file_vehicles",
  "file_emissions",
  "count_study",
  "count_emissions",
  "count_study_wNA",
  "count_study_woNA",
  "count_study_final",
  "count_removedvehicles",
  "count_vehicles",
  "data_bestfit",
  "data_study_bins",
  "data_study_kurtosis",
  "data_study_mean",
  "data_study_median",
  "data_study_mode",
  "data_study_sd",
  "data_study_skewness",
  "fitted_mean",
  "fitted_sd",
  "table_drive",
  "table_fueltype",
  "table_vclass",
  "correlation_matrix",
  "fit"
)

for(obj in removeMe) {
  if (exists(obj)) {
    rm(list = obj)
  }
}

rm(removeMe)

rm(obj)

# LOAD & VALIDATE VEHICLE DATA --------------------------------------------

file_vehicles <- "data/vehicles.csv"
data_vehicles <- read.csv(file_vehicles)
count_vehicles <- nrow(data_vehicles)
head(data_vehicles)
str(data_vehicles)
summary(data_vehicles)

# LOAD & VALIDATE EMISSIONS DATA (future use) -----------------------------

file_emissions <- "data/emissions.csv"
data_emissions <- read.csv(file_emissions)
count_emissions <- nrow(data_emissions)
head(data_emissions)
str(data_emissions)
summary(data_vehicles)

# ADD CALCULATED FIELDS ---------------------------------------------------

data_vehicles$volume <- 
  data_vehicles$hlv + 
  data_vehicles$hpv + 
  data_vehicles$lv2 + 
  data_vehicles$lv4 + 
  data_vehicles$pv2 + 
  data_vehicles$pv4
data_vehicles$transmission <- substr(
  data_vehicles$trany, 
  start=1, 
  stop=3
  )

# CREATE DATASET OF INTEREST ----------------------------------------------

data_study <- data_vehicles[, c(
  "co2TailpipeGpm",
  "barrels08",
  "comb08",
  "cylinders",
  "displ",
  "drive",
  "fuelType",
  "make",
  "transmission",
  "phevBlended",
  "VClass",
  "volume"
  )]
count_study_wNA <- nrow(data_study)
str(data_study)
head(data_study)
summary(data_study)

# REMOVE NULL DATA --------------------------------------------------------

data_study <- na.omit(data_study)
count_study_woNA <- nrow(data_study)

# REMOVE -0- DATA ---------------------------------------------------------
data_study <- subset(data_study, co2TailpipeGpm != 0)
data_study <- subset(data_study, cylinders != 0)
data_study <- subset(data_study, displ != 0)
count_study_final <- nrow(data_study)
summary(data_study$co2TailpipeGpm)

# CREATE PEARSON CORRLEATION COEFFICIENT MATRIX ---------------------------

data_study_numeric <- data_study[, c(
  "co2TailpipeGpm",
  "barrels08",
  "comb08",
  "cylinders",
  "displ",
  "volume"
  )]
count_study_numeric <- nrow(data_study_numeric)
correlation_matrix <- cor(data_study_numeric)
print(correlation_matrix)
ggcorrplot(correlation_matrix, lab = TRUE)
ggcorrplot(correlation_matrix, type = "lower", lab = TRUE)
rm(data_study_numeric)
rm(count_study_numeric)

# Remove variables aligned with CO2 emissions and volume, which is missing for some vehicle types
data_study <- data_study[, c("co2TailpipeGpm","cylinders","displ","drive","fuelType","make","transmission","phevBlended","VClass")]
count_study <- nrow(data_study)
count_removedvehicles <- count_vehicles - count_study
summary(data_study)
