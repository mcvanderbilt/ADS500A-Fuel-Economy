# Exploratory data analysis of fuel economy data from the EPA

# Install Necessary Packages
install.packages("ggplot2")     #Enables visualization of data
install.packages("ggcorrplot")  #Creates correlation matrix heatmap
install.packages("car")         #Used for calculation of Variance Inflation Factor (VIF)
install.packages("dplyr")       #Used for combining relational data

# Load Necessary Packages
library(ggplot2)
library(ggcorrplot)
library(car)
library(dplyr)

# Remove Existing Data
rm(data_vehicles)
rm(data_emissions)
rm(data_study)
rm(file_vehicles)
rm(file_emissions)
rm(count_study)
rm(count_emissions)
rm(count_study)

# Load Vehicles Data
file_vehicles <- "data/vehicles.csv"
data_vehicles <- read.csv(file_vehicles)
count_vehicles <- nrow(data_vehicles)
head(data_vehicles)
head_vehicles <- head(data_vehicles)
str(data_vehicles)
summary(data_vehicles)

# Load Emissions Data (FUTURE USE - NOT FOR CURRENT ANALYSIS)
file_emissions <- "data/emissions.csv"
data_emissions <- read.csv(file_emissions)
count_emissions <- nrow(data_emissions)
head(data_emissions)
str(data_emissions)
summary(data_vehicles)

# Create Dataset of Interest
data_study <- data_vehicles[, c("barrels08","co2TailpipeGpm","comb08","cylinders","displ","fuelType","make","model","trany")]

count <- nrow(data_study)
head(data_study)
str(data_study)
summary(data_study)

# Remove data with co2TailpipeGpm = NULL
data_study <- data_vehicles %>% filter(!is.na(co2TailpipeGpm))
count_study <- nrow(data_study)

# Remove data with co2TailpipeGpm = 0
data_study <- subset(data_study, co2TailpipeGpm != 0)
count_study <- nrow(data_study)
summary(data_study$co2TailpipeGpm)
