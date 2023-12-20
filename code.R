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
str(data_vehicles)
summary(data_vehicles)

# Load Emissions Data (FUTURE USE - NOT FOR CURRENT ANALYSIS)
file_emissions <- "data/emissions.csv"
data_emissions <- read.csv(file_emissions)
count_emissions <- nrow(data_emissions)
head(data_emissions)
str(data_emissions)
summary(data_vehicles)

# Add calculated field(s)
data_vehicles$volume <- data_vehicles$hlv + data_vehicles$hpv + data_vehicles$lv2 + data_vehicles$lv4 + data_vehicles$pv2 + data_vehicles$pv4

# Create Dataset of Interest
data_study <- data_vehicles[, c("co2TailpipeGpm","barrels08","comb08","cylinders","displ","drive","fuelType","make","trany","phevBlended","VClass","volume")]
count <- nrow(data_study)
head(data_study)
str(data_study)
summary(data_study)

# Remove NULL data
data_study <- na.omit(data_study)
count_study <- nrow(data_study)

# Remove -0- data
data_study <- subset(data_study, co2TailpipeGpm != 0)
data_study <- subset(data_study, volume != 0)
count_study <- nrow(data_study)
summary(data_study$co2TailpipeGpm)
count_removedvehicles <- count_vehicles - count_study

# Correlation matrix
data_study_numeric <- data_study[, c("co2TailpipeGpm","barrels08","comb08","cylinders","displ","volume")]
count_study_numeric <- nrow(data_study_numeric)
correlation_matrix <- cor(data_study_numeric)
print(correlation_matrix)
ggcorrplot(correlation_matrix, lab = TRUE)
ggcorrplot(correlation_matrix, type = "lower", lab = TRUE)
