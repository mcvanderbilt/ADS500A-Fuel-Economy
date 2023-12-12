# Exploratory data analysis of fuel economy data from the EPA

# Install Necessary Packages
install.packages("ggplot2")     #Enables visualization of data
install.packages("ggcorrplot")  #Creates correlation matrix heatmap
install.packages("car")         #Used for calculation of Variance Inflation Factor (VIF)

# Load Necessary Packages
library(ggplot2)        #Enables visualization of data
library(ggcorrplot)     #Creates correlation matrix heatmap
library(car)            #Used for calculation of Variance Inflation Factor (VIF)

# Remove Existing Data
rm(data_vehicles)
rm(data_emissions)
rm(file_vehicles)
rm(file_emissions)

# Load Vehicles Data
file_vehicles <- "data/vehicles.csv"
data_vehicles <- read.csv(file_vehicles)
head(data_vehicles)
str(data_vehicles)
summary(data_vehicles)

# Load Emissions Data
file_emissions <- "data/emissions.csv"
data_emissions <- read.csv(file_emissions)
head(data_emissions)
str(data_emissions)
summary(data_vehicles)
