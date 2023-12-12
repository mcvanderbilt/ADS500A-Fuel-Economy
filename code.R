# Exploratory data analysis of fuel economy data from the EPA

# Install Necessary Packages
install.packages("ggplot2")     #Enables visualization of data
install.packages("ggcorrplot")  #Creates correlation matrix heatmap
install.packages("car")         #Used for calculation of Variance Inflation Factor (VIF)
install.packages("dplyr")       #Used for combining relational data
install.packages("MASS")        #Used for creating a best fit function
install.packages("modeest")     #Used to calculate the statistical mode
install.packages("moments")     #Used to calculate the skewness and kurtosis

# Load Necessary Packages
library(ggplot2)
library(ggcorrplot)
library(car)
library(dplyr)
library(MASS)
library(modeest)
library(moments)

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
data_vehicles$transmission <- substr(data_vehicles$trany, start=1, stop=3)

# Create Dataset of Interest
data_study <- data_vehicles[, c("co2TailpipeGpm","barrels08","comb08","cylinders","displ","drive","fuelType","make","transmission","phevBlended","VClass","volume")]
count <- nrow(data_study)
head(data_study)
str(data_study)
summary(data_study)

# Remove NULL data
data_study <- na.omit(data_study)
count_study <- nrow(data_study)

# Remove -0- data
data_study <- subset(data_study, co2TailpipeGpm != 0)
data_study <- subset(data_study, cylinders != 0)
data_study <- subset(data_study, displ != 0)
count_study <- nrow(data_study)
summary(data_study$co2TailpipeGpm)

# Create Pearson Correlation Coefficient Matrix
data_study_numeric <- data_study[, c("co2TailpipeGpm","barrels08","comb08","cylinders","displ","volume")]
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

# Identify Summary Statistics for co2TailpipeGpm
data_study_mean <- mean(data_study$co2TailpipeGpm)
data_study_median <- median(data_study$co2TailpipeGpm)
data_study_mode <- mfv(data_study$co2TailpipeGpm)
data_study_sd <- sd(data_study$co2TailpipeGpm)
data_study_skewness <- skewness(data_study$co2TailpipeGpm)
data_study_kurtosis <- kurtosis(data_study$co2TailpipeGpm)
data_study_bins <- round(sqrt(count_study),0)

# Create histogram of co2TailpipeGpm
ggplot(data = data_study, aes(x = co2TailpipeGpm)) +
  geom_histogram(bins = data_study_bins, fill = "skyblue", color = "black") +
  labs(x = "CO2 Tailpipe Emission (gpm)", y = "Frequency", title = "Histogram of CO2 Tailpipe Emission")

# Create histogram of co2TailpipeGpm with best fit distribution
data_bestfit <- rnorm(count_study, mean = data_study_mean, data_study_sd)  # Generating random data following a normal distribution
fit <- fitdistr(data_bestfit, "normal")
fitted_mean <- fit$estimate[1]
fitted_sd <- fit$estimate[2]
ggplot(data = data_study, aes(x = co2TailpipeGpm)) +
  geom_histogram(bins = data_study_bins, fill = "skyblue", color = "black", aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = fitted_mean, sd = fitted_sd), color = "red", size = 1.2) +
  labs(x = "CO2 Tailpipe Emission (gpm)", y = "Frequency", title = "Histogram of CO2 Tailpipe Emission with Best-fit Normal Distribution Curve")
