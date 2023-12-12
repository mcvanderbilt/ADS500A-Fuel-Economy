# Exploratory data analysis of fuel economy data from the EPA

# Install Necessary Packages
install.packages("ggplot2")     #Enables visualization of data
install.packages("ggcorrplot")  #Creates correlation matrix heatmap
install.packages("car")         #Used for calculation of Variance Inflation Factor (VIF)
install.packages("readxl")      #Enables import of data from Microsoft Excel

# Load Necessary Packages
library(ggplot2)        #Enables visualization of data
library(ggcorrplot)     #Creates correlation matrix heatmap
library(car)            #Used for calculation of Variance Inflation Factor (VIF)
library(readxl)         #Enables import of data from Microsoft Excel

# Load Data

rm(ads500a_data)
ads500a_data <- read_excel("ads500a_data.xlsx", sheet = "vehicles_workingfile", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "numeric", "numeric", "numeric"))
View(ads500a_data)

# Separate Numeric Variables of Interest (replace NA values in Cylinders and Displacement with 0)
rm(ads500a_data_numeric)
ads500a_data_numeric <- ads500a_data[, c("barrels08","co2TailpipeGpm","comb08","cylinders","displ","volume","vehtype","emissionscat","transtype_id","prifueltype","make_id")]
ads500a_data_numeric[is.na(ads500a_data_numeric)] <- 0
View(ads500a_data_numeric)

# Create Pearson Correlation Coefficient Matrix
rm(correlation_matrix)
correlation_matrix <- cor(ads500a_data_numeric)
print(correlation_matrix)
ggcorrplot(correlation_matrix, lab = TRUE)
ggcorrplot(correlation_matrix, type = "lower", lab = TRUE)

# View descriptive statistics of numeric fields <<TODO>>
ads500a_descrstat <- summary(ads500a_data)
print(ads500a_descrstat)

# Chisq Table
table(ads500a_data$vehtype_name, ads500a_data$emissiontype_name)
chisq.test(ads500a_data$vehtype_name, ads500a_data$emissiontype_name)

bivariate_frequency <- table(ads500a_data$vehtype_name, ads500a_data$emissiontype_name)
View(bivariate_frequency)

bivariate_frequency_proportion <- round(prop.table(bivariate_frequency, margin = 2)*100, 1)
View(bivariate_frequency_proportion)

# Create histogram of CO2 with emission type overlay
ggplot(ads500a_data, aes(co2TailpipeGpm)) + geom_histogram(aes(fill = emissiontype_name), color="black")

