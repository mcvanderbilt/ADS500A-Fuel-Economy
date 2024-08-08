# -------------------------------------------------------------------------
# FOR THE CLASSROOM                                                       -
# EXPLORATORY DATA ANALYSIS OF FUEL ECONOMY DATA FROM THE EPA             -
# Matthew C. Vanderbilt, MSBA                                             -
#                                                                         -
# https://github.com/mcvanderbilt/Fuel-Economy                            -
# -------------------------------------------------------------------------
# DATA SOURCE:                            ---------------------------------
# https://fueleconomy.gov/feg/ws/         ---------------------------------
# 20240206 07:58 PST                      ---------------------------------
# -------------------------------------------------------------------------
#                                                                        --
# The objective of this analysis is to statistically investigate the     --
# association of primary fuel tailpipe carbon dioxide emissions in grams --
# per mile to annual primary-fuel petroleum consumption in barrels after --
# controlling for combined miles-per-gallon for the primary fuel type,   --
# vehicle manufacturer, make, engine displacement, engine cylinders,     --
# combined luggage and passenger volume in cubic feet, vehicle type,     --
# transmission type, and primary fuel type.                              --
#                                                                        --
# -------------------------------------------------------------------------

# INSTALL NECESSARY PACKAGES (only needed once per PC) --------------------

if (!require(ggplot2)) {install.packages("ggplot2")}
if (!require(ggformula)) {install.packages("ggformula")}
if (!require(ggcorrplot)) {install.packages("ggcorrplot")}
if (!require(car)) {install.packages("car")}
if (!require(mosaic)) {install.packages("mosaic")}
if (!require(dplyr)) {install.packages("dplyr")}
if (!require(supernova)) {install.packages("supernova")}

# LOAD NECESSARY PACKAGES (needed every time) -----------------------------

library(ggplot2)     # Enables visualization of data
library(ggformula)
library(ggcorrplot)  # Creates correlation matrix heat map
library(car)         # Used for calculation of Variance Inflation Factor
library(mosaic)      # Used for several calculation functions
library(dplyr)       # Used for combining relational data
library(supernova)   # Used for super-anova calculation

# CLEAN UP PREVIOUS RUN ---------------------------------------------------

rm(list = ls()) # clear all objects from the workspace

# LOAD DATASET TO DATAFRAME -----------------------------------------------

dataLocation <- "~/GitHub/Fuel-Economy/"
setwd(dataLocation)
dataFilePath <- "data/vehicles.csv"

rawData <- read.csv(
  file = file.path(
    dataFilePath
  ),
  header = TRUE
)

rawData.count <- nrow(rawData)

# EVALUTE DATA STRUCTURE --------------------------------------------------

str(rawData)
head(rawData)
tail(rawData)

# CREATE DATASET OF INTEREST ----------------------------------------------

epaData <- rawData[, c(
  "co2TailpipeGpm",
  "barrels08",
  "comb08",
  "cylinders",
  "displ",
  "drive",
  "fuelType",
  "fuelType1",
  "make",
  "trany",
  "phevBlended",
  "VClass",
  "hlv",
  "hpv",
  "lv2",
  "lv4",
  "pv2",
  "pv4"
  )]

epaData$volume <- 
  epaData$hlv + 
  epaData$hpv + 
  epaData$lv2 + 
  epaData$lv4 + 
  epaData$pv2 + 
  epaData$pv4


epaData$transmission <- substr(
  epaData$trany, 
  start=1, 
  stop=3
)

epaData$transmission <- ifelse(
  epaData$transmission == "Man",
  "Manual",
  "Automatic"
)

delVariables <- c(
  "hlv",
  "hpv",
  "lv2",
  "lv4",
  "pv2",
  "pv4",
  "trany"
)

epaData <- epaData[, !(
  names(epaData) %in% delVariables
)]

rm(delVariables)

epaData.count <- nrow(epaData)
str(epaData)
head(epaData)

# EVALUATE & REMOVE NULL DATA ---------------------------------------------

# evaluate records that are missing co2TailpipeGpm
epaData.nulls <- subset(
  epaData,
  is.null(co2TailpipeGpm) |
    is.na(co2TailpipeGpm)
) # no nulls were found so we can move forward to the next steps

# evaluate records that are missing any other value
epaData.nulls <- epaData[! complete.cases(epaData),]

# we can drop observations with missing data now
epaData <- na.omit(epaData)

epaData.count <- nrow(epaData)

# DESCRIPTIVE STATISTICS - QUANTITATIVE -----------------------------------

epaData.ds.co2TailpipeGpm <- favstats(epaData$co2TailpipeGpm)
epaData.ds.barrels08      <- favstats(epaData$barrels08)
epaData.ds.comb08         <- favstats(epaData$comb08)
epaData.ds.cylinders      <- favstats(epaData$cylinders)
epaData.ds.displ          <- favstats(epaData$displ)
epaData.ds.volume         <- favstats(epaData$volume)

epaData.ds.qualitative <- data.frame(
  variable = 'co2TailpipeGpm',
  min = epaData.ds.co2TailpipeGpm$min,
  Q1 = epaData.ds.co2TailpipeGpm$Q1,
  median = epaData.ds.co2TailpipeGpm$median,
  IQR = epaData.ds.co2TailpipeGpm$Q3 - epaData.ds.co2TailpipeGpm$Q1,
  Q3 = epaData.ds.co2TailpipeGpm$Q3,
  max = epaData.ds.co2TailpipeGpm$max,
  mean = epaData.ds.co2TailpipeGpm$mean,
  sd = epaData.ds.co2TailpipeGpm$sd,
  n = epaData.ds.co2TailpipeGpm$n,
  na = epaData.ds.co2TailpipeGpm$missing
)

epaData.ds.t1 <- data.frame(
  variable = 'barrels08',
  min = epaData.ds.barrels08$min,
  Q1 = epaData.ds.barrels08$Q1,
  median = epaData.ds.barrels08$median,
  IQR = epaData.ds.barrels08$Q3 - epaData.ds.barrels08$Q1,
  Q3 = epaData.ds.barrels08$Q3,
  max = epaData.ds.barrels08$max,
  mean = epaData.ds.barrels08$mean,
  sd = epaData.ds.barrels08$sd,
  n = epaData.ds.barrels08$n,
  na = epaData.ds.barrels08$missing
)

epaData.ds.t2 <- data.frame(
  variable = 'comb08',
  min = epaData.ds.comb08$min,
  Q1 = epaData.ds.comb08$Q1,
  median = epaData.ds.comb08$median,
  IQR = epaData.ds.comb08$Q3 - epaData.ds.comb08$Q1,
  Q3 = epaData.ds.comb08$Q3,
  max = epaData.ds.comb08$max,
  mean = epaData.ds.comb08$mean,
  sd = epaData.ds.comb08$sd,
  n = epaData.ds.comb08$n,
  na = epaData.ds.comb08$missing
)

epaData.ds.t3 <- data.frame(
  variable = 'cylinders',
  min = epaData.ds.cylinders$min,
  Q1 = epaData.ds.cylinders$Q1,
  median = epaData.ds.cylinders$median,
  IQR = epaData.ds.cylinders$Q3 - epaData.ds.cylinders$Q1,
  Q3 = epaData.ds.cylinders$Q3,
  max = epaData.ds.cylinders$max,
  mean = epaData.ds.cylinders$mean,
  sd = epaData.ds.cylinders$sd,
  n = epaData.ds.cylinders$n,
  na = epaData.ds.cylinders$missing
)

epaData.ds.t4 <- data.frame(
  variable = 'displ',
  min = epaData.ds.displ$min,
  Q1 = epaData.ds.displ$Q1,
  median = epaData.ds.displ$median,
  IQR = epaData.ds.displ$Q3 - epaData.ds.displ$Q1,
  Q3 = epaData.ds.displ$Q3,
  max = epaData.ds.displ$max,
  mean = epaData.ds.displ$mean,
  sd = epaData.ds.displ$sd,
  n = epaData.ds.displ$n,
  na = epaData.ds.displ$missing
)

epaData.ds.t5 <- data.frame(
  variable = 'volume',
  min = epaData.ds.volume$min,
  Q1 = epaData.ds.volume$Q1,
  median = epaData.ds.volume$median,
  IQR = epaData.ds.volume$Q3 - epaData.ds.volume$Q1,
  Q3 = epaData.ds.volume$Q3,
  max = epaData.ds.volume$max,
  mean = epaData.ds.volume$mean,
  sd = epaData.ds.volume$sd,
  n = epaData.ds.volume$n,
  na = epaData.ds.volume$missing
)

epaData.ds.qualitative <- bind_rows(
  epaData.ds.qualitative,
  epaData.ds.t1,
  epaData.ds.t2,
  epaData.ds.t3,
  epaData.ds.t4,
  epaData.ds.t5
)

rm(epaData.ds.t1)
rm(epaData.ds.t2)
rm()
rm()
rm()

removeMe <- c(
  "epaData.ds.t1",
  "epaData.ds.t2",
  "epaData.ds.t3",
  "epaData.ds.t4",
  "epaData.ds.t5",
  "epaData.ds.barrels08",
  "epaData.ds.co2TailpipeGpm",
  "epaData.ds.comb08",
  "epaData.ds.cylinders",
  "epaData.ds.displ",
  "epaData.ds.volume"
)

for(obj in removeMe) {
  if (exists(obj)) {
    rm(list = obj)
  }
}

rm(removeMe)
rm(obj)

print(epaData.ds.qualitative)

# DESCRIPTIVE STATISTICS - QUALITATIVE ------------------------------------

epaData.ds.drive <- epaData |>
  group_by(drive) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

epaData.ds.fuelType <- epaData |>
  group_by(fuelType) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

epaData.ds.fuelType1 <- epaData |>
  group_by(fuelType1) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

epaData.ds.make <- epaData |>
  group_by(make) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

epaData.ds.phevBlended <- epaData |>
  group_by(phevBlended) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

epaData.ds.VClass <- epaData |>
  group_by(VClass) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

epaData.ds.transmission <- epaData |>
  group_by(transmission) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

# HISTOGRAM OF co2TailpipeGpm ---------------------------------------------

epaData.ds.qualitative[1,]

tmpMedian <- epaData.ds.qualitative$median[1]
tmpMean <- epaData.ds.qualitative$mean[1]
tmpSD <- epaData.ds.qualitative$sd[1]

print(tmpMedian)
print(tmpMean)
print(tmpSD)

epaData.hist.co2TailpipeGpm <- gf_dhistogram(
  ~ co2TailpipeGpm, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
  ) |>
  gf_fitdistr(
    ~ co2TailpipeGpm, 
    data = epaData, 
    dist = "norm", 
    color = "red"
    ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of CO2 Tailpipe Emissions with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "CO2 Tailpipe Emissions",
       y = "Frequency"
       )
epaData.hist.co2TailpipeGpm

# HISTOGRAM OF barrels08 --------------------------------------------------

epaData.ds.qualitative[2,]

tmpMedian <- epaData.ds.qualitative$median[2]
tmpMean <- epaData.ds.qualitative$mean[2]
tmpSD <- epaData.ds.qualitative$sd[2]

print(tmpMedian)
print(tmpMean)
print(tmpSD)

epaData.hist.barrels08 <- gf_dhistogram(
  ~ barrels08, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
) |>
  gf_fitdistr(
    ~ barrels08, 
    data = epaData, 
    dist = "norm", 
    color = "red"
  ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of barrels08 with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "Annual Barrels of Patroleum Consumed",
       y = "Frequency"
  )
epaData.hist.barrels08

# HISTOGRAM OF comb08 -----------------------------------------------------

epaData.ds.qualitative[3,]

tmpMedian <- epaData.ds.qualitative$median[3]
tmpMean <- epaData.ds.qualitative$mean[3]
tmpSD <- epaData.ds.qualitative$sd[3]

print(tmpMedian)
print(tmpMean)
print(tmpSD)

epaData.hist.comb08 <- gf_dhistogram(
  ~ comb08, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
) |>
  gf_fitdistr(
    ~ comb08, 
    data = epaData, 
    dist = "norm", 
    color = "red"
  ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of comb08 with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "Combined Highway & City Fuel Efficiency",
       y = "Frequency"
  )
epaData.hist.comb08

# HISTOGRAM OF cylinders --------------------------------------------------

epaData.ds.qualitative[4,]

tmpMedian <- epaData.ds.qualitative$median[4]
tmpMean <- epaData.ds.qualitative$mean[4]
tmpSD <- epaData.ds.qualitative$sd[4]

print(tmpMedian)
print(tmpMean)
print(tmpSD)

epaData.hist.cylinders <- gf_dhistogram(
  ~ cylinders, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
) |>
  gf_fitdistr(
    ~ cylinders, 
    data = epaData, 
    dist = "norm", 
    color = "red"
  ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of cylinders with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "Engine Cylinders",
       y = "Frequency"
  )
epaData.hist.cylinders

# this didn't work well being discrete; we'll be better off with a tally
epaData.ds.cylinders <- epaData |>
  group_by(cylinders) |>
  summarise(n = n()) |>
  mutate(rel.freq = n / sum(n))

# HISTOGRAM OF displ ------------------------------------------------------

epaData.ds.qualitative[5,]

tmpMedian <- epaData.ds.qualitative$median[5]
tmpMean <- epaData.ds.qualitative$mean[5]
tmpSD <- epaData.ds.qualitative$sd[5]

print(tmpMedian)
print(tmpMean)
print(tmpSD)

epaData.hist.displ <- gf_dhistogram(
  ~ displ, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
) |>
  gf_fitdistr(
    ~ displ, 
    data = epaData, 
    dist = "norm", 
    color = "red"
  ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of displ with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "Engine Displacement",
       y = "Frequency"
  )
epaData.hist.displ

# HISTOGRAM OF volume -----------------------------------------------------

epaData.ds.qualitative[6,]

tmpMedian <- epaData.ds.qualitative$median[6]
tmpMean <- epaData.ds.qualitative$mean[6]
tmpSD <- epaData.ds.qualitative$sd[6]

print(tmpMedian)
print(tmpMean)
print(tmpSD)

epaData.hist.volume <- gf_dhistogram(
  ~ volume, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
) |>
  gf_fitdistr(
    ~ volume, 
    data = epaData, 
    dist = "norm", 
    color = "red"
  ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of volume with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "Vehicle Volume",
       y = "Frequency"
  )
epaData.hist.volume

## volume has too many 0s to use in modeling

# MULTIVARIABLE LINEAR MODELING #1 ----------------------------------------

epaData.lm1 <- lm(
  co2TailpipeGpm ~
    barrels08 +
    comb08 +
    cylinders +
    displ +
    drive +
    fuelType1 +
    make +
    VClass +
    transmission,
  data = epaData
)
summary(epaData.lm1)

# that's a lot of levels and high p-values; let's try again without make


# MULTIVARIABLE LINEAR MODELING #2 ----------------------------------------

epaData.lm2 <- lm(
  co2TailpipeGpm ~
    barrels08 +
    comb08 +
    cylinders +
    displ +
    drive +
    fuelType1 +
    VClass +
    transmission,
  data = epaData
)
summary(epaData.lm2)

# that's still not good; let's remove VClass and try again

# MULTIVARIABLE LINEAR MODELING #3 ----------------------------------------

epaData.lm3 <- lm(
  co2TailpipeGpm ~
    barrels08 +
    comb08 +
    cylinders +
    displ +
    drive +
    fuelType1 +
    transmission,
  data = epaData
)
summary(epaData.lm3)

# let's try without drive; it has a number of high p-values, too

# MULTIVARIABLE LINEAR MODELING #4 ----------------------------------------

epaData.lm4 <- lm(
  co2TailpipeGpm ~
    barrels08 +
    comb08 +
    cylinders +
    displ +
    fuelType1 +
    transmission,
  data = epaData
)
summary(epaData.lm4)
supernova(epaData.lm4)
vif(epaData.lm4)

# that's looking much better, but we have high VIF; let's check for
# potential collinearity issues

# CREATE PEARSON CORRLEATION COEFFICIENT MATRIX ---------------------------

epaData.correlation <- epaData[, c(
  "co2TailpipeGpm",
  "barrels08",
  "comb08",
  "cylinders",
  "displ",
  "volume"
)]

epaData.correlation <- cor(epaData.correlation)
print(epaData.correlation)
ggcorrplot(epaData.correlation, lab = TRUE)

# engine displacement and cylinders are highly correlated; let's first
# take a look at them and then try removing cylinders from the study 
# based on our r value and supernova data

# SMOOTHED PLOT OF CO2 BY DISPLACEMENT ------------------------------------

# SCATTERPLOT OF DISPLACEMENT BY CYLINDERS --------------------------------

ggplot(
  epaData,
  aes(
    x = cylinders,
    y = displ
  )
) +
  geom_point(
    alpha = 1 / 10
  ) +
  labs(
    title = "Scatterplot of Engine Displacement by Cylinders",
    x = "Engine Cylinders",
    y = "Engine Displacement (L)"
  )


epaData.model.seviz <- epaData |>
  ggplot(
    aes(
      x = displ, 
      y = co2TailpipeGpm
    )
  ) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = TRUE) +
  labs(
    title = "Scatterplot of CO2 Emissions by Displacement with Grand Mean", 
    x = "Engine Displacement (L)", 
    y = "CO2 Tailpipe Emissions (GPM)"
  )

# There is definitely direct correlation with those values; let's
# remove cylinders and try again

# MULTIVARIABLE LINEAR MODELING #5 ----------------------------------------

epaData.lm5 <- lm(
  co2TailpipeGpm ~
    barrels08 +
    comb08 +
    displ +
    fuelType1 +
    transmission,
  data = epaData
)
summary(epaData.lm5)
supernova(epaData.lm5)
vif(epaData.lm5)

# barrels08 and comb08 both have high VIF still and our r2 value does
# indicate the potential for overfitting. Let's try modeling without 
# those variables, especially since, logically, they are directly
# associated with CO2 emissions rather than representing model
# variables

# MULTIVARIABLE LINEAR MODELING #6 ----------------------------------------

epaData.lm6 <- lm(
  co2TailpipeGpm ~
    displ +
    fuelType1 +
    transmission,
  data = epaData
)
summary(epaData.lm6)
supernova(epaData.lm6)
vif(epaData.lm6) 

# this is looking better. what if we try simplifying our model a little
# bit more?

# MULTIVARIABLE LINEAR MODELING #6 ----------------------------------------

epaData.lm7 <- lm(
  co2TailpipeGpm ~
    displ,
  data = epaData
)
summary(epaData.lm7)
supernova(epaData.lm7)

# look at that - we've explained almost the same amount of variation as
# with our more complex model. we've eliminated collinearity concern and
# have a model that doesn't appear to be overfit (subject to testing)

# LINEAR MODEL OF CO2 BY DISPLACEMENT -------------------------------------

epaData.lm7.viz <- ggplot(
  epaData, 
  aes(
    x = displ, 
    y = co2TailpipeGpm
  )
) +
  geom_point() +
  geom_hline(
    yintercept = mean(epaData$co2TailpipeGpm), 
    color = "red"
  ) +
  geom_hline(
    yintercept = median(epaData$co2TailpipeGpm), 
    linetype = "dashed", 
    color = "red"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  labs(
    title = "Scatterplot of CO2 Emissions by Displacement with Grand Mean", 
    x = "Engine Displacement (L)", 
    y = "CO2 Tailpipe Emissions (GPM)"
  )
epaData.lm7.viz


# ADD PREDICTED AND RESIDUAL VALUES TO DATAFRAME --------------------------

epaData$co2Prediction <- predict(epaData.lm7)

epaData$co2Residual <- resid(epaData.lm7)

# EVALUATE RESIDUALS BY PREDICTED VALUES ----------------------------------

epaData.lm7.resid.predresid <- ggplot(
  epaData,
  aes(
    x = co2Prediction,
    y = co2Residual
  )
) +
  geom_point() +
  labs(
    title = "Scatterplot of Residuals by Predicted CO2 Emissions",
    x = "Predicted CO2 Emissions",
    y = "Residual Values"
  )
epaData.lm7.resid.predresid

# EVALUATE RESIDUALS ------------------------------------------------------

epaData.lm7.resid.sp <- ggplot(
  epaData,
  aes(
    x = co2TailpipeGpm,
    y = co2Residual
  )
) +
  geom_point() +
  labs(
    title = "Scatterplot of Residuals by Observed CO2 Emissions",
    x = "Observed CO2 Emissions",
    y = "Residual Values"
  )
epaData.lm7.resid.sp

# PERFORM NORMALITY CHECK -------------------------------------------------

tmpMedian <- median(epaData$co2Residual)
tmpMean <- mean(epaData$co2Residual)
tmpSD <- sd(epaData$co2Residual)

epaData.lm7.resid.hist <- gf_dhistogram(
  ~ epaData$co2Residual, 
  data = epaData,
  bins = round(sqrt(epaData.count)/4,0),
  fill = "lightblue", 
  color = "black"
) |>
  gf_fitdistr(
    ~ epaData$co2Residual, 
    data = epaData, 
    dist = "norm", 
    color = "red"
  ) |>
  gf_vline(
    xintercept = tmpMean,
    color = "blue",
    linetype = "solid",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMedian,
    color = "blue",
    linetype = "dashed",
    size = 2
  ) |>
  gf_vline(
    xintercept = tmpMean - tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + tmpSD,
    color = "purple",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 2*tmpSD,
    color = "orange",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean - 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) |>
  gf_vline(
    xintercept = tmpMean + 3*tmpSD,
    color = "grey",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Histogram of CO2 Tailpipe Residuals with Normal Distribution
       Includes Empirical Rule 68.2/95.4/99.7 Values",
       x = "CO2 Tailpipe Emissions",
       y = "Frequency"
  )
epaData.lm7.resid.hist

# notice the residuals follow a normal distribution as expected

# EVALUATION OF SS AND SAD ------------------------------------------------

epaData$co2Residual.SSD <- epaData$co2Residual ^ 2
epaData$co2Residual.SAD <- abs(epaData$co2Residual)

epaData.lm7.resid.SSD <- ggplot(
  epaData,
  aes(
    x = co2TailpipeGpm,
    y = epaData$co2Residual.SSD
  )
) +
  geom_point() +
  labs(
    title = "Scatterplot of Squared Deviations by Observed CO2 Emissions",
    x = "Observed CO2 Emissions",
    y = "Squared Residual"
  )
epaData.lm7.resid.SSD

epaData.lm7.resid.SAD <- ggplot(
  epaData,
  aes(
    x = co2TailpipeGpm,
    y = epaData$co2Residual.SAD
  )
) +
  geom_point() +
  labs(
    title = "Scatterplot of Absolute Deviations by Observed CO2 Emissions",
    x = "Observed CO2 Emissions",
    y = "Squared Residual"
  )
epaData.lm7.resid.SAD


# FINAL LOOK AT LINEAR MODEL ----------------------------------------------

summary(epaData.lm7)
supernova(epaData.lm7)

# CONCLUSION --------------------------------------------------------------

# Based on our objective variables, the best model appear to be the use
# of engine displacement to predict CO2 emissions.