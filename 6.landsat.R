# set directory
setwd("c:\\gaines\\projects\\lubrecht\\data\\code\\laser_lef\\laser_lef\\Landsat")

## Import Google Earth Engine Landsat data, calculated within 50m radius of each 
## plot center.

# 2007
max.07.50m <- read.csv("lef_2007_max_50m.csv")
mean.07.50m <- read.csv("lef_2007_mean_50m.csv")
min.07.50m <- read.csv("lef_2007_min_50m.csv")

# 2010
max.10.50m <- read.csv("lef_2010_max_50m.csv")
mean.10.50m <- read.csv("lef_2010_mean_50m.csv")
min.10.50m <- read.csv("lef_2010_min_50m.csv")

# 2015, minus bands 6 and 7, because I didn't get those for 07/10.
max.15.50m <- read.csv("lef_2015_max_50m.csv")
max.15.50m <- max.15.50m[, -c(7,8)]

mean.15.50m <- read.csv("lef_2015_mean_50m.csv")
mean.15.50m <- mean.15.50m[, -c(7,8)]

min.15.50m <- read.csv("lef_2015_min_50m.csv")
min.15.50m <- min.15.50m[, -c(7,8)]

## Merge 2015 (06/01/2015-08/31/2015) Landsat 8 metrics with 'metrics' data frame
metrics <- merge(metrics, max.15.50m, by="RIPID")
metrics <- merge(metrics, mean.15.50m, by="RIPID")
metrics <- merge(metrics, min.15.50m, by="RIPID")

## Rename all these wonky column names
colnames(metrics)[50] <- "B1.max"
colnames(metrics)[51] <- "B2.max"
colnames(metrics)[52] <- "B3.max"
colnames(metrics)[53] <- "B4.max"
colnames(metrics)[54] <- "B5.max"
colnames(metrics)[55] <- "evi.max"
colnames(metrics)[56] <- "mndwi.max"
colnames(metrics)[57] <- "ndvi.max"
colnames(metrics)[58] <- "ndwi.max"
colnames(metrics)[59] <- "B1.mean"
colnames(metrics)[60] <- "B2.mean"
colnames(metrics)[61] <- "B3.mean"
colnames(metrics)[62] <- "B4.mean"
colnames(metrics)[63] <- "B5.mean"
colnames(metrics)[64] <- "evi.mean"
colnames(metrics)[65] <- "mndwi.mean"
colnames(metrics)[66] <- "ndvi.mean"
colnames(metrics)[67] <- "ndwi.mean"
colnames(metrics)[68] <- "B1.min"
colnames(metrics)[69] <- "B2.min"
colnames(metrics)[70] <- "B3.min"
colnames(metrics)[71] <- "B4.min"
colnames(metrics)[72] <- "B5.min"
colnames(metrics)[73] <- "evi.min"
colnames(metrics)[74] <- "mndwi.min"
colnames(metrics)[75] <- "ndvi.min"
colnames(metrics)[76] <- "ndwi.min"

# Calculate change in Landsat metrics from 2010 to 2015
max.change <- max.15.50m - max.10.50m
mean.change <- mean.15.50m - mean.10.50m
min.change <- min.15.50m - min.10.50m

# An example of a for loop to try to figure out how the hell to make one.

u1 <- rnorm(30)

for(i in 1:NROW(u1))
{
  usq[i] <- u1[i] * u1[i]
  print(usq[i])
}

print(i)

# save the metrics r data frame for the other guy
setwd(code.dir)
save(metrics,file="metrics.Rdata")

############# make some graphs
library(lattice)

# EVI

xyplot(mean.crown.mass.lbs ~ evi.max, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Maximum EVI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Maximum Enhanced Vegetation Index")

xyplot(mean.crown.mass.lbs ~ evi.min, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Minimum EVI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Minimum Enhanced Vegetation Index")

xyplot(mean.crown.mass.lbs ~ evi.mean, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Mean EVI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Mean Enhanced Vegetation Index")

# NDVI

xyplot(mean.crown.mass.lbs ~ ndvi.max, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Maximum NDVI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Maximum Normalized Difference Vegetation Index")

xyplot(mean.crown.mass.lbs ~ ndvi.min, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Minimum NDVI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Minimum Normalized Difference Vegetation Index")

xyplot(mean.crown.mass.lbs ~ ndvi.mean, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Mean NDVI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Mean Normalized Difference Vegetation Index")

# MNDWI

xyplot(mean.crown.mass.lbs ~ mndwi.max, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Maximum MNDWI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Maximum Modified Normalized Difference Water Index")

xyplot(mean.crown.mass.lbs ~ mndwi.min, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Minimum MNDWI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Minimum Modified Normalized Difference Water Index")

xyplot(mean.crown.mass.lbs ~ mndwi.mean, data=metrics, 
       type=c("p","smooth"),
       main = "Mean Crown Mass vs. Mean MNDWI",
       ylab = "Mean Crown Mass Per Plot (lbs.)",
       xlab = "Mean Modified Normalized Difference Water Index")
