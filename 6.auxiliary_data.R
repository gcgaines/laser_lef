library(raster)
# load metrics dataframe
setwd(code.dir)
load("metrics.Rdata")

# set directory for auxiliary info
aux.dir<-("c:\\gaines\\projects\\lubrecht\\data\\code\\laser_lef\\laser_lef\\Landsat")
setwd(aux.dir)

# 1. Landsat

# 1.1 Plot-level stuff
# Import Google Earth Engine Landsat 8 2015 data from within 50 m plot buffer
mean.15.50m <- read.csv("lef_2015_mean_50m.csv")
# Columns 1 and 18 have weird stuff in them (18 might be useful later?), so delete them
mean.15.50m <- mean.15.50m[, -c(1,18)]
head(mean.15.50m)
colnames(mean.15.50m)[10]<-"RIPID"
# Merge 2015 (06/01/2015-08/31/2015) Landsat 8 metrics with 'metrics' data frame
metrics <- merge(metrics, mean.15.50m, by="RIPID")
head(metrics)

# 1.2 Rasters
# Read in Landsat 8 composite image, all bands, clipped to LEF boundary
ls8_all <- stack('lef_ls8_all.tif')
ls8_all
ls8_all@layers
ls8_all[[1]]

# Break a single band out into a separate raster
b1 <- ls8_all[[1]]

# Create a grayscale color palette 
grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are not transparent
# Plot Landsat 8 Band 1 in grayscale
plot(b1, 
     col=grayscale_colors, 
     axes=FALSE,
     main="LEF Landsat 8 Band 1") 

# Plot in RGB
plot(ls8_all,r=4,g=3,b=2)

# 2. Sentinel

# 2.1 Plot-level stuff
# Import Google Earth Engine Landsat 8 2015 data from within 50 m plot buffer
mean.15.50m.s2 <- read.csv("lef_2015_sentinel_mean_50m.csv")
head(mean.15.50m.s2)
# First and last columns have weird stuff in them (last might be useful later?), so delete them
mean.15.50m.s2 <- mean.15.50m.s2[, -c(1,21)]
head(mean.15.50m.s2)
colnames(mean.15.50m.s2)[14]<-"RIPID"
# Merge 2015 (06/23/2015-06/23/2016) Sentinel metrics with 'metrics' data frame
metrics <- merge(metrics, mean.15.50m.s2, by="RIPID")
head(metrics)

# 2.2 Rasters

# Read in Sentinel 2 composite image, all bands, clipped to LEF boundary
s2_all <- stack('lef_s2_all.tif')
s2_all
s2_all@layers
s2_all[[1]]

# Break a single band out into a separate raster
b1.s2 <- s2_all[[1]]

# Plot Sentinel 2 Band 1 in grayscale
plot(b1.s2, 
     col=grayscale_colors, 
     axes=FALSE,
     main="LEF Sentinel 2 Band 1")
locs.wgs84 <- rip.db$RIPgpscoords[,c("RIPID","LONG_Predicted","LAT_Predicted")]
points(locs.wgs84$LONG_Predicted,locs.wgs84$LAT_Predicted, col="red",pch=".", cex=2)

# 3. Elevation

# 3.1 Add RIPID plot elevations fro access database to metrics dataframe
elev <- rip.db$RIPgpscoords[,c("RIPID","ELEV_Measured")]
metrics <- merge(metrics, elev, by="RIPID")
head(metrics)
xyplot(metrics$ba.live~metrics$ELEV_Measured)

# 3.2 Plot-level stuff
# Import Google Earth Engine USGS NED Elevation data from within 50 m plot buffer
mean.elev.50m <- read.csv("lef_mean_elev_plots.csv")
head(mean.elev.50m)
# First and last columns have weird stuff in them, and don't need lat/long
mean.elev.50m <- mean.elev.50m[, -c(1,3,4,6)]
head(mean.elev.50m)
colnames(mean.elev.50m)<-c("RIPID","elev.m")
# Round elevation in meters, and add column for elevation in feet
mean.elev.50m$elev.m <- round(mean.elev.50m$elev.m,0)
mean.elev.50m$elev.ft<-round(mean.elev.50m$elev.m*3.2808,0)
# Merge 2015 (06/23/2015-06/23/2016) Sentinel metrics with 'metrics' data frame
metrics <- merge(metrics, mean.elev.50m, by="RIPID")
head(metrics)

# check correspondence between Access DB measured elevations and GEE USGS NED elevations
xyplot(ELEV_Measured ~ elev.m, data=metrics,type = c("p", "r"))

# 3.3 Raster

# Read in USGS NED image clipped to LEF boundary
elev <- raster('lef_elev.tif')
elev

# Plot elevation image in grayscale
plot(elev, 
     col=grayscale_colors, 
     axes=FALSE,
     main="LEF Sentinel 2 Band 1")
points(locs.wgs84$LONG_Predicted,locs.wgs84$LAT_Predicted, col="red",pch=".", cex=2)

# save the metrics r data frame for the other guy
setwd(code.dir)
save(metrics,file="metrics.Rdata")
