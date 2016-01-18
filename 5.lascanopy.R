# 1 generate lidar metrics for each of the plots; lastools outputs a csv
infiles <- "crip*.las"
outfiles <- "lascan.csv"
system(paste(file.path(lastools.dir,"lascanopy.exe"),
             "-i",file.path(products.dir,infiles),
             "-files_are_plots -names", #-cores",cores,
             "-p 5 10 25 50 75 90 -min -max -avg -std -kur -ske -qav",
             "-cov -dns",
             "-int_p 25 50 75",
             "-height_cutoff 1.37",
             "-odir",file.path(products.dir),
             "-o",outfiles),invisible=F)

# import csv from lastools as a data frame
lascan <- read.csv(file.path(products.dir,outfiles))

# Add plot number column to laser metrics
lascan$RIPID <- as.numeric(sub(".las","",
                  sub("C:/gaines/projects/lubrecht/data/LiDAR/Clipped/crip","",
                      as.character(lascan$file_name))))

lascan <- lascan[,-1]

write.csv(plot.summ, file="lefplotsummary15.csv", row.names=F)
write.csv(lascan, file="lefplotlasermetrics2015.csv", row.names=F)

?princomp
pc <- princomp(lascan[,1:(ncol(lascan)-1)], cor=T)

summary(pc)
pc$loadings
biplot(pc)


xyplot(max ~ cov, data=lascan)
xyplot(ske ~ cov, data=lascan)
xyplot(ske ~ max, data=lascan)
