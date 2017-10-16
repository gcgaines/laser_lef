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

# 2 importlastools csv as a data frame
lascan <- read.csv(file.path(products.dir,outfiles))

# 3 Add plot number column to laser metrics
lascan$RIPID <- as.numeric(sub(".las","",
                  sub("C:/gaines/projects/lubrecht/data/code/laser_lef/laser_lef/laser_clipped/crip","",
                      as.character(lascan$file_name))))
lascan <- lascan[,-1]

# 4 Remove ten plots from lascan with no large trees, ie not in plot.summ
# They were plots 13, 14, 89, 1010, 1011, 1012, 1014, 1015, 1016, 1017

lascan <- lascan[-c(13,14,15,17,18,19,20,23,24,106),]

# 5 merge plot.summ and lascan
metrics <- merge(lascan, plot.summ, by="RIPID")

# 6 save the metrics r data file for the other guy
setwd(code.dir)
save(metrics,file="metrics.Rdata")
