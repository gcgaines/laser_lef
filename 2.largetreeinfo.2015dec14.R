#! 1. enhance the large-tree table
large.trees <- rip.db$RIPlargetrees

# add year of measurement information
dateinfo <- rip.db$RIPmeasureschedule[,c(2:4,6)]
dateinfo$year <- as.integer(substr(dateinfo$DateIn,1,4))

large.trees <- merge(large.trees, dateinfo[,c(1:3,5)], all.x=T)

#! 2. get 2015 measurements and check
large.trees15 <- droplevels(large.trees[large.trees$year==2015,])

#xyplot(TotalHt ~ DBH | Species, data=large.trees15,
 #      panel=function(x,y,...){panel.abline(0,1); panel.points(x,y)})

#xyplot(CrownHT ~ TotalHt |  Species, data=large.trees15,
 #      panel=function(x,y,...){panel.abline(0,1); panel.points(x,y)})

# Plot CrownHT/TotalHT by species; Use 'identify' to find weird trees
#spp <- "DF"

#not.missing <- large.trees15$Species==spp&!is.na(large.trees15$TotalHt)&!is.na(large.trees15$CrownHT)

#plot(large.trees15$TotalHt[not.missing], large.trees15$CrownHT[not.missing])
 #     abline(0,1)

#identify(large.trees15$CrownHT[large.trees15$Species==spp], 
 #    large.trees15$TotalHt[large.trees15$Species==spp], 
 #    large.trees15$ID[large.trees15$Species==spp])
    
# crown heights exceed total heights - checked for crown heights>.9(totalht)
#large.trees15[!is.na(large.trees15$CrownHT) & !is.na(large.trees15$TotalHt) &
 #             large.trees15$CrownHT>.9*large.trees15$TotalHt, ]


#table(large.trees15$Status)


# subset analysis data
proj.ds <- large.trees15[!(large.trees15$Status %in% c("C","DD","M")), ]

plot.loc <- rip.db$RIPgpscoords[,c("RIPID","UTMX_Predicted","UTMY_Predicted")]

#write.csv(proj.ds, file="lef2015trees.csv", row.names=F)
#write.csv(plot.loc, file="lef2015plots.csv", row.names=F)

### next steps...

#  1) Calculate and add a BA/tree column in proj.ds
proj.ds$ba <- with(proj.ds, .005454*DBH^2)

#  2) Calculate and add a BA/tree column for LIVE trees in proj.ds
proj.ds$ba.live <- with(proj.ds, ifelse(Status %in% c("DS","DEAD MISSING TAG"),0,ba))

#  3) add a count column with '1' for every row.
proj.ds$total.count <- 1

#  4) add a column with '1' for live trees and '0' for dead trees
proj.ds$live.count <- with(proj.ds, ifelse(Status %in% c("DS","DEAD MISSING TAG"),0,1))

#  5) add a 'squared DBH'column
proj.ds$DBHsq <- proj.ds$DBH^2

#  6) sum and aggregate to plot level 
plot.summ <- aggregate(proj.ds[, c(6,7,9,17,18,19,20,21)], by=list(RIPID=proj.ds$RIPID), sum, na.rm=T)

#  7) Arithmetic mean DBH per plot
plot.summ$meanDBH.total <- with(plot.summ, DBH/total.count)
plot.summ$meanDBH.live <- with(plot.summ, DBH/live.count)

#  8) Quadratic mean DBH per plot
plot.summ$QMD.total <- with(plot.summ, sqrt(DBHsq/total.count))
plot.summ$QMD.live <- with(plot.summ, sqrt(DBHsq/live.count))

#  9) Mean TotalHt per plot
plot.summ$MeanHt.total <- with(plot.summ, TotalHt/total.count)
plot.summ$MeanHt.live <- with(plot.summ, TotalHt/live.count)

# 10) Max TotalHT per plot
maxs <- aggregate(proj.ds[, 7], by=list(RIPID=proj.ds$RIPID), max, na.rm=T)
plot.summ$MaxHt.total <- maxs$x

# 11) Mean CrownHT per plot
plot.summ$MeanCrownHT.total <- with(plot.summ, CrownHT/total.count)
plot.summ$MeanCrownHT.live <- with(plot.summ, CrownHT/live.count)

# 12) BA/ac
plot.summ$ba.ac.total <- with(plot.summ, 4*ba)
plot.summ$ba.ac.live <- with(plot.summ, 4*ba.live)

# 13) TPA
plot.summ$TPA.total <- with(plot.summ, 4*total.count)
plot.summ$TPA.live <- with(plot.summ, 4*live.count)

