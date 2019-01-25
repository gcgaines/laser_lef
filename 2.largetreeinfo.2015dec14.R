#! 1. enhance the large-tree table
large.trees <- rip.db$RIPlargetrees

# add year of measurement information
dateinfo <- rip.db$RIPmeasureschedule[,c(2:4,6)]
dateinfo$year <- as.integer(substr(dateinfo$DateIn,1,4))

large.trees <- merge(large.trees, dateinfo[,c(1:3,5)], all.x=T)

#! 2. get 2015 measurements and check
large.trees15 <- droplevels(large.trees[large.trees$year==2015,])

# xyplot(TotalHt ~ DBH | Species, data=large.trees15,
  #     panel=function(x,y,...){panel.abline(0,1); panel.points(x,y)})

# xyplot(CrownHT ~ TotalHt |  Species, data=large.trees15,
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


# table(large.trees15$Status)


# subset analysis data
proj.ds <- large.trees15[!(large.trees15$Status %in% c("C","DD","M")), ]

plot.loc <- rip.db$RIPgpscoords[,c("RIPID","UTMX_Predicted","UTMY_Predicted")]

#  1) Calculate and add a column for BA per tree, all trees, in proj.ds
proj.ds$ba <- with(proj.ds, .005454*DBH^2)

#  2) Calculate and add a column for BA per tree, live trees, in proj.ds
proj.ds$ba.live <- with(proj.ds, ifelse(Status %in% c("DS","DEAD MISSING TAG"),0,ba))

#  3) add a count column with '1' for every row.
proj.ds$total.count <- 1

#  4) add a column with '1' for live trees and '0' for dead trees
proj.ds$live.count <- with(proj.ds, ifelse(Status %in% c("DS","DEAD MISSING TAG"),0,1))

#  5) add a 'squared DBH'column
proj.ds$dbh.sq <- proj.ds$DBH^2

#  6) crown length 
proj.ds$crown.length <- with(proj.ds, TotalHt-CrownHT)

# add a column with '1' for species = AF
proj.ds$AF <- with(proj.ds, ifelse(Species == "AF",1,0))

# add a column with '1' for species = DF
proj.ds$DF <- with(proj.ds, ifelse(Species == "DF",1,0))

# add a column with '1' for species = LP
proj.ds$LP <- with(proj.ds, ifelse(Species == "LP",1,0))

# add a column with '1' for species = WL
proj.ds$WL <- with(proj.ds, ifelse(Species == "WL",1,0))

# add a column with '1' for species = PP
proj.ds$PP <- with(proj.ds, ifelse(Species == "PP",1,0))

# add a column with '1' for species = ES
proj.ds$ES <- with(proj.ds, ifelse(Species == "ES",1,0))

#  7) Crown biomass per tree by species, lbs., Brown 1978

e <- exp(1)

crown.biomass <- function(species, dbh, ht, c){
  mass <- 0 + (species=="AF")*(7.345 + 1.255*(dbh^2)) +
    (species=="DF")*(27.94-(0.008695*(dbh^2*ht)) + 0.02839*(dbh^2*c)) +
    (species=="ES")*(e^(1.0404 + 1.7096*(log(dbh)))) +
    (species=="LP")*(e^(0.1224 + 1.8820*(log(dbh)))) +
    (species=="PP")*(e^(0.2680 + 2.0740*(log(dbh)))) +
    (species=="WL")*(e^(0.4373 + 1.6786*(log(dbh))))
  mass
}

proj.ds$crown.mass <- crown.biomass(proj.ds$Species, proj.ds$DBH, 
                                    proj.ds$TotalHt, proj.ds$crown.length)

#  8) Crown biomass in kg
proj.ds$crown.mass.kg <- proj.ds$crown.mass/2.2046

#  9) sum and aggregate to plot level 
plot.summ <- aggregate(proj.ds[, c(6,7,9,17:30)], 
                       by=list(RIPID=proj.ds$RIPID), sum, na.rm=T)

# calculate species proportions
plot.summ$AF <- round((plot.summ$AF/plot.summ$total.count), digits = 2)
plot.summ$DF <- round((plot.summ$DF/plot.summ$total.count), digits = 2)
plot.summ$LP <- round((plot.summ$LP/plot.summ$total.count), digits = 2)
plot.summ$WL <- round((plot.summ$WL/plot.summ$total.count), digits = 2)
plot.summ$PP <- round((plot.summ$PP/plot.summ$total.count), digits = 2)
plot.summ$ES <- round((plot.summ$ES/plot.summ$total.count), digits = 2)

#  10) Arithmetic mean DBH per plot
plot.summ$mean.dbh.total <- with(plot.summ, DBH/total.count)
plot.summ$mean.dbh.live <- with(plot.summ, DBH/live.count)

#  11) Quadratic mean DBH per plot
plot.summ$qmd.total <- with(plot.summ, sqrt(dbh.sq/total.count))
plot.summ$qmd.live <- with(plot.summ, sqrt(dbh.sq/live.count))

#  12) Mean TotalHt per plot
plot.summ$mean.ht.total <- with(plot.summ, TotalHt/total.count)
plot.summ$mean.ht.live <- with(plot.summ, TotalHt/live.count)

# 13) Max TotalHT per plot
maxs <- aggregate(proj.ds[, 7], by=list(RIPID=proj.ds$RIPID), max, na.rm=T)
plot.summ$max.ht.total <- maxs$x

# 14) Mean CrownHT per plot
plot.summ$mean.crownht.total <- with(plot.summ, CrownHT/total.count)
plot.summ$mean.crownht.live <- with(plot.summ, CrownHT/live.count)

# 15) BA/ac
plot.summ$ba.ac.total <- with(plot.summ, 4*ba)
plot.summ$ba.ac.live <- with(plot.summ, 4*ba.live)

# 16) TPA
plot.summ$tpa.total <- with(plot.summ, 4*total.count)
plot.summ$tpa.live <- with(plot.summ, 4*live.count)

# 17) Crown Length 
plot.summ$crown.length.total <- with(plot.summ, mean.ht.total-mean.crownht.total)
plot.summ$crown.length.live <- with(plot.summ, mean.ht.live-mean.crownht.live)

# 18) Crown Ratio
plot.summ$crown.ratio.total <- with(plot.summ, crown.length.total/mean.ht.total)
plot.summ$crown.ratio.live <- with(plot.summ, crown.length.live/mean.ht.live)

# 19) Mean live crown biomass per plot, lbs
plot.summ$mean.crown.mass.lbs <- with(plot.summ, crown.mass/live.count)

# 20) Mean live crown biomass per plot, kg
plot.summ$mean.crown.mass.kg <- with(plot.summ, crown.mass.kg/live.count)
