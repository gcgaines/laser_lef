source("1.ReadRIPdb.2015dec14.R",echo=F)
setwd(code.dir)

#! 1. subset the large-tree table
large.trees <- rip.db$RIPlargetrees
large.trees <- large.trees[!(!is.na(large.trees$Status) & large.trees$Status=="OUT"),]

#! 2. simplify the status call to live/dead
levels(large.trees$Status)
large.trees$LiveDead <- factor(ifelse(unclass(large.trees$Status)
                                 %in% c(NA,"FENCE","L"),"LIVE","DEAD"))

#! 3. add year of measurement 
dateinfo <- rip.db$RIPmeasureschedule[,c(2:4,6)]
dateinfo$year <- as.integer(substr(dateinfo$DateIn,1,4))
large.trees <- merge(large.trees[,c(1:7,9,11,12,14)],
                     dateinfo[,c(1:3,5)],all.x=T)
                     
#! 4. add unique tree id
large.trees$unique.tree <- with(large.trees,
                              (as.factor(RIPID):as.factor(Tree))[drop=T])
                              
#! 5. mine the damage codes

# this function scans damage code field for damage codes of interest
assess.damage <- function(special.codes){
  each.tree <- lapply(damage.codes,function(x){
    result <- 0
    if (!is.na(x)){
      len <- floor(nchar(x)/2)
      for (i in 1:len){
        result <- result + (substr(x,2*(i-1)+1,2*(i-1)+2) %in% special.codes)
      }}
    (result>0)
  })
}

# replace 0 damage with NA
large.trees$Damage <- with(large.trees,
                           ifelse(!is.na(Damage) & Damage==0,
                                NA,Damage))

# create a list of damage for each tree entry 
damage.codes <- split(large.trees$Damage,large.trees$ID)

# identify cut trees
# prior to 2015 "Cut" was not a status code; only in damage
large.trees$Cut <- unsplit(assess.damage(11),large.trees$ID)
#92 3227 is not a cut tree, but is dead
large.trees$Cut <- with(large.trees,
                     ifelse(LiveDead=="LIVE" |
                             (RIP_Measure=="92-1" & Tree==3227),
                       FALSE,Cut))
large.trees$Cut <- with(large.trees,
                     ifelse(!is.na(Status) & Status=="C",
                            TRUE,Cut))
large.trees[large.trees$Cut,c(1:6,8:10,15)]

# identify animal damage
large.trees$Animal <- unsplit(assess.damage(60:67),large.trees$ID)
large.trees[large.trees$Animal,c(1,2,4:6,8:10,12,14,15)]

# identify broken missing top damage
large.trees$TopDamage <- unsplit(assess.damage(81:82),large.trees$ID)
                     


#! 6. error checking
#check missing species
with(large.trees,large.trees[is.na(Species) | Species=="",])

#check missing dbhs
large.trees[is.na(large.trees$DBH) & large.trees$LiveDead=="LIVE",]

#check DBH range
summary(large.trees$DBH[large.trees$LiveDead=="LIVE"])


#check missing heights
large.trees[is.na(large.trees$TotalHt) & large.trees$LiveDead=="LIVE" &
              large.trees$year==2010,]
#-> 36:568 

large.trees[is.na(large.trees$TotalHt) & large.trees$LiveDead=="LIVE" &
              large.trees$year==2015,]
#-> none

large.trees[is.na(large.trees$TotalHt) & large.trees$LiveDead=="LIVE",
            c(1:6,10,12,13,14)]
summary(large.trees$Species[is.na(large.trees$TotalHt) & large.trees$LiveDead=="LIVE"])
  #-> 2007 plots 15, 58, 64 no heights by design

  
#check ht-diam relations
xyplot(TotalHt ~ (DBH) | Species, groups=year, scales="free",
  data=large.trees[large.trees$Species %in% c("DF","PP","LP","WL","AF","BC"),],
  #data=large.trees[large.trees$year==2015,],
  auto.key=list(columns=3,"top"), type=c("p","smooth"))

  #-> BC was poorly measured in 2008
large.trees[large.trees$year==2008 & large.trees$Species=="BC",]
large.trees$TotalHt[large.trees$year==2008 & large.trees$Species=="BC"] <- NA

#check dbh-dbh relations
live15 <- large.trees[large.trees$LiveDead=="LIVE" &
                        large.trees$year==2015,
                      c("unique.tree","Species","DBH","TotalHt","TopDamage")]
live10 <- large.trees[large.trees$LiveDead=="LIVE" &
                      large.trees$year==2010,
                      c("unique.tree","Species","DBH","TotalHt")]
live1015 <- merge(live10,live15, by="unique.tree")
with(live1015,plot(DBH.y,DBH.x))
#with(live1015,identify(DBH.y,DBH.x,unique.tree))

with(live1015[live1015$TopDamage==F,],plot(TotalHt.y,TotalHt.x))
#with(live1015[live1015$TopDamage==F,],identify(TotalHt.y,TotalHt.x,unique.tree))




#! 7. impute heights for unmeasured trees
large.trees[large.trees$LiveDead=="LIVE" &
                   large.trees$year==2010 & 
                   is.na(large.trees$TotalHt),]

sameplot <- large.trees[large.trees$LiveDead=="LIVE" &
              large.trees$year==2010 &
              large.trees$RIPID==36 & 
              !is.na(large.trees$TotalHt) & 
              large.trees$Species=="DF" &
              large.trees$TopDamage==FALSE,]
with(sameplot,plot(DBH,TotalHt))

mod <- lm(TotalHt ~ DBH, data=sameplot)

large.trees$TotalHt[large.trees$unique.tree=="36:568" &
              large.trees$year==2010] <- coef(mod) %*% c(1,16)

#! 8. impute heights for live, top damaged trees
large.trees$predHt <- ifelse(large.trees$TopDamage,NA,large.trees$TotalHt)
broken <- large.trees[large.trees$LiveDead=="LIVE" & 
                        large.trees$TopDamage==TRUE &
                         large.trees$year>=2010,
                      c("unique.tree","RIPID","year","Species")]

for (tr in 1:nrow(broken)){
  sameplot <- large.trees[large.trees$LiveDead=="LIVE" &
                          large.trees$RIPID==broken$RIPID[tr] &
                          large.trees$year==broken$year[tr] &
                          large.trees$Species==broken$Species[tr] &
                          large.trees$TopDamage==FALSE,]  
  if (nrow(sameplot)<5){
    sameplot <- large.trees[large.trees$LiveDead=="LIVE" &
                              large.trees$year==broken$year[tr] &
                              large.trees$Species==broken$Species[tr] &
                              large.trees$TopDamage==FALSE,]      
  }
  
  if (nrow(sameplot)>5){
   mod <- lm(TotalHt ~ DBH, data=sameplot)
   
   dbh <- large.trees$DBH[large.trees$unique.tree==broken$unique.tree[tr] &
                             large.trees$year==broken$year[tr]]
   large.trees$predHt[large.trees$unique.tree==broken$unique.tree[tr] &
                      large.trees$year==broken$year[tr]] <- coef(mod) %*% c(1,dbh)
  } else {
    large.trees$predHt[large.trees$unique.tree==broken$unique.tree[tr] &
                         large.trees$year==broken$year[tr]] <- 
      large.trees$TotalHt[large.trees$unique.tree==broken$unique.tree[tr] &
                            large.trees$year==broken$year[tr]]
  }
}

large.trees$predHt <- with(large.trees,ifelse(predHt<TotalHt,TotalHt,predHt))

with(large.trees[large.trees$unique.tree %in% unique(broken$unique.tree) & 
                   large.trees$TopDamage,],
     plot(TotalHt,predHt)); abline(0,1)


#! 9. add volumes
# the major conifers DF,WL,LP,PP,ES,AF come from Flewelling/INGY eqns
# RJ (2 trees) uses the ES equation
# the hardwoods QA (14),BC (25), MA(3) use Schlaegel's 1975 equations
#   for QA in Minn.
vol.needs <- with(large.trees[!is.na(large.trees$DBH) &
                              !is.na(large.trees$TotalHt) &
                              large.trees$year>=2010 &
                              large.trees$LiveDead=="LIVE" &
                              (large.trees$Species %in%
                               c("DF","WL","LP","PP","ES","AF","RJ")),],
               data.frame(TreeID=ID,Species=Species,
                 Region=2,Subregion=16,
                 DBH=DBH,HeightTotal=predHt,
                 StumpHt=0,TopHt=TotalHt))

setwd(file.path(code.dir,"FlewellingTaper"))
vols <- NULL
for (i in vol.needs$TreeID){
  x <- vol.needs[vol.needs$TreeID==i,]
  if (x$Species=="RJ") { x$Species <- "ES" }
  write.table(cbind(x,0), file="ingyvolin.txt", sep=",",
    col.names=F, row.names=F)

  system("FlewellingTaper")

  ret <- read.table("ingyvolout.txt")
  names(ret) <- c("ID","dib","dob","vol","cvb")
  vols <- rbind(vols,ret[,c(1,4)])
}
setwd(code.dir)

large.trees2 <- merge(large.trees,vols,all.x=T)
#large.trees2 <- large.trees; large.trees2$vol <- 0

table(large.trees2$Species[is.na(large.trees2$vol) & 
                             large.trees2$LiveDead=="LIVE" & 
                             large.trees2$year>2009])

large.trees2$vol <- ifelse(large.trees2$LiveDead=="LIVE" &
                      large.trees2$Species %in% c("QA","BC","MA"),
                        exp(.99554*log(large.trees2$TotalHt*
                             (.005454*large.trees2$DBH^2))-.85373),
                        large.trees2$vol)


#with(large.trees2,{plot(DBH,vol,col=Species); identify(DBH,vol,ID)})

levels(large.trees2$Species)
sp0 <- "AF"
with(large.trees2[large.trees2$year>2009 & large.trees2$LiveDead=="LIVE" &
                    large.trees2$Species==sp0,],
     plot(TotalHt*DBH*DBH,vol,col=1+TopDamage))





#! 10. assign trees to sets for the 2010-2015 period: the
#!     sets being (always) Dead, Survivor, Ingrowth, Cut, Mortality
tree.records <- split(large.trees2[large.trees2$year>2009,
                                   c("RIP_Measure","unique.tree","Measurement","year",
                                      "LiveDead","Damage","Cut")],
                  large.trees2$unique.tree[large.trees2$year>2009],drop=T)

# ERROR CHECK: look for records that only show up once in 2010 or 2015
single.rec <- lapply(tree.records,function(x){ nrow(x) })
single.rec <- unlist(single.rec)
singles <- names(single.rec[single.rec==1])
single.rec <- large.trees2[large.trees2$unique.tree %in% singles,c(12:15,2,5:6,11,9,16)]
single.rec[order(single.rec$unique.tree,single.rec$year),]
# all appears to be ingrowth

tree.classify <- lapply(tree.records,function(x){
  if ( sum(x$year==2010)==1 ){
    livedead2010 <- as.character(x$LiveDead[x$year==2010])
    cut2010 <- as.character(x$Cut[x$year==2010])
  } else {
    livedead2010 <- "NA"
    cut2010 <- "NA"
  }
  
  if ( sum(x$year==2015)==1 ){
    livedead2015 <- as.character(x$LiveDead[x$year==2015])
    cut2015 <- as.character(x$Cut[x$year==2015])
  } else {
    livedead2015 <- "NA"
    cut2015 <- "NA"
  }

  status <- "?"
  if (livedead2010=="LIVE" & livedead2015=="LIVE") status <- "Survivor"
  if (livedead2010=="LIVE" & livedead2015=="DEAD" & cut2015=="TRUE") status <- "Cut"
  if (livedead2010=="LIVE" & livedead2015=="DEAD" & cut2015=="FALSE") status <- "Mortality"

  if (livedead2010=="DEAD" & cut2010=="FALSE" & livedead2015=="DEAD" & cut2015=="FALSE") status <- "Dead"
  if (livedead2010=="DEAD" & cut2010=="FALSE" & livedead2015=="DEAD" & cut2015=="TRUE") status <- "DeadCut"
  if (livedead2010=="DEAD" & cut2010=="TRUE" & livedead2015=="DEAD") status <- "PastCut"
  
  if (livedead2010=="NA" & livedead2015=="LIVE" & cut2015=="FALSE") status <- "Ingrowth"
  if (livedead2010=="NA" & livedead2015=="DEAD" & cut2015=="FALSE") status <- "IngrowthMortality"
  if (livedead2010=="NA" & livedead2015=="DEAD" & cut2015=="TRUE") status <- "IngrowthCut"
  data.frame(unique.tree=x$unique.tree[1],treetype=status)
})

tree.status <- data.frame(do.call("rbind",tree.classify))
table(tree.status$treetype)

tree.status[tree.status$treetype=="?",]

#! 11 reattach tree attributes
large.trees3 <- merge(large.trees2[large.trees2$year>2009,],
                      tree.status,by="unique.tree",all.x=T)

large.trees3[is.na(large.trees3$treetype),]

histogram(~ DBH | treetype,
          data=large.trees3[large.trees3$year==2015,])

