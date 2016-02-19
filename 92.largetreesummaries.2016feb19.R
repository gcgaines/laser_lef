#save(large.trees3,file="lt3.Rdata")
#load("lt3.Rdata")

#! 1. load spatial library and data
library(RgoogleMaps)
library(rgdal)
library(jpeg)

#! colors for species charts
spp.col <- function(spp){
  colr <- rep("grey",length(spp))
  colr[spp=="WL"] <- "darkgreen"
  colr[spp=="DF"] <- "slateblue"
  colr[spp=="PP"] <- "gold" #darkorange"
  colr[spp=="LP"] <- "darkorange"
  colr[spp=="AF"] <- "firebrick"
  colr[spp=="ES"] <- "khaki"
  #colr[spp %in% c("BC","QA","MA")] <- "grey"
  colr
}



locs <- rip.db$RIPgpscoords

#! 2. run mapping and photo scripts
source("subscripts\\standandstocktablefunction.R")
source("subscripts\\minimaps.2011jul12.R")

#! 3. plot info function
plot.info <- function(who,mx.yr){
  temp <- par(mar=rep(.5,4))
  plot(0:10,0:10,type="n",axes=F,xlab="",ylab="")
  text(0,9.5,paste("Plot",who),cex=1.4,pos=4)
  text(6,9.5,mx.yr,cex=1.4,pos=4)
  
  text(0,7.5,ifelse(who>1000,"Bandy Ranch","Lubrecht Forest"),cex=1,pos=4)
  loc <- locs[locs$RIPID==who,]
  if (who==86){
    loc <- with(rip.db$RIPmapcoords[rip.db$RIPmapcoords$RIPID==who,],
            data.frame(LAT_Predicted=DD_LAT,LONG_Predicted=DD_LONG,
                       UTMX_Predicted=UTM_X,UTMY_Predicted=UTM_Y,
                       ELEV_Measured=NA))
  }
  with(loc,{
    text(0,6.5,paste("Lat",round(LAT_Predicted,4),#"N  ",
                   "Long",round(LONG_Predicted,4)),#,"W  "),
                   cex=1,pos=4)
    text(0,5.5,paste("UTM (",round(UTMX_Predicted)," m, ",
                             round(UTMY_Predicted)," m) ",
                   " 12N",sep=""),cex=1,pos=4)
    text(0,4.5,paste("Elev",round(ELEV_Measured*3.280),"ft"),pos=4)
  })
  with(rip.db$RIPmapplss[rip.db$RIPmapplss$RIPID==who,],
    text(0,3.5,paste("T",Township," R",Range," S",Section),pos=4))
    
  with(rip.db$RIPplotinfo[rip.db$RIPplotinfo$RIPID==who,],{
    text(0,1.5,paste("Habitat type ",Habtype1,
             ifelse(is.na(Habtype2) | Habtype2=="","",paste("/",Habtype2,sep="")),
             ifelse(is.na(Habtype3) | Habtype3=="","",paste("/",Habtype3,sep="")),sep=""),pos=4)
    #text(0,.5,paste("Aspect ",Aspect," deg  Slope ",Slope,"%",sep=""),pos=4)
    text(0,.5,labels=bquote("Aspect "*.(Aspect)*degree*" Slope "*.(Slope)*"%"),pos=4)
  })
  par(temp)
}

#plot.info(10,2015)

#! 4. growth function
growth.info <- function(who){
  tree.dat <- large.trees3[large.trees3$RIPID==who,]
  
  temp <- par(mar=rep(.5,4))
  plot(0:10,0:10,type="n",axes=F,xlab="",ylab="")
  if (nrow(tree.dat)>0){
  last.trees <- tree.dat[tree.dat$Measurement==max(tree.dat$Measurement),]
  last.yr <- unique(last.trees$year)
  prev.trees <- tree.dat[tree.dat$Measurement==(max(tree.dat$Measurement)-1),]
  prev.yr <- unique(prev.trees$year)
  
  last.ba <- (pi*sum(last.trees$DBH[last.trees$LiveDead=="LIVE"]^2)/4/144)*4
  prev.ba <- (pi*sum(prev.trees$DBH[prev.trees$LiveDead=="LIVE"]^2)/4/144)*4
  surv <- 4*pi*(sum(last.trees$DBH[last.trees$treetype=="Survivor"]^2) -
                sum(prev.trees$DBH[prev.trees$treetype=="Survivor"]^2))/4/144
  ingr <- ifelse(sum(tree.dat$treetype=="Ingrowth")==0,0,
           4*pi*(sum(last.trees$DBH[last.trees$treetype=="Ingrowth"]^2) -
                 sum(prev.trees$DBH[prev.trees$treetype=="Ingrowth"]^2))/4/144)
  mort <- ifelse(sum(tree.dat$treetype=="Mortality")==0,0,
           4*pi*(#sum(last.trees$DBH[last.trees$TreeCat=="Mortality"]^2) -
                 sum(prev.trees$DBH[prev.trees$treetype=="Mortality"]^2))/4/144)
  harv <- ifelse(sum(tree.dat$treetype=="Cut")==0,0,
           4*pi*(#sum(last.trees$DBH[last.trees$TreeCat=="Cut-Mortality"]^2) -
                 sum(prev.trees$DBH[prev.trees$treetype=="Cut"]^2))/4/144)
                
  text(0,9.5,"For trees >5 in DBH:",pos=4)
  text(0,8,paste(" ",last.yr,"basal area"),pos=4)
    text(8.5,8,labels=bquote(.(format(round(last.ba,1),nsmall=1))*" ft"^2*"/ac"),pos=2)   
  text(0,7,paste(" ",prev.yr,"basal area"),pos=4)
    text(8.5,7,labels=bquote(.(format(round(prev.ba,1),nsmall=1))*" ft"^2*"/ac"),pos=2)     
  text(0,6,paste("  ","Survivor growth"),pos=4)
    text(8.5,6,labels=bquote(.(format(round(surv,1),nsmall=1))*" ft"^2*"/ac"),pos=2)       
  text(0,5,paste("  ","Ingrowth"),pos=4)
    text(8.5,5,labels=bquote(.(format(round(ingr,1),nsmall=1))*" ft"^2*"/ac"),pos=2)     
  text(0,4,paste("  ","Mortality"),pos=4)
    text(8.5,4,labels=bquote(.(format(round(mort,1),nsmall=1))*" ft"^2*"/ac"),pos=2)     
  text(0,3,paste("  ","Harvest"),pos=4)
    text(8.5,3,labels=bquote(.(format(round(harv,1),nsmall=1))*" ft"^2*"/ac"),pos=2)     
  } else { text(0,9.5,"No trees >5 in DBH.",pos=4) }
  par(temp)
}


#! 5. photograph function
photo <- function(who){
  file.dir <- "L:\\ResourceInventoryPlots\\2010Materials\\PlotPhotos\\"
  file.nm <- paste("pl",ifelse(who<100,"0",""),ifelse(who<10,"0",""),
                     who,"tran0dist30.jpg",sep="")
  x <- try(readJPEG(paste(file.dir,file.nm,sep="")),TRUE)
  temp <- par(mar=rep(.2,4))
  if (class(x)[1]!="try-error"){
    imgsize <- dim(x)[2:1]
    plot(1:max(imgsize),1:max(imgsize),type="n",asp=1,
         axes=F,xlab="", ylab="",main="")
    if (imgsize[1]<=imgsize[2]){
      diff <- round((imgsize[2]-imgsize[1])/2)
      rasterImage(x,diff,1,imgsize[1]+diff,imgsize[2])      
      text(max(imgsize)/2,imgsize[2]*.05,"2010",col="red",cex=.9,font=2)
    } else {
      diff <- round((imgsize[1]-imgsize[2])/2)
      rasterImage(x,1,diff,imgsize[1],imgsize[2]+diff)            
      text(max(imgsize)/2,diff*1.15,"2010",col="red",cex=.9,font=2)
    }
  } else {
    plot(0:10,0:10,type="n",axes=F,xlab="",ylab="",main="")
    text(5,5,"<No 2010 photograph available>",cex=.8)
  }
  par(temp)
}


#! 6. output 1 plot
who <- 8
yr <- 2010

png("trythis.png",width=300*7,height=300*10.5,pointsize=12,res=300)
  split.screen(c(3,2))
  
  #top left
  screen(1); plot.info(who,yr)

  #top right
  screen(2); terr.map(who)

  #middle
  livetr <- nrow(large.trees3[large.trees3$RIPID==who &
                              large.trees3$year==yr &
                              large.trees3$LiveDead=="LIVE",])>0
  if (livetr){
    screen(3); stand.tabl(who,2015)
    screen(4); stock.tabl(who,2015)
  }

  #bottom left
  screen(5); growth.info(who)

  #bottom right
  screen(6); photo(who)

  close.screen(all=TRUE)
dev.off()

#! output all plots
#rips <- unique(rip.db$RIPplotinfo$RIPID)
rips <- unique(large.trees3$RIPID)
rips <- rips[rips>1000] #bandy
 
for (i in rips){
who <- i
yr <- 2015
summfile <- paste("plotsummaries\\rip",who,".",yr,".png",sep="")

png(file=summfile,width=300*7,height=300*10.5,pointsize=12,res=300)
  split.screen(c(3,2))

  #top left
  screen(1); plot.info(who,yr)

  #top right
  screen(2); terr.map(who)

  #middle
  livetr <- nrow(large.trees2[large.trees2$RIPID==who &
                              large.trees2$year==yr &
                              large.trees2$LiveDead=="LIVE",])>0
  if (livetr){
    screen(3); stand.tabl(who,2010)
    screen(4); stock.tabl(who,2010)
  }

  #bottom left
  screen(5); growth.info(who)

  #bottom right
  screen(6); photo(who)

  close.screen(all=TRUE)
dev.off()
}




#! error checking
chk <- large.trees2[large.trees2$RIPID==1013,]
chk <- large.trees2[large.trees2$RIPID==56,]
chk <- large.trees2[large.trees2$RIPID==72 & large.trees2$TreeCat=="Survivor",]
chk <- large.trees2[large.trees2$RIPID==52 & large.trees2$TreeCat=="Survivor",]
chk <- large.trees2[large.trees2$RIPID==44 & large.trees2$TreeCat=="Survivor",]
