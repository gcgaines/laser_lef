terr.map <- function(who,typ="terrain"){
  utm.center <- as.numeric(locs[locs$RIPID==who,c("UTMX_Predicted","UTMY_Predicted")])
  who.is.close <- ((locs$UTMX_Predicted - utm.center[1])^2 +
                   (locs$UTMY_Predicted - utm.center[2])^2 < 2000^2) &
                  (locs$RIPID!=who)
  map.center <- locs[locs$RIPID==who,c("LAT_Predicted","LONG_Predicted")]
  offst <- .00915 #.008
  boxit <- qbbox(map.center[,1]+offst*c(-1,-1,1,1),
                 map.center[,2]+offst*c(-1,1,-1,1), TYPE="all")
  basemap <- GetMap.bbox(boxit$lonR,boxit$latR, size=c(640,640),
               destfile="tempbase.png", maptype=typ) #"mapmaker-hybrid",
    tmp <- PlotOnStaticMap(basemap,mar=rep(1,4))
    tmp <- PlotOnStaticMap(basemap,cex=3.6+(who>999)*1.2,pch=21,add=T,
             lat=locs[who.is.close,"LAT_Predicted"],
             lon=locs[who.is.close,"LONG_Predicted"])
    tmp <- PlotOnStaticMap(basemap,cex=.8-(who>999)*.05,col="black",add=T,
             lat=locs[who.is.close,"LAT_Predicted"],
             lon=locs[who.is.close,"LONG_Predicted"],
             FUN=text,labels=locs$RIPID[who.is.close])
    tmp <- PlotOnStaticMap(basemap,cex=5.5+(who>999)*1,pch=21,col="blue",add=T,
             lat=map.center[,1],lon=map.center[,2])
    tmp <- PlotOnStaticMap(basemap,cex=1.1,col="blue",add=T,
             lat=map.center[,1],lon=map.center[,2],
             FUN=text,labels=who)
}

#pdf("trythis.pdf",width=6.6,height=9.9)
#  split.screen(c(3,2))
#  screen(2)
#  terr.map(65)
#
#  temp <- par(mar=c(3,3,.1,.1))
#  screen(3)
#  stand.tabl(who,2010)
#  screen(4)
#  stock.tabl(who,2010)
#  par(temp)
#
#  close.screen(all=TRUE)
#dev.off()


#png("trythis.png",width=300*6.6,height=300*9.9,pointsize=36)
#  split.screen(c(3,2))
#  screen(2)
#  terr.map(65)
#
#  temp <- par(mar=c(3,3,.1,.1))
#  screen(3)
#  stand.tabl(who,2010)
#  screen(4)
#  stock.tabl(who,2010)
#  par(temp)
#
#  close.screen(all=TRUE)
#dev.off()


terr.map.pdf <- function(who,outmap,typ="terrain"){
  utm.center <- as.numeric(locs[locs$RIPID==who,c("UTMX_Predicted","UTMY_Predicted")])
  who.is.close <- ((locs$UTMX_Predicted - utm.center[1])^2 +
                   (locs$UTMY_Predicted - utm.center[2])^2 < 4000^2) &
                  (locs$RIPID!=who)
  map.center <- locs[locs$RIPID==who,c("LAT_Predicted","LONG_Predicted")]
  offst <- .01 #.008
  boxit <- qbbox(map.center[,1]+offst*c(-1,-1,1,1),
                 map.center[,2]+offst*c(-1,1,-1,1), TYPE="all")
  basemap <- GetMap.bbox(boxit$lonR,boxit$latR, size=c(640,640),
               destfile="temp.png", maptype=typ) #"mapmaker-hybrid",
  pdf(outmap) #png(outmap,640,640)
    tmp <- PlotOnStaticMap(basemap)
    tmp <- PlotOnStaticMap(basemap,cex=3.6+(who>999)*1.2,pch=21,bg="grey",add=T,
             lat=locs[who.is.close,"LAT_Predicted"],
             lon=locs[who.is.close,"LONG_Predicted"])
    tmp <- PlotOnStaticMap(basemap,cex=.8-(who>999)*.05,col="black",add=T,
             lat=locs[who.is.close,"LAT_Predicted"],
             lon=locs[who.is.close,"LONG_Predicted"],
             FUN=text,labels=locs$RIPID[who.is.close])
    tmp <- PlotOnStaticMap(basemap,cex=5.5+(who>999)*1,pch=21,bg="white",add=T,
             lat=map.center[,1],lon=map.center[,2])
    tmp <- PlotOnStaticMap(basemap,cex=1.1,col="black",add=T,
             lat=map.center[,1],lon=map.center[,2],
             FUN=text,labels=who)
  dev.off()
}
#terr.map.pdf(65,"map.pdf")

