# stand table = stand.tabl and stock table stock.tabl
#  functions for Lubrecht

stand.tabl <- function(who,yr,
     colored.species=c("PP","LP","DF","WL","AF","ES")){
  temp <- par(mar=c(4.2,3.4,2.1,.5))
  # get data subset
  dbh.dat <- large.trees3[large.trees3$RIPID==who &
                            large.trees3$year==yr &
                            large.trees3$LiveDead=="LIVE",]
  # find 2-inch breaks; add breaks if v. short
  plt.range <- (range(dbh.dat$DBH,na.rm=T)+.01)/2
  plt.range <- seq(2*floor(plt.range[1]),
                    2*ceiling(plt.range[2]),by=2)
  if(length(plt.range)<4){
    plt.range <- c(min(plt.range)-2,plt.range,max(plt.range)+2)}
  # make overall histogram and axes
  hst <- hist(dbh.dat$DBH,breaks=plt.range,col="lightgrey",cex.lab=.8,
    #xlab="DBH class (in)",ylab="Number of live trees >5 in DBH per ac",
    xlab="",ylab="",main="",axes=F)
  title(xlab="DBH class (in)",line=2.5,cex.lab=.9)
  title(ylab="Number of live trees >5 in DBH per ac",line=2.2,cex.lab=.8)
  axis(1,cex.axis=.7)
  axis(2,at=pretty(c(0,hst$counts),n=6),
             labels=4*pretty(c(0,hst$counts),n=6),xpd=T,cex.axis=.7)
  # stack boxes for identified species
  counts <- rep(0,length(hst$counts))
  for (grp in colored.species){
    hst.sub <- hist(dbh.dat$DBH[dbh.dat$Species==grp],
                 breaks=plt.range,plot=F)
    for (j in 1:length(plt.range)){
      polygon(x=plt.range[j]+c(0,2,2,0),
        y=counts[j]+c(0,0,hst.sub$counts[j],hst.sub$counts[j]),
        col=spp.col(grp))
    }
    counts <- counts+hst.sub$counts
  }
  spp.used <- unique(dbh.dat$Species) #,drop=T)
  legend(x=max(plt.range)+3/diff(range(plt.range)),
         y=max(pretty(c(0,hst$counts))),
    ncol=1,legend=spp.used,fill=spp.col(spp.used),
    border="black",xjust=1, bty="n",xpd=T,cex=.9)
  par(temp)
}


stock.tabl <- function(who,yr,
     colored.species=c("PP","LP","DF","WL","AF","ES")){
  temp <- par(mar=c(4.2,3.4,2.1,.5))
  # get data subset
  dbh.dat <- large.trees3[large.trees3$RIPID==who &
                            large.trees3$year==yr &
                            large.trees3$LiveDead=="LIVE",]
  # find 2-inch breaks; add breaks if v. short
  plt.range <- (range(dbh.dat$DBH,na.rm=T)+.01)/2
  plt.range <- seq(2*floor(plt.range[1]),
                    2*ceiling(plt.range[2]),by=2)
  if(length(plt.range)<4){
    plt.range <- c(min(plt.range)-2,plt.range,max(plt.range)+2)}
  # sum volumes by 2-inch class
  dclass <- 2*floor(dbh.dat$DBH/2)
  vols <- aggregate(4*dbh.dat$vol,by=list(DBH=dclass),sum,na.rm=T)
  vols2 <- merge(cbind(DBH=plt.range[-length(plt.range)]),vols,all.x=T)
  vols2$x <- ifelse(is.na(vols2$x),0,vols2$x)
  # make overall histogram and axes
  hst <- hist(dbh.dat$DBH,breaks=plt.range,plot=F)
  hst$counts <- vols2$x
  plot(hst,col="lightgrey",axes=F,cex.lab=.8,
    #xlab="DBH class (in)",ylab="Volume in live trees >5 in DBH (ft?/ac)",
    xlab="",ylab="",main="")
  title(xlab="DBH class (in)",line=2.5,cex.lab=.9)
  #title(ylab="Volume in live trees >5 in DBH (ft?/ac)",line=2.5,cex.lab=.9)
  title(ylab=expression("Volume in live trees >5 in DBH (ft "^{3}*"/ac)"),
        line=2.2,cex.lab=.8)
  axis(1,cex.axis=.7)
  axis(2,at=pretty(c(0,hst$counts),n=6),
             labels=pretty(c(0,hst$counts),n=6),xpd=T,cex.axis=.7)
  # stack boxes for identified species
  counts <- rep(0,length(hst$counts))
  for (grp in colored.species){
    dclass.sub <- dclass[dbh.dat$Species==grp]
    if (length(dclass.sub)>0){
      vols.sub <- aggregate(4*dbh.dat$vol[dbh.dat$Species==grp],
                    by=list(DBH=dclass.sub),sum,na.rm=T)
    } else { vols.sub <- cbind(x=0) }
    vols2.sub <- merge(cbind(DBH=plt.range[-length(plt.range)]),
                  vols.sub,all.x=T)
    vols2.sub$x <- ifelse(is.na(vols2.sub$x),0,vols2.sub$x)
    for (j in 1:length(plt.range)){
      polygon(x=plt.range[j]+c(0,2,2,0),
        y=counts[j]+c(0,0,vols2.sub$x[j],vols2.sub$x[j]),
        col=spp.col(grp))
    }
    counts <- counts+vols2.sub$x
  }
  par(temp)
}
