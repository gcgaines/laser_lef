#1 read plot locations
#setwd(db.dir)
#load("ripdb.Rdata")
#setwd(code.dir)

locs <- rip.db$RIPgpscoords[,c("RIPID","UTMX_Predicted","UTMY_Predicted")]

#2 get laser tiles for those locations
ogrInfo(tile.indx.dir, tile.indx)
tiles <- readOGR(tile.indx.dir, tile.indx)


#3 store locations as SpatialPoints class for overlay
locs2 <- as.matrix(locs[,2:3])
dimnames(locs2)[[1]] <- locs$RIPID

locs.sp <- SpatialPoints(coords=locs2)
proj4string(locs.sp) <- proj4string(tiles) #same projection as tiles

# 4 spatial overlay
tileids <- over(locs.sp, tiles)
tileids$RIPID <- rownames(tileids)
tileids$LAS_ID <- as.character(tileids$LAS_ID)

locs <- merge(locs,tileids,all.x=T)


#5 clip laser tiles by plot and get canopy height model
for (i in 1:nrow(locs)){
  rip <- locs$RIPID[i]

  # buffer plot location  
  buff <- 40
  square1 <- cbind(x=locs[i,2]+buff*c(-1,1,1,-1,-1),
                   y=locs[i,3]+buff*c(-1,-1,1,1,-1))
  # send to a textfile, separated by a line with just a "#"
  write.table(square1,file.path(products.dir,"clips.txt"),row.names=F,col.names=F)
  
  # grab point cloud near plot (within buffer)
  infile <- file.path(tile.dir,ifelse(rip>1000,"Bandy","Lubrecht"),
                      paste(locs$LAS_ID[i],".las",sep=""))
  outfile <- paste("rip",rip,".las",sep="")
  system(paste(file.path(lastools.dir,"lasclip.exe"),
               "-i",infile,
               "-poly",file.path(products.dir,"clips.txt"),
               "-odir",file.path(products.dir),
               "-olas -o",outfile),invisible=F)
  
  # run ground classify and make canopy height model around the plot
  system(paste(file.path(lastools.dir,"lasground.exe"),
               "-i",file.path(products.dir,outfile),
               "-ultra_fine -olas -cores",cores,  
               "-odir",file.path(products.dir),
               "-compute_height -replace_z",
               "-odix _normht"),invisible=F)
  
  # knock out the plot itself
  infile2 <- paste("rip",rip,"_normht.las",sep="")
  outfile2 <- paste("crip",rip,".las",sep="")
  system(paste(file.path(lastools.dir,"las2las.exe"),
               "-i",file.path(products.dir,infile2),
               "-odir",file.path(products.dir),
               "-o",outfile2,
               "-keep_circle",locs[i,2],locs[i,3],18),invisible=F)
  
  # delete buffer files
  file.remove(file.path(products.dir,outfile))
  file.remove(file.path(products.dir,infile2))

}

system(paste(file.path(lastools.dir,"lasview.exe"),
             "-i",file.path(products.dir,outfile)),invisible=F)

system(paste(file.path(lastools.dir,"lasview.exe"),
             "-i",file.path(products.dir,infile2)),invisible=F)

system(paste(file.path(lastools.dir,"lasview.exe"),
             "-i",file.path(products.dir,outfile2)),invisible=F)
