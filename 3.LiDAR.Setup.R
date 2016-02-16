library(RODBC)
library(lattice)
library(rgdal)
library(sp)

# 1. Set the working directories
# code.dir <- "C:/DLRA/LEFInventory/LEFInvCode"     #where is this script?

# lidar stuff
lastools.dir <- "c:/lastools/bin"      #where is lastools?
cores <- 1  # how many cores does your machine have?

tile.dir <- "//10.8.103.120/data/points"      #where is your tile?

tile.indx.dir <- "//10.8.103.120/data/vectors"      #where is your tile index?
tile.indx <- "Lubrecht_LAS_Index"

# where do you want to the lastools output to go?
products.dir <- "C:/gaines/projects/lubrecht/data/code/laser_lef/laser_lef/laser_clipped"  

#create output directory
dir.create(file.path(products.dir), showWarnings = F)

# access db stuff
# db.dir <- "C:/DLRA/LEFInventory/InvDB"
# db.file <- "CFC_RIPDB_2015dec14.accdb"

