library(RODBC)
library(lattice)
library(rgdal)
library(sp)

# 1. Set the working directories
# code.dir <- "C:/DLRA/LEFInventory/LEFInvCode"     # where is this script?

# lidar stuff
lastools.dir <- "c:/lastools/bin"      # where is lastools?
cores <- 1  # how many cores does your machine have?

# Old
# tile.dir <- "//10.8.103.120/data/points"  # where is your tile?
# New NAS lidar drive? George 01/25/19
tile.dir <- "//10.8.200.6/Data/Lubrecht_2015/Lubrecht_Raw_2015"  # where is your tile?

# Old
# tile.indx.dir <- "//10.8.103.120/data/vectors"  # where is your tile index?
# New NAS lidar drive? George 01/25/19
tile.indx.dir <- "//10.8.200.6/Data/Lubrecht_2015/Lubrecht_2015/Vectors" # where is your tile index?
tile.indx <- "/Lubrecht_LAS_Index"

# where do you want to the lastools output to go?
products.dir <- file.path(code.dir,"laser_clipped")  

#create output directory
dir.create(file.path(products.dir), showWarnings = F)

# access db stuff
# db.dir <- "C:/DLRA/LEFInventory/InvDB"
# db.file <- "CFC_RIPDB_2015dec14.accdb"

