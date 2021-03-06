# reads in the database tables

library(lattice)
library(RODBC)

# 1. set directory and file name
# accessdb <- "CFC_RIPDB_2015_021916.accdb" where did this file go?
accessdb <- "CFC_RIPDB_2015_010316.accdb"
code.dir <- getwd()
db.dir <- file.path(code.dir,"rip_db")


# 2. read the tables from the Access DB
setwd(db.dir)
path <- odbcConnectAccess2007(accessdb)
accesscontents <- sqlTables(path)
(accesstables <- with(accesscontents, TABLE_NAME[TABLE_TYPE=="TABLE"]))
rip.db <- list()
for (i in accesstables){
  rip.db <- c(rip.db, list(sqlFetch(path, i)))
}
names(rip.db) <- accesstables
close(path)

names(rip.db)

#! 3. error checking species
# levels(rip.db$RIPlargetrees$Species)
# allowable.spp <- c("DF","AF","BC","ES","LP","PP","QA","RJ","WL","MA","UN")
# rip.db$RIPlargetrees[!(rip.db$RIPlargetrees$Species %in% allowable.spp),]

