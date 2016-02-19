# volumefunction.R
#
# Obtains cubic foot volume and inside/outside bark diameters at
# any height using Flewelling's multi-point taper equations.
# Needs the ***.exe and its coefficient files.
#
# 1. set working dir to where the executable lives
#    (with coefficient files and fortran dlls)
# 2. open a loop to run through the trees or height calls
# 3. output the record data into "ingyvolin.txt" in this format:
#      line 1:   id,spp,region,subr,dbh,totalht,stumpht,topht,n
#      line 2:   extraht(1),   extradob(1)
#      line n+1: extraht(n+1), extradob(n+1)
#    where
#      id - integer
#      spp - with quotes: "DF", WL GF PP LP WC WH MH WP ES AF BS
#      region - 1 = west-side; 2 = east-side
#      subr - subregion integer
#              (WA west-side) NO RS SO WE WC = 3 4 5 6 8
#              (OR west-side) CO EV WV       = 1 2 7
#              (US east-side) E-Casc, Okan, BluMtn,Koot = 11-14
#              (US east-side) Central ID, SW Montana    =15-16
#              (CAN east-side) FIZ D E F    =         21 22 23
#              OR for no sub-region: 0
#      dbh - as double, outside-bark (inches)
#      totalht - as double (feet)
#      stumpht - as double (feet)
#      topht - as double (feet)
#      n - integer num of extra upper-stem diameters and heights
#      extraht(x) - double upper-stem diameter height (feet)
#      extradob(x) - double upper-stem diameter ob (inches)
# 4. run the system command to get volumes and dib/dob
# 5. read the data from "ingyvolout.txt" in this format:
#
# 6. restore working directory.

# 1. working directory
cur.dir <- getwd()
setwd()

# 2. loop

  # 3. write the input
  x <- data.frame(spp="DF",2,16,
                  11.00,50.00,
                  00.00,50.00,2)
  write.table(cbind(1,x),
    file = "data.txt",sep=",",
    col.names=F,row.names=F)
  write.table(cbind(40.93,4),
    file = "data.txt",sep=",",
    col.names=F,row.names=F,
    append=T)

  # 4. system command
  system("LastFlewellingFortranAttempt")
  
  # 5. read the output
  ret <- read.table("ingyvolout.txt")
  names(ret) <- c("id","dib","dob","cv","cvb")

# 6. restore wd
setwd(cur.dir)
  

