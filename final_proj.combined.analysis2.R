#Ecological Statistics
#Final Project
#assignment #5
#Dec 2016
# Sam Pannoni/ George Gaines
##

## George's Notes #####
# I am starting over with the analysis script for our Lubrecht RIP/Lidar project
# after having completed an independent study in regression modeling. 8/23/2016.
# I will restrict this modeling exercise to an effort to predict a single
# variable: mean tree height (live and dead). I will fit models of various forms
# which incorporate different combinations and/or transformations of lidar
# metrics (output from LAStools, using the raw point cloud clipped to a column 
# extending above each plot) as regressor variables. I will then assess relative
# model performance.

### Libraries ######
library(lattice)
library(rpart)
library(partykit)
library(rpart.plot)
library(rattle)	
library(gam)
#library(tree)

#### Load Data #######
load("metrics.rdata")
lef.wd <- "C:/gaines/projects/lubrecht/data/code/laser_lef/laser_lef"
#coursera.wd <- "C:/gaines/projects/coursera/07_RegressionModels"
head(metrics)
names(metrics)

##### Sam's Notes ########
    # crown mass for carbon sequestration
    # fire modeling maybe
    # LiDAR data parsed from the point cloud
    
    # LiDAR data 
    # RIPID = PLOT # 10 achre circle plots
    # min = minimum point height in meters (1.37m = 4.5ft= DBH)
    # max = max point
    # p = percentile heights
    # cov = cover [ # of first returns above 1.37/# all first returns]
    # dns = density [# all returns above 1.37/ # all returns]
    # kur = kurtosis of the height values
    
    # landSAT data
    # each band = segment of color spectrum
    # dont use 21, 22, 23, 24, 25, 26, 27, 28
    # use means, max, min, .ac.
    
    # response variable = mean crown mass (estimate using Brown's eq.)
    
    #  predictors = any LiDAR, plot level metrics, LandSAT[B1 - end of data]
###### get the data columns of interest #######
names(metrics)

metrics$RIPID <-factor(metrics$RIPID)
my.data <- c()
my.data <- cbind(metrics[1:20], metrics[30:39], metrics[49:76])
#remove some NAs

names(my.data)
my.data <- my.data[,-c(1,23,25,28,31)]

# Remove : RIPID, mean.dbh.live, mean.ht.live, mean.crownht.live, crown.ratio.live
names(my.data)
####### Correlations ##########

cor.mydata <- cor(my.data)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
PDF_title <- "pairs_plot_LiDAR.pdf"
pdf(PDF_title)
pairs(metrics, lower.panel = panel.smooth, upper.panel = panel.cor)
dev.off() # this will close the pdf
######## CART method for parameter selection #######
hist(my.data$crown.mass.kg)
# response variable looks normal!! yay
crown.full<-rpart(crown.mass.kg ~., 
                   data=my.data, method="anova", model = TRUE,
                   control=rpart.control(minsplit= 5,cp=0.0001))
?rpart
par(mfrow = c(1,1),  oma = c(1,1,1,1) + 0.1,
    mar = c(4,4,1,1) + 0.1)
plotcp(crown.full) # cp=0.031
printcp(crown.full)
crown.full2 <- as.party(crown.full)
plot(crown.full2)
par(mfrow=c(1,1))
prp(crown.full, varlen = 0, fallen=F, box.palette = "auto")
fancyRpartPlot(crown.full)
####prune the tree
###update cp value accoding to reduciton in error
crown.full.pr <- prune(crown.full, cp=0.031)
crown.full.party <- as.party(crown.full.pr)
plot(crown.full.party) # cx and cy drive the tree

crown.full.pr2 <- prune(crown.full, cp=0.057)
crown.full.party2 <- as.party(crown.full.pr2)
plot(crown.full.party2) # cx and cy drive the tree




######### Visualize the basic relationships using x and y #######
metrics$RIPID
par(mfrow = c(1,1)) 
with( metrics, {
  plot( mean.crown.mass.lbs, mean.ht.total, xlab = "Mean crown mass (lbs)",
        ylab = "Mean height total (ft)" )
  lines( loess.smooth( mean.crown.mass.lbs, mean.ht.total ), col = "grey" ) } )
with( metrics, {
  plot( p25, mean.crown.mass.lbs, xlab = "Mean crown mass (lbs)",
        ylab = "ID" )
lines( loess.smooth( p25, mean.crown.mass.lbs ), col = "grey" ) } )

########## Fit a GAM and step function then a GLM ####
names(my.data)
gam.null<-gam(crown.mass.kg~1,family =gaussian(link ="identity"),data=my.data) #fit a null model
?gam
step.gam.model<-step.gam(gam.null,scope=list(
  "avg"=~1+avg+poly(avg,2)+s(avg),
  "ba.ac.live"= ~1+ba.ac.live+poly(ba.ac.live,2)+s(ba.ac.live),  
  "evi.max" =~1+evi.max+poly(evi.max,2)+s(evi.max), 
  "p25"=~1+p25+ poly(p25,2)+s(p25),
  "cov"= ~1+cov+poly(cov,2)+s(cov),
  # this is where CART info stops
  "mean.crownht.total"=~1+mean.crownht.total+poly(mean.crownht.total,2) +s(mean.crownht.total),
  "max.ht.total"=~1+ max.ht.total+poly(max.ht.total,2)+s(max.ht.total), 
  "mean.ht.total"=~1+mean.ht.total+poly(mean.ht.total,2)+s(mean.ht.total),
  "mean.dbh.total"= ~1+mean.dbh.total+poly(mean.dbh.total,2)+ s(mean.dbh.total)),
  direction="both")

#plot(step.gam.model, resid=T, se= T) #redundant base plot
par(mfrow = c(3,2))
termplot(step.gam.model,partial.resid = T, se=T )
summary(step.gam.model)
#top model
# gam(formula = crown.mass.kg ~ poly(ba.ac.live, 2) + s(evi.max) + 
# p25 + mean.crownht.total + mean.ht.total + s(mean.dbh.total), 
# family = gaussian(link = "identity"), data = my.data, trace = FALSE)

glm1 <- glm(crown.mass.kg ~ poly(ba.ac.live, 2) + s(evi.max) + 
                        p25 + mean.crownht.total + mean.ht.total + 
                        s(mean.dbh.total), data=my.data,
                        family=gaussian(link = "identity"))
par(mfrow = c(3,2))
termplot(glm1,partial.resid = T, se=T )
summary(glm1)
anova(glm1,test="Chisq")
# looks like mean.crownht.total should go
par(mfrow = c(1,4)); plot(glm1)

glm2 <- glm(crown.mass.kg ~ poly(ba.ac.live, 2) + s(evi.max) + 
              p25 + mean.ht.total + 
              s(mean.dbh.total), data=my.data,
            family=gaussian(link = "identity"))

summary(glm2)
anova(glm2,test="Chisq")
par(mfrow = c(1,4)); plot(glm2)

# looks like s(mean.dbh.total) should go
glm3 <- glm(crown.mass.kg ~ poly(ba.ac.live, 2) + s(evi.max) + 
              p25 + mean.ht.total , data=my.data,
            family=gaussian(link = "identity"))
summary(glm3)
anova(glm3,test="Chisq")
par(mfrow = c(1,4)); plot(glm3)


AIC(glm1, glm2, glm3)
# meh pretty similar
# i dont like all the patterns left in the residuals

Pearson.resids <- residuals(glm2,"pearson")
Pearson.chisq <- sum(Pearson.resids^2)
Pearson.chisq/glm2$df.residual
#[1] 246233.4
# how extreme or normal is this Pearson value?
# simulate 1000 iterations from the model

sims <- 1000
sim.outcome <- numeric(length=sims)
for (i in 1:sims){
  sim.data <- simulate(glm2)[,1]
  sim.fit <- glm(sim.data ~ poly(ba.ac.live, 2) + s(evi.max) + 
                   p25 + mean.crownht.total + mean.ht.total + 
                   s(mean.dbh.total), data=my.data,
                 family=gaussian(link = "identity"))

  sim.ssresids <- sum(residuals(sim.fit,"pearson")^2)
  sim.outcome[i] <- sim.ssresids/glm2$df.residual
}
par(mfrow = c(1,1)); hist(sim.outcome,xlim=range(c(sim.outcome,3)))
abline(v=246233,col="red")
#typical pattern

########### find out why there are patterns in the residuals #######
my.data2 <- my.data[-107,]
# Based on AIC this model is best so far so lets try getting rid of row 107
noOutlier.glm2 <- glm(crown.mass.kg ~ poly(ba.ac.live, 2) + s(evi.max) + 
              p25 + mean.ht.total + 
              s(mean.dbh.total), data=my.data2,
            family=gaussian(link = "identity"))

summary(noOutlier.glm2)
anova(noOutlier.glm2,test="Chisq")
par(mfrow = c(1,4)); plot(noOutlier.glm2)
# doesnt remove the pattern in the residuals but much more behaved
# test for interactions
interact.glm1 <- glm(crown.mass.kg ~ poly(ba.ac.live, 2) + s(evi.max) + 
                       p25 + mean.ht.total + poly(ba.ac.live, 2):mean.ht.total,
                       s(mean.dbh.total), data=my.data2,
                     family=gaussian(link = "identity"))

summary(interact.glm1)
anova(interact.glm1,test="Chisq")
par(mfrow = c(1,4)); plot(interact.glm1)


# maybe we should try a different family of distributions and use a link fuction...?







############ PCA #######

dim(my.data)
dim(metrics)
?princomp
pc <- princomp(my.data, cor=T)
#summary(pc)
#pc$loadings
par(mfrow = c(1,1)) 
plot(pc)
biplot(pc)
?biplot
biplot(pc, )
  # code required
plot(pcEdat$x[,1], pcEdat$x[,2],
     pch = 20, 
     cex = 1.6,
     col = myPCA.EColors,
     mtext("(C)",
           line = -2),
     xlab = "First Principal Component",
     ylab = "Second Principal Component")

############# final model plots #######

### Standardized residuals plotted against all explanatory variables, separately
# looking specifically at data outliers
#glm(formula = crown.mass.kg ~ poly(ba.ac.live,2)+s(evi.max)+p25+mean.ht.total+s(mean.dbh.total), 
    #family = gaussian(link = "identity")

rstand.glm2 <- rstandard(glm2) #calculates standardized residuals
inflm.glm2 <- influence.measures(glm2)
infl.vec <- which(apply(inflm.glm2$is.inf, 1, any))

#X11(type = "cairo", w= 8, h= 8)
par(mfrow = c(2,3), oma = c(2,5,0.3,1) + 0.1,
    mar = c(4,0,1,0) + 0.1)
names(my.data)
data.names <- c("ba.ac.live (?)", "evi.max (?)", "p25", "mean.ht.total (?)", "mean.dbh.total (?)")
plot.data <- my.data[,c(26,31,10,22,21)]  #creates new data frame without W
for(i in 1:5){
  my.sub <- plot.data[,i]
  if (i == 1|5){
    plot(my.sub, rstand, xlab = data.names[i], ylab = "Standardized Residuals")
    abline(h = 0)
    lines(loess.smooth(my.sub, rstand), col = "blue") # adds smoothing curve
    points(my.sub[infl.vec], rstand[infl.vec], pch = 20, col = 4)
    points(my.sub[107], rstand[107], pch = 20, col = 2)
  }
  else{
    plot(my.sub, rstand, xlab = data.names[i], ylab = NULL, yaxt= "n" )
    abline(h = 0)
    lines(loess.smooth(my.sub, rstand), col = "blue") # adds smoothing curve
  }
}
plot(fitted(glm2), rstand, ylab = NULL, yaxt = "n", 
     xlab = "Model 2 Fitted Values")
abline(h = 0)
lines(loess.smooth(fitted(glm2), rstand), col = "blue")
points(fitted(glm2)[107], rstand[107], pch = 20, col = 2)
title(ylab = "Standardized Residuals",
      outer = TRUE, line = 3)
#savePlot(filename = "Standard_resid_and_mod2vsfitted.jpeg", "jpeg", device = dev.cur())
### end residuals plot


# back to which points are influential in the predictions
inflm.glm2 <- influence.measures(glm2)
infl.vec <- which(apply(inflm.glm2$is.inf, 1, any))
# which observations 'are' influential
summary(inflm.glm2) # only these
inflm.glm2          # all
## The 'infl' argument is not needed, but avoids recomputation:
rs <- rstandard(glm2)
iflglm2 <- influence(glm2)
identical(rs, rstandard(glm2, infl = iflglm2))
#true

par(mfrow = c(2,2))
## plot influential points with residuals
xh1 <- c(hatvalues(glm2))
yh1 <- c(rstudent(glm2))
# rstudent is a way to standardize residuals (like cooks but different)
summary(lmH1 <- lm(yh1 ~ xh1))
(im1 <- influence.measures(lmH1))
plot(xh1,yh1, main = "Least sq. line and influential obs.") 
abline(lmH1); 
points(xh1[infl.vec], yh1[infl.vec], pch = 20, col = 4) # blue points are all influentials in the model glm2
points(xh1[im1$is.inf], yh1[im1$is.inf], pch = 20, col = 2) # all influential in student standardized residuals lmH1


# plot residuals with influential 107 gone from the model (model is above) * only removing the outlier and not re-optimizing the model may cause lower p-value
# which points are influential in the predictions
inflm.no.glm2 <- influence.measures(noOutlier.glm2)
infl.vec.no <- which(apply(inflm.no.glm2$is.inf, 1, any))
xh <- c(hatvalues(noOutlier.glm2))
yh <- c(rstudent(noOutlier.glm2)) # standardize residuals
summary(lmH <- lm(yh ~ xh))
(im <- influence.measures(lmH))
infl.vec.no2 <- which(apply(im$is.inf, 1, any))
plot(xh,yh, main = "Least sq. and influential obs.without 107")
abline(lmH); 
points(xh[infl.vec.no], yh[infl.vec.no], pch = 20, col = 4) # blue points are all influentials in noOtlier model
points(xh[infl.vec.no2], yh[infl.vec.no2], pch = 20, col = 2) # red are influential after standardizing


#plot basic observed vs predicted with and without point 107
xpred.w <- predict(glm2)
yactual.w <- my.data$crown.mass.kg
fit.lm.1 <- lm((yactual.w~xpred.w))
# this part didnt work so great so I hard coded value 107 from above
im.fit.lm.1 <- influence.measures(fit.lm.1)
infl.vec.check <- which(apply(im.fit.lm.1$is.inf, 1, any))
plot(xpred.w, yactual.w, main = "obs vs predicted with 107")
abline(fit.lm.1);
#points(xpred.w[infl.vec.check], yactual.w[infl.vec.check], pch = 20, col = 2)
# decided not to plot all influential points because they dont really matter since they are not outliers
points(xpred.w[107], yactual.w[107], pch = 20, col = 2)


#now without point 107
xpred <- predict(noOutlier.glm2)
yactual <- my.data$crown.mass.kg[1:106]
fit.lm <- lm((yactual~xpred))
# I hard coded value 107 but it should be gone
im.fit.lm <- influence.measures(fit.lm)
infl.vec.check2 <- which(apply(im.fit.lm$is.inf, 1, any))
plot(xpred, yactual, main = "obs vs predicted without 107")
abline(fit.lm);
points(xpred[107], yactual[107], pch = 20, col = 2)

# end of this plot

summary(fit.lm) # summary of model with outlier removed
# could rebuild with new terms now that 107 is gone...


########################## Old code from George's example below ########
    ################# some kind of model plotting #######
## mean plot tree height ~ mean plot laser return height (convert from m to ft)
y <- metrics$mean.ht.total
x <- metrics$avg*3.28084
  
mdl1 <- lm(y ~ x)
yhat <- predict(mdl1)
e <- resid(mdl1)

# center data, omit intercept, because I know how, but not sure why.
yc <- y-mean(y)
xc <- x-mean(x)
mdl1.no.int <- lm(yc ~ xc -1)

# normalize data, because I know how, but not sure why.
yn <- yc/sd(y)
xn <- xc/sd(x)
mdl1.norm <- lm(yn ~ xn)

# plot mean tree height against mean laser return height with 1:1 and ols lines.
xyplot(y ~ x,
       ylab="Mean tree ht (ft)",
       xlab="Mean laser return ht (ft)",
       data=metrics,
       abline=c(0,1),
       type=c("p","r"),
       main="Mean tree height (ft) ~ mean laser return height (ft)")

#plot centered and normalized data
xyplot(yc ~ xc, data=metrics, type=c("p","r"))
xyplot(yn ~ xn, data=metrics, type=c("p","r"))

cor(y,x)

# Visualize residuals with base R graphics; they're the vertical red lines:
n <- length(metrics[,76])
plot(x, y,  
     ylab = "Mean Tree Height (ft)", 
     xlab = "Mean laser return (ft)", 
     bg = "lightblue", 
     col = "darkgoldenrod1", cex = 1, pch = 6,frame = FALSE)
abline(mdl1, lwd = 2) # object 'mdl1' specifies the regression line as abline.
for (i in 1 : n) # 'for-loop' over data values to plot red lines
  lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "cyan3" , lwd = 1)

# Plot residuals (y axis) v. avg (x axis); regression line is horizontal
plot(x, e,  
     xlab = "Mean laser return height(ft)", 
     ylab = "Residuals (ft)", 
     bg = "lightblue", 
     col = "darkgreen", cex = 1, pch = 12,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
  lines(c(x[i], x[i]), c(e[i], 0), col = "darkolivegreen3" , lwd = 1)
# Use diagnostics to assess the influence of that one biggest outlier at 
# approximately (34,91)
?influence.measures
metrics[metrics$mean.ht.total>90 & (metrics$avg*3.28084)<40,]

# according to this is it is RIPID 22; display dfbeta and hatvalues for RIPID's
# 1:30, to compare those around RIPID 22

# change in predicted response/individual coefficients when the ith data point
# removed and the model is reft:
round(dfbetas(mdl1)[1 : 30,], 3)

# Measure of leverage/influence 
round(hatvalues(mdl1)[1 : 30], 3)

# Rest of influence measures (what are *'s under "Inf" column?):
influence.measures(mdl1)

summary(mdl1) # detailed model summary table
summary(mdl1)$coef # abbreviated model summary table
summary(mdl1)$sigma # residual variance
sqrt(sum(resid(mdl1)^2) / (n - 2)) # manual residual variance calculation

coef(mdl1)
coef(mdl1.no.int)
coef(mdl1.norm)

## Confidence intervals
# Assign summary of coefficients to an object
sumCoef <- summary(mdl1)$coef

# Construct confidence intervals for slope and intecept coefficients;
# note: c(-1,1) performs plus/minus, will spit out two numbers; 
sumCoef[1,1] + c(-1, 1) * qt(.975, df = mdl1$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1, 1) * qt(.975, df = mdl1$df) * sumCoef[2, 2]

# Same thing using confint 
?confint
confint(mdl1, level = 0.95)

### Now fit and visualize a few more single variable models

x2 <- metrics$max*3.28084 # max laser return height, ft
mdl2 <- lm(y ~ x2)

x3 <- metrics$p90*3.28084 # 90th percentile laser return height, ft
mdl3 <- lm(y ~ x3)

x4 <- metrics$qav*3.28084 # quadratic average laser return height, ft
mdl4 <- lm(y ~ x4)

x5 <- metrics$int_p75 # 75th percentile laser return intensity
mdl5 <- lm(y ~ x5)

x6 <- metrics$kur # kurtosis, or "peakness", of the laser return frequency dist
mdl6 <- lm(y ~ x6)

x7 <- metrics$std*3.28084 # standard deviation of laser return heights, ft
mdl7 <- lm(y ~ x7)

par(mfrow=c(2,4), mar=c(5,4,3,1))
plot(x,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "Mean laser return (ft)",
     abline(mdl1, lwd = 1,col = "brown1"))
plot(x2,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "Max laser return (ft)",
     abline(mdl2, lwd = 1,col = "brown1"))
plot(x3,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "p90 laser return (ft)",
     abline(mdl3, lwd = 1,col = "brown1"))
plot(x4,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "Quad Mean laser return (ft)",
     abline(mdl4, lwd = 1,col = "brown1"))
plot(x5,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "p75 Laser Intensity",
     abline(mdl5, lwd = 1,col = "brown1"))
plot(x6,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "Laser Return Kurtosis",
     abline(mdl6, lwd = 1,col = "brown1"))
plot(x7,y,
     ylab = "Mean Tree Height (ft)", 
     xlab = "St Dev laser return (ft)",
     abline(mdl7, lwd = 1,col = "brown1"))
    ################# lm objects ######
# Fit and visualize multi-variable OLS linear regression models
mult.mdl1 <- lm(y~x+x2)    # plot height ~ mean and max laser return height

mult.mdl2 <- lm(y~x+x2+x3) # plot height ~ mean, max, and 90th percentile height

mult.mdl3 <- lm(y~x+x2+x3+x4) # plot height ~ mean, max, 90th percentile and 
                              # quadratic average height

mult.mdl4 <- lm(y~x+x2+x3+x4+x5) # plot height ~ mean, max, 90th percentile and 
                                 # quadratic average height, 75th percentile 
                                 # laser return intensity

mult.mdl5 <- lm(y~x+x2+x3+x4+x5+x6) # plot height ~ mean, max, 90th percentile 
                                    # and quadratic average height, 75th 
                                    # percentile laser return intensity,
                                    # laser return kurtosis

mult.mdl6 <- lm(y~x+x2+x3+x4+x5+x6+x7) # plot height ~ mean, max, 90th 
                                       # percentile and quadratic average 
                                       # height, 75th percentile laser return 
                                       # intensity, kurtosis and std dev

    ################# old notes #######
# Analysis of variance

# "Omit important variables --> bias --> underfit"
# "Include unneccessary variables --> variance inflation --> overfit"
# In ANOVA table, F = 
# ((deviance(lm with fewer x's))-(deviance(lm with more x's)/difference
# in number of regressors))/deviance(lm with more x's)/residual df's 

# P-value: Pr(we'd draw a value of "F" or larger from an "F distribution" with
# parameters = (difference  in # regressors, residual df's of model with more 
# regressors).

# Our null hypothesis was "added regressors are not significant". So,if p-value 
# is very small, a false rejection is very unlikely, and...???

# Caveat: assumes residuals are approximately normally distributed.

anova(mult.mdl1, mult.mdl2, mult.mdl3, mult.mdl4, mult.mdl5, mult.mdl6)
