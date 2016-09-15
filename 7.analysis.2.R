# I am starting over with the analysis script for our Lubrecht RIP/Lidar project
# after having completed an independent study in regression modeling. 8/23/2016.
# I will restrict this modeling exercise to an effort to predict a single
# variable: mean tree height (live and dead). I will fit models of various forms
# which incorporate different combinations and/or transformations of lidar
# metrics (output from LAStools, using the raw point cloud clipped to a column 
# extending above each plot) as regressor variables. I will then assess relative
# model performance.

lef.wd <- "C:/gaines/projects/lubrecht/data/code/laser_lef/laser_lef"
coursera.wd <- "C:/gaines/projects/coursera/07_RegressionModels"

library(lattice)

################################################################################
######## Fit and visualize single-variable OLS linear regression models ########
################################################################################
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
################################################################################
######## Fit and visualize multi-variable OLS linear regression models #########
################################################################################

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

################################################################################
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