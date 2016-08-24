# I am starting over with the analysis script for our Lubrecht RIP/Lidar project
# after having completed an independent study in regression modeling. 8/23/2016.
# I will restrict this modeling exercise to an effort to predict a single
# variable: mean tree height (live and dead). I will fit models of various forms
# which incorporate different combinations and/or transformations of lidar
# metrics (output from LAStools, using the raw point cloud clipped to a column 
# extending above each plot) as regressor variables. I will then assess relative
# model performance.

library(lattice)

################################################################################
#########Fit and visualize single-variable OLS linear regression models#########
################################################################################
## mean plot tree height ~ mean plot laser return height 
y <- metrics$mean.ht.total
x <- metrics$avg*3.28084
  
mdl1 <- lm(y ~ x)
yhat <- predict(mdl1)
e <- resid(mdl1)

# center data, omit intercept, but why?
yc <- y-mean(y)
xc <- x-mean(x)
mdl1.no.int <- lm(yc ~ xc -1)

# normalize data, but why?
yn <- yc/sd(y)
xn <- xc/sd(x)
mdl1.norm <- lm(yn ~ xn)

xyplot(y ~ x,
       ylab="Mean tree ht (ft)",
       xlab="Mean laser return ht (ft)",
       data=metrics,
       abline=c(0,1),
       type=c("p","r"),
       main="Mean tree height (ft) ~ mean laser return height (ft)")

xyplot(yc ~ xc, data=metrics, type=c("p","r"))

cor(y,x)

# Visualize residuals with base R graphics; they're the vertical red lines:

n <- length(metrics[,76])
plot(x, y,  
     ylab = "Mean Tree Height (ft)", 
     xlab = "Mean laser return (m)", 
     bg = "lightblue", 
     col = "darkgoldenrod1", cex = 1, pch = 6,frame = FALSE)
abline(mdl1, lwd = 2) # object 'mdl1' specifies the regression line as abline.
for (i in 1 : n) # 'for-loop' over data values to plot red lines
  lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "cyan3" , lwd = 2)

# Plot residuals (y axis) v. avg (x axis)

plot(x, e,  
     xlab = "Mean laser return height(ft)", 
     ylab = "Residuals (ft)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
  lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)

summary(mdl1)
coef(mdl1)
coef(mdl1.no.int)
coef(mdl1.norm)
