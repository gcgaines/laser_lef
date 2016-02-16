# the metrics data frame is saved in "metrics.Rdata"
load(file="metrics.Rdata")


# 1 Principle Component Analysis

?princomp
pc <- princomp(metrics[,1:(ncol(metrics)-1)], cor=T)

summary(pc)
pc$loadings
biplot(pc)


xyplot(max ~ cov, data=metrics)
xyplot(ske ~ cov, data=metrics)
xyplot(ske ~ max, data=metrics)

# Quadratic mean diameter ~ max laser return height
xyplot(QMD.live ~ max, data=metrics)
xyplot(QMD.total ~ max, data=metrics)

# Quadratic mean diameter ~ percent canopy cover
xyplot(QMD.live ~ cov, data=metrics)
xyplot(QMD.total ~ cov, data=metrics)

fm1 <- lm(QMD.total ~ cov, data=metrics)
summary(fm)

lrf1 <- loess(QMD.total ~ cov, metrics)
summary(lrf1)

plot(metrics$QMD.total, metrics$cov)
lines(spline(metrics$cov, fitted(lrf1)), col=2)
abline(0,1,lty=3,col=3)
abline(fm1, col=4)


# Basal Area/Acre ~ max laser return height--sucks
xyplot(ba.ac.live ~ max, data=metrics)
xyplot(ba.ac.total ~ max, data=metrics)

# Basal Area/Acre ~ percent canopy cover
xyplot(ba.ac.live ~ cov, data=metrics)
xyplot(ba.ac.total ~ cov, data=metrics)

splom(~metrics[c(36,18,19)])

# Basal Area/Acre ~ percent canopy density
xyplot(ba.ac.live ~ dns, data=metrics)
xyplot(ba.ac.total ~ dns, data=metrics)

# Basal Area/Acre ~ percent canopy cover*max laser return height
xyplot(ba.ac.live ~ cov*max, data=metrics)
xyplot(ba.ac.total ~ cov*max, data=metrics)

# Basal Area/Acre ~ percent canopy density*max laser return height
xyplot(ba.ac.live ~ dns*max, data=metrics)
xyplot(ba.ac.total ~ dns*max, data=metrics)

# Max laser return ~ Max tree height measured on plot
xyplot(MaxHt.total ~ max, data=metrics)

# Trees/Acre ~ percent canopy cover
xyplot(TPA.live ~ cov, data=metrics)
xyplot(TPA.total ~ cov, data=metrics)

splom(~metrics[c(38,18,19)])

# Trees/Acre ~ percent canopy density
xyplot(TPA.live ~ dns, data=metrics)
xyplot(TPA.total ~ dns, data=metrics)

# Crown Length ~ percent canopy cover
xyplot(crown.length.live ~ cov, data=metrics)
xyplot(crown.length.total ~ cov, data=metrics)

# Crown Length ~ percent canopy density
xyplot(crown.length.live ~ dns, data=metrics)
xyplot(crown.length.total ~ dns, data=metrics)

# Crown Length*BA/ac ~ percent canopy cover
xyplot((crown.length.live*ba.ac.live) ~ cov, data=metrics)
xyplot((crown.length.total*ba.ac.total) ~ cov, data=metrics)

# Crown Length*BA/ac ~ percent canopy density
xyplot((crown.length.live*ba.ac.live) ~ dns, data=metrics)
xyplot((crown.length.total*ba.ac.total) ~ dns, data=metrics)





