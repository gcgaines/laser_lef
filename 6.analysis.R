# 1 Principle Component Analysis

?princomp
pc <- princomp(lascan[,1:(ncol(lascan)-1)], cor=T)

summary(pc)
pc$loadings
biplot(pc)


xyplot(max ~ cov, data=lascan)
xyplot(ske ~ cov, data=lascan)
xyplot(ske ~ max, data=lascan)