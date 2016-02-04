# 1 Principle Component Analysis

?princomp
pc <- princomp(lascan[,1:(ncol(lascan)-1)], cor=T)

summary(pc)
pc$loadings
biplot(pc)


xyplot(max ~ cov, data=metrics)
xyplot(ske ~ cov, data=metrics)
xyplot(ske ~ max, data=metrics)

xyplot(QMD.live ~ max, data=metrics)
xyplot(QMD.total ~ max, data=metrics)

xyplot(QMD.live ~ cov, data=metrics)
xyplot(QMD.total ~ cov, data=metrics)

xyplot(ba.ac.live ~ max, data=metrics)
xyplot(ba.ac.total ~ max, data=metrics)

xyplot(ba.ac.live ~ cov, data=metrics)
xyplot(ba.ac.total ~ cov, data=metrics)

xyplot(ba.ac.total ~ cov/max, data=metrics)
xyplot(ba.ac.total ~ cov*max, data=metrics)
