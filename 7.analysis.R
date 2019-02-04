# the metrics data frame is saved in "metrics.Rdata"
load(file="metrics.Rdata")

# make some figures
library(lattice)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
head(metrics)
dim(metrics)

### live ba ~ lidar metrics
pairs(metrics[,c(24,2:19)],lower.panel=panel.smooth, upper.panel=panel.cor)
# Highest-correlated lidar metrics: density and cover
# density 
xyplot(metrics$ba.live ~ metrics$dns,
       type=c("p","smooth"))
xyplot(metrics$ba.live ~ metrics$dns | metrics$dom)
# cover 
xyplot(metrics$ba.live ~ metrics$cov,
       type=c("p","smooth"))
xyplot(metrics$ba.live ~ metrics$cov | metrics$dom)

# Look at height metrics
pairs(metrics[,c(24,18,19)],lower.panel=panel.smooth, upper.panel=panel.cor)
# Highest-correlated height percentiles: p25, p10, p50, avg, p05, ske
pairs(metrics[,c(24,4,7,9:12)],lower.panel=panel.smooth, upper.panel=panel.cor)

### live ba ~ landsat 8 metrics 
pairs(metrics[,c(24,57:71)],lower.panel=panel.smooth, upper.panel=panel.cor)
# Highest-correlated Landsat 8 metrics
# Band 1: 0.435-0.451 μm (Ultra blue)
# Band 2: 0.452-0.512 μm (Blue)
# Band 3: 0.533-0.590 μm (Green)
# Band 4: 0.636-0.673 μm (Red)
# Band 6: 1.566-1.651 μm (shortwave infrared 1)
# Band 7: 2.107-2.294 μm (shortwave infrared 2)
# NDVI ->
pairs(metrics[,c(24,57:60,62,63,70)],lower.panel=panel.smooth, upper.panel=panel.cor)
splom(metrics[,c(24,57:60,62,63,70)])

## Live ba ~ Sentinel 2 Metrics
pairs(metrics[,c(24,72:88)],lower.panel=panel.smooth, upper.panel=panel.cor)

# Highest-correlated Sentinel 2 metrics:
pairs(metrics[,c(24,74,75,79:84,87)],lower.panel=panel.smooth, upper.panel=panel.cor)
# Band 5: 703.9nm (S2A) / 703.8nm (S2B) Red Edge 1
# Band 6: 740.2nm (S2A) / 739.1nm (S2B)	Red Edge 2
# Band 7: 782.5nm (S2A) / 779.7nm (S2B)	Red Edge 3
# Band 8: 835.1nm (S2A) / 833nm (S2B)	NIR
# Band 8a: 864.8nm (S2A) / 864nm (S2B) Red Edge 4
# Band 9: 945nm (S2A) / 943.2nm (S2B)	Water vapor
# Band 11: 1613.7nm (S2A) / 1610.4nm (S2B)	SWIR 1
# Band 12: 2202.4nm (S2A) / 2185.7nm (S2B)	SWIR 2
# MNDWI (don't even remember what this is)

## Live ba ~ elevation
pairs(metrics[,c(24,90:92)],lower.panel=panel.smooth, upper.panel=panel.cor)
xyplot(ELEV_Measured ~ elev.m, data=metrics,type = c("p", "r"))
xyplot(ba.live ~ elev.m, data=metrics,type = c("p", "r"))

# some models
fit.data <- metrics[metrics$ba.live>0,]
fit1 <- lm(ba.live ~ dns + p25 + B3.x + B8.y + ELEV_Measured, data = fit.data)
summary(fit1)

x.new <- predict(fit1)
plot(x.new,fit.data$ba.live)
cor(x.new,fit.data$ba.live)
abline(fit1)

# 1 Principle Component Analysis
?princomp
pc <- princomp(metrics[,1:(ncol(metrics)-1)], cor=T)

summary(pc)
pc$loadings
biplot(pc)

