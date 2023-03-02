## ---- fig.show='hold'---------------------------------------------------------
library(goal)
data("goaldata.data2", package = "goal")
plot(goaldata.data2) 

#xy=data.frame(x=c(25.92548,25.92970,25.92927,25.92489,25.92548), y=c(39.13743,39.13591,39.13500,39.13641,39.13743 )) 
library(sp)
#coordinates(xy) <- ~x+y
#plot(xy)

## ---- warning=F, fig.cap = "Your figure caption."-----------------------------
one = uav.GenerateTargets(goaldata.data2)

par(mar=c(0, 0, 0, 0))
plot(one$Polygon)
plot(one$simia, col= as.numeric(as.factor(one$simia$type)), add=TRUE,  
     pch=3*as.numeric(as.factor(one$simia$type)) )
plot(one$lines, add=T)
text(one$simia , labels = one$simia$id, col="brown", pos=2)
#library(rgdal)
#rgdal::writeOGR(one$simia,dsn=paste0("Targets.shp"),layer="Targets",driver="ESRI Shapefile", overwrite_layer = T)


## ---- warning=F, fig.cap =c("one","two","three"), results='asis', fig.height=5----
library(knitr)

par(mar=c(0, 0, 3, 0))
five = uav.showFocal(insimia = one$simia, inpol=one$Polygon, toplot = T, size = 5) 
kable(as.data.frame(five), col.names=c("Number of points","Frequency"))

ten = uav.showFocal(insimia = one$simia, inpol=one$Polygon, toplot = T, size = 10)
kable(as.data.frame(ten), col.names=c("Number of points","Frequency"))

fifteen = uav.showFocal(insimia = one$simia, inpol=one$Polygon, toplot = T, size = 15)
 kable(as.data.frame(fifteen), col.names=c("Number of points","Frequency"))

## ---- warning=F, fig.cap =c("one","two","three"), results='asis', fig.height=5----
result = goal::uav.DoReduction(one, insize=10)

par(mar=c(0, 0, 3, 0))
plot(one$Polygon, main=paste0(length(one$simia)," initial points in grey\n",length(result)," final points in blue"))
#plot(one$simia, col="blue", add=TRUE, pch=20)
plot(one$simia,  add=TRUE, col="grey")
text(one$simia, labels=one$simia$id, pos=4, col="grey")

plot(result, col="blue", pch=20, add=TRUE)
text(result, labels=result$id, pos=2, col="navyblue")

