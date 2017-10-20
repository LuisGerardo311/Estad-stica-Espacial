library (maptools)
getinfo.shape("D:/ESTADISTICA_ESPACIAL/sids2/sids2.shp")
sids<-readShapePoly("D:/ESTADISTICA_ESPACIAL/sids2/sids2.shp")
class(sids)


library(rgdal)
sids<- readOGR(dsn="D:/ESTADISTICA_ESPACIAL/sids2", layer="sids2")
class(sids)
  

proj4string(sids)<-CRS("+proj=longlat +ellps=WGS84")
plot(sids)


library(spdep)
sids_nbq<-poly2nb(sids)
coords<-coordinates(sids)
plot(sids)
plot(sids_nbq, coords, add=T)

