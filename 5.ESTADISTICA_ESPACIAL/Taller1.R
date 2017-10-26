library(maptools)
library(rgdal)
library(spdep)

getinfo.shape("D:/5.ESTADISTICA_ESPACIAL/sids2/sids2.shp")
sids<-readShapePoly("D:/5.ESTADISTICA_ESPACIAL/sids2/sids2.shp")
class(sids)
plot (sids)

#importar un shape 
sids<-readOGR(dsn="D:/EstadisticaEspacial/sids2", layer= "sids2")
class(sids)

#PROYECTAR UN SHAPEFILE
proj4string(sids)<-CRS("+proj=longlat +ellps=WGS84")
 
sids_NAD<-spTransform(sids, CRS("+init=epsg:3358"))
plot(sids_NAD)
sids_SP<-spTransform(sids, CRS("+init=ESRI:102719"))
plot(sids_SP)
#VECINO BASADOS EN CONTIGUIDAD
#CASO QUEEN
sids_nbq<-poly2nb(sids)
coords<-coordinates(sids)
plot(sids)
plot(sids_nbq, coords, add=T)
#CASO ROOK
sids_nbr<-poly2nb(sids, queen = FALSE)
coords<-coordinates(sids)
plot(sids)
plot(sids_nbr, coords, add=T)

#VECINOS BASADOS EN K=VECINOS MAS CERCANOS
coords<-coordinates(sids_SP)
IDs<-row.names(as(sids_SP,"data.frame"))
sids_kn1<-knn2nb(knearneigh(coords, k=1), row.names=IDs)
plot(sids_SP)
plot(sids_kn1, coords, add=T)

sids_kn2<-knn2nb(knearneigh(coords, k=2), row.names=IDs)
plot(sids_SP)
plot(sids_kn2, coords, add=T)

sids_kn4<-knn2nb(knearneigh(coords, k=4), row.names=IDs)
plot(sids_SP)
plot(sids_kn4, coords, add=T)

# DATOS PUNTUALES
bost<-read.csv("D:/EstadisticaEspacial/boston.csv",sep=",", header=T)
b.coord<-SpatialPoints(bost[,c("LON","LAT")])
bost2<-SpatialPointsDataFrame(b.coord, bost)
coord_b<-coordinates(bost2)
class(coord_b) [1]
bost_k2<-knn2nb(knearneigh(coord_b, k=2, longlat=T))
plot(as(bost2,"Spatial"),axes=T)
plot(bost_k2, coord_b, add=T)
plot(bost2[bost2$CHAS==1,], col="blue", add=TRUE)
#VECINOS BASADOS EN DISTANCIAS ESPECIFICAS
dist<-unlist(nbdists(sids_kn1, coords))
summary(dist)
max_k1<-max(dist)
sids_kd1<-dnearneigh(coords, d1=0, d2=0.75*max_k1,row.names=IDs)

plot(as(sids_kd1,"Spatial"),axes=T)
sids_kd2<-dnearneigh(coords, d1=0, d2=1*max_k1, row.names=IDs)
sids_kd3<-dnearneigh(coords, d1=0, d2=1.5*max_k1, row.names=IDs)
#DISTANCIA MAXIMA
sids_ran1<-dnearneigh(coords, d1=0, d2=134600, row.names=IDs)
#MATRICES DE PESOS ESTANDARIZADAS POR FILA
sids_nbq_w<- nb2listw(sids_nbq)
sids_nbq_w
#PESOS BINARIOS
sids_nbq_wb<-nb2listw(sids_nbq,style="B")
sids_nbq_wb
#VECINOS BASADOS EN IDW
dist<-nbdists(sids_nbq, coordinates(sids_SP))
idw<-lapply(dist, function(x) 1/(x/1000))
sids_nbq_idwb<-nb2listw(sids_nbq, glist=idw, style ="B" )
summary(unlist(sids_nbq_idwb$weights))
#examinar la autocorrelacion espacial 
#autocorrelacion espacial global Moran's
moran.test(sids_NAD$SIDR79, listw = sids_nbq_w, alternative = "two.sided")
moran.test(sids_NAD$SIDR79, listw = sids_nbq_wb)































library(maptools)
library(rgdal)
library(spdep)

getinfo.shape("D:/5.ESTADISTICA_ESPACIAL/boston/boston.shp")
boston<-readShapePoints("D:/5.ESTADISTICA_ESPACIAL/boston/boston.shp")
class(boston)
plot (boston)


#LEER EL CSV DE DATOS DE BOSTON
bost<-read.csv("D:/5.ESTADISTICA_ESPACIAL/boston.csv",sep=",", header=T)
b.coord<-SpatialPoints(bost[,c("LON","LAT")])
bost2<-SpatialPointsDataFrame(b.coord, bost)
coord_b<-coordinates(bost2)
class(coord_b) [1] 
bost_k2<-knn2nb(knearneigh(coord_b, k=2, longlat=T))
plot(as(bost2,"Spatial"),axes=T)
plot(bost_k2, coord_b, add=T)
plot(bost2[bost2$CHAS==1,], col="red", add=TRUE)
coords<-coordinates(boston_SP)
IDs<-row.names(as(boston_SP,"data.frame"))
boston_kn1<-knn2nb(knearneigh(coords, k=1), row.names=IDs)
boston_kn2<-knn2nb(knearneigh(coords, k=2), row.names=IDs)
boston_kn4<-knn2nb(knearneigh(coords, k=4), row.names=IDs)
plot(boston_SP, axes=T)
plot(boston_kn1, coords, add=T) 
plot(boston_kn2, coords, add=T)
plot(boston_kn4, coords, add=T) 

plot(as(boston_kn1, coords, add=T),axes=T) 
boston_nbq_wb<-nb2listw(boston_nbq, style="B")







