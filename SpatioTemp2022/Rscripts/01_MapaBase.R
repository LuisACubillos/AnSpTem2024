### MAPA BASE
## 
rm(list = ls())
library(ggplot2)
library(sp)
library(sf)

# Directorio
setwd("~/01Proyectos/IFOPTalleres/AnSpTem2024/SpatioTemp2022")
# MAPA CHILE y CROP
# Lee chile.shp 
Ch <- st_read("Data/polyChile/Chilecontinentalpoly.shp")
#load("Data/Chile_mapa.RData")
baseMap <- st_crop(Ch, xmin= -75,xmax= -68, ymin= -38, ymax = -22)
# lee 200 and 400 m bottom depth (m) from
# Batimetria
iso200m = st_read("Data/batimetria_GEBCO/Bat200_GEBCO/bat_200m.shp")
iso400m = st_read("Data/batimetria_GEBCO/Bat400_GEBCO/bat_400m.shp")

iso200m = st_crop(iso200m, xmin= -75,xmax= -68, ymin= -38, ymax = -22)
iso400m = st_crop(iso400m, xmin= -75,xmax= -68, ymin= -38, ymax = -22)

# LOCALIDADES
localidad <- data.frame(matrix(c(
  -70.480303, -25.406494, "Taltal",
  -70.827044, -27.06425833, "Caldera",
  -71.2192, -28.4664, "Huasco",
  -71.2542,-29.9077,"Coquimbo",
  -71.5833,-33,"Valparaíso",
  #-71.5667,-33.6333,"San Antonio",
  -72.6,-35.39,"Constitución",
  -73.11684, -36.72494, "Talcahuano"),
  ncol=3,byrow = TRUE))
colnames(localidad) = c("Lon","Lat","city")
localidad$Lon = as.numeric(localidad$Lon)
localidad$Lat = as.numeric(localidad$Lat)
head(localidad)
pos <- st_as_sf(localidad,coords = c("Lon","Lat"),crs=4326,agr="constant")

f1 <- ggplot(data = baseMap)+
  geom_sf()+
  #geom_sf(data=iso200m,col="black",size=0.3)+
  geom_sf(data=iso400m,col="grey",size=0.3)+
  geom_sf(data=pos,size=0.5,col="black")+
  geom_sf_text(data = pos,aes(label=city),nudge_x=1.3,size=3,col = "black")+
  scale_x_continuous(breaks = c(-74,-72,-70,-68))+
  #scale_y_continuous(breaks = c(-38,-37,-36,-35,-34,-33,-32,-31,-30,-29,-28,-27))+
  xlab("Longitud (ºW)")+ylab("Latitud (ºS)")+
  theme_bw(14)
f1

baseMap = baseMap %>% st_transform(24878)
baseMap = st_transform(baseMap,gsub("units=m","units=km",st_crs(baseMap)$proj4string))
pos_loc = st_transform(pos,gsub("units=m","units=km",st_crs(baseMap)$proj4string))
iso400m = st_transform(iso400m,gsub("units=m","units=km",st_crs(baseMap)$proj4string))

f2 = ggplot()+
  geom_sf(data=baseMap)+
  geom_sf(data=iso400m,col="grey",size=0.3)+
  geom_sf(data=pos,size=1,col="black")+
  #geom_sf_label(data = pos,aes(label=city),nudge_x=110,size=2,col = "black")+
  geom_sf_text(data = pos,aes(label=city),nudge_x=110,size=2,col = "black")+
  scale_x_continuous(breaks = c(-74,-72,-70,-68))+
  xlab("Longitude (ºW)")+ylab("Latitude (ºS)")+
  theme_bw(14)
f2

#save.image("Resultados/MapaBase.RData")

