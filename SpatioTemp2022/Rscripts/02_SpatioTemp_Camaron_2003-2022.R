##############################################
#   Evaluacion de Crustaceos Demersales
#   Espaciotemporal: Camaron (2003-2022)
#############################################
rm(list=ls())
library(INLA)
library(sdmTMB)
#devtools::install_github("https://github.com/gfalbery/ggregplot")
library(ggplot2)
library(dplyr)
library(fields)
# Carga el mapa base

# DATOS
bd = read.csv("Data/SurveyCrustaceos2003-2022.csv")

# EDA ---------------------------------------------------------------------
d = bd %>%
  select(Especieobjetivo,Year,Mes,Region,Lon,Lat,Prof,ABkm,Ycervjoh,Ypleumon,Yheteree)

d = d %>%
  mutate(Ylcol = ifelse(Ypleumon > 0, Ypleumon/(ABkm*1000), 0),
         Ylam = ifelse(Ycervjoh > 0, Ycervjoh/(ABkm*1000), 0),
         Ycam = ifelse(Yheteree > 0, Yheteree/(ABkm*1000), 0),
         Plcol = ifelse(Ypleumon > 0, 1, 0),
         Plam = ifelse(Ycervjoh > 0, 1, 0),
         Pcam = ifelse(Yheteree > 0, 1, 0))

d = d[d$Prof>0,,]

# Paso 1: calcule Northing and Easting ------------------------------------
d <- add_utm_columns(d,
                     ll_names = c("Lon","Lat"),
                     utm_names = c("X","Y"),
                     utm_crs = 24878,
                     units = "km")


# OLD: Prepara datos de latitud - longitud -------------------------------------
# PASO 1: Transforma latitud - longitud a UTM: Northing y Easting en metros
#coor_dec <- SpatialPoints(cbind(d$Lon,d$Lat),proj4string = CRS("+proj=longlat"))
#kmproj <- CRS("+proj=utm +zone=18 +south ellps=WGS84 +units=km")
#coor_utm <- spTransform(coor_dec,CRS=kmproj)
#coor_utm <- geometry(coor_utm) 
#bd$X <- coor_utm$coords.x1 # km
#bd$Y <- coor_utm$coords.x2 
#plot(bd$X,bd$Y,col="blue",cex=0.2)

# PASO 2: DOMINIO
loc_xy <- cbind("X"=d$X,"Y"=d$Y)
plot(loc_xy,asp=1)
# Se utiliza INLA nonconvex hull
bound <- inla.nonconvex.hull(loc_xy,convex = -0.003,
                             concave = -0.003,
                             resolution = c(300,300))
plot(bound$loc,type="l")
points(loc_xy,cex=0.3,col="brown")
glimpse(bound)
source("Rfun/bound2dom.R")
dom = bound2dom(data = bound)
glimpse(dom)
dom = st_as_sf(dom,coords = c("X","Y"),crs=st_crs(baseMap))

dom_mpoly = st_sf(
  aggregate(dom$geometry,
            list(ID=dom$ID),
            FUN = function(g){st_cast(st_combine(g),"POLYGON")}))
glimpse(dom)
ggplot()+
  geom_sf(data = baseMap)+
  geom_sf(data=iso400m,col="black")+
  geom_sf(data=dom_mpoly,alpha=0.5)+
  #geom_point(data=d,aes(x=X,y=Y),size=0.3,col="brown")+
  theme_bw()

# PASO 3: ENTRAMADO (mesh) 
mesh1 <- inla.mesh.2d(loc=loc_xy,loc.domain = dom,
                      max.edge = c(300,600),
                      cut= 5)
mesh1$n
plot(mesh1,asp=1)
points(loc_xy, col="brown",pch=19,cex=0.2)
# PASO 4: DEFINE SPDE
mesh <- mesh1
#Distancia <- dist(loc_xy)
#hist(Distancia, freq = TRUE, main = "", xlab = "Distancia entre lances (km)",ylab = "Frecuencia")
#plot(x = sort(Distancia), y = (1:length(Distancia))/length(Distancia), type = "l",xlab = "Distancia entre lances (km)",ylab = "Cumulative proportion")

# PASO 4: DEFINE SPDE
0.5*diff(range(loc_xy[,2])) #a lo largo de la costa
sd(d$Ycam,na.rm = TRUE)/mean(d$Ycam,na.rm = TRUE) #CV
#spde = inla.spde2.matern(mesh, alpha = 2)
spde = inla.spde2.pcmatern(mesh,
                           prior.range = c(300, 0.5),
                           prior.sigma = c(2.7, 0.1))

# PASO 5: Estructura del modelo espaciotemporal
#Modelo : EFECTO PROGRESIVO
range(d$Year)
d$year = d$Year
table(repl <- d$year - 2002) # Indice años
nyr <- 20 #numero de años
A.1 <- inla.spde.make.A(mesh = mesh,loc = loc_xy, group = repl)
s.index.1 <- inla.spde.make.index(name = "s",n.spde = mesh$n, n.group = nyr)

# PASO 6: Apilamiento de datos (stack data)
# Variable dependiente: Numero de huevos = Numb
# Predictores:
# Prof = Profundidad de fondo
# Apilamiento Modelo 1: Presencia-ausencia
stk.est.z <- inla.stack(data=list(Pr=d$Pcam),
                        A = list(A.1,1),
                        tag = "est",
                        effects=list(s.index.1,
                                     data.frame(b0=1,year=d$year,Prof=d$Prof)
                        ))

# Apilamiento Modelo 1: CPUA
stk.est.y <- inla.stack(data=list(CPUA=d$Ycam),
                         A = list(A.1,1),
                         tag = "est",
                         effects=list(s.index.1,
                                      data.frame(b0=1,year=d$year,Prof=d$Prof)
                         ))


## Lee la grilla de predicción
pred_data <- read.csv("Data/crusdem_pred_grd.csv",sep=";")
head(pred_data)
repl <- pred_data$year - 2002
table(repl)
pred_loc_xy = as.matrix(pred_data[,1:2])
head(pred_loc_xy)
# Apilamiento de los datos en la grilla de predicción
A.1pred = inla.spde.make.A(mesh = mesh,loc = pred_loc_xy,group = repl)

stk.pred.z = inla.stack(data = list(Pr=NA),
                        A = list(A.1pred,1),
                        tag = "pred",
                        effects=list(s.index.1,
                                     data.frame(b0=1,year=pred_data$year,Prof=pred_data$Prof)
                        ))

stk.pred.y = inla.stack(data = list(CPUA=NA),
                        A = list(A.1pred,1),
                        tag = "pred",
                        effects=list(s.index.1,
                                     data.frame(b0=1,year=pred_data$year,Prof=pred_data$Prof)
                        ))


stk.full.z <- inla.stack(stk.est.z, stk.pred.z)
stk.full.y <- inla.stack(stk.est.y, stk.pred.y)

#PASO
form1z <- Pr ~ 0 + b0 + f(year,model="iid", constr=TRUE)+f(inla.group(Prof,n=10),model = "rw2")   + f(s,model = spde, group = s.group, control.group = list(model='ar1'))
form1y <- CPUA ~ 0 + b0 + f(year,model="iid", constr=TRUE)+f(inla.group(Prof,n=10),model = "rw2")  + f(s,model = spde, group = s.group, control.group = list(model='ar1'))

m1z <- inla(formula = form1z,
                 family = 'binomial',
                 control.inla = list(tolerance = 1e-9), #TODO: contro.inla=list(strategy='laplace')
                 #control.inla=list(strategy='laplace'),
                 data = inla.stack.data(stk.full.z),
                 control.predictor = list(A=inla.stack.A(stk.full.z),compute=TRUE),
                 quantiles = NULL,
                 verbose = TRUE,
                 control.compute = list(waic=TRUE,dic = TRUE,cpo=TRUE))

summary(m1z)

m1y <- inla(formula = form1y,
                 family = 'gamma',
                 control.inla = list(tolerance = 1e-9), #TODO: contro.inla=list(strategy='laplace')
                 #control.inla=list(strategy='laplace'),
                 data = inla.stack.data(stk.full.y),
                 control.predictor = list(A=inla.stack.A(stk.full.y),compute=TRUE),
                 quantiles = NULL,
                 verbose = TRUE,
                 #control.results(return.marginals.predictor=FALSE, return.marginals.random=FALSE),
                 control.compute = list(waic=TRUE,dic = TRUE,cpo=TRUE))

summary(m1y)

