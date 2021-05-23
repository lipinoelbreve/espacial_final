rm(list=ls())

url_estaciones <- 'https://drive.google.com/uc?export=download&id=1UMCF93AaWFn39lxv4zL9B9HOFXUbGw_b'
url_datos <- 'https://drive.google.com/uc?export=download&id=1uVAhAQK4TdTWb3A0643iAlHlp7_GBSRh'
url_shapefile_arg <- 'https://drive.google.com/uc?export=download&id=1slJexHZX5qlur_kjrrXWr7roEIVNJnrl'
url_arg_gis <- 'https://drive.google.com/uc?export=download&id=1sKNHyMh89HAZE2Jt24vC6oRbOF-JTugN'
  
download.file(url_estaciones, destfile='estaciones_smn.xlsx', mode='wb')
download.file(url_datos, destfile='datos_meteorologicos.xlsx', mode='wb')
download.file(url_shapefile_arg, destfile='pais.shp')

temp_shapefile <- tempfile()
download.file(url_arg_gis, temp_shapefile, mode='wb')
unzip(temp_shapefile)

# Importo librerías
library(tidyverse)
library(readxl)
library(leaflet)
library(geoR)
library(spdep)
library(tmap)
library(viridis)
library(gstat)
library(raster)

# Cargo datos
estaciones <- read_excel('estaciones_smn.xlsx')
View(estaciones)

# Observaciones en el mapa
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = estaciones,
                   lat = ~LAT,
                   lng = ~LON,
                   radius=1)

pais <- st_read("pais.shp")

# Tenemos varios reportes para mismos puntos
datos_meteorologicos <- read_excel('datos_meteorologicos.xlsx')
View(datos_meteorologicos)

# Tomamos solamente la temperatura
temperatura <- datos_meteorologicos[,c('HORA','TEMP','NOMBRE')]
View(temperatura)

# Saco NAs en temp
temperatura <- na.omit(temperatura)
estaciones[!estaciones$NOMBRE %in% temperatura$NOMBRE,]

# El chilecito no tiene datos

# Guardo estaciones con temperatura
estaciones <- estaciones[estaciones$NOMBRE %in% temperatura$NOMBRE,]

# Saco promedio por estación
resumen <- temperatura %>%
  group_by(NOMBRE) %>% 
   summarise(mean = mean(TEMP), n = n())

# O saco una hora sola para cada estación
nombres_estaciones <- unique(temperatura$NOMBRE)

horarios <- data.frame(matrix(data=NA, nrow=length(nombres_estaciones), ncol=24),row.names=nombres_estaciones)
names(horarios) <- c(0:23)

for(estacion in nombres_estaciones){
  horas <- temperatura[temperatura$NOMBRE == estacion,'HORA']
  datos_estacion <- temperatura[temperatura$NOMBRE == estacion,]
  horas <- as.character(datos_estacion$HORA)
  horarios[estacion,horas] <- datos_estacion$TEMP
}

complete <- apply(horarios, 2, function(x) !any(is.na(x)))

# A las 9, 15 y 21 hay datos para todas las estaciones
horarios[,complete]

for(estacion in nombres_estaciones){
  estaciones[estaciones$NOMBRE==estacion, 'temp_promedio'] = resumen[resumen$NOMBRE==estacion,'mean']
  estaciones[estaciones$NOMBRE==estacion, 'temp_9'] = horarios[estacion,'9']
}

# Temperatura promedio - Exploración
ggplot() +
  geom_histogram(data = estaciones,
                 aes(x = temp_promedio),
                 bins = 50) +
  labs(x = 'Temperatura Promedio - Grados', y = 'Observaciones', title = 'La distribución es asimétrica')

# La Base Belgrano es un dato algo atípico, con temperatura promedio de -16 grados
estaciones[estaciones$temp_promedio < -10,]

ggplot() +
  geom_boxplot(data = estaciones,
               aes(x = temp_promedio))

q_25 <- quantile(estaciones$temp_promedio,
                 probs = 0.25,
                 na.rm=TRUE)
q_75 <- quantile(estaciones$temp_promedio,
                 probs = 0.75,
                 na.rm=TRUE)
iqr <- IQR(estaciones$temp_promedio,
           na.rm=TRUE)

# Con el criterio de Cuartil_25 - 1.5 IQR eliminaríamos gran parte de la Patagonia
estaciones[estaciones$temp_promedio <= q_25-1.5*iqr,]

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = estaciones[estaciones$temp_promedio <= q_25-1.5*iqr,],
                   lat = ~LAT,
                   lng = ~LON,
                   radius=1)

estaciones_filtrado <- estaciones %>%
  filter(temp_promedio > (q_25 - 1.5*iqr),
         temp_promedio < (q_75 + 1.5*iqr))

ggplot() +
  geom_histogram(data = estaciones_filtrado,
                 aes(x = temp_promedio),
                 bins = 50)

# No se ven muy normales, pero tampoco están tan mal
par(mfrow=c(1,2))

qqnorm(estaciones$temp_promedio, main='Antes')
qqline(estaciones$temp_promedio)

qqnorm(estaciones_filtrado$temp_promedio, main='Después', ylab='')
qqline(estaciones_filtrado$temp_promedio)
dev.off()

# Previo a realizar el variograma, vemos si la temperatura guarda alguna relación con la ubicación
# A priori se sospecha que sí (más al Norte --> más cálido el clima)

par(mfrow=c(1,2))

# En efecto, hay una fuerte relación entre la Latitud y la temperatura promedio
cor_lat = cor(estaciones$LAT, estaciones$temp_promedio)
cor_lon = cor(estaciones$LON, estaciones$temp_promedio)

plot(estaciones$LAT,
     estaciones$temp_promedio,
     ylab='Temperatura Promedio - grados',
     xlab='Latitud',
     main = bquote(rho == ~ .(round(cor_lat,3)) ))
     
# Con la Longitud no hay una relación muy clara
plot(estaciones$LON,
     estaciones$temp_promedio,
     xlab='Longitud',
     ylab='',
     main = bquote(rho == ~ .(round(cor_lon,3)) ))
dev.off()


# Ubicamos las observaciones en el mapa, coloreando según temp. promedio
colores <- colorFactor(palette = 'plasma', estaciones_filtrado$temp_promedio)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = estaciones_filtrado,
                   lat = ~LAT,
                   lng = ~LON,
                   radius= 1,
                   color=~colores(temp_promedio))

# Autocorrelación Espacial
# Por puntos
pares <- estaciones_filtrado %>% dplyr::select(LON, LAT)
coordinates(pares) <- ~LON + LAT
pares_grilla <- dnearneigh(pares, 0, 3)
plot(pares_grilla, pares)
title('Todos los puntos tienen al menos un vecino')


pesos_grilla <- nb2listw(pares_grilla, style='W')
moran.test(estaciones_filtrado$temp_promedio, pesos_grilla)
# Rechazamos H0 (que los datos no se autocorrelacionan) --> justifica todo el análisis que estamos haciendo

# Construimos el variograma nube
data_variograma <- estaciones_filtrado %>% dplyr::select(LAT, LON, temp_promedio)
coordinates(data_variograma) <- ~LON + LAT

geodata_variograma <- as.geodata(obj = data_variograma)
variograma_nube <- variog(geodata_variograma, option='cloud')
plot(variograma_nube, main='La nube sugiere un cutoff de 15')

# Usamos un cutoff de 14 y un width de 1
variograma_empirico <- variogram(temp_promedio~1, data_variograma, cutoff=14, width=1)
plot(variograma_empirico)

# Hacemos uno con tendencia en la LATITUD
variograma_tendencia <- variogram(temp_promedio~LAT, data_variograma, cutoff=14, width=1)
plot(variograma_tendencia)

variograma_tendencia_2 <- variogram(temp_promedio~LAT+LON, data_variograma, cutoff=14, width=1)
plot(variograma_tendencia_2)

# Ajustamos modelos esférico y exponencial
dev.off()
vtent_exp = fit.variogram(variograma_tendencia, vgm(10, "Exp", 14, 1))
vtent_exp
error_exp <- attr(vtent_exp, 'SSErr')
error_exp
plot(variograma_tendencia , vtent_exp)

vtent_sph = fit.variogram(variograma_tendencia, vgm(10, "Sph", 14, 1))
vtent_sph
error_sph <- attr(vtent_sph, 'SSErr')
error_sph
plot(variograma_tendencia , vtent_sph)

# Lo mismo para el de LAT + LON
vtent_exp_2 = fit.variogram(variograma_tendencia_2, vgm(10, "Exp", 14, 1))
vtent_exp_2
error_exp <- attr(vtent_exp_2, 'SSErr')
error_exp
plot(variograma_tendencia_2 , vtent_exp_2)

vtent_sph_2 = fit.variogram(variograma_tendencia_2, vgm(10, "Sph", 14, 1))
vtent_sph_2
error_sph_2 <- attr(vtent_sph_2, 'SSErr')
error_sph_2
plot(variograma_tendencia_2 , vtent_sph_2)


# El exponencial tiene un menor error, por lo que lo elegimos por sobre el esférico

# Hay isotropía?
variograma_mapa <- variogram(temp_promedio~LAT,
                             data_variograma,
                             cutoff=14,
                             width=1,
                             map=T)
plot(variograma_mapa)

# No parecería haber grandes diferencias entre variogramas según la dirección
# Tomamos el proceso como isotrópico (a ojo)
omni_variograma = plot(variog4(geodata_variograma, uvec = seq(0,15,l=10)))


# Para kriging, vamos con la versión Universal
# El proceso no es estacionario (la media cambia con la latitud)
min_lon <- min(estaciones$LON)
max_lon <- max(estaciones$LON)
min_lat <- min(estaciones$LAT)
max_lat <- max(estaciones$LAT)

grilla <- expand.grid('LON'=seq(min_lon, max_lon, by=(max_lon-min_lon)/100),
                      'LAT'=seq(min_lat, max_lat, by=(max_lat-min_lat)/100))

gridded(grilla) = ~LON+LAT
plot(grilla)

k_universal <- krige(temp_promedio~LAT, data_variograma, grilla, model=vtent_exp, nmax=20)
spplot(k_universal["var1.pred"],
       main = "Kriging Universal: Valores Predichos",
       col.regions=terrain.colors)
spplot(k_universal["var1.var"],
       main = "Kriging Universal: Varianza de las Predicciones",
       col.regions=terrain.colors)

# Cargo el polígono de Argentina para ver el Kriging dentro de los límites
# A diferencia de hacer una grilla, así podemos interpretar mejor los resultados

ggplot() +
  geom_sf(data = pais)

r <- raster(k_universal,
            layer = 'var1.pred')
r.m <- mask(r, pais)
plot(r.m, main = 'Kriging Universal')

r <- raster(k_universal,
            layer = 'var1.var')
r.m <- mask(r, pais)
plot(r.m, main = 'Varianza de las predicciones')


# Validación cruzada
k_LAT_exp <- krige.cv(temp_promedio~LAT, data_variograma, vtent_exp, nfold=155)
k_LAT_sph <- krige.cv(temp_promedio~LAT, data_variograma, vtent_sph, nfold=155)
k_LAT_LON_exp <- krige.cv(temp_promedio~LAT+LON, data_variograma, vtent_exp_2, nfold=155)
k_LAT_LON_sph <- krige.cv(temp_promedio~LAT+LON, data_variograma, vtent_sph_2, nfold=155)

bubble(k_LAT_exp, "residual", main = "meuse Model 1")
bubble(k_LAT_sph, "residual", main = "meuse Model 1")
bubble(k_LAT_LON_exp, "residual", main = "meuse Model 1")
bubble(k_LAT_LON_sph, "residual", main = "meuse Model 1")

error_LAT_exp <- mean(k_LAT_exp$residual^2)
error_LAT_sph <- mean(k_LAT_sph$residual^2)
error_LAT_LON_exp <- mean(k_LAT_LON_exp$residual^2)
error_LAT_LON_sph <- mean(k_LAT_LON_sph$residual^2)

dev.off()
par(mfrow=c(2,2))
plot(k_LAT_exp$observed, k_LAT_exp$var1.pred,
     xlab='', ylab='Val. Predichos', main=paste("LAT - Exp. - MSE:", round(error_LAT_exp,2)))
segments(x0=0,y0=0,x1=45,y1=45)

plot(k_LAT_sph$observed, k_LAT_sph$var1.pred,
     xlab="", ylab="", main=paste("LAT - Sph. - MSE:", round(error_LAT_sph,2)))
segments(x0=0,y0=0,x1=45,y1=45)

plot(k_LAT_LON_exp$observed, k_LAT_LON_exp$var1.pred,
     xlab="Val. Observados", ylab="Val. Predichos", main=paste("LAT + LON - Exp. - MSE:", round(error_LAT_LON_exp,2)))
segments(x0=0,y0=0,x1=45,y1=45)

plot(k_LAT_LON_sph$observed, k_LAT_LON_sph$var1.pred,
     xlab="Val. Observados", ylab="", main=paste("LAT + LON - Sph. - MSE:", round(error_LAT_LON_sph,2)))
segments(x0=0,y0=0,x1=45,y1=45)
