rm(list=ls())

url_estaciones <- 'https://drive.google.com/uc?export=download&id=1UMCF93AaWFn39lxv4zL9B9HOFXUbGw_b'
url_datos <- 'https://drive.google.com/uc?export=download&id=1uVAhAQK4TdTWb3A0643iAlHlp7_GBSRh'

download.file(url_estaciones, destfile='estaciones_smn.xlsx')
download.file(url_datos, destfile='datos_meteorologicos.xlsx')

# No tengo idea de lo que estoy importando
library(tidyverse)
library(readxl)
library(leaflet)
library(geoR)
library(spdep)
library(tmap)
library(viridis)
library(gstat)

# Cargo datos
estaciones <- read_excel('estaciones_smn.xlsx')
View(estaciones)

# Primer golpe de vista
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = estaciones,
                   lat = ~LAT,
                   lng = ~LON,
                   radius=1)

# Varios reportes para mismos puntos
datos_meteorologicos <- read_excel('datos_meteorologicos.xlsx')
View(datos_meteorologicos)

# Me quedo con temp solo
temperatura <- datos_meteorologicos[,c('HORA','TEMP','NOMBRE')]
View(temperatura)

# Saco NAs en temp
temperatura <- na.omit(temperatura)

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
                 bins = 50)

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

estaciones_filtrado <- estaciones %>%
  filter(temp_promedio > (q_25 - 1.5*iqr),
         temp_promedio < (q_75 + 1.5*iqr))

ggplot() +
  geom_histogram(data = estaciones_filtrado,
                 aes(x = temp_promedio),
                 bins = 50)

qqnorm(estaciones_filtrado$temp_promedio)
qqline(estaciones_filtrado$temp_promedio)

qqnorm((estaciones_filtrado$temp_promedio - mean(estaciones_filtrado$temp_promedio))/sd(estaciones_filtrado$temp_promedio))
qqline((estaciones_filtrado$temp_promedio - mean(estaciones_filtrado$temp_promedio))/sd(estaciones_filtrado$temp_promedio))

hist((estaciones_filtrado$temp_promedio - mean(estaciones_filtrado$temp_promedio))/sd(estaciones_filtrado$temp_promedio))

# Previo a realizar el variograma, vemos si la temperatura guarda alguna relación con la ubicación
# A priori se sospecha que sí (más al Norte --> más cálido el clima)

plot(estaciones$LAT, estaciones$temp_promedio)
# En efecto, hay una fuerte relación entre la Latitud y la temperatura promedio

plot(estaciones$LON, estaciones$temp_promedio)
# Con la Longitud no hay una relación muy clara


colores <- colorFactor(palette = 'plasma', estaciones$temp_promedio)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = estaciones,
                   lat = ~LAT,
                   lng = ~LON,
                   radius= 1,
                   color=~colores(temp_promedio))

# Autocorrelación Espacial
# Por puntos
pares <- estaciones_filtrado %>% select(LON, LAT)
coordinates(pares) <- ~LON + LAT
pares_grilla <- dnearneigh(pares, 0, 3)
plot(pares_grilla, pares)

pesos_grilla <- nb2listw(pares_grilla, style='W')
moran.test(estaciones_filtrado$temp_promedio, pesos_grilla)
# Rechazamos H0 (que los datos no se autocorrelacionan)

# Construimos el variograma
data_variograma <- estaciones_filtrado %>% select(LAT, LON, temp_promedio)
coordinates(data_variograma) <- ~LON + LAT

geodata_variograma <- as.geodata(obj = data_variograma)
variograma_nube <- variog(geodata_variograma, option='cloud')
plot(variograma_nube)

# Usamos un cutoff de 25 y un width de 2
variograma_empirico <- variogram(temp_promedio~1, data_variograma, cutoff=14, width=1)
plot(variograma_empirico)

# Hacemos uno con tendencia en la LATITUD
variograma_tendencia <- variogram(temp_promedio~LAT, data_variograma, cutoff=14, width=1)
plot(variograma_tendencia)

# Ajustamos modelos esférico y exponencial
vtent_exp = fit.variogram(variograma_tendencia, vgm(10, "Exp", 14, 1))
vtent_exp
plot(variograma_tendencia , vtent_exp)

vtent_sph = fit.variogram(variograma_tendencia, vgm(10, "Sph", 14, 1))
vtent_sph
plot(variograma_tendencia , vtent_sph)

attr(vtent_exp, 'SSErr')
attr(vtent_sph, 'SSErr')

# El exponencial tiene un menor error, por lo que lo elegimos por sobre el esférico

# Isotropía?
variograma_mapa <- variogram(temp_promedio~LAT,
                             data_variograma,
                             cutoff=14,
                             width=1,
                             map=T)
plot(variograma_mapa)

# No parecería haber grandes diferencias entre variogramas según la dirección
omni_variograma = plot(variog4(geodata_variograma, uvec = seq(0,15,l=10)))

