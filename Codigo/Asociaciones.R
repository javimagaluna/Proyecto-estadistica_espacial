# Librerias ---------------------------------------------------------------

library(dplyr)
library(gstat)
library(sp)
library(ggplot2)
library(sf)
library(tmap)


##################
#CARGANDO DATOS DENSIDAD 2017

library(readr)
Densidad_2017 <- read_csv("Datos_limpios/Densidad_2017.csv", 
                          col_types = cols(X1 = col_skip()))

# Agrupando datos por comuna
D2017 <- Densidad_2017 %>% 
  rename(Comuna = COMUNA) %>% 
  group_by(Comuna) %>% 
  summarise( Superficie_Ha = sum(Sup_Ha),
             Densidad_Viv = sum(Den_Viv),
             Total_Viv = sum(TOTAL_VIVI))

# Cargo sf de las comunas
Chile <- sf::st_read("Datos/Comunas","comunas")

# Selecciono solo los de la RM
RM <- Chile %>% 
  dplyr::filter(Region == "Región Metropolitana de Santiago")


# filtrando comunas de ambas bases
D2017_sf <- RM[tolower(RM$Comuna) %in% D2017$Comuna, ]

# para join
D2017_sf$Comuna <- tolower(D2017_sf$Comuna)

# Uniendo
D2017_sf <- D2017_sf %>% left_join(D2017)

##################
#CARGANDO DATOS DENSIDAD 2016

Densidad_2016 <- read_csv("Datos/Densidad_Viviendas_Santiago_2016.csv", 
                          col_types = cols(X1 = col_skip()))

# Agrupando datos por comuna
D2016 <- Densidad_2016 %>% 
  rename(Comuna = des_comu) %>% 
  group_by(Comuna) %>% 
  summarise( Superficie = sum(superficie),
             Densidad_Viv = sum(dens_viv),
             Total_Viv = sum(total_viv))



# filtrando comunas de ambas bases
D2016_sf <- RM[toupper(RM$Comuna) %in% D2016$Comuna, ]

# para join
D2016_sf$Comuna <- toupper(D2016_sf$Comuna)

# Uniendo
D2016_sf <- D2016_sf %>% left_join(D2016)

##################

# Variogramas -------------------------------------------------------------

# Variograma experimental para el log del total de viviendas 2017:

v1<- gstat::variogram(log(Total_Viv) ~ 1, data= D2017_sf)
v1
plot(v1, pch= 19)
## A medida que aumenta la distancia, también aumenta la semivarianza


# show.vgms()

# Ajustando variograma "Hol"
mod <- vgm(psill = 4, range= 30000, nugget = 0.1,"Hol")
mod
v1_fit <- gstat::fit.variogram(v1, model= mod)
v1_fit

plot(v1, model= v1_fit, pch= 19)



# Variograma experimental para el log del total de viviendas 2016:

v2<- gstat::variogram(log(Total_Viv) ~ 1, data= D2016_sf)
v2
plot(v2, pch= 19)
## A medida que aumenta la distancia, también aumenta la semivarianza

show.vgms()

# Ajustando variograma "Pow"
mod2 <- vgm(psill = 4, range= 2, nugget = 0.1, "Pow")
mod2
v2_fit <- gstat::fit.variogram(v2, model= mod2)
v2_fit

plot(v2, model= v2_fit, pch= 19)

# D: no caxo que volá los variogramas.



# Asociacion espacial -----------------------------------------------------

# Para total viviendas 2017
vecino <- spdep::poly2nb(D2017_sf)

B_pesos <- spdep::nb2listw(vecino,
                           style= "B", # pesos espaciales binarios
                           zero.policy= TRUE)

W_pesos <- spdep::nb2listw(vecino,
                           style= "W", # pesos espaciales estandarizados
                           zero.policy= TRUE)


# Coeficiente de Moran #
#------
spdep::moran(D2017_sf$Total_Viv, B_pesos, 
      dim(D2017_sf)[1], spdep::Szero(B_pesos),
      zero.policy= TRUE)

# Tenemos un I de -0.0867, por lo que podríamos decir que no existe una relación espacial de la distribución total de viviendas(?). -> hay independencia (?)

spdep::moran(D2017_sf$Total_Viv, W_pesos, 
             dim(D2017_sf)[1], spdep::Szero(B_pesos),
             zero.policy= TRUE)

# I negativo y pequeño, misma conclucion


# Coeficiente de Geary #
#------
spdep::geary(D2017_sf$Total_Viv, B_pesos, dim(D2017_sf)[1],
             dim(D2017_sf)[1]-1, spdep::Szero(B_pesos),
             zero.policy= TRUE)

# Puesto que C es 1.227419, podríamos decir que (no) existe correlación espacial positiva. (?) no estoy segura por que en general es entre 0 y 1 D:

spdep::geary(D2017_sf$Total_Viv, W_pesos, dim(D2017_sf)[1],
             dim(D2017_sf)[1]-1, spdep::Szero(B_pesos),
             zero.policy= TRUE)

# Puesto que C es 0.2357951, podríamos decir que existe correlación espacial positiva.


# Correlograma #
#------

plot(spdep::sp.correlogram(vecino, D2017_sf$Total_Viv, 
                    order = 4,            # Hasta el vecino de orden 4
                    method = "I",         # I de Moran
                    style = "B"),         # pesos espaciales binarios
     main=" Correlograma con I de Moran")
grid()

# Del correlograma se evidencia que no existe asociacion evidente desde el primer vecino, pues la banda incluye al cero.

plot(spdep::sp.correlogram(vecino, D2017_sf$Total_Viv, 
                           order = 4,            # Hasta el vecino de orden 4
                           method = "I",         # I de Moran
                           style = "W"),         # pesos espaciales estandarizados
     main=" Correlograma con I de Moran")
grid()

# Mismo resultado anterior.



# Correlaciones entre Densidad y delitos ----------------------------------

## Corr densidades
psych::pairs.panels(D2016[,-1], main= "Correlación para densidades 2016", pch=20)
psych::pairs.panels(D2017[, -1], main= "Correlación para densidades 2017", pch=20)

# Correlación alta, como era de esperarse.

## Corr delitos
Delitos <- readxl::read_excel("Datos/Delitos_todos.xlsx")
psych::pairs.panels(Delitos[, c(-1,-8)], pch=20)

# o:

## Corr total de viviendas y delitos:
Delitos_2016 <- Delitos %>%
  filter(ANIO == 2016) %>%
  rename(Comuna= COMUNA)
                   
Delitos_2016$Comuna <- toupper(Delitos_2016$Comuna)

Delitos_2016 = Delitos_2016 %>% left_join(D2016)

psych::pairs.panels(Delitos_2016[, c(-1,-8, -9, -10)], pch=20)

