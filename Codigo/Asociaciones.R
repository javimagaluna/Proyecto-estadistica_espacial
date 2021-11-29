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

# Variogramas -------------------------------------------------------------

## Lo veré despues, no recuerdo bien las def...
v1<- gstat::variogram(log(Total_Viv) ~ 1, data= D2017_sf)
plot(v1)


# Ajustando variograma "Hol"
mod <- vgm(psill = 4, range= 650, nugget = 0.01,"Hol")
mod
v1_fit <- gstat::fit.variogram(v1, model= mod)

plot(v1, model= v1_fit)




# Asociacion espacial -----------------------------------------------------

# (Para total viviendas)
vecino <- spdep::poly2nb(D2017_sf)

B_pesos <- spdep::nb2listw(vecino,
                           style= "B", # pesos espaciales binarios
                           zero.policy= TRUE)

# Coeficiente de Moran #
#------
spdep::moran(D2017_sf$Total_Viv, B_pesos, 
      dim(D2017_sf)[1], Szero(B_pesos),
      zero.policy= TRUE)

# Tenemos un I de -0.0867, por lo que podríamos decir que no existe una relación espacial de la distribución total de viviendas(?). -> hay independencia (?)

# Coeficiente de Geary #
#------
spdep::geary(D2017_sf$Total_Viv, B_pesos, dim(D2017_sf)[1],
             dim(D2017_sf)[1]-1, Szero(B_pesos),
             zero.policy= TRUE)

# Puesto que C es 1.227419, podríamos decir que existe correlación espacial positiva. (?) no estoy segura por que en general es entre 0 y 1 D:


# Correlograma #
#------

plot(sp.correlogram(vecino, D2017_sf$Total_Viv, 
                    order = 4,            # Hasta el vecino de orden 8
                    method = "I",         # I de Moran
                    style = "B"),         # pesos espaciales binarios
     main=" Correlograma con I de Moran")
grid()

# Del correlograma se evidencia que no existe asociacion evidente desde el primer vecino, pues la banda incluye al cero.


