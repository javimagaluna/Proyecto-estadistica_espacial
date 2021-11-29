# Librerias ---------------------------------------------------------------

library(dplyr)
library(gstat)
library(sp)
library(leaflet)
library(ggplot2)
library(kableExtra)
library(sf)
library(tmap)

# Bases -------------------------------------------------------------------
library(readr)

Densidad_2017 <- read_csv("Datos_limpios/Densidad_2017.csv", 
                          col_types = cols(X1 = col_skip()))


###################################
# NO
Densidad2017_sf <- sf::st_read("Datos/Indicadores_Territoriales", "Indicadores_Territoriales")


class(Densidad2017_sf)


ggplot(data = Densidad2017_sf) +
  geom_sf(aes(fill = COMUNA))

# as(Densidad2017_sf, Class = "Spatial")



ggplot(Densidad2017_sf) +
  geom_sf(aes(fill = Den_Viv)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Densidad de viviendas") +
  theme_void()

ggplot(Densidad2017_sf) +
  geom_sf(aes(fill = Sup_Ha)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Superficie de manzana en hectáreas") +
  theme_void()

ggplot(Densidad2017_sf) +
  geom_sf(aes(fill = TOTAL_VIVI)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Total viviendas") +
  theme_void()

####################################

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

D2017_sf %>% names()


## Graficos---

ggplot(D2017_sf) +
  geom_sf(aes(fill = Densidad_Viv)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Densidad de viviendas") +
  theme_minimal()

# ke?

ggplot(D2017_sf) +
  geom_sf(aes(fill = Superficie_Ha)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Superficie de manzana en hectáreas") +
  theme_minimal()

ggplot(D2017_sf) +
  geom_sf(aes(fill = Total_Viv)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Total viviendas")+
  theme_minimal()








