# Librerias ---------------------------------------------------------------

library(dplyr)
library(gstat)
library(sp)
library(leaflet)
library(ggplot2)
library(kableExtra)
library(sf)
library(tmap)

# Base 2017 ----------------------------------------------------------------
library(readr)

Densidad_2017 <- read_csv("Datos_limpios/Densidad_2017.csv", 
                          col_types = cols(X1 = col_skip()))

#---------

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
# D2017_sf <- RM[tolower(RM$Comuna) %in% D2017$Comuna, ]

# para join
RM$Comuna <- tolower(RM$Comuna)

# Uniendo
D2017_sf <- RM %>% left_join(D2017)

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




# Base 2016 ----------------------------------------------------------------
library(readr)

Densidad_2016 <- read_csv("Datos/Densidad_Viviendas_Santiago_2016.csv", 
                          col_types = cols(X1 = col_skip()))

Densidad_2016 %>% glimpse()

#---------

# Agrupando datos por comuna
D2016 <- Densidad_2016 %>% 
  rename(Comuna = des_comu) %>% 
  group_by(Comuna) %>% 
  summarise( Superficie = sum(superficie),
             Densidad_Viv = sum(dens_viv),
             Total_Viv = sum(total_viv))

# Cargo sf de las comunas
Chile <- sf::st_read("Datos/Comunas","comunas")

# Selecciono solo los de la RM
RM <- Chile %>% 
  dplyr::filter(Region == "Región Metropolitana de Santiago")


# filtrando comunas de ambas bases
# D2016_sf <- RM[tolower(RM$Comuna) %in% D2017$Comuna, ]

# para join
RM$Comuna <- toupper(RM$Comuna)

# Uniendo
D2016_sf <- RM %>% left_join(D2016)

D2016_sf %>% names()


## Graficos---

ggplot(D2016_sf) +
  geom_sf(aes(fill = Densidad_Viv)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Densidad de viviendas") +
  theme_minimal()

# ke?

ggplot(D2016_sf) +
  geom_sf(aes(fill = Superficie)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Superficie de manzana en hectáreas") +
  theme_minimal()

ggplot(D2016_sf) +
  geom_sf(aes(fill = Total_Viv)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Total viviendas")+
  theme_minimal()





