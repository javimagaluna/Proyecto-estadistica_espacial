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
Delitos_todos <- read_csv("Datos_limpios/Delitos_todos_38com.csv", 
                          col_types = cols(X1 = col_skip()))


Densidad_2016 <- read_csv("Datos_limpios/Densidad_2016.csv", 
                          col_types = cols(X1 = col_skip()))


Densidad_2017 <- read_csv("Datos_limpios/Densidad_2017.csv", 
                          col_types = cols(X1 = col_skip()))



# Analisis exploratorio ---------------------------------------------------

Delitos_todos %>% 
  group_by(ANIO) %>%
  ggplot( aes(ANIO, VIO.MUJ, group= COMUNA, col= COMUNA))+
  geom_line()

# xd

## Comunas con mas denuncias:

glimpse(Delitos_todos)

Delitos_todos = Delitos_todos %>% 
  mutate(Total = apply(Delitos_todos[, c(2:7)], 1, FUN = sum))


###################################

Densidad2017_sf <- sf::st_read("Datos/Indicadores_Territoriales.geojson")

Densidad2016_sf <- sf::st_read("Datos/Densidad_Viviendas_Santiago_2016.geojson")

class(Densidad2017_sf)

tmap_leaflet(tm_shape(Densidad2017_sf)+
  tm_dots(c("Den_Viv"),
          palette= "plasma"))


ggplot(data = Densidad2017_sf) +
  geom_sf(aes(fill = COMUNA))
