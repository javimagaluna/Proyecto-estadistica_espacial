# Librerías
library(tidyverse) # Para Manipular datos

# Cargo sf de las comunas
Chile <- sf::st_read("Datos/Comunas","comunas")

# Selecciono solo los de la RM
RM <- Chile %>% 
  dplyr::filter(Region == "Región Metropolitana de Santiago")

# Cargo la base de los delitos
Delitos <- readxl::read_excel("Datos/Delitos_todos.xlsx")
glimpse(Delitos)
unique(Delitos$COMUNA) # 52 Comunas

# Arreglar el nombre de la comuna
Delitos$COMUNA[Delitos$COMUNA == "Caracaví"] <- "Curacaví"

# Veo si coinciden los nombres de RM con los de Delitos
coincidencias <-  c()
unique(Delitos$COMUNA)

for (comuna in unique(Delitos$COMUNA)){
  if (comuna %in% RM$Comuna){
    coincidencias <- c(coincidencias, comuna)
  }
}
coincidencias
# Coinciden, tienen el mismo nombre, se puede cruzar.

# Dejo con el mismo nombre la columna
RM <- RM %>% 
  rename(COMUNA = Comuna)

# Entrecruzo
Delitos_RM <- RM %>% 
  left_join(Delitos)

glimpse(Delitos_RM)
unique(Delitos_RM$ANIO)

# Delito Alcohol en vía pública
ggplot(Delitos_RM %>% 
         filter(ANIO == "2010")) +
  geom_sf(aes(fill = ALCOHOL)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia Delito Alcohol Vía Pública 2010") +
  theme_void()

ggplot(Delitos_RM %>% 
         filter(ANIO == "2011")) +
  geom_sf(aes(fill = ALCOHOL)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia Delito Alcohol Vía Pública 2011") +
  theme_void()

ggplot(Delitos_RM %>% 
         filter(ANIO == "2017")) +
  geom_sf(aes(fill = ALCOHOL)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia Delito Alcohol Vía Pública 2017") +
  theme_void()

# Delito Violencia Adulto Mayor
ggplot(Delitos_RM %>% 
         filter(ANIO == "2010")) +
  geom_sf(aes(fill = VIO.ADULT)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia Delito Violencia Adulto Mayor 2010") +
  theme_void()

ggplot(Delitos_RM %>% 
         filter(ANIO == "2011")) +
  geom_sf(aes(fill = VIO.ADULT)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia Delito Violencia Adulto Mayor 2011") +
  theme_void()

ggplot(Delitos_RM %>% 
         filter(ANIO == "2017")) +
  geom_sf(aes(fill = VIO.ADULT)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Frecuencia Delito Violencia Adulto Mayor 2011") +
  theme_void()


# Otra opción más "bonita"
Delitos2010 <- Delitos_RM %>% 
  filter(ANIO =="2010")

mapview::mapview(Delitos2010["VIO.ADULT"],
                 layer.name = "Delito Viol. Contra Adult. Mayor",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , Delitos2010$COMUNA
                   , "<br><b>Delitos: </b>"
                   , Delitos2010$VIO.ADULT
                 ))
mapview::mapview(Delitos2010["VIO.MUJ"],
                 layer.name = "Delito Viol. Contra Mujer",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , Delitos2010$COMUNA
                   , "<br><b>Delitos: </b>"
                   , Delitos2010$VIO.MUJ
                 ))

# Este se me queda pegado con la Ñ
mapview::mapview(Delitos2010["VIO.NIN"],
                 layer.name = "Delito Viol. Contra Nino",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , Delitos2010$COMUNA
                   , "<br><b>Delitos: </b>"
                   , Delitos2010$VIO.NIN
                 ))

# Se pega con tildes también 
mapview::mapview(Delitos2010["ALCOHOL"],
                 layer.name = "Delito Alcohol Via Publica",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , Delitos2010$COMUNA
                   , "<br><b>Delitos: </b>"
                   , Delitos2010$ALCOHOL
                 ))

mapview::mapview(Delitos2010["ABUSO"],
                 layer.name = "Delito Abuso Sexual",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , Delitos2010$COMUNA
                   , "<br><b>Delitos: </b>"
                   , Delitos2010$ABUSO
                 ))

mapview::mapview(Delitos2010["RUIDO"],
                 layer.name = "Delito Ruidos Molestos",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , Delitos2010$COMUNA
                   , "<br><b>Delitos: </b>"
                   , Delitos2010$RUIDO
                 ))

# transformando sf a sp
sp_Delitos2010 <- as(Delitos2010, "Spatial")



tmap_leaflet(tm_shape(sp_Delitos2010) +
               tm_polygons(c("ABUSO", "ALCOHOL"),
                       palette= "plasma"))


# No se por qué el circulo rojo se pone detrás del mapa :c
tmap_mode("view")
tm_shape(sp_Delitos2010) +
  tm_polygons(c("ABUSO", "ALCOHOL"), id= "COMUNA",
              palette= "plasma") + 
  tm_facets(sync = TRUE, ncol = 2)


## GIF
animacion <- tm_shape(Delitos_RM) +
  tm_polygons("ABUSO", style = "cont",
              id = "COMUNA",
              palette= "plasma") + 
  tm_facets(along = "ANIO")

tmap_animation(animacion, delay = 70) # , filename = "nombre.gif"


