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
  labs(title = "Frecuencia Delito Alcohol Vía Pública 2011") +
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


# Otra opción
Delitos2010 <- Delitos_RM %>% 
  filter(ANIO =="2010")

mapview::mapview(Delitos2010["VIO.ADULT"])
mapview::mapview(Delitos2010["VIO.MUJ"])
mapview::mapview(Delitos2010["VIO.NIN"])
mapview::mapview(Delitos2010["ALCOHOL"])
mapview::mapview(Delitos2010["ABUSO"])
mapview::mapview(Delitos2010["RUIDO"])
