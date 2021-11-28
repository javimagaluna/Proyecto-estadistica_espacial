
# Librerias ---------------------------------------------------------------

library(dplyr)
library(janitor)
library(gstat)
library(sp)
library(leaflet)


# Bases -------------------------------------------------------------------

##### Delitos 2010 #####

library(readxl)
Delitos2010 <- read_excel("Datos/Delitos.xlsx", sheet = "2010") %>% 
  mutate_each(funs = tolower)

Delitos2010$COMUNA %>% unique()

Delitos2010$COMUNA[Delitos2010$COMUNA == "caracaví"] <- "curacaví"


##### Densidad viviendas 2016 #####

library(readr)
Densidad_Viviendas_2016 <- read_csv("Datos/Densidad_Viviendas_Santiago_2016.csv") %>% 
  mutate_each(funs = tolower)


Densidad_Viviendas_2016$region %>% unique()       # Eliminar
Densidad_Viviendas_2016$provincia %>% unique()    # No se si eliminarlo
Densidad_Viviendas_2016$comuna %>% unique()
Densidad_Viviendas_2016$des_regi %>%  unique()    # Eliminar
Densidad_Viviendas_2016$des_prov %>% unique()

Densidad_Viviendas_2016 = Densidad_Viviendas_2016[, -c(2, 5)]





### Revisando que comuna no está en la base
Delitos2010$COMUNA  %in% (Densidad_Viviendas_2016$des_comu %>% unique())   # Eliminar san pedro de la base delitos !!

Delitos2010 <- Delitos2010[-47, ]

##### Densidad viviendas 2017 #####

Densidad_2017 <- read_csv("Datos/Indicadores_Territoriales.csv") %>% 
  mutate_each(funs = tolower)

Densidad_2017$OBJECTID %>% unique() # Eliminar
Densidad_2017$REGION %>% unique()   # Eliminar

Densidad_2017 = Densidad_2017[, -c(1, 2)]

Delitos2010 = Delitos2010[Delitos2010$COMUNA %in% (Densidad_2017$COMUNA %>% unique()), ]


Densidad_Viviendas_2016 = Densidad_Viviendas_2016[ Densidad_Viviendas_2016$des_comu %in% Delitos2010$COMUNA, ]


##### Delitos todos ######

Delitos_todos <- read_excel("Datos/Delitos_todos.xlsx") %>% 
  mutate_each(funs = tolower)


Delitos_todos = Delitos_todos[Delitos_todos$COMUNA %in% Delitos2010$COMUNA,]



##### TOTAL 38 COMUNAS.



# dir.create("Datos_limpios")

# write.csv(Delitos_todos, file= "Datos_limpios/Delitos_todos_38com.csv", fileEncoding = "UTF-8")
# write.csv(Densidad_2017, file= "Datos_limpios/Densidad_2017.csv", fileEncoding = "UTF-8")
# write.csv(Densidad_Viviendas_2016, file= "Datos_limpios/Densidad_2016.csv", fileEncoding = "UTF-8")



