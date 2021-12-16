# Librerías ---------------------------------------------------------------

library(dplyr)
library(gstat)
library(sp)
library(ggplot2)
library(sf)
library(tmap)
library(readr) 
library(readxl)
library(spdep)


# Bases -------------------------------------------------------------------

D2017 <- readxl::read_excel("Datos/Pob2017.xlsx")

# Cargo sf de las comunas
Chile <- sf::st_read("Datos/Comunas","comunas")

# Selecciono solo los de la RM
RM <- Chile %>% 
  dplyr::filter(Region == "Región Metropolitana de Santiago")

# Dejo con el mismo nombre la columna
RM <- RM %>% 
  rename(COMUNA = Comuna)

# Entrecruzo
D2017 <- RM %>% 
  left_join(D2017)

# Cantidad de habitantes por vivienda
D2017 <- D2017 %>% 
  mutate(Pob_Viv = POB/VIV)


# Autocorrelación ---------------------------------------------------------

# Defino los vecinos
vecinos <- poly2nb(D2017)

# Hago las matrices de pesos
b_pesos <- nb2listw(vecinos, style ="B", zero.policy = TRUE)
w_pesos <- nb2listw(vecinos, style ="W", zero.policy = TRUE)

ver_b_pesos <- listw2mat(b_pesos)
ver_w_pesos <- listw2mat(w_pesos)

### Hacinamiento
moran.test(D2017$HACI, b_pesos, zero.policy = TRUE)
moran.plot(D2017$HACI, b_pesos, zero.policy = TRUE,
           ylab = "Spatially Lagged Hacinamiento",
           xlab = "% Hacinamiento",
           main = "Moran Test")

moran.test(D2017$HACI, w_pesos, zero.policy = TRUE)
moran.plot(D2017$HACI, w_pesos, zero.policy = TRUE,
           ylab = "Spatially Lagged Hacinamiento",
           xlab = "% Hacinamiento",
           main = "Moran Test")

geary.test(D2017$HACI, b_pesos, zero.policy = TRUE)
geary.test(D2017$HACI, w_pesos, zero.policy = TRUE)

# Monte Carlo
moran.mc(D2017$HACI, b_pesos, zero.policy = TRUE, nsim = 1000)
moran.mc(D2017$HACI, w_pesos, zero.policy = TRUE, nsim = 1000)
geary.mc(D2017$HACI, b_pesos, zero.policy = TRUE, nsim = 1000)
geary.mc(D2017$HACI, w_pesos, zero.policy = TRUE, nsim = 1000)

# Mapa

mapview::mapview(D2017["HACI"],
                 layer.name = "% Hacinamiento",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , D2017$COMUNA
                   , "<br><b>% Hacinamiento: </b>"
                   , D2017$HACI
                 ))

### Indice 
moran.test(D2017$Pob_Viv, b_pesos, zero.policy = TRUE)
moran.plot(D2017$Pob_Viv, b_pesos, zero.policy = TRUE,
           ylab = "Spatially Lagged Hacinamiento",
           xlab = "% Hacinamiento",
           main = "Moran Test")

moran.test(D2017$Pob_Viv, w_pesos, zero.policy = TRUE)
moran.plot(D2017$Pob_Viv, w_pesos, zero.policy = TRUE,
           ylab = "Spatially Lagged Hacinamiento",
           xlab = "% Hacinamiento",
           main = "Moran Test")

geary.test(D2017$Pob_Viv, b_pesos, zero.policy = TRUE)
geary.test(D2017$Pob_Viv, w_pesos, zero.policy = TRUE)

# Monte Carlo
moran.mc(D2017$Pob_Viv, b_pesos, zero.policy = TRUE, nsim = 1000)
moran.mc(D2017$Pob_Viv, w_pesos, zero.policy = TRUE, nsim = 1000)
geary.mc(D2017$Pob_Viv, b_pesos, zero.policy = TRUE, nsim = 1000)
geary.mc(D2017$Pob_Viv, w_pesos, zero.policy = TRUE, nsim = 1000)

# Mapa

mapview::mapview(D2017["Pob_Viv"],
                 layer.name = "Hab por Viv",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , D2017$COMUNA
                   , "<br><b>% Hab/Viv: </b>"
                   , D2017$Pob_Viv
                 ))

