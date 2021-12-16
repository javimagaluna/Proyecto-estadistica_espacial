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
library(spatialreg)



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

# Base delitos

Del <- readxl::read_excel("Datos/Delitos_todos.xlsx")

# Arreglar el nombre de la comuna
Del$COMUNA[Del$COMUNA == "Caracaví"] <- "Curacaví"

D2017_Del <- D2017 %>% 
  mutate(COMUNA = tolower(D2017$COMUNA))

D2017_Del <- D2017 %>% 
  left_join(Del)
  
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
           ylab = "Spatially Lagged Hab por Viv",
           xlab = "% Hacinamiento",
           main = "Moran Test")

moran.test(D2017$Pob_Viv, w_pesos, zero.policy = TRUE)
moran.plot(D2017$Pob_Viv, w_pesos, zero.policy = TRUE,
           ylab = "Spatially Lagged Hab por Viv",
           xlab = "% Hacinamiento",
           main = "Moran Test")
?moran.plot

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

### Indice MASCU
moran.test(D2017$MASCU, b_pesos, zero.policy = TRUE)
geary.test(D2017$MASCU, b_pesos, zero.policy = TRUE)
mapview::mapview(D2017["MASCU"],
                 layer.name = "Índice Masculinidad",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , D2017$COMUNA
                   , "<br><b>Índice Masculinidad: </b>"
                   , D2017$Pob_Viv
                 ))

### Escolaridad
moran.test(D2017$ESC, b_pesos, zero.policy = TRUE)
geary.test(D2017$ESC, b_pesos, zero.policy = TRUE)
mapview::mapview(D2017["ESC"],
                 layer.name = "Escolaridad",
                 legend = TRUE,
                 popup = paste0(
                   "<b>COMUNA: </b>"
                   , D2017$COMUNA
                   , "<br><b>Escolaridad: </b>"
                   , D2017$Pob_Viv
                 ))

### Delito VIO.MUJ

unique(D2017_Del$ANIO)
## 2010
D2017_Del_2010 <- D2017_Del %>% 
  filter(ANIO == 2010)

moran.test(D2017_Del_2010$VIO.MUJ, b_pesos, zero.policy = TRUE) 
moran.test(D2017_Del_2010$VIO.MUJ, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2010$VIO.MUJ, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2010$VIO.MUJ, w_pesos, zero.policy = TRUE)

### 2011
D2017_Del_2011 <- D2017_Del %>% 
  filter(ANIO == 2011)

moran.test(D2017_Del_2011$VIO.MUJ, b_pesos, zero.policy = TRUE) # A 0.1
moran.test(D2017_Del_2011$VIO.MUJ, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2011$VIO.MUJ, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2011$VIO.MUJ, w_pesos, zero.policy = TRUE)

### Delito VIO.NIN
## 2010
moran.test(D2017_Del_2010$VIO.NIN, b_pesos, zero.policy = TRUE)
moran.test(D2017_Del_2010$VIO.NIN, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2010$VIO.NIN, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2010$VIO.NIN, w_pesos, zero.policy = TRUE)

### 2011
moran.test(D2017_Del_2011$VIO.MUJ, b_pesos, zero.policy = TRUE) # A 0.1
moran.test(D2017_Del_2011$VIO.MUJ, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2011$VIO.MUJ, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2011$VIO.MUJ, w_pesos, zero.policy = TRUE)

### Delito VIO.ADUlT
## 2010
moran.test(D2017_Del_2010$VIO.ADULT, b_pesos, zero.policy = TRUE)
moran.test(D2017_Del_2010$VIO.ADULT, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2010$VIO.ADULT, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2010$VIO.ADULT, w_pesos, zero.policy = TRUE)

### 2011
moran.test(D2017_Del_2011$VIO.ADULT, b_pesos, zero.policy = TRUE)
moran.test(D2017_Del_2011$VIO.ADULT, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2011$VIO.ADULT, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2011$VIO.ADULT, w_pesos, zero.policy = TRUE)

### Delito ALCOHOL
## 2010
moran.test(D2017_Del_2010$ALCOHOL, b_pesos, zero.policy = TRUE)
moran.test(D2017_Del_2010$ALCOHOL, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2010$ALCOHOL, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2010$ALCOHOL, w_pesos, zero.policy = TRUE)


### Delito ABUSO
## 2010
moran.test(D2017_Del_2010$ABUSO, b_pesos, zero.policy = TRUE) # Este sí
moran.test(D2017_Del_2010$ABUSO, w_pesos, zero.policy = TRUE) # A 0.1

geary.test(D2017_Del_2010$ABUSO, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2010$ABUSO, w_pesos, zero.policy = TRUE)

### 2011
moran.test(D2017_Del_2011$ABUSO, b_pesos, zero.policy = TRUE) # A 0.1
moran.test(D2017_Del_2011$ABUSO, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2011$VIO.MUJ, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2011$VIO.MUJ, w_pesos, zero.policy = TRUE)

### Delito RUIDO
## 2010
moran.test(D2017_Del_2010$RUIDO, b_pesos, zero.policy = TRUE)
moran.test(D2017_Del_2010$RUIDO, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2010RUIDO, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2010$RUIDO, w_pesos, zero.policy = TRUE)

### 2011
moran.test(D2017_Del_2011$RUIDO, b_pesos, zero.policy = TRUE)
moran.test(D2017_Del_2011$RUIDO, w_pesos, zero.policy = TRUE)

geary.test(D2017_Del_2011$RUIDO, b_pesos, zero.policy = TRUE)
geary.test(D2017_Del_2011$RUIDO, w_pesos, zero.policy = TRUE)

# Modelo ------------------------------------------------------------------

# Defino los vecinos
vecinos2 <- poly2nb(D2017_Del)

# Hago las matrices de pesos
b_pesos2 <- nb2listw(vecinos2, style ="B", zero.policy = TRUE)
w_pesos2 <- nb2listw(vecinos2, style ="W", zero.policy = TRUE)

ver_b_pesos2 <- listw2mat(b_pesos2)
ver_w_pesos2 <- listw2mat(w_pesos2)

## MODELO LINEAL
mod_lin <- lm(ABUSO ~ HACI + MASCU + ESC + Pob_Viv, data = D2017_Del)
summary(mod_lin)
AIC(mod_lin)
plot(residuals(mod_lin))

## MODELO CAR

mod_CAR <- spautolm(ABUSO ~ HACI + MASCU + ESC + Pob_Viv, data = D2017_Del, listw = b_pesos2, family = "CAR")
summary(mod_CAR)
shapiro.test(residuals(mod_CAR))
plot(residuals(mod_CAR))

## MODELO SAR

mod_SAR <- spautolm(ABUSO ~ HACI + MASCU + ESC + Pob_Viv, data = D2017_Del, listw = b_pesos2, family = "SAR")
summary(mod_SAR)
plot(residuals(mod_SAR))

###

## MODELO LINEAL
mod_lin <- lm(ABUSO ~ HACI + ESC + Pob_Viv, data = D2017_Del)
summary(mod_lin)
AIC(mod_lin)
plot(residuals(mod_lin))

## MODELO CAR

mod_CAR <- spautolm(ABUSO ~ HACI + ESC + Pob_Viv, data = D2017_Del, listw = b_pesos2, family = "CAR")
summary(mod_CAR)
plot(residuals(mod_CAR))
shapiro.test(residuals(mod_CAR))

## MODELO SAR

mod_SAR <- spautolm(ABUSO ~ HACI + ESC + Pob_Viv, data = D2017_Del, listw = b_pesos2, family = "SAR")
summary(mod_SAR)
plot(residuals(mod_SAR))

# MODELO CAR TIENE MENOR AIC, TODOS AL SACAR MASC DISMINUYEN AIC

## MODELO LINEAL
mod_lin <- lm(ABUSO ~ HACI + Pob_Viv, data = D2017_Del)
summary(mod_lin)
AIC(mod_lin)
plot(residuals(mod_lin))

## MODELO CAR

mod_CAR <- spautolm(ABUSO ~ HACI + Pob_Viv, data = D2017_Del, listw = b_pesos2, family = "CAR")
summary(mod_CAR)
plot(residuals(mod_CAR))
shapiro.test(residuals(mod_CAR))

## MODELO SAR

mod_SAR <- spautolm(ABUSO ~ HACI + Pob_Viv, data = D2017_Del, listw = b_pesos2, family = "SAR")
summary(mod_SAR)
plot(residuals(mod_SAR))

# MODELO CAR ES EL MEJOR, LM AUMENTA AL SACAR ESC

## MODELO LINEAL
mod_lin <- lm(ABUSO ~ HACI, data = D2017_Del)
summary(mod_lin)
AIC(mod_lin)
plot(residuals(mod_lin))


## MODELO CAR

mod_CAR <- spautolm(ABUSO ~ HACI, data = D2017_Del, listw = b_pesos2, family = "CAR")
summary(mod_CAR)
plot(residuals(mod_CAR))
shapiro.test(residuals(mod_CAR))
?shapiro.test


## MODELO SAR

mod_SAR <- spautolm(ABUSO ~ HACI, data = D2017_Del, listw = b_pesos2, family = "SAR")
summary(mod_SAR)
plot(residuals(mod_SAR))

# MODELO CAR ES EL MEJOR, TODOS AUMENTAN AL SACAR Pob_Viv
