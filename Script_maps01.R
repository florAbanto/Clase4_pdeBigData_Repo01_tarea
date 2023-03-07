#### Librerias Iniciales ####

# Limpiamos memoria 
rm(list = ls())
library(tidyverse)
library(sf)
library(ggrepel)
#library(tmap)

#### Datos ####
# Definamos una estructura de directorios , tanto para inputs 
# como para outputs
wd <- list()
# 
wd$root <- "C:/Users/Usuario/Desktop/clase4_depBigData2023/Clase4_pdeBigData_Repo01_tarea/"
wd$inputs <- paste0(wd$root, "01_inputs/01_inputs/")
wd$shapef <- paste0(wd$inputs, "shapefiles/")
wd$datasets <- paste0(wd$inputs, "datasets/")
wd$outputs <- paste0(wd$root, "02_outputs/")

# Carguemos en memoria la informacion espacial 
peru_sf <- st_read(paste0(wd$shapef, "INEI_LIMITE_DEPARTAMENTAL.shp"))

#### Primer Mapa Base ####
ggplot(data = peru_sf)+
  geom_sf()

# Guardar en el directorio de outputs este gráfico 
ggsave(filename = paste0(wd$outputs, "MapaBasePeru.png"),
                         width = 8.5,
                         height = 11)

# LIsta de Departamentos 
unique(peru_sf$NOMBDEP)

#### Mapa de Lima ####
ggplot(data = peru_sf %>% 
         filter(NOMBDEP == "LIMA"))+
  geom_sf(fill = "gray", color = "black", alpha = 0.7)
# 
# Guardamos en disco duro 
ggsave(filename = paste0(wd$outputs, "mapalima.png"),
       width = 8, height = 8)

#### Calculo de centroides ####
peru_sf <- peru_sf %>% mutate(centroid = map(geometry, st_centroid),
                              coords = map(centroid, st_coordinates),
                              coords_x = map_dbl(coords, 1),
                              coords_y = map_dbl(coords, 2)
                              )

#### Se asigna los nombres a cada dpto ####
ggplot(data = peru_sf)+
  geom_sf(fill = "green", color = "black", alpha = 0.7)+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP),
                  size = 2)
# 
# Guarda en disco duro 
ggsave(filename = paste0(wd$outputs, "mapaPeru_Centroide.png"),
       width = 8.5,
       height =11)

#### Carga la información de los fallecidos segun sinadef ####
# Información de los fallecidos

fallecidos <- read.csv(paste0(wd$datasets, "fallecidos_sinadef.csv" ), sep = "|")

unique(fallecidos$AÑO)

unique(fallecidos$SEXO)

unique(fallecidos$ESTADO.CIVIL)

#Selecciono los datos de la tabla de fallecidos: Departamento, Año, Sexo, Estado civil
# renombro el nombre de departamento.domicilio con el mismo nombre de la tabla peru_sf 
# para unirlos

fallecidos_1 <- select(fallecidos, DEPARTAMENTO.DOMICILIO, AÑO, SEXO, ESTADO.CIVIL)

fallecidos_1 <- rename(fallecidos_1, NOMBDEP = DEPARTAMENTO.DOMICILIO, ESTADOCIVIL = ESTADO.CIVIL)

unique(fallecidos_1$SEXO)

# Se junta la tabla peru_sf con la tabla de fallecidos_1
peru_fallecidos <- peru_sf %>% 
  left_join(fallecidos_1)



#### GRAFICO 1: Fallecidos por sexo y por departamento ####





#### GRAFICO 2: Fallecidos por año y por departamento ####




#### GRAFICO 3: Fallecidos por estadocivil y por departamento ####



































