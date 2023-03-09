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
#Solo voy a tomar las columnas DEPARTAMENTO.DOMICILIO, AÑO, SEXO, ESTADO.CIVIL
fallecidos <- fallecidos [,c(3, 4, 6, 10,11,15)]

unique(fallecidos$AÑO)

unique(fallecidos$SEXO)

unique(fallecidos$ESTADO.CIVIL)

#Voy agrupar el número de fallecidos por departamento de los años  desde 2017 al 2023

total_fa <- fallecidos %>%
  group_by(DEPARTAMENTO.DOMICILIO) %>%
  dplyr::summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

#Luego quiero ver los 10 TOP fallecidos
top_10 <- total_fa %>%
  top_n(wt = count, n = 10) %>%
  arrange(desc(count)) %>%
  mutate(DEPARTAMENTO.DOMICILIO = factor(DEPARTAMENTO.DOMICILIO, levels = DEPARTAMENTO.DOMICILIO))

# grafico de barras de los TOP 10 fallecidos entre [2017:2023]
g_top10 <- ggplot(data = top_10, aes(x = DEPARTAMENTO.DOMICILIO, y= count)) +
  geom_col() +
  labs(x = '',
       y = 'Fallecidos',
       caption = 'Datos de fallecido del Sinadef') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle(label = 'Los 10 Departamentos con mayor fallecidos en el Perú ')
  ggsave(filename = paste0(wd$outputs, "Los10departamentos_con_mayor_nro_fallecidos.png"),
         width = 8.5,
         height =11)

#Ahora adiciono al mapa el total de fallecidos por departamento
peru_fa <- peru_sf %>% 
  left_join(total_fa, by =c('NOMBDEP' = 'DEPARTAMENTO.DOMICILIO'))

#GRAFICO del Perú con el numero de fallecidos entre el 2017 al 2023
ggplot(peru_fa)+
  geom_sf(aes(fill = count))+
  labs(title = "Fallecidos entre 2017 al 2023",
       caption = "Fuente de datos : SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Numero de Fallecidos")
#  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP),
#                  size = 2)
ggsave(paste0(wd$outputs, "MapaFallecidos_2017_al2023.png"),
       width = 8.5, height = 11)

#Vamos a identificar cuál es el Año que ha tenido mayor cantidad de fallecidos.
fa_ano <- fallecidos %>%
  group_by(AÑO) %>%
  dplyr::summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

#GRAFICO DE BARRAS: Fallecidos por Año
g_faano <- ggplot(data = fa_ano, aes(x = AÑO, y= count)) +
  geom_col() +
  labs(x = '',
       y = 'Fallecidos',
       caption = 'Datos de Fallecido por Año') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle(label = 'Los fallecidos por Año ')
ggsave(filename = paste0(wd$outputs, "Total_Fallecidos_por_año.png"),
       width = 8.5,
       height =11)

#Vamos a analizar el numero de fallecidos por Sexo entre los años 2017 al 2023
fa_sexo <- fallecidos %>%
  group_by(SEXO) %>%
  dplyr::summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

#GRAFICO DE BARRAS: Cantidad de fallecidos por sexo
g_fasexo <- ggplot(data = fa_sexo, aes(x = SEXO, y= count)) +
  geom_col() +
  labs(x = '',
       y = 'Fallecidos',
       caption = 'Datos de Fallecido por Sexo') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle(label = 'Los fallecidos por Sexo')
ggsave(filename = paste0(wd$outputs, "Total_Fallecidos_sexo.png"),
       width = 8.5,
       height =11)

#GRAFICO MAPA PERU: Total de Fallecidos y cuál es el total de Mujeres y el total de Hombre entre el 2017 al 2023
#Agrupo los datos para las mujeres y hombres
fa_mujer <- fallecidos %>%
  filter(SEXO=='FEMENINO')%>%
  group_by(DEPARTAMENTO.DOMICILIO) %>%
  dplyr::summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) 

fa_hombres <- fallecidos %>%
  filter(SEXO=='MASCULINO')%>%
  group_by(DEPARTAMENTO.DOMICILIO) %>%
  dplyr::summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) 

#Ahora adiciono al mapa el total de fallecidos de mujeres y de hombres
peru_fat <- peru_fa %>% 
  left_join(fa_mujer, by =c('NOMBDEP' = 'DEPARTAMENTO.DOMICILIO')) %>%
  left_join(fa_hombres, by =c('NOMBDEP' = 'DEPARTAMENTO.DOMICILIO'))

#GRAFICO MAPA
ggplot(peru_fat)+
  geom_sf(mapping = aes(fill = count.y))+
  geom_point(aes(x = coords_x , y = coords_y, size = count), color = "darkseagreen")+
  labs(title = "Fallecimiento de Mujeres y Hombres ",
       x = "Longitud",
       y = "Latitud",
       caption = "Fuente : SINADEF",
       fill = "Numero mujeres",
       size = "numero hombres")+
  geom_text_repel(data = peru_fat %>% 
                    filter(count.y > 10000),
                  mapping = aes(x = coords_x, y = coords_y, label = NOMBDEP),
                  size = 2,
                  max.overlaps = Inf
  )
ggsave(filename = paste0(wd$outputs, "MapaPeruFallecidosSexo.png"),
       width = 8.5,
       height = 11)










































