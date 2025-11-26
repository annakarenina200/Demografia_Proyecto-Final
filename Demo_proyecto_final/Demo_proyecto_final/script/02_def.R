#**************************************************************************************#
#**************************************************************************************#
#
#                       Trabajo final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                                   Defunciones
#                             
#
#         Creado por:               Ingrid Dayana Martinez M.
#                                   Ana Karen Morelos B.
#         Fecha de creación:        11/11/2025
#         Actualizado por:          Ingrid Dayana Martinez M.
#                                   Ana Karen Morelos B.
#         Fecha de actualización:   14/11/2025
#         Contacto:                 ingriddayana@ciencias.unam.mx
#                                   annakareninabojo@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#
# Preámbulo ----

#Limpieza de gráficos ----
graphics.off()

#Limpieza de memoria ----                                
rm(list = ls())

#Carga de paquetes y funciones----
source("script/Funciones.R")  
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)


## Carga de tablas de datos ----
def_pro <- fread("data/def_pro.csv") %>%
  .[year %in% c(2009, 2010, 2011, 2019, 2021)]

## Calculo del promedio para el año de referencia 
def_pro[ , year_new := ifelse ( year %in% 2009:2011, 2010,
                                ifelse (year %in% 2018:2019, 2019, year))]

#Datos preparados de defunciones 
def <- 
  def_pro[ ,
           .(deaths = mean ( deaths )),
           .(year = year_new, sex, age)]

#Guardar tabla de defunciones
write.csv(def, "data/def.csv" , row.names = F)

#-----FIN--------#

