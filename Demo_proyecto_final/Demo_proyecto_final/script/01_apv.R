#**************************************************************************************#
#**************************************************************************************#
#
#                       Trabajo final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                       Tabla de mortalidad México 2010, 2020
#                             
#
#         Creado por:               Ingrid Dayana Martinez M.
#                                   Ana Karen Morelos B.
#         Fecha de creación:        04/11/2025
#         Actualizado por:          Ingrid Dayana Martinez M.
#                                   Ana Karen Morelos B.
#         Fecha de actualización:   06/11/2025
#         Contacto:                 ingriddayana@ciencias.unam.mx
#                                   annakareninabojo@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#
# Preámbulo ----

#Limpieza de gráficos
graphics.off()

#Limpieza de memoria----                                
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
censos_pro <- fread("data/censos_pro.csv")

# Cálculo de APV 2010 (población a mitad de año) ----

N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2010.5)

apv2010 <- censos_pro[year==2010, .(age, sex, N)]
apv2010[,year := 2010]

ggplot(apv2010, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2010",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

# Cálculo de APV 2019 ---- 

N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2019.5)

apv2019 <- censos_pro[year==2010, .(age, sex, N)]
apv2019[,year := 2019]

# Cálculo de APV 2020 (población a mitad de año) ----

N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2020.5)

apv2020 <- censos_pro[year==2020, .(age, sex, N)]
apv2020[,year := 2019]

ggplot(apv2020, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2019",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

# Cálculo de APV 2021 ----

N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2021.5)

apv2021 <- censos_pro[year==2020, .(age, sex, N)]
apv2021[,year := 2021]


#Consolidar tablas 2010 y 2020
apv <- rbind(apv2010, apv2019, apv2021)

# Pirámide Poblacional Comparativa 2010 vs 2020 ----
ggplot(apv, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), 
                fill = interaction(factor(year), sex))) +
  geom_col(width = 0.9, alpha = 0.8, position = "dodge") +  # Aumentado a 0.9
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("2010.male" = "#1f77b4", "2010.female" = "#d62728",
               "2019.male" = "#aec7e8", "2019.female" = "#ff9896"),
    labels = c("2010 Hombres", "2010 Mujeres", "2019 Hombres", "2019 Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional Comparativa: 2010 vs 2019",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Año y Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

# Pirámide Poblacional con Facetas ----
ggplot(apv, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  facet_wrap(~year, ncol = 2, labeller = labeller(year = c("2010" = "Año 2010", "2019" = "Año 2019"))) +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámides Poblacionales: 2010 vs 2019",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

#Guardar tabla de APV ----
write.csv(apv, "data/apv.csv")


#-----FIN--------#
