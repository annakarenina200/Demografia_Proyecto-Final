#**************************************************************************************#
#**************************************************************************************#
#
#                       Trabajo final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                   Descomposición de cambios en esperanza de vida
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

# Carga de paquetes y funciones ----
source("script/Funciones.R")  
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Carga de datos ----
lt_output <- fread("data/lt_output.csv")

# Preparar datos para descomposición ----

# Filtrar y preparar tablas de mortalidad para cada año y sexo

# Hombres 2010
lt_h2010 <- lt_output[sex == "m" & year == 2010, 
                      .(age, lx, nLx = Lx, Tx, ex)]

# Hombres 2019  
lt_h2019 <- lt_output[sex == "m" & year == 2019,
                      .(age, lx, nLx = Lx, Tx, ex)]

# Hombres 2021
lt_h2021 <- lt_output[sex == "m" & year == 2021,
                      .(age, lx, nLx = Lx, Tx, ex)]

# Mujeres 2010
lt_f2010 <- lt_output[sex == "f" & year == 2010,
                      .(age, lx, nLx = Lx, Tx, ex)]

# Mujeres 2019
lt_f2019 <- lt_output[sex == "f" & year == 2019,
                      .(age, lx, nLx = Lx, Tx, ex)]

# Mujeres 2021
lt_f2021 <- lt_output[sex == "f" & year == 2021,
                      .(age, lx, nLx = Lx, Tx, ex)]

# Realizar descomposiciones ----

# Hombres 2010-2019
desc_h1019 <- arriaga_decomp(lt_h2010, lt_h2019)

# Hombres 2019-2021  
desc_h1921 <- arriaga_decomp(lt_h2019, lt_h2021)

# Mujeres 2010-2019
desc_f1019 <- arriaga_decomp(lt_f2010, lt_f2019)

# Mujeres 2019-2021
desc_f1921 <- arriaga_decomp(lt_f2019, lt_f2021)

# Crear tablas resumen ----

crear_tabla_resumen <- function(desc, periodo) {
  tabla <- desc$descomposicion
  tabla[, periodo := periodo]
  tabla[, grupo_edad := ifelse(age == 0, "0", 
                               ifelse(age == 1, "1-4",
                                      ifelse(age == 85, "85+", 
                                             paste0(age, "-", age+4))))]
  
  # Agrupar por grupos de edad estándar
  tabla_agrupada <- tabla[, .(contribucion = sum(contribucion),
                              porcentaje = sum(porcentaje)),
                          by = .(periodo, grupo_edad)]
  
  return(tabla_agrupada)
}

# Consolidar todas las descomposiciones
tabla_desc <- rbind(
  crear_tabla_resumen(desc_h1019, "Hombres 2010-2019"),
  crear_tabla_resumen(desc_h1921, "Hombres 2019-2021"),
  crear_tabla_resumen(desc_f1019, "Mujeres 2010-2019"), 
  crear_tabla_resumen(desc_f1921, "Mujeres 2019-2021")
)

# Resumen de diferencias en esperanza de vida
resumen_ex <- data.table(
  periodo = c("Hombres 2010-2019", "Hombres 2019-2021", 
              "Mujeres 2010-2019", "Mujeres 2019-2021"),
  ex_inicial = c(desc_h1019$ex1, desc_h1921$ex1,
                 desc_f1019$ex1, desc_f1921$ex1),
  ex_final = c(desc_h1019$ex2, desc_h1921$ex2,
               desc_f1019$ex2, desc_f1921$ex2),
  diferencia = c(desc_h1019$diferencia_total, desc_h1921$diferencia_total,
                 desc_f1019$diferencia_total, desc_f1921$diferencia_total)
)

# Visualizaciones ----

# Gráfico 1: Diferencias en esperanza de vida
ggplot(resumen_ex, aes(x = periodo, y = diferencia, fill = diferencia > 0)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f", diferencia)), 
            vjust = ifelse(resumen_ex$diferencia > 0, -0.5, 1.2),
            size = 3) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    guide = "none") +
  labs(title = "Cambio en Esperanza de Vida al Nacer",
       subtitle = "Descomposición de Arriaga - Colima",
       x = "Período y Sexo",
       y = "Diferencia en Años") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 2: Contribuciones por edad para cada período
ggplot(tabla_desc, aes(x = grupo_edad, y = contribucion, fill = contribucion > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ periodo, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    guide = "none") +
  labs(title = "Contribución por Grupo de Edad al Cambio en Esperanza de Vida",
       subtitle = "Método de Arriaga",
       x = "Grupo de Edad",
       y = "Contribución (Años)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Comparación hombres vs mujeres
tabla_comparativa <- tabla_desc %>%
  mutate(sexo = ifelse(grepl("Hombres", periodo), "Hombres", "Mujeres"),
         años = ifelse(grepl("2010-2019", periodo), "2010-2019", "2019-2021"))

ggplot(tabla_comparativa, aes(x = grupo_edad, y = contribucion, fill = sexo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~ años, ncol = 2) +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "firebrick")) +
  labs(title = "Comparación de Contribuciones por Sexo",
       subtitle = "Descomposición de Arriaga",
       x = "Grupo de Edad", 
       y = "Contribución (Años)",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar resultados ----
write.csv(tabla_desc, "data/arriaga_descomposicion.csv", row.names = FALSE)
write.csv(resumen_ex, "data/arriaga_resumen_ex.csv", row.names = FALSE)

# Imprimir resumen en consola
cat("=== RESUMEN DESCOMPOSICIÓN ARRIAGA ===\n")
print(resumen_ex)

cat("\n=== CONTRIBUCIONES PRINCIPALES ===\n")
for(periodo in unique(tabla_desc$periodo)) {
  cat("\n", periodo, ":\n")
  temp <- tabla_desc[periodo == periodo]
  mayores_contrib <- temp[order(-abs(contribucion))][1:3]
  print(mayores_contrib[, .(grupo_edad, contribucion, porcentaje)])
}

# -------- FIN ----------