#**************************************************************************************#
#**************************************************************************************#
#
#                       Trabajo final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                              Decrementos Multiples
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

#Limpieza de memoria ----                                
rm(list = ls())

#Carga de paquetes y funciones ----

source("script/Funciones.R")  
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

#Carga de tabla de datos ----

lt_output <- fread("data/lt_output.csv")

# --- FUNCIÓN DECREMENTOS MÚLTIPLES ---
decrementos_multiples_sexo <- function(lt_output, years = c(2010, 2019, 2021)) {
  
  result_list <- list()
  
  for(yr in years) {
    for(s in c("m", "f")) {
      
      lt_year_sex <- lt_output[year == yr & sex == s]
      
      # Ordenar por edad
      setorder(lt_year_sex, age)
      
      # Calcular decrementos múltiples
      lx_vector <- numeric(nrow(lt_year_sex))
      lx_vector[1] <- 100000  # Radix
      
      # Calcular lx para edades subsiguientes
      for(i in 2:nrow(lt_year_sex)) {
        lx_vector[i] <- lx_vector[i-1] * (1 - lt_year_sex$qx[i-1])
      }
      
      lt_year_sex[, lx := lx_vector]
      
      # Calcular dx (defunciones)
      lt_year_sex[, dx := c(-diff(lx), lx[.N])]
      
      # Calcular n (intervalo de edad)
      n <- c(diff(lt_year_sex$age), NA)
      
      # Calcular ax (fracción del intervalo vivida por los que mueren)
      ax <- n/2
      
      # Ajustar ax para edades 0 y 1-4 según Coale-Demeny
      if(s == "m") {
        if(lt_year_sex$mx[1] >= 0.107) {
          ax[1] <- 0.330
          ax[2] <- 1.352
        } else {
          ax[1] <- 0.045 + 2.684 * lt_year_sex$mx[1]
          ax[2] <- 1.651 - 2.816 * lt_year_sex$mx[1]
        }
      } else {
        if(lt_year_sex$mx[1] >= 0.107) {
          ax[1] <- 0.350
          ax[2] <- 1.361
        } else {
          ax[1] <- 0.053 + 2.800 * lt_year_sex$mx[1]
          ax[2] <- 1.522 - 1.518 * lt_year_sex$mx[1]
        }
      }
      
      # Calcular Lx (años persona vividos)
      Lx <- numeric(nrow(lt_year_sex))
      for(i in 1:(nrow(lt_year_sex)-1)) {
        Lx[i] <- n[i] * lx_vector[i+1] + ax[i] * lt_year_sex$dx[i]
      }
      Lx[nrow(lt_year_sex)] <- lt_year_sex$lx[nrow(lt_year_sex)] / lt_year_sex$mx[nrow(lt_year_sex)]
      
      lt_year_sex[, Lx := Lx]
      
      # Calcular Tx (años persona vividos acumulados)
      Tx <- rev(cumsum(rev(Lx)))
      lt_year_sex[, Tx := Tx]
      
      # Calcular ex (esperanza de vida)
      lt_year_sex[, ex := Tx / lx]
      
      # Calcular px (probabilidad de supervivencia)
      lt_year_sex[, px := 1 - qx]
      
      result_list[[paste(yr, s, sep = "_")]] <- lt_year_sex[, .(
        year = yr,
        sex = s,
        age,
        n = c(diff(age), NA),
        mx,
        qx,
        px,
        ax,
        lx = round(lx, 0),
        dx = round(dx, 0),
        Lx = round(Lx, 0),
        Tx = round(Tx, 0),
        ex = round(ex, 2)
      )]
    }
  }
  
  # Combinar todos los resultados
  result <- rbindlist(result_list)
  return(result)
}

# Carga de datos ----
lt_output <- fread("data/lt_output.csv")

# Filtrar para años específicos
years_dm <- c(2010, 2019, 2021)
lt_dm <- lt_output[year %in% years_dm]

# Calcular decrementos múltiples por sexo y año ----
dm_result <- decrementos_multiples_sexo(lt_dm, years = years_dm)

# Verificar resultados
cat("=== ESPERANZA DE VIDA AL NACER ===\n")
print(dm_result[age == 0, .(year, sex, ex)])

# CREAR TABLAS SEPARADAS POR AÑO Y SEXO ----

# 2010 Hombres
dm_2010_hombres <- dm_result[year == 2010 & sex == "m", .(
  Edad = age,
  n,
  mx = round(mx, 6),
  qx = round(qx, 6),
  px = round(px, 6),
  ax = round(ax, 3),
  lx,
  dx,
  Lx,
  Tx,
  ex
)]

# 2010 Mujeres
dm_2010_mujeres <- dm_result[year == 2010 & sex == "f", .(
  Edad = age,
  n,
  mx = round(mx, 6),
  qx = round(qx, 6),
  px = round(px, 6),
  ax = round(ax, 3),
  lx,
  dx,
  Lx,
  Tx,
  ex
)]

# 2019 Hombres
dm_2019_hombres <- dm_result[year == 2019 & sex == "m", .(
  Edad = age,
  n,
  mx = round(mx, 6),
  qx = round(qx, 6),
  px = round(px, 6),
  ax = round(ax, 3),
  lx,
  dx,
  Lx,
  Tx,
  ex
)]

# 2019 Mujeres
dm_2019_mujeres <- dm_result[year == 2019 & sex == "f", .(
  Edad = age,
  n,
  mx = round(mx, 6),
  qx = round(qx, 6),
  px = round(px, 6),
  ax = round(ax, 3),
  lx,
  dx,
  Lx,
  Tx,
  ex
)]

# 2021 Hombres
dm_2021_hombres <- dm_result[year == 2021 & sex == "m", .(
  Edad = age,
  n,
  mx = round(mx, 6),
  qx = round(qx, 6),
  px = round(px, 6),
  ax = round(ax, 3),
  lx,
  dx,
  Lx,
  Tx,
  ex
)]

# 2021 Mujeres
dm_2021_mujeres <- dm_result[year == 2021 & sex == "f", .(
  Edad = age,
  n,
  mx = round(mx, 6),
  qx = round(qx, 6),
  px = round(px, 6),
  ax = round(ax, 3),
  lx,
  dx,
  Lx,
  Tx,
  ex
)]

# GRÁFICAS ----

# 1. ESPERANZA DE VIDA AL NACER
ggplot(dm_result[age == 0], aes(x = factor(year), y = ex, fill = sex)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.2f", ex)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("m" = "#1f77b4", "f" = "#d62728"),
                    labels = c("m" = "Hombres", "f" = "Mujeres")) +
  labs(title = "ESPERANZA DE VIDA AL NACER - DECREMENTOS MÚLTIPLES",
       subtitle = "Colima 2010, 2019, 2021",
       x = "Año", y = "Esperanza de Vida (años)",
       fill = "Sexo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5))

# 2. CURVAS DE SUPERVIVIENTES (lx) POR AÑO
for(yr in years_dm) {
  p <- ggplot(dm_result[year == yr], aes(x = age, y = lx, color = sex)) +
    geom_line(size = 1.2) +
    geom_point(size = 1) +
    scale_color_manual(values = c("m" = "#1f77b4", "f" = "#d62728"),
                       labels = c("m" = "Hombres", "f" = "Mujeres")) +
    labs(title = paste("CURVA DE SUPERVIVIENTES - AÑO", yr),
         subtitle = "Decrementos Múltiples - Colima",
         x = "Edad", y = "lx (sobrevivientes)",
         color = "Sexo") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  print(p)
}

# 3. PROBABILIDADES DE MUERTE (qx) POR AÑO
for(yr in years_dm) {
  p <- ggplot(dm_result[year == yr & age <= 85], aes(x = age, y = qx, color = sex)) +
    geom_line(size = 1) +
    geom_point(size = 0.8) +
    scale_y_log10(labels = scales::comma) +
    scale_color_manual(values = c("m" = "#1f77b4", "f" = "#d62728"),
                       labels = c("m" = "Hombres", "f" = "Mujeres")) +
    labs(title = paste("PROBABILIDAD DE MUERTE (qx) - AÑO", yr),
         subtitle = "Escala logarítmica - Colima",
         x = "Edad", y = "qx (probabilidad de muerte)",
         color = "Sexo") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  print(p)
}

# 4. ESPERANZA DE VIDA POR EDAD POR AÑO
for(yr in years_dm) {
  p <- ggplot(dm_result[year == yr], aes(x = age, y = ex, color = sex)) +
    geom_line(size = 1.2) +
    geom_point(size = 1) +
    scale_color_manual(values = c("m" = "#1f77b4", "f" = "#d62728"),
                       labels = c("m" = "Hombres", "f" = "Mujeres")) +
    labs(title = paste("ESPERANZA DE VIDA POR EDAD - AÑO", yr),
         subtitle = "Decrementos Múltiples - Colima",
         x = "Edad", y = "ex (esperanza de vida)",
         color = "Sexo") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  print(p)
}

# 5. COMPARATIVA INTERANUAL - HOMBRES
p_hombres <- ggplot(dm_result[sex == "m"], aes(x = age, y = ex, color = factor(year))) +
  geom_line(size = 1) +
  scale_color_manual(values = c("2010" = "#1f77b4", "2019" = "#ff7f0e", "2021" = "#2ca02c")) +
  labs(title = "EVOLUCIÓN ESPERANZA DE VIDA - HOMBRES",
       subtitle = "Comparativa 2010 vs 2019 vs 2021 - Colima",
       x = "Edad", y = "ex (esperanza de vida)",
       color = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p_hombres)

# 6. COMPARATIVA INTERANUAL - MUJERES
p_mujeres <- ggplot(dm_result[sex == "f"], aes(x = age, y = ex, color = factor(year))) +
  geom_line(size = 1) +
  scale_color_manual(values = c("2010" = "#d62728", "2019" = "#ff9896", "2021" = "#9467bd")) +
  labs(title = "EVOLUCIÓN ESPERANZA DE VIDA - MUJERES",
       subtitle = "Comparativa 2010 vs 2019 vs 2021 - Colima",
       x = "Edad", y = "ex (esperanza de vida)",
       color = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p_mujeres)

# RESUMEN ESTADÍSTICO ----
cat("\n=== RESUMEN DETALLADO DECREMENTOS MÚLTIPLES ===\n")

for(yr in years_dm) {
  cat(paste("\n--- AÑO", yr, "---\n"))
  
  for(s in c("m", "f")) {
    sexo_nombre <- ifelse(s == "m", "HOMBRES", "MUJERES")
    dm_sub <- dm_result[year == yr & sex == s]
    
    cat(paste("\n", sexo_nombre, ":\n"))
    cat(sprintf("  Esperanza de vida al nacer (e₀): %.2f años\n", dm_sub[age == 0]$ex))
    cat(sprintf("  Mortalidad infantil (q₀): %.6f\n", dm_sub[age == 0]$qx))
    cat(sprintf("  Supervivientes a los 65 años: %d\n", dm_sub[age == 65]$lx))
    cat(sprintf("  Esperanza de vida a los 65 años: %.2f años\n", dm_sub[age == 65]$ex))
  }
}

# Guardar también en CSV individuales
write.csv(dm_2010_hombres, "data/decrementos_hombres_2010.csv", row.names = FALSE)
write.csv(dm_2010_mujeres, "data/decrementos_mujeres_2010.csv", row.names = FALSE)
write.csv(dm_2019_hombres, "data/decrementos_hombres_2019.csv", row.names = FALSE)
write.csv(dm_2019_mujeres, "data/decrementos_mujeres_2019.csv", row.names = FALSE)
write.csv(dm_2021_hombres, "data/decrementos_hombres_2021.csv", row.names = FALSE)
write.csv(dm_2021_mujeres, "data/decrementos_mujeres_2021.csv", row.names = FALSE)
write.csv(dm_result, "data/decrementos_completo.csv", row.names = FALSE)

