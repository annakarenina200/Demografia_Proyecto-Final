lt_abr <- function(x, mx, sex="f", IMR=NA){
  
  m <- length(x)
  n <- c(diff(x), NA)  
  
  ax <- n/2    
  
  # Pag. 4 notas de clase - cuadro
  
  ## Coale y Demeny edades 0 a 1
  
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[1] <- 0.330 }else{
      ax[1] <- 0.045+2.684*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[1] <- 0.350 }else{
      ax[1] <- 0.053+2.800*mx[1]
    }  
  }
  
  ## Coale y Demeny edades 1 a 4
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[2] <- 1.352 }else{
      ax[2] <- 1.651-2.816*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[2] <- 1.361 }else{
      ax[2] <- 1.522-1.518*mx[1]
    }  
  }
  
  # Probabilidad de muerte
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[m] <- 1
  
  # Proba de sobrevivir
  px <- 1-qx
  
  # l_x
  lx <- 100000 * cumprod(c(1,px[-m]))
  
  # Defunciones
  dx <- c(-diff(lx), lx[m])
  
  # Años persona vividos
  Lx <- n* c(lx[-1], 0) + ax*dx
  Lx[m] <- lx[m]/mx[m]
  
  # Años persona vividos acumulados
  
  Tx <- rev(cumsum(rev(Lx)))
  
  # Esperanza de vida
  ex <- Tx/lx
  
  return(data.table(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex))
  
  
}

# Uso la función lt_abr
# lt_abr(x, mx)



# 3. Crecimiento exponencial ----

expo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- log(N_T/N_0)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * exp(r*h)  
  
  return(N_h)
  
}

# Descomposición por edad de la diferencia de la e_0 entre períodos----

# Descomposición de Arriaga ----
arriaga_decomp <- function(lt1, lt2, edad_final = 85) {
  # lt1 y lt2 son data.tables con las tablas de mortalidad
  # Deben tener las columnas: age, lx, nLx, Tx, ex
  
  # Filtrar hasta la edad final
  lt1 <- lt1[age <= edad_final]
  lt2 <- lt2[age <= edad_final]
  
  # Ordenar por edad
  setorder(lt1, age)
  setorder(lt2, age)
  
  # Inicializar vector de contribuciones
  n <- nrow(lt1)
  contribuciones <- numeric(n)
  
  # Calcular contribuciones para cada grupo de edad (fórmula de Arriaga)
  for(i in 1:(n-1)) {
    term1 <- (lt1$lx[i] / lt1$lx[1]) * 
      ((lt2$nLx[i] / lt2$lx[i]) - (lt1$nLx[i] / lt1$lx[i]))
    
    term2 <- (lt2$Tx[i+1] / lt1$lx[1]) * 
      ((lt1$lx[i] / lt1$lx[i]) - (lt1$lx[i+1] / lt1$lx[i]))
    
    contribuciones[i] <- term1 + term2
  }
  
  # Último grupo de edad (85+)
  contribuciones[n] <- (lt1$lx[n] / lt1$lx[1]) * 
    ((lt2$Tx[n] / lt2$lx[n]) - (lt1$Tx[n] / lt1$lx[n]))
  
  # Crear data.table de resultados
  resultado <- data.table(
    age = lt1$age,
    contribucion = contribuciones,
    porcentaje = (contribuciones / sum(contribuciones)) * 100
  )
  
  # Diferencia total en esperanza de vida
  diff_ex <- lt2$ex[1] - lt1$ex[1]
  
  return(list(
    descomposicion = resultado,
    diferencia_total = diff_ex,
    ex1 = lt1$ex[1],
    ex2 = lt2$ex[1]
  ))
}

# Función para calcular decrementos múltiples por sexo y año
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

