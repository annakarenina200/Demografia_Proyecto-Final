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

c2010 <- read_xlsx("data/INEGI_censo2010.xlsx", sheet = 1, 
                   range = "A7:D31")
c2020 <- read_xlsx("data/INEGI_censo2020.xlsx", sheet = 1, 
                   range = "A7:D31")


# Preprocesamiento censo 2010 ----

c2010 <- read_xlsx("data/INEGI_censo2010.xlsx", sheet = 1, 
                   range = "A7:D31")

names(c2010) <- c("age", "tot", "male", "female")
setDT(c2010)

c2010 <- c2010[-1 , ] 

c2010[ , age := gsub("De ", "", age)]
c2010[ , age := substr(age, 1, 2)]
c2010[age=="No", age:=NA]
c2010[ , age:=as.numeric(age)]

c2010[ , tot := as.numeric(gsub(",", "", tot))]
c2010[ , male := as.numeric(gsub(",", "", male))]
c2010[ , female := as.numeric(gsub(",", "", female))]

c2010 <- c2010[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

c2010 <- melt.data.table(c2010, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

sum(c2010$pop)

c2010[ , year:=2010]



# Preprocesamiento censo 2020

c2020 <- read_xlsx("data/INEGI_censo2020.xlsx", sheet = 1, 
                   range = "A7:D31")

names(c2020) <- c("age", "tot", "male", "female")
setDT(c2020)

c2020 <- c2020[-1 , ] 

c2020[ , age := gsub("De ", "", age)]
c2020[ , age := substr(age, 1, 2)]
c2020[age=="No", age:=NA]
c2020[ , age:=as.numeric(age)]

c2020[ , tot := as.numeric(gsub(",", "", tot))]
c2020[ , male := as.numeric(gsub(",", "", male))]
c2020[ , female := as.numeric(gsub(",", "", female))]

c2020 <- c2020[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

c2020 <- melt.data.table(c2020, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

sum(c2020$pop)

c2020[ , year:=2020]


# Unir los dos censos

censos <- rbind(c2010, c2020)

# Prorrateo de los valores perdidos (missing)

censos_pro <- censos[ !is.na(age) ] %>% 
  .[ , p_pop := pop / sum(pop), .(year, sex)] %>% 
  merge( censos[ is.na(age), 
                 .(sex, year, na_pop=pop)], 
         by = c("sex", "year")) %>% 
  .[ , pop_adj := pop + na_pop * p_pop] %>% 
  .[ , .(year, sex, age, pop = pop_adj) ]

#Comprobación de prorrateo
censos_pro[ , sum(pop), .(year, sex)]
censos[ , sum(pop), .(year, sex)]

#Guardar tabla de censo
write.csv(censos_pro, "data/censos_pro.csv")

"---------------------------------------------------------"

# Preprocesamiento de defunciones 1990-2024----
def <- read_xlsx("data/INEGI_def_ocu.xlsx", sheet = 1, 
                 range = "A6:G20560")

names(def) <- c("age", "year", "reg", 
                "tot", "male", "female", "ns")
setDT(def)

# Filtro
def <- def[age!="Total" & year!="Total" & year>=1990]

def[ , .N, .(age)]

def[ , age := gsub("Menores de ", "", age)]
def[ , age := substr(age, 1, 2)]
def[age=="1 ", age:=0]
def[age=="1-", age:=1]
def[age=="5-", age:=5]
def[age=="No", age:=NA] # prorrateo
def[ , age:=as.numeric(age)]

#Convertir a numero las def 
def[ , tot := as.numeric(gsub(",", "", tot))]
def[ , male := as.numeric(gsub(",", "", male))]
def[ , female := as.numeric(gsub(",", "", female))]
def[ , ns := as.numeric(gsub(",", "", ns))]

# Tabla de defunciones - comprobación
def_comp <- def[ , .(tot=sum(tot, na.rm = T),
                     male=sum(male, na.rm = T), 
                     female=sum(female, na.rm = T),
                     ns=sum(ns, na.rm = T)), 
                 .(year)]

# Imputación
def[year=="No especificado", year:=reg] 
def[ , year:=as.numeric(year)] 
def_comp[ , sum(tot)]


# Tabla final prorrateo de defunciones 
def_pro <- def[ , .(male=sum(male, na.rm = T), 
                    female=sum(female, na.rm = T),
                    ns=sum(ns, na.rm = T)), 
                .(year, age)]


# Prorrateo de los valores perdidos (missing)
## Sexo
# def_pro[ , tot:=male + female][ , := (p_male=male/tot, p_female=female/tot)]
def_pro[ , tot := male + female][,
                                 `:=` (p_male = male/tot, p_female = female/tot)]
#def_pro[ , :=(male_adj=male+p_male*ns, female_adj=female+p_female*ns)]
def_pro[ , `:=`(male_adj = male + p_male * ns,
                    female_adj =female + p_female *ns)]
sum(def_pro$male)+sum(def_pro$female)
def_pro <- def_pro[ , .(year, age, male=male_adj, female=female_adj)]
sum(def_pro$male)+sum(def_pro$female)

#Formato long
def_pro <- melt.data.table(def_pro, 
                           id.vars = c("year", "age"),
                           measure.vars = c("male", "female"),
                           variable.name = "sex",
                           value.name = "deaths")

#Extra, formato wide
#dcast(def_pro, formula = year + age - sex)

sum(def_pro$deaths)
def_pro[ , sum(deaths), .(year, sex)]

## Edad
def_pro <- def_pro[ !is.na(age) ] %>% 
  .[ , p_deaths := deaths / sum(deaths), .(year, sex)] %>% 
  merge( def_pro[ is.na(age), 
                  .(sex, year, na_deaths=deaths)], 
         by = c("sex", "year")) %>% 
  .[ , deaths_adj := deaths + na_deaths * p_deaths] %>% 
  .[ , .(year, sex, age, deaths = deaths_adj) ]

# Gráfica
def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]


# Guardar tabla de DEF prorrateadas----
write.csv(def_pro, "data/def_pro.csv", row.names = F)

# -------- FIN ----------*

