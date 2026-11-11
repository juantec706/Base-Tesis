## Nombre del script: Oaxaca_2010.R

## Propósito: Obtener los resultados electorales por municipio de las elecciones de 2010
##            para gobernador de Oaxaca

## Autor: Juan Oliva

## Fecha de creación: 05-12-2024

## Archivos usados: Oaxaca_2010_Crudo.xls

## Archivos creados: Oaxaca_2010.csv
#1. INSTALAR PAQUETES-----------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(readxl,imputeTS,openxlsx,stringr,lubridate,purrr,tidyr,plyr)
install.packages("dplyr")
library(dplyr)

estatal <- data.frame(
  Año = integer(),
  Municipio = numeric(),
  Mun = character(),
  v01 = numeric(), v02 = numeric(), v03 = numeric(), v04 = numeric(), v05 = numeric(),
  v06 = numeric(), v07 = numeric(), v08 = numeric(), v09 = numeric(), v10 = numeric(), v11= numeric (),
  efec = numeric(), nr = numeric(), nulos = numeric(), total = numeric(),
  lisnom = numeric(),
  l01 = numeric(), l02 = numeric(), l03 = numeric(), l04 = numeric(), l05 = numeric(),
  l06 = numeric(), l07 = numeric(), l08 = numeric(), l09 = numeric(), l10 = numeric(), l11=numeric()
)

# Vector con las rutas de los archivos
rutas <- c(
  "C:/Resultados estatales/Oaxaca/1992/Oaxaca_1992.xlsm", 
  "C:/Resultados estatales/Oaxaca/1998/Oaxaca_1998.xlsx", 
  "C:/Resultados estatales/Oaxaca/2004/Oaxaca_2004.xlsx", 
  "C:/Resultados estatales/Oaxaca/2010/Oaxaca_2010.xlsx", 
  "C:/Resultados estatales/Oaxaca/2016/Oaxaca_2016.xlsx",
  "C:/Resultados estatales/Puebla/2004/Puebla_2004.xlsx",
  "C:/Resultados estatales/Puebla/2010/Puebla_2010.xlsm",
  "C:/Resultados estatales/Puebla/2016/Puebla_2016.xlsx",
  "C:/Resultados estatales/Puebla/2018/Puebla_2018.xlsx",
  "C:/Resultados estatales/Veracruz/1998/Veracruz_1998.xlsx",
  "C:/Resultados estatales/Veracruz/2004/Veracruz_2004.xlsx",
  "C:/Resultados estatales/Veracruz/2010/Veracruz_2010.xlsx",
  "C:/Resultados estatales/Veracruz/2016/Veracruz_2016.xlsx",
  "C:/Resultados estatales/Veracruz/2018/Veracruz_2018.xlsx",
  "C:/Resultados estatales/Estado de México/1993/Edomex_1993.xlsx",
  "C:/Resultados estatales/Estado de México/1999/Edomex_1999.xlsx",
  "C:/Resultados estatales/Estado de México/2005/Edomex_2005.xlsx",
  "C:/Resultados estatales/Estado de México/2011/Edomex_2011.xlsx",
  "C:/Resultados estatales/Estado de México/2017/Edomex_2017.xlsx",
  "C:/Resultados estatales/Chiapas/2000/Chiapas_2000.xlsx",
  "C:/Resultados estatales/Chiapas/2006/Chiapas_2006.xlsx",
  "C:/Resultados estatales/Chiapas/2012/Chiapas_2012.xlsx",
  "C:/Resultados estatales/Chiapas/2018/Chiapas_2018.xlsx",
  "C:/Resultados estatales/Michoacán/1992/Michoacán_1992.xlsm",
  "C:/Resultados estatales/Michoacán/1995/Michoacán_1995.xlsx",
  "C:/Resultados estatales/Michoacán/2001/Michoacán_2001.xlsm",
  "C:/Resultados estatales/Michoacán/2007/Michoacán_2007.xlsx",
  "C:/Resultados estatales/Michoacán/2011/Michoacán_2011.xlsx",
  "C:/Resultados estatales/Michoacán/2015/Michoacán_2015.xlsm",
  "C:/Resultados estatales/Yucatán/1995/Yucatán_1995.xlsm",
  "C:/Resultados estatales/Yucatán/2001/Yucatán_2001.xlsx",
  "C:/Resultados estatales/Yucatán/2007/Yucatán_2007.xlsx",
  "C:/Resultados estatales/Yucatán/2012/Yucatán_2012.xlsx",
  "C:/Resultados estatales/Yucatán/2018/Yucatán_2018.xlsx",
  "C:/Resultados estatales/Sonora/1997/Sonora_1997.xlsx",
  "C:/Resultados estatales/Sonora/2003/Sonora_2003.xlsm",
  "C:/Resultados estatales/Sonora/2009/Sonora_2009.xlsx",
  "C:/Resultados estatales/Sonora/2015/Sonora_2015.xlsx",
  "C:/Resultados estatales/Chihuahua/1992/Chihuahua_1992.xlsx",
  "C:/Resultados estatales/Chihuahua/1998/Chihuahua_1998.xlsm",
  "C:/Resultados estatales/Chihuahua/2004/Chihuahua_2004.xlsx",
  "C:/Resultados estatales/Chihuahua/2010/Chihuahua_2010.xlsx",
  "C:/Resultados estatales/Chihuahua/2016/Chihuahua_2016.xlsx",
  "C:/Resultados estatales/Hidalgo/1999/Hidalgo_1999.xlsm",
  "C:/Resultados estatales/Hidalgo/2005/Hidalgo_2005.xlsx",
  "C:/Resultados estatales/Hidalgo/2010/Hidalgo_2010.xlsx",
  "C:/Resultados estatales/Hidalgo/2016/Hidalgo_2016.xlsx",
  "C:/Resultados estatales/Zacatecas/1998/Zacatecas_1998.xlsx",
  "C:/Resultados estatales/Zacatecas/2004/Zacatecas_2004.xlsx",
  "C:/Resultados estatales/Zacatecas/2010/Zacatecas_2010.xlsx",
  "C:/Resultados estatales/Zacatecas/2016/Zacatecas_2016.xlsx",
  "C:/Resultados estatales/San Luis Potosí/1997/SLP_1997.xlsx",
  "C:/Resultados estatales/San Luis Potosí/2003/SLP_2003.xlsx",
  "C:/Resultados estatales/San Luis Potosí/2009/SLP_2009.xlsx",
  "C:/Resultados estatales/San Luis Potosí/2015/SLP_2015.xlsx",
  "C:/Resultados estatales/Nuevo León/1991/NuevoLeón_1991.xlsx",
  "C:/Resultados estatales/Nuevo León/1997/NuevoLeón_1997.xlsx",
  "C:/Resultados estatales/Nuevo León/2003/NuevoLeón_2003.xlsm",
  "C:/Resultados estatales/Nuevo León/2009/NuevoLeón_2009.xlsm",
  "C:/Resultados estatales/Nuevo León/2015/NuevoLeón_2015.xlsx",
  "C:/Resultados estatales/Guerrero/1993/Guerrero_1993.xlsx",
  "C:/Resultados estatales/Guerrero/1999/Guerrero_1999.xlsx",
  "C:/Resultados estatales/Guerrero/2005/Guerrero_2005.xlsm",
  "C:/Resultados estatales/Guerrero/2010/Guerrero_2010.xlsx",
  "C:/Resultados estatales/Guerrero/2015/Guerrero_2015.xlsx",
  "C:/Resultados estatales/Tlaxcala/2004/Tlaxcala_2004.xlsx",
  "C:/Resultados estatales/Tlaxcala/2010/Tlaxcala_2010.xlsx",
  "C:/Resultados estatales/Tlaxcala/2016/Tlaxcala_2016.xlsx",
  "C:/Resultados estatales/Jalisco/1995/Jalisco_1995.xlsx",
  "C:/Resultados estatales/Jalisco/2000/Jalisco_2000.xlsm",
  "C:/Resultados estatales/Jalisco/2012/Jalisco_2012.xlsx",
  "C:/Resultados estatales/Jalisco/2018/Jalisco_2018.xlsx",
  "C:/Resultados estatales/Guanajuato/1995/Guanajuato_1995.xlsx",
  "C:/Resultados estatales/Guanajuato/2000/Guanajuato_2000.xlsx",
  "C:/Resultados estatales/Guanajuato/2006/Guanajuato_2006.xlsx",
  "C:/Resultados estatales/Guanajuato/2012/Guanajuato_2012.xlsx",
  "C:/Resultados estatales/Guanajuato/2018/Guanajuato_2018.xlsx",
  "C:/Resultados estatales/Coahuila/1993/Coahuila_1993.xlsx",
  "C:/Resultados estatales/Coahuila/1999/Coahuila_1999.xlsx",
  "C:/Resultados estatales/Coahuila/2005/Coahuila_2005.xlsx",
  "C:/Resultados estatales/Coahuila/2011/Coahuila_2011.xlsx",
  "C:/Resultados estatales/Coahuila/2017/Coahuila_2017.xlsx",
  "C:/Resultados estatales/Aguascalientes/1992/Aguascalientes_1992.xlsx",
  "C:/Resultados estatales/Aguascalientes/1998/Aguascalientes_1998.xlsx",
  "C:/Resultados estatales/Aguascalientes/2004/Aguascalientes_2004.xlsx",
  "C:/Resultados estatales/Aguascalientes/2010/Aguascalientes_2010.xlsx",
  "C:/Resultados estatales/Aguascalientes/2016/Aguascalientes_2016.xlsx",
  "C:/Resultados estatales/Baja California/1995/BajaCalifornia_1995.xlsx",
  "C:/Resultados estatales/Baja California/2001/BajaCalifornia_2001.xlsx",
  "C:/Resultados estatales/Baja California/2007/BajaCalifornia_2007.xlsx",
  "C:/Resultados estatales/Baja California/2013/BajaCalifornia_2013.xlsx",
  "C:/Resultados estatales/Baja California/2019/BajaCalifornia_2019.xlsx"

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
)

archivos<-list()
for (ruta in rutas) {
  # Leer la hoja "Final" del archivo de Excel
  data <- read_excel(ruta, sheet = "Final", col_names = TRUE)
  
  # Combinar con el data.frame estatal
  estatal <- rbind.fill(estatal, data)
}
estatal[, 4:19] <- lapply(estatal[, 4:19], function(x) as.numeric(as.character(x)))
estatal<-estatal%>%
  mutate(edon = str_sub(Municipio, 1, nchar(Municipio) - 3))%>%
  select(Año,edon,Municipio,Mun,v01,l01,v02,l02,v03,l03,v04,l04,v05,l05,v06,l06,v07,l07,v08,l08,v09,l09,v10,l10,v11,l11,efec,nr,nulos,total,lisnom)
combinaciones <- table(estatal$Año, estatal$Municipio)
combinaciones[combinaciones > 1]
# Ver las combinaciones completas que se repiten
combinaciones_detalladas <- as.data.frame(as.table(combinaciones))
colnames(combinaciones_detalladas) <- c("Año", "Municipio", "Frecuencia")

# Filtrar las combinaciones que tienen una frecuencia mayor a 1
combinaciones_repetidas <- combinaciones_detalladas %>% filter(Frecuencia > 1)

# Ver las combinaciones que se repiten
print(combinaciones_repetidas)


#CHECAR VALORES CORRECTOS
columnas_v <- grep("^v", names(estatal), value = TRUE)
# Sumar los valores por fila
estatal$suma_v <- rowSums(estatal[, columnas_v], na.rm = TRUE)
incon<-estatal%>%
  filter(suma_v!=efec)
incon2 <- estatal %>%
  filter((suma_v + ifelse(is.na(nulos), 0, nulos) + ifelse(is.na(nr), 0, nr)) != total)


magar_gobernadores <- read.csv("C:/Users/jmom_/OneDrive/Escritorio/R para tesis/gobers.csv", na.strings = "NA")%>%
  select(-date.in,-observaciones,-career.steps)
magar_gobernadores <- magar_gobernadores %>%
  rename(
    Año = yr
  )%>%
  mutate(part_lag=lag(part),
         part2_lag=lag(part.elec))%>%
  select(edon,edo,Año,part,part_lag,part2_lag)


estatal <- merge(estatal, magar_gobernadores, by = c("edon", "Año"))
  

# Encuentra las columnas que empiezan con "l" seguido de un número entre 0 y 2
columns_l <- grep("^l[0-2]", names(estatal), value = TRUE)

# Filtrar las filas donde al menos una columna tiene espacios en los valores
f_filtrado <- estatal %>%
  filter(
    apply(estatal[, columns_l], 1, function(row) any(grepl(" ", as.character(row))))
  )

# Mostrar el dataframe filtrado
f_filtrado


estatal$part_lag <- str_squish(str_trim(estatal$part_lag))
#14. CREAR COLUMNA VOTOOFICIAL, QUE ME DICE EL % DE VOTOS QUE TUVO EL PARTIDO EN EL PODER------------

columnas_l <- grep("^l[0-2]", names(estatal), value = TRUE) #lisnom empieza con l. No la incluimos
columnas_partido <- c("part_lag")
buscar_valor_con_prioridad <- function(row, columnas_l, columna_ganador, columnas_partido) {
  
  # Primero revisamos si Ganador_Anterior está definido
  if (is.na(row[[columna_ganador]])) {
    return(NA)  # Si Ganador_Anterior es NA, devolvemos NA
  }
  for (col in columnas_l) {
    if (!is.na(row[[col]]) && row[[col]] == row[[columna_ganador]]) {
      return(row[[which(names(row) == col) - 1]])  # Si hay coincidencia con Ganador_Anterior, retorna la columna izquierda
    }
  }
  
  # Si Ganador_Anterior no coincide, procedemos a buscar en las columnas de partidos
  for (col_partido in columnas_partido) {
    if (!is.na(row[[col_partido]])) {
      for (col in columnas_l) {
        if (!is.na(row[[col]])) {
          # Dividir la columna "l" por guiones
          subcadenas <- strsplit(row[[col]], "-")[[1]]
          
          # Verificar si el valor de PartidoX está contenido en las subcadenas
          if (row[[col_partido]] %in% subcadenas) {
            return(row[[which(names(row) == col) - 1]])  # Si hay coincidencia, retorna el valor de la columna izquierda porque ahí están los votos (columnas v)
          }
        }
      }
    }
  }
  
  # Si no se encuentra ninguna coincidencia con los partidos, el partido oficial no postuló candidato.
  return(0)
}
estatal$VotoOficial <- apply(estatal, 1, buscar_valor_con_prioridad, 
                           columnas_l = columnas_l, 
                           columna_ganador = "part_lag", 
                           columnas_partido = columnas_partido)

rm(buscar_valor_con_prioridad)

estatal<-estatal%>%
  mutate(VotoOficial2=as.numeric(VotoOficial)/suma_v)


final <- merge(sobrino_,estatal, by = c("Municipio", "Año"),all.x = TRUE)

tesis<-final%>% 
  filter(!is.na(VotoOficial2))%>%
  arrange(Municipio,Año)%>%
  group_by(Municipio)%>%
  mutate(PresenciaPREOTAT = cumsum(Presencia),
         PresenciaOTAT= if_else(PresenciaPREOTAT>0,1,0))
tesis2<-tesis%>%
  filter(edo!='oax')
