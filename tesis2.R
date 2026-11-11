## Nombre del script: CreaciónBasesTesis.R

## Propósito: Crear la base de datos para la tesis

## Autor: Juan Oliva

## Fecha de creación: 27-11-2024

## Archivos usados: iniciales_salario_clean.csv

## Archivos creados: iniciales_salario_clean.csv
#1. INSTALAR PAQUETES-----------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(dplyr,readxl,imputeTS,openxlsx,stringr,lubridate,purrr,tidyr,haven,zoo,imputeTS)
#2. IMPORTAR LAS BASES DE SOBRINO Y MAGAR Y HOMOGENEIZAMOS NOMBRES DE AÑOS Y MUNICIPIO-----------------------------------------------

#Base de Sobrino
sobrino <- read.csv("C:/Users/jmom_/OneDrive/Escritorio/R para tesis/CartelPresence_wide_FINAL (1).csv")

sobrino <- sobrino %>%
  rename(
    Año = Years,
    Municipio = Muns
  )

#Base de Magar
magar <- read_excel("C:/Users/jmom_/OneDrive/Escritorio/R para tesis/BaseMagar2.xlsx")%>%
  select(-ife,-ncoal,-ncand)
magar <- magar %>%
  rename(
    Año = yr,
    Municipio = inegi
  )

#Otra base de magar
magar_reeleccion<- read.csv("C:/Users/jmom_/OneDrive/Escritorio/R para tesis/actualizado2023.csv")%>%
  select(-source,-prior.inc.part,-inc.part.after,-ddied,-drepe,-drepg,-dlegacy,-dmujer,-ife,-dextra,-who,-mun,-ord,-incumbent,-runnerup)
magar_reeleccion <- magar_reeleccion %>%
  rename(
    Año = yr,
    Municipio = inegi
  )
magar_gobernadores <- read.csv("C:/Users/jmom_/OneDrive/Escritorio/R para tesis/gobers.csv", na.strings = "NA")%>%
  select(-date.in,-observaciones,-career.steps)
magar_gobernadores <- magar_gobernadores %>%
  rename(
    Año = yr
  )
#3. EMPEZAMOS CON SOBRINO. MODIFICAMOS SU BASE PARA QUE ONCE TREATED, ALWAYS TREATED TAMBIÉN AÑADIMOS AÑOS HASTA 2024-----------------------------------------------

#Guardamos una base para que podamos ver qué tanto cambian los números el supuesto.
sobrino_<-sobrino

#Volviendo a la base original, ponemos una x_ antes de los nombres de las columnas con cárteles
sobrino<- sobrino%>%
  rename_with(~paste0("x_", .), 3:ncol(sobrino))

#Si un cartel estaba en un municipio en el año t, lo estará en los siguientes años
sobrino <- sobrino %>%
  arrange(Municipio,Año) %>%  
  group_by(Municipio) %>%   
  mutate(across(starts_with("x_"), ~cummax(.)))

#Agregamos más años
municipios_2020 <- sobrino %>%
  filter(Año == 2020) %>%
  select(Municipio)
nuevas_filas <- municipios_2020 %>%
  mutate(Año = 2021)
nuevas_filas2 <- municipios_2020 %>%
  mutate(Año = 2022)
nuevas_filas3 <- municipios_2020 %>%
  mutate(Año = 2023)
nuevas_filas4 <- municipios_2020 %>%
  mutate(Año = 2024)
# Agregar las nuevas filas a sobrino
sobrino <- bind_rows(sobrino, nuevas_filas)
sobrino <- bind_rows(sobrino, nuevas_filas2)
sobrino <- bind_rows(sobrino, nuevas_filas3)
sobrino <- bind_rows(sobrino, nuevas_filas4)
sobrino <- replace(sobrino, is.na(sobrino), 0)

sobrino <- sobrino %>%
  arrange(Municipio,Año) %>%  
  group_by(Municipio) %>%   
  mutate(across(starts_with("x_"), ~cummax(.)))
rm(municipios_2020)
rm(nuevas_filas)
rm(nuevas_filas2)
rm(nuevas_filas3)
rm(nuevas_filas4)
#4. CREAR COLUMNA DE PRESENCIA Y QUITAR LAS COLUMNAS QUE NO NECESITAMOS-----------------

#Presencia es 1 si en un municipio había por lo menos un cártel en un año determinado
sobrino$Presencia <- ifelse(rowSums(sobrino[, 3:ncol(sobrino)]) > 0, 1, 0)

#SumaPresencia suma el número de cárteles presentes en un municipio un año
sobrino$SumaPresencia<- rowSums(sobrino[, 3:ncol(sobrino)])
#Nos quedamos con cuatro columnas: Año, Municipio, Presencia, SumaPresencia
sobrino <- sobrino[, -c(3:76)]


#5. VER QUÉ TANTO CAMBIA LA BASE AL INCORPORAR EL SUPUESTO DE ONCE TREATED ALWAYS TREATED-----------------

#Primero creamos la columna Presencia en la base que guardamos anterirmente
sobrino_$Presencia <- ifelse(rowSums(sobrino_[, 3:ncol(sobrino_)]) > 0, 1, 0)
sobrino_$SumaPresencia<- rowSums(sobrino_[, 3:ncol(sobrino_)])
sobrino_ <- sobrino_[, -c(3:76)]

#Acumulado_Presencia2 nos dice en cuántos años hubo cárteles presentes.
#Sobrino2 respeta la base original. Es decir, si hubo cártel en 2014 y en los demás años no,
#Acumulado_Presencia=1. 
#En cambio, en sobrino1, Acumulado_Presencia será 7, pues la base llega hasta 2020.
#Recordamos que tenemos el supuesto de que si en un año hubo cártel, en los siguientes también habrá

sobrino2 <- sobrino_ %>%
  group_by(Municipio) %>%
  summarise(Acumulado_Presencia2 = sum(Presencia, na.rm = TRUE))

sobrino1 <- sobrino %>%
  filter(Año<=2020)%>% #tambien le quitamos lo de los años 2021,2022,2023  y 2024
  group_by(Municipio) %>%
  summarise(Acumulado_Presencia = sum(Presencia, na.rm = TRUE))

#Juntamos ambas bases para ver cómo difieren Acumulado_Presencia
merge_<-merge(sobrino1,sobrino2,by=c("Municipio"))

merge_ <- merge_ %>%
  mutate(Diff = if_else(Acumulado_Presencia == Acumulado_Presencia2, 0, 1))

#Este cálculo nos dice que en el 60% de los municipios se modifica ante el supuesto de once treated always treated c
sum(merge_$Diff, na.rm = TRUE)/nrow(merge_)

#Esta columna nos dicé el procentaje de años que tiene Acumulado_Presencia que efectivamente son los años en que estuvo presente el cártel
merge_ <- merge_ %>%
  mutate(Percentage = if_else(Diff == 1, Acumulado_Presencia2/Acumulado_Presencia, NA))

mean(merge_$Percentage, na.rm = TRUE)
#Vamos a guardar la base de sobrino_ para ver robustez y
#borramos sobrino1 y sobrino2 del GlobalEnviroment
rm(sobrino1)
rm(sobrino2)
#6. CREAR COLUMNAS PARA EL TIME TO EVENT-----------------
#Treatment toma 0 si nunca ha habido un cártel en el municipio. 
sobrino <- sobrino %>%
  group_by(Municipio) %>%
  mutate(Treatment = ifelse(sum(Presencia) == 0, 0, 1)) 

#Tiempo son los años. La diferencia es que en lugar de 1990 es 0
sobrino <- sobrino %>%
  group_by(Municipio) %>%
  mutate(Tiempo = dense_rank(Año)-1)


#Event_Time me dice el año en que llego el cártel. Es cero si el cartel llegó cuando inicia la base (1990)
sobrino <- sobrino %>% 
  arrange(Municipio,Año) %>%
  group_by(Municipio) %>% 
  mutate(Event_Time = Tiempo[Presencia > 0][1]) 
#Para cada municipio, tomamos las filas en que tenían al menos un cártel
#(Presencia>0). Despues, nos fijamos a qué valores de Tiempo corresponden 
# esos año con presencia. Al final, tomamos el primer valor de Tiempo.

#Hasta ahora, cuando Tratamiento=0, tenemos NA.
#Queremos que tambien tenga 0.
sobrino$Event_Time[sobrino$Treatment == 0] <- 0

#Creamos la variable Time to Event. Quiero saber cuantos años faltan para que llegue el primer cártel.
# O cuántos años han pasado desede que llegó el primer cartel. Si no hay cárteles, es cero.
sobrino <- sobrino%>% 
  group_by(Municipio) %>% 
  mutate(Time_to_Event = ifelse(Treatment== 1, Tiempo - Event_Time, 0))

#Treatment_Post toma 1 en el momento en que el cartel ya está presente
sobrino<-sobrino%>% 
  mutate(Treatment_Post=Presencia*Treatment)

#Creamos la columna AñoLlegada
sobrino<-sobrino%>%
  mutate(AñoLlegada=if_else(Treatment==0,0,Event_Time+1990))

#7. HACEMOS LO MISMO PARA LA BASE SOBRINO_-----------------

sobrino_ <- sobrino_ %>%
  group_by(Municipio) %>%
  mutate(Treatment = ifelse(sum(Presencia) == 0, 0, 1)) 
sobrino_ <- sobrino_ %>%
  group_by(Municipio) %>%
  mutate(Tiempo = dense_rank(Año)-1)
sobrino_ <- sobrino_ %>% 
  arrange(Municipio,Año) %>%
  group_by(Municipio) %>% 
  mutate(Event_Time = Tiempo[Presencia > 0][1]) 
sobrino_$Event_Time[sobrino_$Treatment == 0] <- 0
sobrino_ <- sobrino_%>% 
  group_by(Municipio) %>% 
  mutate(Time_to_Event = ifelse(Treatment== 1, Tiempo - Event_Time, 0))
sobrino_<-sobrino_%>% 
  mutate(Treatment_Post=Presencia*Treatment)
sobrino_<-sobrino_%>%
  mutate(AñoLlegada=if_else(Treatment==0,0,Event_Time+1990))
sobrino_ <- merge(sobrino_, merge_, by = "Municipio")
rm(merge_)
#Este data frame reduce la muestra y se queda unicamente con los municipios en que una vez que llego un cartel,
#todos los años tuvo presencia. Es decir, no necesitariamos el supuesto deonce treated always treated porque se cumpliria
sobrino_1<-sobrino_%>%
  filter(Diff==0)
#Podemos relajar este supuesto permitiendo que se quden municipios en los que la coincidencia era más del 90%
sobrino_2<-sobrino_%>%
  filter(is.na(Percentage) | Percentage > .90)
#Tambien podemos convertir los municipios en que solo uno un cartel en uno o dos años y suponer que nunca hubo cartel
sobrino_3 <- sobrino_ %>%
  mutate(
    Presencia = if_else(Percentage < 0.20, 0, Presencia),
    Event_Time = if_else(Percentage < 0.20, 0, Event_Time),
    Treatment_Post = if_else(Percentage < 0.20, 0, Treatment_Post),
    Time_to_Event = if_else(Percentage < 0.20, 0, Time_to_Event),
    Treatment= if_else(Percentage < 0.20, 0, Treatment),
    Año_Llegada=if_else(Percentage < 0.20, 0, AñoLlegada)
  )
sobrino_3 <- sobrino_3%>%
  filter(is.na(Percentage) | Percentage > 0.80 | Percentage < 0.20)
#8. AHORA ANALIZAMOS LA BASE DE MAGAR. EMPEZAMOS CON OBSERVAR POSIBLES INCONSISTENCIAS------------------------------------
#Quitamos donde Municipio sea NA
magar<-magar%>%
  filter(!is.na(Municipio))
#En las columnas que empiezan con v tenemos los votos que obtuvo cada candidato
#Y en la columna efec están los votos efectivos. La suma de los votos de
# todos los candidatos debe ser igual que los efectivos
magar$efec <- as.numeric(magar$efec)
magar_efec <- magar %>%
  mutate(Suma_v = rowSums(select(., starts_with("v")), na.rm = TRUE))%>%
  mutate(Diff=Suma_v-efec)%>%
  select(Año,Municipio,efec,Suma_v,Diff)%>%
  filter(Diff!=0) #Sí se cumple. Vemos que las diferencias con mínimas y solo en 19 casos
rm(magar_efec)

#Ahora vamos a ver que efectivamente v01 sea mayor que v02,,,v03

columnas_v <- grep("^v", names(magar), value = TRUE)
magar_v <- magar %>%
  mutate(verificacion = .[[columnas_v[1]]] > .[[columnas_v[2]]] & 
           .[[columnas_v[1]]] > .[[columnas_v[3]]] &
           .[[columnas_v[1]]] > .[[columnas_v[4]]] & 
           .[[columnas_v[1]]] > .[[columnas_v[5]]] &
           .[[columnas_v[1]]] > .[[columnas_v[6]]] &
           .[[columnas_v[1]]] > .[[columnas_v[7]]] &
           .[[columnas_v[1]]] > .[[columnas_v[8]]] &
           .[[columnas_v[1]]] > .[[columnas_v[9]]] &
           .[[columnas_v[1]]] > .[[columnas_v[10]]] &
           .[[columnas_v[1]]] > .[[columnas_v[11]]] &
           .[[columnas_v[1]]] > .[[columnas_v[12]]] &
           .[[columnas_v[1]]] > .[[columnas_v[13]]] &
           .[[columnas_v[1]]] > .[[columnas_v[14]]] &
           .[[columnas_v[1]]] > .[[columnas_v[15]]] &
           .[[columnas_v[1]]] > .[[columnas_v[16]]] &
           .[[columnas_v[1]]] > .[[columnas_v[17]]] &
           .[[columnas_v[1]]] > .[[columnas_v[18]]] &
           .[[columnas_v[1]]] > .[[columnas_v[19]]] &
           .[[columnas_v[1]]] > .[[columnas_v[20]]] &
           .[[columnas_v[1]]] > .[[columnas_v[21]]] &
           .[[columnas_v[1]]] > .[[columnas_v[22]]]&
           .[[columnas_v[1]]] > .[[columnas_v[23]]])%>%
  filter(verificacion==FALSE & l01!=0)

#Vemos que en los casos en que no se cumple es cuando hubo empates
#Filtramos para que l01 no fuera cero porque en ese caso no hay registrode los datos de las elecciones
#Aprox la mitad de los casos los empates dieron lugar a una elección entraordinaria (columna emm tiene a o b)
#En los casos en que no hubo elección extraordinaria verifiqué en magar que efectviamente el partido
#que aparece en l01 sea el que tomó el poder. 
#En todos los casos sí aparecía, menos en el municipio 20340 que no pude comprobarlo. Se queda así.


#Encontré inconsistencia en el municipio 21139: por un lado, en magar_reelección viene
#que en 2010 no hubo reelección (p-lost), pero vemos en la siguiente fila que ganó el mismo partido
#Si nos fijamos en magar, pan-prd viene en v01, lo que quiere decir que sí hubo reelección.
#Vamos a dejar esas filas porque las necesitamos para lagear correctamente las fechas de las elecciones
#No obstante, serán elimanadas en el paso 18

rm(magar_v)

#Vamos a ver el numero de observaciones por municipio

tabla_frecuencias <- table(magar$Municipio)
# Convertirla a un dataframe
df_tabla_frecuencias <- as.data.frame(tabla_frecuencias)
#Eliminamos municipios que tengan solo una elección
municipios_frecuencia_1 <- names(tabla_frecuencias[tabla_frecuencias == 1])
municipios_lista <- list(municipios_frecuencia_1)
magar <- magar[!magar$Municipio %in% municipios_frecuencia_1, ]
rm(df_tabla_frecuencias)
#Ahora vamos a ver la distancia entre las elecciones
magar<-magar%>%
  arrange(Municipio,Año)%>%
  group_by(Municipio)%>%
  mutate(Ventana = Año - lag(Año))
tabla_ventana <- table(magar$Ventana)
df_tabla_ventana <- as.data.frame(tabla_ventana)
rm(df_tabla_ventana)
magar_ventana <- magar %>%
  group_by(Municipio) %>%
  filter(any(Ventana==21)) %>%
  ungroup() #Es un municipio que fue un tiempo de usos y costumbres
#Lo eliminamos
magar<-magar%>%
  filter(Municipio!=20088)
rm(magar_ventana)
#9. OBTENER PORCENTAJE DE VOTOS EN LUGAR DE NÚMERO DE VOTOS-----------------

#Dividimos los votos de cada candidato entre los votos efectivos

magar[columnas_v] <- magar[columnas_v] / magar$efec
#10.OBTENER LA FECHA DE LA ELECCION-----------------
#Vamos a ver que todo este en orden con las fechas
unique(magar$date)
magarfecha <- magar %>%
  mutate(
    Año = substr(date, 1, 4),  # Extrae los primeros 4 caracteres (Año)
    Mes = substr(date, 5, 6),  # Extrae los caracteres en las posiciones 5 y 6 (Mes)
    Dia = substr(date, 7, 8)   # Extrae los caracteres en las posiciones 7 y 8 (Día)
  )
unique(magarfecha$Año)
unique(magarfecha$Mes)
unique(magarfecha$Dia)
rm(magarfecha)
#Convertimos la columna date en formato de fecha
magar <- magar %>%
  mutate(date = str_trim(date))
magar <- magar %>%
  mutate(
    date = case_when(
      nchar(as.character(date)) == 8 ~ ymd(as.character(date)),  # Formato YYYYMMDD
      nchar(as.character(date)) == 6 ~ ym(as.character(date)),   # Formato YYYYMM
      nchar(as.character(date)) == 4 ~ ymd(paste0(as.character(Año), "01", "01")),  # Formato YYYY, añadiendo 01-01
      TRUE ~NA_Date_  #Toma el valor de la columna Año
    ))

#Vamos a ver si el año de fecha y Año coindicen.
verificar <- magar %>%
  mutate(DifferentYear = if_else(year(date) == Año, 0, 1))
sum(verificar$DifferentYear==1) #Nos da 0
rm(verificar)
#11.HOMOGENEIZAR NOMBRES DE PARTIDOS Y VER CASOS DE PRESIDENTES SIN PARTIDO (INDEPENDIENTES)-------------
#Paso 1. Cambiar conve por mc y pna por panal
columnas_l <- grep("^l[0-2]", names(magar), value = TRUE)
magar <- magar %>%
  mutate(across(.cols = columnas_l, 
                .fns = ~str_replace_all(., c("conve" = "mc", "pna" = "panal"))))

#Paso 2. Ver caso independientes. Queremos ver las elecciones en las que ganaron los independientes
#y las posteriores para ver si hubo alternancia o no.
independientes <- magar %>%
  arrange(Municipio,Año)%>%
  group_by(Municipio) %>%
  filter(any(grepl("indep", l01))) %>% #Quiero las elecciones donde gano un independiente y las posteriores
  filter(Año >= min(Año[grepl("indep", l01)])) %>%
  #select(Año, Municipio,mun,emm, l01, l02, l03, l04,l05,l06,l07,l08,l09,l10,l11,l12,l13)%>%
  mutate(prev_l01 = lag(l01)) %>% 
  filter(!(Año == 2021 & grepl("indep", l01) & !(grepl("indep", prev_l01)))) %>%
  select(-prev_l01) 
colnames(independientes)
colnames(magar)
magar<-anti_join(magar,independientes)
#Quitamos las elecciones de 2021 donde gano un independiente. No las queremos porque no sabemos lo que paso despuues
#(elecciones 2024 por venir). Solo dejamos elecciones 2021 en la que gano un indedniente cuando en la eleccion anterior
#tambien gnao un independiente (hay que ver si se reeligio)


#Yo quiero saber si el mismo candidato independiente se quiso reelegir o no.
#Vamos a hacer merge con la base de magar_reeleccion para ver qué pasó después.
reeleccion <- merge(independientes, magar_reeleccion, by = c("Municipio", "Año"), all.x = TRUE) %>%
  mutate(race.after = if_else(grepl("indep", l01), race.after, NA_character_),
         race.after= if_else(Año==2021,NA_character_,race.after)) #Si el candidato que gano no es independiente, no me interesa saber que paso despues. Solo dejo race.after para cuando gnao un independiente

#1er mutate: si el independiente se reeligio, me aseguro que en la columna de ganador sea el mismo nombre porque no siempre era asi
#2do mutate: el independiente se postulo por un partido y perdio. en la columna l02 de la eleccion donde perdio le pongo "independiente-partido".
#3er mutate: el independiente se postulo por un partido y gano. en la columna l01 de la reeleccion le pongo "indpendiente-partido".
#4to mutate: el independiente volvio a competir y perdio. En esos casos no aparece un candidato independiente, por lo que supongo
#que el indeependiente se postulo por un partido. En ese caso, supongo que quedo en segundo lugar y a l02 le pongo "independiente-partido"
#La unica excepcion son dos municipios que pongo ahi

#Cuando el ind. se va a un partido le pongo el partido para matchearlo con el partido del gobernador.

reeleccion <- reeleccion %>%
  arrange(Municipio,Año)%>%
  group_by(Municipio) %>%
  mutate(l01 = if_else(lag(race.after) == "Reelected"&grepl("indep", lag(l01)), lag(l01), l01))%>%
  mutate(l02 = if_else(
    lag(race.after) == "Reran-beaten-dif-p" & grepl("indep", lag(l01)),
    paste(lag(l01), str_extract(lag(notes), "(?<=reran under )\\S+"),sep = "-"),
    l02 
  ))%>%
  mutate(l01 = if_else(
    lag(race.after) == "Reelected-dif-p" & grepl("indep", lag(l01)),
    paste(lag(l01), str_extract(lag(notes), "(?<=reran under )\\S+"),sep = "-"),
    l01
  ))%>%
  mutate(l02 = if_else(
  lag(race.after) == "Reran-beaten" & grepl("indep", lag(l01))&!(Municipio %in% c(21060, 11009)),
  paste(lag(l01),l02,sep = "-"),
  l02
))%>%
  select(-emm.y,-edon.y,-part,-race.after,-part2nd,-mg,-notes)%>%
  rename(
    emm = emm.x,
    edon = edon.x
  )
magar <- rbind(magar, reeleccion)

#11.OBTENER PARTIDO QUE ESTABA EN EL PODER EN CADA ELECCIÓN-----------------------
#Paso 1:
#Identificar las elecciones que no tuvieron validez. Es decir, que el reusltado de esa elección NO
#nos va a dar el gobernante. Las identificamos con una letra "a" o "b" en la columna emm

magar$Elecc_Val <- ifelse(grepl("[ab](?=\\.)", magar$emm, perl = TRUE), 0, 1)
sum(magar$Elecc_Val==0)

#Paso 2:

#Obtener la fecha de la elección que dio el trifunfo

magar <- magar %>%
  arrange(Municipio,date)%>%
  group_by(Municipio) %>%
  mutate(Fecha_Elección = ifelse(lag(Elecc_Val)==0,lag(date,2),lag(date)))%>%
  mutate(Fecha_Elección=as.Date(Fecha_Elección))
if (FALSE){
#Paso 1:

#Para cada fila, le ponemos la fecha en que ocurrió la elección que le dio la victoria al
#alcalde en turno. 
#Si no hubiera elecciones extraordinarias, siempre correspondería a la fecha anterior (es decir, simplemente ponemos lag).

#La funcion lag_custom por lo general tomará simplemente la fecha anterior. No obstante, si el año anterior
#es el mismo o un año atrás, tomará la fecha de dos años atrás.
#Esto significa que cuando hay elecciones el mismo año o el año siguiente
#lo tomamos como si fueran elecciones extraordinarias. #Por lo tanto, la fecha
# de la elección que le dio la victoria al alcalde al momento de esa elección corresponde dos fechas atrás
# En otras palabras, si tengo 1987,1990,1993, tomará NA, 1987, 1990.
# Pero si tengo 1987,1990, 1991 ó 1987,1990,1990, tomará: NA, 1987,1987.
# Evidentemente, para el primer año toma NA porque no sé cuándo fue la elección anterior

#Nota: esto no consiera casos en que hubo más de dos elecciones extraordinarias seguidas
magar <- magar %>% 
  arrange(Municipio, date)

lag_custom <- function(x,edon) {
  n <- length(x)
  
  # Inicializar el vector year_lagged con NA
  year_lagged <- rep(NA, n)
  
  # Caso cuando solo hay un valor en x. X es el vector de fechas para cada municipio
  if (n == 1) {
    year_lagged[1] <- NA  # Si solo hay un valor, asignamos NA
  } else {
    # Para el primer valor (x[1]), asignamos NA
    year_lagged[1] <- NA
    
    # Recorremos desde el segundo valor en adelante
    for (i in 2:n) {
     
      if (year(x[i]) == year(x[i - 1])) {  
        # Si los años son iguales, se copia el valor de year_lagged[i-1]
        year_lagged[i] <- year_lagged[i - 1]
      } else if (year(x[i]) == year(x[i - 1]) + 1&edon[i]!=5) { #Quito el caso de Coahuila porque en 2017 hubo elecciones solo apra un año y en 2018 hubo otras
        # Si los años son consecutivos, se copia el valor de year_lagged[i-1]
        year_lagged[i] <- year_lagged[i - 1]
      } else {
        # Si no se cumple ninguna de las condiciones anteriores, se asigna el valor de x[i-1]
        year_lagged[i] <- x[i - 1]
      }
    }}
  
  return(year_lagged) 
}

magar <- magar %>%
  group_by(Municipio) %>%
  mutate(Fecha_Elección2 = ifelse(row_number() == 1, NA, lag_custom(date,edon)))%>%
  mutate(Fecha_Elección2=as.Date(Fecha_Elección2))
rm(lag_custom)}
#Paso 3:

#Hay columnas que no sabemos el partido y Magar puso cero. Vamos a ponerles NA
sum(magar$l01==0)
sum(is.na(magar$l01))
magar <- magar %>%
  mutate(across(starts_with("l0") | starts_with("l1") | starts_with("l2"), 
                ~ ifelse(. == 0, NA, .)))
sum(magar$l01==0)
sum(is.na(magar$l01))
#De una vez hacemos lo mismo con las de v que dice NaN
sum(magar$v01==0)
sum(magar$v01=="NaN")
sum(is.nan(magar$v01))
magar <- magar %>%
  mutate(across(starts_with("V"), 
                ~ ifelse(is.nan(.), NA, .)))
sum(is.nan(magar$v01))
sum(is.na(magar$v01))
#También eliminamos posibles espacios
magar <- magar %>%
  mutate(across(starts_with(c("l0", "l1", "l2")), ~ trimws(.)))
#Ahora vamos a poner el partido que ganó en la elección pasada.
#Yo quiero que me dé el ganador (se encuentra en la columna l01) que resultó de la Fecha_Elección.
# Es decir, ahora tendré el partido que estaba en el poder al momento de la elección
magar <- magar %>%
  arrange(Municipio,date)%>%
  group_by(Municipio) %>%
  mutate(Ganador_Anterior = ifelse(row_number() == 1, NA, l01[match(Fecha_Elección, date)]),
         Primera_Elección=ifelse(row_number() == 1,1,0)) #Creamos una dummy para saber si es la primera elección (y por tanto es normal no saber quién es el incumbente)

#Tengo casos en los que l01 es NA (antes cero). Por lo tanto, no puedo saber quién ganó esa elección. Por lo tanto,
#tampoco puedo saber quién será el partido gobernante en la próxima elección
sum(is.na(magar$l01))
#Ahora vamos a ver cuántos votos tuvo el partido de ese Ganador Anterior
if (FALSE){

#Hay 284 casos en que no sé quién ganó la elección y no es la primera elección. Vamos a ver si son casos despues de 1990, que son los que me interesan
sum(is.na(magar$Ganador_Anterior)&magar$Primera_Elección==0)
magar <- magar %>%
  mutate(dummy = if_else(is.na(Ganador_Anterior) & Primera_Elección == 0, 1, 0))

grupo_filtrado <- magar %>%
  group_by(Municipio) %>%
  filter(any(dummy == 1),year(date)>=1990)%>%
  select(emm,Municipio,date,l01,v01,dummy,Primera_Elección,Ganador_Anterior)
sum(grupo_filtrado$dummy==1)
length(unique(grupo_filtrado$Municipio))
#Me doy cuenta que hay 221 elecciones que no sé quién ganó en 246 municipios. Por lo que veo normalmente son elecciones en los noventa.
#Por ello, por lo general son las primeras elecciones de la muestra. Vamos a ver
grupo_filtrado <- grupo_filtrado %>%
  arrange(Municipio,date)%>%
  group_by(Municipio) %>%
  mutate(Primera_Elección2=ifelse(row_number() == 1,1,0),
         dummy2=if_else(is.na(Ganador_Anterior) & Primera_Elección2 == 0, 1, 0))
grupo_filtrado2<-grupo_filtrado%>%
  filter(any(dummy2 == 1),year(date)>=1990)
sum(grupo_filtrado2$dummy2==1)
length(unique(grupo_filtrado2$Municipio))}
#Ahora tengo 42 elecciones en 37 municipios
#12.OBTENER PORCENTAJE DE VOTOS QUE OBTUVO EL PARTIDO EN EL PODER-----------------------


#Paso 1. Para los casos en que el alcalde ganó por uno o más partidos, quiero un partido por columna.
#Tenemos coaliciones de hasta 6 partidos, por eso hay columna Partido 6
magar <- magar %>%
  mutate(
    conteo_guiones = str_count(Ganador_Anterior, "-"),  # Contamos los guiones
    Partido1 = case_when(
      is.na(Ganador_Anterior) ~ NA_character_, 
      conteo_guiones == 0 ~ Ganador_Anterior,  # Si no hay guiones, asigna "Ganador Anterior"
      conteo_guiones >= 1 ~ str_extract(Ganador_Anterior, "^[^\\-]+")  # Extrae la primera palabra antes del primer guion
    ),
    Partido2 = case_when(
      conteo_guiones >= 1 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 2) parts[, 2] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido3 = case_when(
      conteo_guiones >= 2 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 3) parts[, 3] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido4 = case_when(
      conteo_guiones >= 3 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 4) parts[, 4] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido5 = case_when(
      conteo_guiones >= 4 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 5) parts[, 5] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido6 = case_when(
      conteo_guiones >= 5 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 6) parts[, 6] else NA_character_
      },
      TRUE ~ NA_character_
    )
  )
#Paso 2. Creamos columna VotoOficial. Me dice el porcentaje de votos que tuvo el o los partidos en el poder en la
# próxima elección

columnas_l <- grep("^l[0-2]", names(magar), value = TRUE) #lisnom empieza con l. No la incluimos
columnas_partido <- c("Partido1", "Partido2", "Partido3", "Partido4", "Partido5", "Partido6")
buscar_valor_con_prioridad <- function(row, columnas_l, columna_ganador, columnas_partido) {
  
  # Primero revisamos si Ganador_Anterior está definido
  if (is.na(row[[columna_ganador]])) {
    return(NA)  # Si Ganador_Anterior es NA, devolvemos NA
  }
  
   #Si Ganador_Anterior no es NA, pero l01 es NA, devolvemos "No se sabe si postuló"
  if (is.na(row[["l01"]])) {
    return(NA) #Es decir, sí sé quién ganó, pero no me dicen los resultados electorales. Si no hay l01, tampoco hay las demas
  }
  
  # Recorremos las columnas l para ver si hay coincidencias con Ganador_Anterior
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
            return(row[[which(names(row) == col) - 1]])  # Si hay coincidencia, retorna el valor de la columna izquierda
          }
        }
      }
    }
  }
  
  # Si no se encuentra ninguna coincidencia con los partidos, devolver "No postuló"
  return(0)
}

#Esta parte del código verifica que si Ganador_Anterior es igual a unas de las columnas que comienzan con l
#Esto quiere decir que el partido o la coalición gobernante postuló a un candidato con los mismos partidos con los que ganó
#Si esto ocurre, selecciona la columna a la izquierda (que es donde están los votos)

#Pero puede ocurrir que el alcade sea del PRI-PAN y en la próxima elección se postule un candidato PRI-PAN-PRD
#Este ahora usa las columnas de Partido 1,...,Partido 6
#También divide las columnas que empiezan con l por partido. De esta manera, busca si Partido 1 tiene coincidencias
#con alguno de los partidos en las columnas l, selecciona el valor de la izquierda de esa columna l.
# Si no encuentra coincidencias con Partido 1, se va con Partido 2 y así sucesivamente.
#Nota que si la coalición política se rompe, el resultado de VotoOficial dependerá de la coincidencia que encuentre con Partido 1.
#Es decir, el alcade llegó por Partido 1= PRI y Partido 2= PAN. Pero en la próxima elección se pelean y van separados
#Este código tomará como Voto Oficial los votos que haya tenido el PRI y no el PAN en la próxima elección.
#De cualquier modo, nos vamos a deshacer de las elecciones en las que la coalición se haya roto porque no es claro
# qué partido es el oficial
  # Si Ganador_Anterior no coincide, procedemos a buscar en las columnas de partidos
magar$VotoOficial <- apply(magar, 1, buscar_valor_con_prioridad, 
                           columnas_l = columnas_l, 
                           columna_ganador = "Ganador_Anterior", 
                          columnas_partido = columnas_partido)

rm(buscar_valor_con_prioridad)
#13.COALICIONES POLÍTICAS ROTAS-----------------------

#Ahora me interesa saber las elecciones en las que la coalición gobernante se rompió y cada una postuló
#un candidato distinto.
#O el partido en el poder no postuló candidato


contar_coincidencias_unicas <- function(row, columnas_l, columnas_partido) {
  columnas_con_coincidencias <- c() # Lista para almacenar las columnas con coincidencias
  if (is.na(row[["VotoOficial"]])) {
    return(NA)  
  }
  # Buscar coincidencias
  for (col_partido in columnas_partido) {
    if (!is.na(row[[col_partido]])) {
      for (col in columnas_l) {
        if (!is.na(row[[col]])) { #La columna l no puede NA
          # Dividir la columna "l" por guiones
          subcadenas <- strsplit(row[[col]], "-")[[1]]
          
          # Verificar si PartidoX coincide con alguna subcadena
          if (row[[col_partido]] %in% subcadenas) {
            columnas_con_coincidencias <- c(columnas_con_coincidencias, col) # Registrar la columna
          }
        }
      }
    }
  }
  
  # Contar columnas únicas con coincidencias
  columnas_unicas <- unique(columnas_con_coincidencias)
  return(length(columnas_unicas))
}

magar$Separación <- apply(magar, 1, contar_coincidencias_unicas, 
                    columnas_l = columnas_l, 
                    columnas_partido = columnas_partido)
table(magar$Separación) #Vemos que en la gran mayoría el partido que estaba en el poder postuló candidato o la
#coalición permaneció intacta = más de 22000 elecciones
rm(contar_coincidencias_unicas)


#sum(is.na(magar2$VotoOficial))
#sum(is.na(magar2$Ganador_Anterior))

options(scipen = 999)
#SI LA COALICION GOBERNANTE SE ROMPIO, SUPONGO QUE EL INCUMBENTE ERA EL PARTIDO QUE TUVO MAS VOTOS-----------------
magar_separacion<-magar%>%
  filter(Separación>=2)
magar<-anti_join(magar,magar_separacion)
encontrarpartidoseparacion <- function(row, columnas_l, columna_ganador, columnas_partido) {
  valores_encontrados <- c()  # Inicializamos un vector vacío para almacenar los valores encontrados
  
  for (col_partido in columnas_partido) {
    if (!is.na(row[[col_partido]])) {
      for (col in columnas_l) {
        if (!is.na(row[[col]])) {
          # Dividir la columna "l" por guiones
          subcadenas <- strsplit(row[[col]], "-")[[1]]
          
          # Verificar si el valor de PartidoX está contenido en las subcadenas
          if (row[[col_partido]] %in% subcadenas) {
            # Si hay coincidencia, guardamos el valor de la columna izquierda
            valores_encontrados <- c(valores_encontrados, row[[which(names(row) == col) - 1]])
          }
        }
      }
    }
  }
  
  if (length(valores_encontrados) > 0) {
    return(max(valores_encontrados)) # Retornamos el valor máximo
  } else {
    return(NA)  # Si no se encontraron coincidencias, retornamos NA
  }
}


magar_separacion$VotoOficial <- apply(magar_separacion, 1, encontrarpartidoseparacion, 
                           columnas_l = columnas_l, 
                           columna_ganador = "Ganador_Anterior", 
                           columnas_partido = columnas_partido)
magar<-rbind(magar,magar_separacion)
#15.ANALIZAR SI HUBO REELECCIÓN DEL PARTIDO EN EL PODER---------------------      

#Este codigo lo que hace es que busca si hay alguna coincidencia entre el o los partidos que ganaron
#la eleccion (l01) y el o los partidos en el poder (Partido 1,..., Partido 6)
reeleccion <- function(row, columnas_l, columnas_partido) {
  # Inicializar reelección en 0 por defecto
  reeleccion <- 0
  if (is.na(row[["Partido1"]])) {
    return(NA)  
  }
  if (is.na(row[["l01"]])) {
    return(NA) #Es decir, sí sé quién ganó, pero no me dicen los resultados electorales.
  }
  # Buscar coincidencias
  for (col_partido in columnas_partido) {
    if (!is.na(row[[col_partido]])) { 
      for (col in columnas_l) {
        if (!is.na(row[[col]]) && row[[col]] != 0 && !is.nan(row[[col]])) {  # Verificar que l no sea NA, NaN ni 0
          # Dividir la columna "l" por guiones
          subcadenas <- strsplit(row[[col]], "-")[[1]]
          
          # Verificar si PartidoX coincide con alguna subcadena
          if (row[[col_partido]] %in% subcadenas) {
            reeleccion <- 1  # Si hay coincidencia, asignar 1
            break  # Salir del bucle, ya que hemos encontrado la coincidencia
          }
        }
      }
    }
  }
  
  return(reeleccion)  # Retornar el valor de Reelección para esta fila (0 o 1)
}
# Aplicar la función fila por fila usando apply()
magar$Reelección <- apply(magar, 1, reeleccion, 
                          columnas_l = "l01", 
                          columnas_partido = columnas_partido)
rm(reeleccion)
#14.CREAR DUMMYS PARA CASOS EN LOS QUE NO POSTULÓ O NO SE SABE SI POSTULÓ------------
if (FALSE){
magar<-magar%>%
  mutate(Postuló= if_else(VotoOficial=="No postuló",0,1),
         l01conocido=if_else(VotoOficial=="No se sabe si postuló",0,1),
         Reelección=if_else(Reelección=="No se sabe si postuló",NA,Reelección),
         Separación=if_else(Separación=="No se sabe si postuló",NA,Separación),
         VotoOficial=if_else(VotoOficial=="No postuló","0",VotoOficial),
         VotoOficial=if_else(VotoOficial=="No se sabe si postuló",NA,VotoOficial))}




#16.PROCESAR LA OTRA BASE DE MAGAR PARA COMPARAR SI MIS RESULTADOS DE REELECCIÓN SON CORRECTOS-------       

#En esta sección trabajamos magar_reelección

#La columna rece.after de esta base me dice si el partido en el poder ganó o perdió
unique(magar_reeleccion$race.after)

#Vamos a quitar algunas filas de la columna race.after:

magar_reeleccion <- magar_reeleccion %>%
  mutate(Reelección2 = case_when(
    race.after %in% c("Term-limited-p-lost", "Reran-beaten-dif-p", "Reran-beaten", 
                      "Out-p-lost", "Dead-p-lost", "Dead-term-limited-p-lost", "Reran-p-lost","Reran-dead-p-lost") ~ 0,
    race.after %in% c("Term-limited-p-won", "Reelected","Reran-dif-p", "Dead-p-won", 
                      "Reran-dead-p-won", "out-dead-p-won", "Reelected-dif-p","out-p-won", "Out-p-won") ~ 1,
    TRUE ~ NA_real_
  ))

#La manera en construir la columna Reelección me genera un problema:
#En esta base, Reelección2 signifca si el partido que ganó en esa elección permaneció en el poder la siguiente.
#En la otra base de magar, Reelección significa si el partido que ganó en la pasada elección permaneció en el poder en esa elección 
#Necesito hacer un lag en esta base para poder hacer una comparación correcta.

magar_reeleccion<-magar_reeleccion%>%
  arrange(Municipio,Año)%>%
  group_by(Municipio)%>%
  mutate(Reelección2_lag=lag(Reelección2),
         notes_lag=lag(notes),
         race.after_lag=lag(race.after),
         part_lag=lag(part)
         )%>%
  select(Año,Municipio,race.after_lag,Reelección2_lag,notes_lag,part_lag,race.after,notes)
#magarmuertos <- magarmuertos %>%
  #filter(!is.na(race.after) & 
           #(race.after %in% c("uyc", "litigio") | grepl("^Dead", race.after)))
#IMPUTAR-----------------
magard<-magar%>%
  select(Municipio,Año,lisnom)%>%
  mutate(lisnom=as.numeric(lisnom))%>%
  filter(Municipio==13001)
  
magard<-magard%>%
  mutate(
    lisnom_imputado = ifelse(is.na(lisnom), 
                             predict(lm(lisnom ~ Año, data = .)),  # Imputación según tendencia temporal
                             lisnom),
    lisnom_imputado3 = zoo::na.approx(lisnom, x = Año,rule=2),
    lisnom_imputado4= ifelse(is.na(lisnom),na_interpolation(lisnom,option='stine'),lisnom))
#17.JUNTAR AMBAS BASES DE MAGAR PARA COMPARARLAS------------------

#La base magar tiene todas las eleccciones, hayan sido anuladas o no. 
#La base magar_reelección tiene elecciones que no fueron anuladas.
#Voy a crear una dummy en magar para saber si fue elección efectiva o no
#Una vez con esta dummy, voy a poder juntar las elecciones con magar_reelección


#Creamos una dummy para diferenciar si fue efectiva la elección o no
magar <- magar %>%
  #mutate(Efectiva = ifelse(grepl("[a-zA-Z]", sub(".*-", "", emm)), 0, 1))%>%
  filter(Año>=1990)#%>%
  #select(-v23, -l23, -v22, -l22, -v21, -l21, -v20, -l20, -v19, -l19, 
         #-v18, -l18, -v17, -l17, -v16, -l16, -v15, -l15,-v14,-l14,-v13,-l13,-v12,-l12,-v11,-l11,-v10,-l10,-v09,-l09,-emm)
#magar$orden<-rep(1:nrow(magar))
#Creo una dummy diciendo que todas fueron efectivas
magar_reeleccion<-magar_reeleccion%>%
  filter(Año>=1990&Municipio!="")%>%
  mutate(Municipio=as.numeric(Municipio),
         Elecc_Val=1)
magar<-merge(magar,magar_reeleccion,by=c("Municipio","Año","Elecc_Val"),all.x=TRUE)
#magar_<-magar%>%
  #select(Municipio,Año,Separación,VotoOficial,Reelección,Reelección2_lag,Ganador_Anterior,l01,l02,l03,l04,notes_lag)


#18. ANALIZAR LOS CASOS EN LOS QUE DIGO QUE EL PARTIDO EN EL PODER NO POSTULÓ CANDIDATO Y MAGAR SÍ------------

#El primer caso es cuando Separación=0. Esto quiere decir que la coalición goberante
#no postuló candidato en las próximas elecciones.Pero en realiadad sí pudeo haber habido
separacion0 <- magar %>%
  filter(Separación == 0 & 
           grepl("^reran", notes_lag))
magar<-anti_join(magar,separacion0)
separacion0<-separacion0%>%
  mutate(Reelección=Reelección2_lag,
         Ganador_Anterior=str_extract(notes_lag, "(?<=reran under )\\S+"))
if (FALSE){
magar <-magar%>% 
  mutate(VotoOficial=if_else(Separación==0&Reelección2_lag==1,as.character(v01),VotoOficial),
         Ganador_Anterior=if_else(Separación==0&Reelección2_lag==1,as.character(l01),Ganador_Anterior),
         Partido1=if_else(Separación==0&Reelección2_lag==1,as.character(l01),Partido1),
         Reelección=if_else(Separación==0&Reelección2_lag==1,"1",Reelección))}
#18. PARTE 1:CASOS EN QUE MAGAR_REELECCIÓN Y MAGAR DAN RESULTADOS DISTINTOS PARA REELECCIÓN-----------

#Separacion=1 significa que la coalición municipal gobernante postuló solo a un candidato
#en la elección siguiente. Lo que yo quiero ver son los posibles casos en que el presidente municipal
#dejó a esos partidos y se fue con otros
separacion1 <- magar %>%
  filter(Separación == 1 & 
           grepl("^reran", notes_lag))
magar<-anti_join(magar,separacion1)
separacion1 <- separacion1 %>%
  mutate(notes_lag=if_else(Municipio==19012&Año==2018,'reran under indep',notes_lag))
separacion1<-separacion1%>%
  mutate(Reelección=Reelección2_lag,
         Ganador_Anterior=str_extract(notes_lag, "(?<=reran under )\\S+"))




#En las filas en las que hay discrepancia y Magar no puso notas las voy a eliminar de mi análisis.
#En las filas en las que la nota dice bajo qué partido ganó se quedan y se cambian.

#En ese sentido, modificar las filas de la base Magar, pero solo las que cumplen con la condición_común
#VotoOficial para los casos en que Reeleeción2_lag es cero las modifico en pasos siguientes

#Nota que cuando un presidente municipal decide reelegirse por otro partido, ahora ese partido será considerado
#como ganador anterior. Esto es para poder asignar qué votos tuvo ese partido
#y tambien para matchearlo correctamente al momento de considerar el partido del goberndor
#si un presidente era del pri y se cambia al pan y el gobernador es del pan, considero que ya son del mismo partido
if (FALSE){
magar1 <- magar %>%
  filter(notes_lag != "" & 
           Separación == 1 & 
           Reelección2_lag != Reelección & 
           !Municipio %in% c(29034, 13026, 24034, 1011))%>%
  mutate(
    Reelección =Reelección2_lag,
    Ganador_Anterior = if_else(Reelección2_lag == 1, as.character(l01), Ganador_Anterior),
    VotoOficial = if_else(Reelección2_lag == 1, as.character(v01), VotoOficial),
    Ganador_Anterior = if_else(Reelección2_lag == 0, 
                               str_extract(notes_lag, "(?<=reran under )\\S+"), 
                               Ganador_Anterior))}#%>%
  #select(-v23, -l23, -v22, -l22, -v21, -l21, -v20, -l20, -v19, -l19, 
                #-v18, -l18, -v17, -l17, -v16, -l16, -v15, -l15,-v14,-l14,-v13,-l13,-v12,-l12,-v11,-l11,-v10,-l10,-v09,-l09,-emm)
#18. PARTE 2:CASOS EN QUE MAGAR_REELECCIÓN Y MAGAR DAN RESULTADOS DISTINTOS PARA REELECCIÓN-----------

#Separacion>=2 significa que la coalición municipal gobernante postuló a más de un candidato
#en la elección siguiente. Lo que yo quiero ver son los posibles casos en que el presidente municipal
#solo se postuló por alguno de esos partidos. En ese sentido, sería claro quién es el incumbente
separacion2 <- magar %>%
  filter(Separación >= 2 & 
           grepl("^reran", notes_lag)|grepl("^ran under", notes_lag))
magar<-anti_join(magar,separacion2)
separacion2 <- separacion2 %>%
  mutate(notes_lag=if_else(Municipio==15042&Año==2021,'reran under mc and lost',notes_lag))%>%
  mutate(notes_lag=if_else(Municipio==18009&Año==2021,'reran under pt only and won',notes_lag))
separacion2<-separacion2%>%
  mutate(Reelección=Reelección2_lag,
         Ganador_Anterior=str_extract(notes_lag, "(?<=reran under )\\S+"))
separacion<-rbind(separacion0,separacion1,separacion2)

if (FALSE){
magar2 <- magar %>%
  filter(notes_lag != "" & 
           Separación >= 2 & 
           Reelección2_lag != Reelección & 
           !Municipio %in% c(9017))%>%
  mutate(notes_lag=if_else(Municipio==21166,"reran under pan-pri-prd and won",notes_lag),
         notes_lag=if_else(Municipio==15042,"reran under mc and lost",notes_lag),
         Reelección =Reelección2_lag,
         Ganador_Anterior = if_else(Reelección2_lag == 1, as.character(l01), Ganador_Anterior),
         VotoOficial = if_else(Reelección2_lag == 1, as.character(v01), VotoOficial),
         Ganador_Anterior = if_else(Reelección2_lag == 0, 
                                    str_extract(notes_lag, "(?<=reran under )\\S+"), 
                                    Ganador_Anterior)) }
#MODIFICAR COLUMNA DE VOTO OFICIAL PARA LOS CASOS DE PARTE 1 Y 2--------------------------------------

#Hago los mismo que antes: veo si ganador anterior tiene uno o más partidos
#Hago exactamante lo mismo para magar1 y 2
separacion <- separacion %>%
  mutate(
    conteo_guiones = str_count(Ganador_Anterior, "-"),  # Contamos los guiones
    Partido1 = case_when(
      is.na(Ganador_Anterior) ~ NA_character_, 
      conteo_guiones == 0 ~ Ganador_Anterior,  # Si no hay guiones, asigna "Ganador Anterior"
      conteo_guiones >= 1 ~ str_extract(Ganador_Anterior, "^[^\\-]+")  # Extrae la primera palabra antes del primer guion
    ),
    Partido2 = case_when(
      conteo_guiones >= 1 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 2) parts[, 2] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido3 = case_when(
      conteo_guiones >= 2 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 3) parts[, 3] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido4 = case_when(
      conteo_guiones >= 3 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 4) parts[, 4] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido5 = case_when(
      conteo_guiones >= 4 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 5) parts[, 5] else NA_character_
      },
      TRUE ~ NA_character_
    ),
    Partido6 = case_when(
      conteo_guiones >= 5 ~ {
        parts <- str_split(Ganador_Anterior, "-", simplify = TRUE)
        if (ncol(parts) >= 6) parts[, 6] else NA_character_
      },
      TRUE ~ NA_character_
    )
  )
#Ahora con esta funcion asigno el voto oficial. Es casi lo mismo que la anterior,
#solo que esta primero veo que Reelección2_lag sea cero.
buscar_valor_con_prioridad <- function(row, columnas_l, columna_ganador, columnas_partido) {
  # Recorremos las columnas l para ver si hay coincidencias con Ganador_Anterior
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
            return(row[[which(names(row) == col) - 1]])  # Si hay coincidencia, retorna el valor de la columna izquierda
          }
        }
      }
    }
  }
  
  return(NA)
}
separacion$VotoOficial <- apply(separacion, 1, buscar_valor_con_prioridad, 
                            columnas_l = columnas_l,
                            columnas_partido=columnas_partido,
                            columna_ganador = "Ganador_Anterior")
separacion_<-separacion%>%
  select(Ganador_Anterior,notes_lag,v01,l01,v02,l02,v03,l03,v04,l04,v05,l05,Partido1,Partido2,VotoOficial,Partido3)
#AGREGAR LAS NUEVAS FILAS A LA BASE PRINCIPAL---------------------
magar<-rbind(magar,separacion)

#Primero le quito las filas de separacion 1 y 2.
if (FALSE) {
  magar <- magar %>%
    anti_join(separacion1, by = c("orden"))
  magar <- magar %>%
    anti_join(separacion2, by = c("orden"))
  
  # Ahora le pongo las filas de separacion 1 y 2 modificadas
  magar <- rbind(magar, magar1)
  magar <- rbind(magar, magar2)
  
  rm(separacion1)
  rm(separacion2)
}

#VALORES NA---------------
#Si Reeleccion es NA y Reeleccion2_lag no, le pondre Reeleccion2_lag
na1<-magar%>%
  filter(!is.na(Reelección)&is.na(Reelección2_lag))%>%
  select(-v23, -l23, -v22, -l22, -v21, -l21, -v20, -l20, -v19, -l19, -v18, -l18, -v17, -l17, -v16, -l16, -v15, -l15,-v14,-l14,-v13,-l13,-v12,-l12,-v11,-l11,-v10,-l10,-v09,-l09,-emm)
na2<-magar%>%
  filter(is.na(Reelección)&!is.na(Reelección2_lag))%>%
select(-v23, -l23, -v22, -l22, -v21, -l21, -v20, -l20, -v19, -l19, -v18, -l18, -v17, -l17, -v16, -l16, -v15, -l15,-v14,-l14,-v13,-l13,-v12,-l12,-v11,-l11,-v10,-l10,-v09,-l09,-emm)

#3.PASAMOS A LA BASE DE GOBERNADORES. PRIMERO ANALIZAMOS ELECCIONES EXTRAORDINARIAS--------------------

result <- magar_gobernadores %>%
  group_by(edo) %>%
  arrange(edo, Año) %>%
  mutate(
    igual = if_else(Año - lag(Año) == 0,1,0),
    consecutivos=if_else(Año - lag(Año) == 1,1,0)
  ) %>%
  ungroup()

#Encontramos dos casos:

#1. Colima 2016. 
#2. Puebla 2019.

#Tambien en el 2001 en Tabasco hubo una extraordinaria

#En las elecciones de Colima de 2016 solo se eligió al gobernador.

#En 2019 hubo elecciones extraordinarias en Puebla no solo para elegir gobernador, sino también
#5 ayuntamientos. En ese sentido, para 2019 el partido del gobernador era el de Martha Erika.

#En Tabasco en 2001 solo se eligió al gobernador.

#Como en Puebla el partido de la difunta gobernadora debio haber operado para la elección
#de los cinco ayuntamientos, no la consideraré extraordinaria.
         
#Creamos variable dummy porque eleccion fue al siguiente año
magar_gobernadores <- magar_gobernadores %>%
  mutate(extraordinaria = if_else(( (edo == "col" & Año == 2016) |  
                                      (edo == "tab" & Año == 2001)),
                                  1,  # Si cumple la condición, le pone 1
                                  0))
#Eliminanos las elecciones extraordinarias 
magar_gobernadores <- magar_gobernadores %>%
  filter(
    !( (edo == "col" & Año == 2015)  |
         (edo == "tab" & Año == 2000) ) 
  )
#5.OBTENEMOS EL PERIODO EN QUE ESTUVO CADA GOBERNADOR--------------------
         
magar_gobernadores <- magar_gobernadores%>%
           mutate(inicio=if_else(extraordinaria==0,Año+1,Año))
magar_gobernadores <- magar_gobernadores%>%
           group_by(edo)%>%
           arrange(edo, Año) %>%
           mutate(fin=if_else(lead(extraordinaria)==1,lead(Año)-1,lead(Año)))

#Vamos a ver si no hay huecos de años
magar_gobernadores_comprobacion<-magar_gobernadores%>%
  group_by(edo)%>%
  arrange(edo, Año) %>%
  mutate(comprobacion=if_else(fin==lead(inicio)-1,0,1))
rm(magar_gobernadores_comprobacion)
#5.QUEREMOS SABER QUÉ PARTIDO TENÍA EL GOBERNADOR AL MOMENTO DE CADA ELECCIÓN MUNICIPAL--------------------
#Quitamos espacios adicionales
magar_gobernadores$part <- trimws(magar_gobernadores$part)
magar_gobernadores$part.elec <- trimws(magar_gobernadores$part.elec)
# Vamos a crear un dataframe vacío donde almacenaremos los resultados
magar_final <- data.frame()

# Conseguir los valores únicos de "edon"
edon_values <- unique(magar$edon)

# Iterar sobre cada valor único de "edon"
for (edon_value in edon_values) {
  
  # Filtrar magar2 y gobernadores2 por el valor actual de "edon"
  magar_subset <- magar %>% filter(edon == edon_value)
  gobernadores_subset <- magar_gobernadores %>% filter(edon == edon_value)
  
  # Aplicar el código de la mutación y cálculo para el subset actual de magar
  magar_subset <- magar_subset %>%
    mutate(
      partido_gobernador = sapply(Año, function(x) {
        # Filtrar los datos de gobernadores donde el año está dentro del rango
        partido <- gobernadores_subset %>%
          filter(x >= inicio & x <= fin) %>%
          pull(part)
        
        # Si hay un partido, devolverlo; si no, devolver NA
        if (length(partido) > 0) partido[1] else NA
      }),
      partido_gobernador2 = sapply(Año, function(x) {
        # Filtrar los datos de gobernadores donde el año está dentro del rango
        partido2 <- gobernadores_subset %>%
          filter(x >= inicio & x <= fin) %>%
          pull(part.elec)
        
        # Si hay un partido, devolverlo; si no, devolver NA
        if (length(partido2) > 0) partido2[1] else NA
      })
    )
  
  # Agregar el resultado de este subset al dataframe final
  magar_final <- bind_rows(magar_final, magar_subset)%>%
    select(orden,partido_gobernador,partido_gobernador2)
}
magar<-merge(magar,magar_final,by=c('orden'),all.x = TRUE)
#***AHORA DETERMINAR SI EL PRESIDENTE MUNICIPAL EN TURNO COMPARTÍA PARTIDO CON EL GOBERNADOR--------
mismopartidogober <- function(row, columna_gober, columnas_partido) {
  # Inicializar reelección en 0 por defecto
  mismopgober <- 0
  if (is.na(row[["Partido1"]])) {
    return(NA)  
  }
  # Buscar coincidencias
  for (col_partido in columnas_partido) {
    if (!is.na(row[[col_partido]])) {  # Asegurarse de que el partido no sea NA
      for (col in columna_gober) {
        if (!is.na(row[[col]])) {  # Verificar que l no sea NA
          # Dividir la columna "l" por guiones
          subcadenas <- strsplit(row[[col]], "-")[[1]]
          
          # Verificar si PartidoX coincide con alguna subcadena
          if (row[[col_partido]] %in% subcadenas) {
            mismopgober <- 1  # Si hay coincidencia, asignar 1
            break  # Salir del bucle, ya que hemos encontrado la coincidencia
          }
        }
      }
    }
  }
  
  return(mismopgober)  # Retornar el valor de Reelección para esta fila (0 o 1)
}

# Aplicar la función fila por fila usando apply()
magar$MismoPartidoGobernador <- apply(magar, 1,mismopartidogober , 
                          columna_gober = "partido_gobernador2", 
                          columnas_partido = columnas_partido)
#OBTENER RESULTADOS ELECCIONES 2024  Y JUNTARLA CON BASE MAGAR-----------------------------
magar__<-magar%>%
  select(Año,Municipio,v01,l01,Partido1,Partido2,partido_gobernador,partido_gobernador2,MismoPartidoGobernador)
#Vamos a obtener las elecciones de 2024

#elec_2024 <- magar_reeleccion %>%
  #filter(Año==2024&!is.na(Reelección2_lag)&Municipio!="")%>%
  #mutate(Reelección=as.numeric(Reelección2_lag))%>% #Le cambiamos el nombre para poder hacer rbind
  #select(Año,Municipio,Reelección)%>%
  #mutate(Separación=1)%>%
  #mutate(Municipio=as.numeric(Municipio))

#magar_efectivas_merge<-magar_efectivas_merge%>%
  #select(Año,Municipio,Reelección,Separación)%>%
  #filter(!is.na(Reelección))%>%
  #mutate(Reelección=as.numeric(Reelección),
         #Separación=as.numeric(Separación))

#magar_efectivas_merge <- rbind(magar_efectivas_merge, elec_2024)

#JUNTAR BASES MAGAR Y SOBRINO-----------------------------
magar2 <- magar %>%
  arrange(Municipio, Año) %>%
  group_by(Municipio) %>%
  filter(!is.na(VotoOficial),Año<=2020)%>%
  #filter(!is.na(Municipio),Municipio!=0,!(Municipio==20088&Año==1992))%>%
  mutate(Ventana = if_else(row_number() == 1, 20, Año - lag(Año)))
table(magar2$Ventana)
#Hay cinco municipios que Sobrino no registra: 9017,12036,14122,21162,32016
final2 <- merge(sobrino_,magar2, by = c("Municipio", "Año"),all.x = TRUE)%>% #MismoPartidoGobernador
  select(Año,edon,Municipio,Presencia,SumaPresencia,VotoOficial,Reelección,Separación,AñoLlegada,Time_to_Event,Treatment,Treatment_Post,Elecc_Val,Ventana)%>%
  mutate(VotoOficial=as.numeric(VotoOficial),
         Reelección=as.numeric(Reelección))
table(final2$Ventana)
#BASE FINAL PARA ECONOMETRÍA-------------------------
tesis<-final2%>% 
  arrange(Municipio,Año)%>%
  group_by(Municipio)%>%
  mutate(PresenciaPREOTAT = cumsum(Presencia),
         PresenciaOTAT= if_else(PresenciaPREOTAT>0,1,0),
         AñoLlegadaChais = ifelse(Treatment == 0, 2021, AñoLlegada)) %>%
  mutate(grupo=if_else(Año<AñoLlegadaChais,0,NA),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año == AñoLlegadaChais), 1, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-1 == AñoLlegadaChais), 2, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-2 == AñoLlegadaChais), 3, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-3 == AñoLlegadaChais), 4, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-4 == AñoLlegadaChais), 5, grupo))%>%
  #mutate(Presencia=if_else(lead(Presencia)==1&lag(Presencia)==1&Presencia==0,1,Presencia))%>%
  mutate(Cambio=if_else(Presencia==1&lag(Presencia)==0,1,0))%>%
  mutate(
    grupo2 = if_else(Presencia == 0, 0, NA_real_),
    grupo2 = if_else(is.na(grupo2) & Cambio == 1 & !is.na(VotoOficial), 1, grupo2),
    grupo2 = if_else(is.na(grupo2) & Cambio == 1 & !is.na(lead(VotoOficial, 1)), 2, grupo2),
    grupo2 = if_else(is.na(grupo2) & Cambio == 1 & !is.na(lead(VotoOficial, 2)), 3, grupo2),
    grupo2 = if_else(is.na(grupo2) & Cambio == 1 & !is.na(lead(VotoOficial, 3)), 4, grupo2),
    grupo2 = if_else(is.na(grupo2) & Cambio == 1 & !is.na(lead(VotoOficial, 4)), 5, grupo2)
  )%>%
  fill(grupo2, .direction = "down")
tesis<-tesis%>%
  filter(!is.na(VotoOficial))

write_dta(tesis, "C:/Users/jmom_/OneDrive/Escritorio/R para tesis/archivo.dta")
