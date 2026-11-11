## Nombre del script: CreaciónBasesTesis.R

## Propósito: Crear la base de datos para la tesis

## Autor: Juan Oliva

## Fecha de creación: 27-11-2024

## Archivos usados: CartelPresence_wide_FINAL (1).csv, BaseMagar2.xlsx, actualizado2023.csv, gobers.csv

## Archivos creados: iniciales_salario_clean.csv
#1.  INSTALAR PAQUETES-----------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(dplyr,readxl,imputeTS,openxlsx,stringr,lubridate,purrr,tidyr,haven,zoo,imputeTS)
#2.  IMPORTAR LAS BASES DE SOBRINO Y MAGAR E IGUALAMOS NOMBRES DE AÑOS Y MUNICIPIOS-----------------------------------------------

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

#3.  MODIFICAR BASE SOBRINO. PONER SI HUBO PRESENCIA DE CARTELES------------
sobrino_<-sobrino%>%
  arrange(Municipio,Año)
sobrino_$Presencia <- ifelse(rowSums(sobrino_[, 3:ncol(sobrino_)]) > 0, 1, 0)
sobrino_$SumaPresencia<- rowSums(sobrino_[, 3:ncol(sobrino_)])
sobrino_ <- sobrino_[, -c(3:76)]
#4.  CREAR COLUMNAS EN SOBRINO PARA VER SI UN MUNICIPIO FUE TRATADO, EL TIEMPO QUE LLEVA TRATADO, EL AÑO EN QUE LLEGÓ EL CÁRTEL, ETC.------------
sobrino_ <- sobrino_ %>%
  group_by(Municipio) %>%
  mutate(Treatment = ifelse(sum(Presencia) == 0, 0, 1)) 
sobrino_ <- sobrino_ %>%
  group_by(Municipio) %>%
  mutate(Tiempo = dense_rank(Año)-1)
sobrino_ <- sobrino_ %>% 
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

#5.  ANALIZAR LA BASE DE MAGAR. EMPEZAMOS CON OBSERVAR POSIBLES INCONSISTENCIAS. PARTE 1------------------------------------

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

rm(magar_v)
#6.  VER CUANTAS ELECCIONES TENEMOS POR MUNICIPIO------------------
#Vamos a ver el numero de observaciones por municipio

tabla_frecuencias <- table(magar$Municipio)
# Convertirla a un dataframe
df_tabla_frecuencias <- as.data.frame(tabla_frecuencias)
table(df_tabla_frecuencias$Freq)
#Eliminamos municipios que tengan solo una elección
municipios_frecuencia_1 <- names(tabla_frecuencias[tabla_frecuencias == 1])
municipios_lista <- list(municipios_frecuencia_1)
magar <- magar[!magar$Municipio %in% municipios_frecuencia_1, ]
rm(df_tabla_frecuencias)
rm(municipios_lista)
#7.  OBTENER PORCENTAJE DE VOTOS EN LUGAR DE NÚMERO DE VOTOS-----------------

#Dividimos los votos de cada candidato entre los votos efectivos

magar[columnas_v] <- magar[columnas_v] / magar$efec
#8.  OBTENER LA FECHA DE LA ELECCION-----------------
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

#9.  PARTIDO CONVERGENCIA SE LLAMARÁ MC y PNA, PANAL-------------
columnas_l <- grep("^l[0-2]", names(magar), value = TRUE)
magar <- magar %>%
  mutate(across(.cols = columnas_l, 
                .fns = ~str_replace_all(., c("conve" = "mc", "pna" = "panal"))))
#10. CASOS EN QUE PRESIDENTES FUERON INDEPENDIENTES-----------------------------
#Ver las elecciones en las que ganaron los independientes y las posteriores para ver si hubo alternancia o no.
independientes <- magar %>%
  arrange(Municipio,Año)%>%
  group_by(Municipio) %>%
  filter(any(grepl("indep", l01))) %>% #Quiero las elecciones donde gano un independiente
  filter(Año >= min(Año[grepl("indep", l01)])) %>% #Tambien quiero las posteriores
  #select(Año, Municipio,mun,emm, l01, l02, l03, l04,l05,l06,l07,l08,l09,l10,l11,l12,l13)%>%
  mutate(prev_l01 = lag(l01)) %>% 
  filter(!(Año == 2021 & grepl("indep", l01) & !(grepl("indep", prev_l01)))) %>% #Quitamos las elecciones de 2021 donde gano un independiente. No las queremos porque no sabemos lo que paso despuues. Solo dejamos elecciones 2021 en la que gano un independiente cuando en la eleccion anterior
  #tambien gnao un independiente (hay que ver si se reeligio)
  select(-prev_l01) 
all.equal(colnames(independientes), colnames(magar))
magar<-anti_join(magar,independientes)

#Yo quiero saber si el mismo candidato independiente se reeligio o no.
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
all.equal(colnames(reeleccion), colnames(magar))
colnames(reeleccion)==colnames(magar)
reeleccion<-reeleccion%>%
  select(emm,Año,edon,Municipio, everything())
magar <- rbind(magar, reeleccion)
rm(independientes)
rm(reeleccion)
#11. OBTENER LA FECHA DE LA ELECCION ANTERIOR PARTIDO QUE ESTABA EN EL PODER EN CADA ELECCIÓN-----------------------

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
#12. OBTENER EL PARTIDO QUE GANO EN LA FECHA ANTERIOR, ES DECIR, EL PARTIDO QUE GOBERNABA AL MOMENTO DE ESA ELECCION-------

#Yo quiero que me dé el ganador (se encuentra en la columna l01) que resultó de la Fecha_Elección.
magar <- magar %>%
  arrange(Municipio,date)%>%
  group_by(Municipio) %>%
  mutate(Ganador_Anterior = ifelse(row_number() == 1, NA, l01[match(Fecha_Elección, date)]),
         Primera_Elección=ifelse(row_number() == 1,1,0)) #Creamos una dummy para saber si es la primera elección (y por tanto es normal no saber quién es el incumbente)

#Tengo casos en los que l01 es NA (antes cero). Por lo tanto, no puedo saber quién ganó esa elección. Por lo tanto,
#tampoco puedo saber quién será el partido gobernante en la próxima elección
sum(is.na(magar$l01)&magar$Primera_Elección==0)
#13. SEPARAR EN COLUMNAS CADA PARTIDO DE LA COALICION GOBERNANTE-----------------------

#Paso 1. Para los casos en que el alcalde ganó por uno o más partidos, quiero un partido por columna.
#Tenemos coaliciones de hasta 6 partidos, por eso hay columna Partido 6
count_guiones <- str_count(magar$Ganador_Anterior, "-")
max(count_guiones,na.rm=TRUE)

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
#14. CREAR COLUMNA VOTOOFICIAL, QUE ME DICE EL % DE VOTOS QUE TUVO EL PARTIDO EN EL PODER------------

columnas_l <- grep("^l[0-2]", names(magar), value = TRUE) #lisnom empieza con l. No la incluimos
columnas_partido <- c("Partido1", "Partido2", "Partido3", "Partido4", "Partido5", "Partido6")
buscar_valor_con_prioridad <- function(row, columnas_l, columna_ganador, columnas_partido) {
  
  # Primero revisamos si Ganador_Anterior está definido
  if (is.na(row[[columna_ganador]])) {
    return(NA)  # Si Ganador_Anterior es NA, devolvemos NA
  }
  if (is.na(row[["l01"]])) {
    return(NA) #Cuando l01 es NA, tampoco hay datos para las demás columnas con l
  }
  
  # Recorremos las columnas l para ver si hay coincidencias con  la columna Ganador_Anterior
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
magar$VotoOficial <- apply(magar, 1, buscar_valor_con_prioridad, 
                           columnas_l = columnas_l, 
                           columna_ganador = "Ganador_Anterior", 
                           columnas_partido = columnas_partido)

rm(buscar_valor_con_prioridad)

#En el momento en que no encuentre coincidencia con Ganador_Anterior, se va a las columnas de Partido 1,,, Partido 6.
#La razon es que puede ocurrir que el alcade sea del PRI-PAN y en la próxima elección se postule un candidato PRI-PAN-PRD
# De esta manera, busca si Partido 1 (PRI) tiene coincidencias con alguno de los partidos en las columnas l, selecciona el valor de la izquierda de esa columna l.
# Si no encuentra coincidencias con Partido 1, se va con Partido 2 y así sucesivamente.
#Nota que si la coalición política se rompe, el resultado de VotoOficial dependerá de la coincidencia que encuentre con Partido 1.
#Es decir, el alcade llegó por Partido 1= PRI y Partido 2= PAN. Pero en la próxima elección se pelean y van separados
#Este código tomará como Voto Oficial los votos que haya tenido el PRI y no el PAN en la próxima elección.
#En el proximo paso lo veo mas a detalle.

#15. COALICIONES POLÍTICAS ROTAS-----------------------

#Ahora me interesa saber las elecciones en las que la coalición gobernante se rompió y cada una postuló
#un candidato distinto.

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
#coalición permaneció intacta = más de 22000 elecciones.
#Separacion==0 significa que no encontro coincidencias, es decir, no se postuló candidato!
rm(contar_coincidencias_unicas)

options(scipen = 999)
#16. ¿QUÉ VALOR DE VOTOOFICIAL PONGO CUANDO LA COALICIÓN SE ROMPIÓ? -----------------

#Voy a poner el partido que tuvo más votos. Es decir, si Partido 1 encuentra coincidencias en una
#columna l, el codigo va a seguir buscando hasta terminar las seis columnas de partidos.
#Va a guardar todos los % de votos que encuentre y me va a dar el mayor.

magar_separacion<-magar%>%
  filter(Separación>=2)
all.equal(colnames(magar_separacion),colnames(magar))
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

all.equal(colnames(magar_separacion),colnames(magar))
magar<-rbind(magar,magar_separacion)
rm(magar_separacion)
#17. ANALIZAR SI HUBO REELECCIÓN DEL PARTIDO EN EL PODER---------------------      

#Este codigo lo que hace es que busca si hay alguna coincidencia entre el o los partidos que ganaron
#la eleccion (l01) y el o los partidos en el poder (Partido 1,..., Partido 6)
reeleccion <- function(row, columnas_l, columnas_partido) {
  # Inicializar reelección en 0 por defecto
  reeleccion <- 0
  if (is.na(row[["Ganador_Anterior"]])) {
    return(NA)  
  }
  if (is.na(row[["l01"]])) {
    return(NA)
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
#18. IMPUTAR VALORES FALTANTES DE LISTA NOMINAL-----------------
magar<-magar%>%
  mutate(lisnom=as.numeric(lisnom))
if (FALSE){
imputar <- magar %>%
  group_by(Municipio) %>%
  summarise(
    num_observaciones = n(),                # Número total de observaciones por municipio
    num_na_lisnom = sum(is.na(lisnom)),
    validos= num_observaciones-num_na_lisnom
  )
participacion<-magar%>%
  mutate(participacion=efec/lisnom)%>%
  select(Año,Municipio,emm,participacion)
  
extrapolar <- magar %>%
  arrange(Municipio, Año) %>%
  group_by(Municipio) %>%
  select(Año,Municipio,lisnom,edon,emm)%>%
  slice_head(n = 1)
sum(is.na(extrapolar$lisnom))

magard<-magard%>%
  mutate(
    lisnom_imputado = ifelse(is.na(lisnom), 
                             predict(lm(lisnom ~ Año, data = .)),  # Imputación según tendencia temporal
                             lisnom),
    lisnom_imputado3 = zoo::na.approx(lisnom, x = Año,rule=2),
    lisnom_imputado4= ifelse(is.na(lisnom),na_interpolation(lisnom,option='stine'),lisnom))}
#19. PROCESAR MAGAR_REELECCION PARA COMPARAR SI MIS RESULTADOS DE REELECCIÓN SON IGUALES-------       

#La columna rece.after de esta base me dice si el partido en el poder ganó o perdió
unique(magar_reeleccion$race.after)
sum(magar_reeleccion$race.after=="")
#Vamos a quitar algunas filas de la columna race.after:
magar_reeleccion<-magar_reeleccion%>%
  filter(Año<=2022)
magar_reeleccion <- magar_reeleccion %>%
  mutate(Reelección2 = case_when(
    race.after %in% c("Term-limited-p-lost", "Reran-beaten-dif-p", "Reran-beaten", 
                      "Out-p-lost", "Dead-p-lost", "Dead-term-limited-p-lost", "Reran-p-lost","Reran-dead-p-lost") ~ 0,
    race.after %in% c("Term-limited-p-won", "Reelected","Reran-dif-p", "Dead-p-won", 
                      "Reran-dead-p-won", "out-dead-p-won", "Reelected-dif-p","out-p-won", "Out-p-won") ~ 1,
    TRUE ~ NA_real_
  ))

#Tambien cambiar nombres de pna oy conve por panal y mc
magar_reeleccion <- magar_reeleccion %>%
  mutate(part = str_replace_all(part, c("conve" = "mc", "pna" = "panal")))

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
  select(Año,Municipio,race.after_lag,Reelección2_lag,notes_lag,part_lag,part)
magar_reeleccion <- magar_reeleccion%>%
  filter(!is.na(Reelección2_lag))
#20  MERGE MAGAR Y MAGAR_REELECCION PARA COMPARAR RESULTADOS------------------

#La base magar tiene todas las eleccciones, hayan sido anuladas o no. 
#La base magar_reelección tiene elecciones que no fueron anuladas.
#Voy a crear una dummy en magar para saber si fue elección efectiva o no
#Una vez con esta dummy, voy a poder juntar las elecciones con magar_reelección

#Creamos una dummy para diferenciar si fue efectiva la elección o no
magar <- magar %>%
  filter(Año>=1990)

magar_reeleccion<-magar_reeleccion%>%
  filter(Año>=1990&Municipio!="")%>%
  mutate(Municipio=as.numeric(Municipio),
         Elecc_Val=1)
magar<-merge(magar,magar_reeleccion,by=c("Municipio","Año","Elecc_Val"),all.x=TRUE)
sum(magar$Reelección != magar$Reelección2_lag, na.rm = TRUE)
#No son muchos casos, pero hay que arreglarlo.

#21. DIVIDIR CASOS EN QUE SON DISTINTOS: EMPEZAMOS CON PARTE 1 EN QUE SEPARACION==0---------------------

#Primero modifico unas lineas que en notas no empiezan con reran en notas
magar<-magar%>%
  mutate(notes_lag=if_else(Año==2013&Municipio==29034,'reran under pan and won',notes_lag))%>%
  mutate(notes_lag=if_else(Año==2021&Municipio==4005,'reran under pan-pri-prd and won',notes_lag))%>%
  mutate(notes_lag=if_else(Año==2021&Municipio==21166,'reran under pan-pri-prd and won',notes_lag))

#Separacion0 quiere decir que la coalición goberante no postuló candidato en las próximas elecciones. Pero se pudo haber cambiado de partido
separacion0 <- magar %>%
  filter(Separación == 0 & 
           grepl("^reran", notes_lag))
all.equal(colnames(separacion0),colnames(magar))
magar<-anti_join(magar,separacion0) #Le quito esas filas porque se van a cambiar
separacion0<-separacion0%>%
  mutate(Reelección=Reelección2_lag,
         Ganador_Anterior=str_extract(notes_lag, "(?<=reran under )\\S+"))
#22. PARTE 2: SEPARACION==1-------------

#Separacion=1 significa que la coalición municipal gobernante postuló solo a un candidato
#en la elección siguiente. Lo que yo quiero ver son los posibles casos en que el presidente municipal
#dejó a esos partidos y se fue con otros

separacion1 <- magar %>%
  filter(Separación == 1 & 
           grepl("^reran", notes_lag))
all.equal(colnames(separacion1),colnames(magar))
magar<-anti_join(magar,separacion1)
separacion1 <- separacion1 %>%
  mutate(notes_lag=if_else(Municipio==19012&Año==2018,'reran under indep',notes_lag))
separacion1<-separacion1%>%
  mutate(Reelección=Reelección2_lag,
         Ganador_Anterior=str_extract(notes_lag, "(?<=reran under )\\S+"))
#Nota que cuando un presidente municipal decide reelegirse por otro partido, ahora ese partido será considerado
#como ganador anterior. Esto es para poder asignar qué votos tuvo ese partido
#y tambien para matchearlo correctamente al momento de considerar el partido del goberndor
#si un presidente era del pri y se cambia al pan y el gobernador es del pan, considero que ya son del mismo partido
#23. PARTE 3: SEPARACION>=2---------------------
separacion2 <- magar %>%
  filter(Separación >= 2 & 
           grepl("^reran", notes_lag)|grepl("^ran under", notes_lag))
all.equal(colnames(separacion2),colnames(magar))
magar<-anti_join(magar,separacion2)
separacion2 <- separacion2 %>%
  mutate(notes_lag=if_else(Municipio==15042&Año==2021,'reran under mc and lost',notes_lag))%>%
  mutate(notes_lag=if_else(Municipio==18009&Año==2021,'reran under pt only and won',notes_lag))
separacion2<-separacion2%>%
  mutate(Reelección=Reelección2_lag,
         Ganador_Anterior=str_extract(notes_lag, "(?<=reran under )\\S+"))

#24. JUNTAR LAS TRES BASES DE SEPARACION Y OBTENER VOTOOFICIAL---------------
separacion<-rbind(separacion0,separacion1,separacion2)
rm(separacion0)
rm(separacion1)
rm(separacion2)
#Como se cambiaron de partido los presidentes, cambiamos la columna de Ganador Anterior.
#Por lo tanto, hay que cambiar las columnas de Partidos.

#Tambien va a cambiar la columna de VotoOficial
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
#25. AGREGAR NUEVAS COLUMNAS MODIFICADAS A LA BASE PRINCIPAL 'MAGAR' Y ANALIZAR CASOS QUE SIGUEN SIENDO DISTINTOS-----------
all.equal(colnames(magar),colnames(separacion))
magar<-rbind(magar,separacion)
rm(separacion)
#26. ANALIZAR CASOS EN QUE TENEMOS DISTINTO GANADOR EN MAGAR_REELECCION Y EN MAGAR--------------

#Modifico los nombres de partidos porque en magar los asisntentes le pusieron un numero a algunos partidos
magar <- magar %>%
  mutate(part_lag2= str_replace_all(part_lag, c("pd" = "pd1", "pj" = "pj1","pj11" = "pj1", "pd1s" = "pds", "pd11" = "pd1",'pl'='pl1','pl1m'='plm','pd1m'='pdm')))
#Le digo que no me ponga los casos en que habia una nota
#La razon es que cuando hay nota dice reran under bla bla, por lo tanto, cambie ganador anterior para modificar al partido al que chapulineo el presidente
#Tambien quito los casos independientes
diferenciapartido<-magar%>%
  filter(Ganador_Anterior!=part_lag2,notes_lag=='',part_lag!='indep')%>%
  select(Año,Municipio,Ganador_Anterior,part_lag2,l01,l02,l03,l04,Reelección,Reelección2_lag,notes_lag,race.after_lag,part_lag)
diferenciareeleccion<-magar%>%
  filter(Reelección !=Reelección2_lag)%>%
  select(Año,Municipio,Ganador_Anterior,part_lag2,l01,l02,l03,l04,Reelección,Reelección2_lag,notes_lag,race.after_lag,part_lag)
#Asi se queda
rm(diferenciapartido)
rm(diferenciareeleccion)
#27. ANALIZAR CUANDO REELECCION NO ES NA Y REELECCION 2_LAG SI-------------

#Si Reeleccion es NA y Reeleccion2_lag no, le pondre Reeleccion2_lag
#La gran mayoria son de los 90 porque magar_reeleccion no abarca tan lejos
na1<-magar%>%
  filter(!is.na(Reelección)&is.na(Reelección2_lag))
table(na1$Año)
rm(na1)
#28. REELECCION NA Y REELECCION2_LAG NO
#Hay que quitar cuando el incumbente era consejo municipal.
na2<-magar%>%
  filter(is.na(Reelección)&!is.na(Reelección2_lag)&part_lag!='concejo municipal')
sum(is.na(magar$VotoOficial))

na2<-magar%>%
  filter(is.na(Reelección)&!is.na(Reelección2_lag)&part_lag!='concejo municipal')
#Vamos a ver en que casos podria modificarlo, es decir, cuando l01 no es NA
na2<-na2%>%
  filter(!is.na(l01))
#En los casos en que hubo reeleccion vamos a modificar. En Ganador Anterior ponemos a l01 y en VotoOficial a v01
all.equal(colnames(na2),colnames(magar))
magar<-anti_join(magar,na2)
#na2<-na2%>%
 # mutate(
  #  VotoOficial = as.character(v01))
na2<-na2%>%
  mutate(
    Reelección = if_else(Reelección2_lag == 1, Reelección2_lag, Reelección),
    Ganador_Anterior = if_else(Reelección2_lag == 1, as.character(l01), Ganador_Anterior),
    VotoOficial = if_else(Reelección2_lag == 1, as.character(v01), VotoOficial),
    Partido1 = if_else(Reelección2_lag == 1, as.character(l01), Partido1))
all.equal(colnames(na2),colnames(magar))
magar<-rbind(magar,na2)
rm(na2)
#29. GANADOR ANTERIOR ES NA Y PART_LAG NO------------
na3<-magar%>%
  filter(is.na(Ganador_Anterior)&!is.na(part_lag))
rm(na3)
na4<-magar%>%
  filter(part=='concejo municipal')%>%
  select(Año,Municipio,Ganador_Anterior,part,v01,l01,v02,l02,VotoOficial,Reelección,Reelección2_lag)
rm(na4)
#Es consejo municipal. Se quedan en NA
#XX. ELIMINAR LOS MUNICIPIOS QUE TENGO SOLA UNA ELECCION (AHORA FILTRADO A PARTIR DE 1990)-------------
if (FALSE) {tabla_frecuencias <- table(magar$Municipio)
# Convertirla a un dataframe
df_tabla_frecuencias <- as.data.frame(tabla_frecuencias)
table(df_tabla_frecuencias$Freq)
#Eliminamos municipios que tengan solo una elección
municipios_frecuencia_1 <- names(tabla_frecuencias[tabla_frecuencias == 1])
municipios_lista <- list(municipios_frecuencia_1)
magar <- magar[!magar$Municipio %in% municipios_frecuencia_1, ]
rm(df_tabla_frecuencias)
rm(municipios_lista)}
#XX. JUNTAR BASES DE MAGAR Y SOBRINO---------------------------
magar2 <- magar %>%
  filter(Año<=2020,Elecc_Val==1)
final <- merge(sobrino_,magar2, by = c("Municipio", "Año"),all.x = TRUE)%>% #MismoPartidoGobernador
  select(Año,edon,Municipio,Presencia,SumaPresencia,VotoOficial,Reelección,Separación,AñoLlegada,Time_to_Event,Treatment,Treatment_Post)%>%
  mutate(VotoOficial=as.numeric(VotoOficial),
         Reelección=as.numeric(Reelección))
#XX. QUITAR OBSERVACIONES VOTO OFICIAL==NA Y VER LA DISTANCIA ENTRE LAS ELECCIONES
final<-final%>%
  filter(!is.na(VotoOficial)) %>%
  arrange(Municipio, Año) %>%
  group_by(Municipio) %>%
  mutate(Ventana = if_else(row_number() == 1, 20, Año - lag(Año)))
table(final$Ventana)
length(unique(final$Municipio))

tesis<-final%>% 
  arrange(Municipio,Año)%>%
  group_by(Municipio)%>%
  mutate(PresenciaPREOTAT = cumsum(Presencia),
         PresenciaOTAT= if_else(PresenciaPREOTAT>0,1,0),
         AñoLlegadaChais = ifelse(Treatment == 0, 2021, AñoLlegada))%>%
  mutate(grupo=if_else(Año<AñoLlegadaChais,0,NA),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año == AñoLlegadaChais), 1, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-1 == AñoLlegadaChais), 2, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-2 == AñoLlegadaChais), 3, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-3 == AñoLlegadaChais), 4, grupo),
         grupo = if_else(is.na(grupo) & any(!is.na(VotoOficial) & Año-4 == AñoLlegadaChais), 5, grupo))


#XX. CREAR COLUMNA PRESENCIA ONCE TREATED ALWAYS TREATED

#tres <- magar2 %>%
 # group_by(Municipio) %>%
  #filter(all(Ventana %in% c(3,0,4, 20)))
#length(unique(tres$Municipio))
dos <- tesis %>%
  group_by(Municipio) %>%
  mutate(
    stop_flag = cumsum(!(Ventana %in% c(2, 20))) > 0
  ) %>%
  filter(!stop_flag) %>%
  ungroup()

tres <- tesis %>%
  group_by(Municipio) %>%
  mutate(
    stop_flag = cumsum(!(Ventana %in% c(3, 20))) > 0
  ) %>%
  filter(!stop_flag) %>%
  ungroup()
cuatro <- tesis %>%
  group_by(Municipio) %>%
  mutate(
    stop_flag = cumsum(!(Ventana %in% c(4, 20))) > 0
  ) %>%
  filter(!stop_flag) %>%
  ungroup()
