## Nombre del script: Oaxaca_2004.R

## Propósito: Obtener los resultados electorales por municipio de las elecciones de 2004
##            para gobernador de Oaxaca

## Autor: Juan Oliva

## Fecha de creación: 05-12-2024

## Archivos usados: Oaxaca_2004_Crudo.xls

## Archivos creados: Oaxaca_2004
#1. INSTALAR PAQUETES-----------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(dplyr,readxl,imputeTS,openxlsx,stringr,lubridate,purrr,tidyr)
#2. IMPORTAR LA BASE-----------------------------------------------
archivo_excel <-"C:/Users/jmom_/OneDrive/Escritorio/Resultados estatales/Oaxaca/2004/crudo/Oaxaca_2004_Crudo.xls"
hojas <- excel_sheets(archivo_excel)
hojas <- hojas[-length(hojas)] #No queremos la ultima hoja
#3. FUNCION PARA JUNTAR LAS HOJAS DE LA BASE DE EXCEL-----------------------------------------------
encabezado <- read_excel(archivo_excel, sheet = "DTTO I", range = "A2:P2", col_names = FALSE)

#El problema de esta base es que en hojas como DTTO II las filas donde ponen el total de votos
#estan combinadas. #Por lo tanto, me pone NA donde debería estar el nombre, que es la A. El nombre lo pone en la primer columna
#Por esa razon, tengo que decirle a R que si la columna C es NA y la columna A no, le pase el valor a la columna C.


datos_parciales <- read_excel(archivo_excel, sheet = "DTTO II", range = "A3:C500", col_names = FALSE)
leer_hoja <- function(archivo, hoja) {
  # Leer las columnas A la C hasta la fila 500
  datos_parciales <- read_excel(archivo, sheet = hoja, range = "A3:C500", col_names = FALSE)
  
  #Con esta funcion me deshago del problema de las filas combinadas
  colnames(datos_parciales) <- c("A", "B", "C")  
  datos_parciales <- datos_parciales %>%
    mutate(A = as.character(A))%>%
    mutate(C = if_else(is.na(C) & !is.na(A), A, C))
  
  # Encontrar la última fila que tiene valores que me interesan
  # Estas hojas tienen una fila al final que tiene el total e votos por Distrito. Yo no quiero esto
  # Cuando llego a la ultima fila de votos por municipio, hay una fila vacia y despues la del distrito
  # Le digo que me busque el minimo donde la columna C es NA. Por lo tanto, una fila antes será hasta donde me interesa
  # Para enteder mejor ver el archivo crudo de Excel
  
  ultima_fila_con_valor <- min(which(is.na(datos_parciales$C)))+1

  # Leer los datos completos desde la fila 3 hasta la fila que quiero
  datos_completos <- read_excel(archivo, sheet = hoja, range = paste0("A3:P", ultima_fila_con_valor), col_names = FALSE)
  datos_completos[] <- lapply(datos_completos, as.character)
  
  colnames(datos_completos) <- unlist(encabezado)
  
  #Hacer lo mismo que hago en datos_parciales
  datos_completos <- datos_completos %>%
    mutate(`#` = as.character(`#`)) %>%  # Convierte la columna '#' a tipo character
    mutate(MUNICIPIO = if_else(is.na(MUNICIPIO) & !is.na(`#`), `#`, MUNICIPIO))
  
  datos_completos <- datos_completos %>%
    mutate(Hoja = hoja) 
  return(datos_completos)}
lista_hojas <- lapply(hojas, function(hoja) {
  leer_hoja(archivo_excel, hoja)
})
datos_combinados <- bind_rows(lista_hojas)
#4. CAMBIAR NOMBRES DE COLUMNAS, QUITAR COLUMNAS, CONVERTIR CARACTERES EN NUMEROS---------------------
datos_combinados <- datos_combinados %>%
  rename(
    Mun = "MUNICIPIO"
  )
#Adios columnas innecesarias
datos_combinados <- datos_combinados %>%
  select(-1, -4, -5,-6,-15, -16)

#Columnas numericas
datos_combinados <- datos_combinados %>%
  mutate(across(-c(2, 11), as.numeric))

#5. CHECAR ERROR Y OBTENER DATOS AGREGADOS POR MUNICIPIO-------------------------------

#En esta base no tenemos el error que teniamos en 1998
error<-datos_combinados%>%
  filter(is.na(CVE)&is.na(Mun))

#Pero la base tiene otro error: en el municipio 262 le agregaron una seccion del municipio 269.
#Ver fila 66 de DTOO XVI. Compara con las bases que hay de 1998 y 2004 y esa sección no está en ese municipio

#Primero la guardamos
clave_269<-datos_combinados%>%filter(CVE == 269& Mun == "SAN MIGUEL CHICAHUA")

#Despues de la quitamos al dataframe
datos_combinados<-datos_combinados%>%
  filter(!(CVE == 269& Mun == "SAN MIGUEL CHICAHUA"))
  

#Ahora le digo que me dé la clave de cada municipio para las filas de suma de total de votos
#En las filas del total de votos por municipio la columa clava está vacía. Le voy a decir
#que me dé la de la fila anterior.

datos_combinados<-datos_combinados%>%
  mutate(clave=if_else(is.na(CVE),lag(CVE),CVE))%>%
  filter(is.na(CVE))%>%
  select(-CVE)

datos_combinados <- datos_combinados %>%
  select(clave, Mun, T.S.O, N.F.O., P.U.P., `NO REG.`, `TOT. VOT.`, ABSTEN., `L.N`,Hoja)
#6.QUITAR PALABRA TOTAL DE COLUMNA MUNICIPIO------------------
datos_combinados <- datos_combinados %>%
  mutate(Mun = gsub("Total ", "", Mun, ignore.case = TRUE)) #ignore case ignora masyusculas,minusculas,etc
#7.VER SI HAY UN MUNICIPIO EN MAS DE UNA FILA---------------------------
#Vamos a ver si un municipio está repetido dos veces o no:
table(datos_combinados$clave)[table(datos_combinados$clave)>1]
#Solo la capital está dos veces. Primero corregimos el error y despues sumamos por municipio
#8. CORREGIR ERROR-------------------------------

#Pusieron una sección  que corresponde a San Miguel Huautla a San Miguel Chicahua.
#Aunque sí respetaron la sección correcta (269)
#Lo que tenemos que hacer es restarle estos votos al total de San Miguel Chicahua y sumarselos al total de San Miguel Huautla

#Hay que ver el Excel crudo para entender lo que se está haciendo aqui:


#Esto ya lo habiamos guardado. Es la sección que está en otro municipio
huautla<-clave_269%>%
  mutate(clave=CVE,
         Mun="SAN MIGUEL MIGUEL HUAUTLA EN OTRO LADO")%>%
  select(clave, Mun, T.S.O, N.F.O., P.U.P., `NO REG.`, `TOT. VOT.`, ABSTEN., `L.N`,Hoja)

clave_262<-datos_combinados%>%filter(clave == 262) #Este municipio tiene votos sobreestimados. Hay que quitarselos

datos_combinados<-datos_combinados%>%
  filter(clave!=262)

resta_resultado <- as.data.frame(clave_262[3:9] - clave_269[3:9]) #Le quitamos los votos a Chicahua que no le corresponden
otras_columnas <- clave_262 %>% select(clave, Mun)
chicahua <- bind_cols(otras_columnas, resta_resultado)

#Ahora le ponemos los votos correctos de Chicahua a la base
datos_combinados <- bind_rows(datos_combinados, chicahua)

#Por ahora tendre dos valores con clave 269, como en Toluca. En el siguiente paso los sumo
datos_combinados <- bind_rows(datos_combinados, huautla)  
#9. SUMAR VOTOS DE LA CAPITAL OAXACA DE JUAREZ Y SAN MIGUEL HUAUTLA-------------------------------
#Vemos que la capital está dos veces y Huautla están dos veces. Con el group_by sumamos

table(datos_combinados$clave)[table(datos_combinados$clave)>1]

Oaxaca_2004 <- datos_combinados %>%
  group_by(clave) %>%
  select(-Hoja)%>%
  summarise(
    Mun=first(Mun),
    across(2:(ncol(datos_combinados)-2), ~ sum(. , na.rm = TRUE)),
  )
table(Oaxaca_2004$clave)[table(Oaxaca_2004$clave)>1]
#10.VER PARTIDOS DE LOS CANDIDATOS Y AGREGAR V01,L01,ETC--------------
  
#Checar pagina 108 de Memorias_2004 y encontramos que
#Todos Somos Oaxaca: pan-prd-mc
#Nueva Fuerza Oaxaqueña: pri-pt-pvem
#Unidad Popular: pup (partido unidad popular)

Oaxaca_2004<-Oaxaca_2004%>%
  mutate(
    l01 = tolower("PAN-PRD-MC"),             
    l02 = tolower("PRI-PT-PVEM"),             
    l03 = tolower("PUP"),          
    l04 = tolower("NO_REGISTRADO"),   
    l05 = tolower("VALIDOS"),           
    l06 = tolower("NULOS"),
    l07 = tolower("LISTA_NOMINAL") 
  )
Oaxaca_2004 <- Oaxaca_2004 %>%
  rename_with(
    ~ paste0("v", str_pad(seq(1, 7), width = 2, side = "left", pad = "0")), 
    3:9
  )
#11. AGREGAR COLUMNAS AÑO Y MUNICIPIO--------------------
Oaxaca_2004 <- Oaxaca_2004 %>%
  mutate(Año = 2004,
         Municipio = NA) %>%  
  relocate(Año,Municipio, .before = everything())   
