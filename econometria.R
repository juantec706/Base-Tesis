## Nombre del script: Econometria.R

## Propósito: Hacer distintos métodos econométricos para la tesis

## Autor: Juan Oliva

## Fecha de creación: 08-01-2025

## Archivos usados:

## Archivos creados: 
#1. INSTALAR PAQUETES-----------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(dplyr,did,didimputation,ggplot2,DIDmultiplegtDYN,gridExtra,ggplotify,fixest)

#2. PREPARAR BASE DE DATOS-----------
tesis2 <- tesis %>%
  arrange(Municipio, Año) %>%  # Aseguramos el orden
  group_by(Municipio) %>%
  mutate(imputation = if_else(first(Año)>=AñoLlegada,1,0),
         imputation = if_else(AñoLlegada==0,0,imputation))%>%
  ungroup()%>%
  filter(imputation==0)
tesis<-tesis%>%
  filter(Separación<2)

#CALLAWAY Y SANTA ANNA-----------------------------------------
tesis<-tesis%>%
  mutate(oposicion=if_else(MismoPartidoGobernador==0,1,0),
         oficialismo=if_else(MismoPartidoGobernador==1,1,0))
tesis<-tesis%>%
  filter(Separación<2)
opositor<-tesis%>%
  filter(oposicion==1)
mismo<-tesis%>%
  filter(oficialismo==1)

Permanencia2Paralell<-att_gt(yname="VotoOficial",
                             tname="Año",
                             idname="Municipio",
                             gname = "AñoLlegada",
                             xformla = ~1,
                             allow_unbalanced_panel = TRUE,
                             clustervars='Municipio',
                             bstrap=TRUE,
                             control_group= "notyettreated",
                             est_method = "reg",
                             base_period = "universal",
                             data=tesis,
                             
)
agg.Permanencia2Paralell<-aggte(Permanencia2Paralell,type="dynamic",na.rm=TRUE)
summary(agg.Permanencia2Paralell)

agg.Permanencia2Paralell <- data.frame(
  att_egt = agg.Permanencia2Paralell[["att.egt"]],
  egt = agg.Permanencia2Paralell[["egt"]],
  se_egt = agg.Permanencia2Paralell[["se.egt"]]
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
agg.Permanencia2Paralell <- agg.Permanencia2Paralell %>%
  mutate(
    conf.low.90 = att_egt - z_90 * se_egt,
    conf.high.90 = att_egt + z_90 * se_egt
  )

colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Callaway_VO <- ggplot(data = agg.Permanencia2Paralell, mapping = aes(y = att_egt, x = egt)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Callaway_VO<- Callaway_VO +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-0.1, .3))


Permanencia2Paralell<-att_gt(yname="Reelección",
                             tname="Año",
                             idname="Municipio",
                             gname = "AñoLlegada",
                             xformla = ~1,
                             allow_unbalanced_panel = TRUE,
                             clustervars='Municipio',
                             bstrap=TRUE,
                             control_group= "notyettreated",
                             est_method = "reg",
                             base_period = "universal",
                             data=tesis
                             
)
agg.Permanencia2Paralell<-aggte(Permanencia2Paralell,type="dynamic",na.rm=TRUE)
summary(agg.Permanencia2Paralell)

agg.Permanencia2Paralell <- data.frame(
  att_egt = agg.Permanencia2Paralell[["att.egt"]],
  egt = agg.Permanencia2Paralell[["egt"]],
  se_egt = agg.Permanencia2Paralell[["se.egt"]]
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
agg.Permanencia2Paralell <- agg.Permanencia2Paralell %>%
  mutate(
    conf.low.90 = att_egt - z_90 * se_egt,
    conf.high.90 = att_egt + z_90 * se_egt
  )

colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Callaway_R <- ggplot(data = agg.Permanencia2Paralell, mapping = aes(y = att_egt, x = egt)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("Reelección.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Callaway_R<- Callaway_R +
  scale_x_continuous(breaks = seq(-10, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.2, .4))

#CALLAWAY Y SANTA ANNA OPOSITOR------------
Permanencia2Paralell<-att_gt(yname="VotoOficial",
                             tname="Año",
                             idname="Municipio",
                             gname = "AñoLlegada",
                             xformla = ~1,
                             allow_unbalanced_panel = TRUE,
                             clustervars='Municipio',
                             bstrap=TRUE,
                             control_group= "notyettreated",
                             est_method = "reg",
                             base_period = "universal",
                             data=opositor
                             
)
agg.Permanencia2Paralell<-aggte(Permanencia2Paralell,type="dynamic",na.rm=TRUE)
summary(agg.Permanencia2Paralell)

agg.Permanencia2Paralell <- data.frame(
  att_egt = agg.Permanencia2Paralell[["att.egt"]],
  egt = agg.Permanencia2Paralell[["egt"]],
  se_egt = agg.Permanencia2Paralell[["se.egt"]]
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
agg.Permanencia2Paralell <- agg.Permanencia2Paralell %>%
  mutate(
    conf.low.90 = att_egt - z_90 * se_egt,
    conf.high.90 = att_egt + z_90 * se_egt
  )

colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Callaway_Opo_VO <- ggplot(data = agg.Permanencia2Paralell, mapping = aes(y = att_egt, x = egt)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("VotoOficial.Opositor.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Callaway_Opo_VO<- Callaway_Opo_VO +
  scale_x_continuous(breaks = seq(-10, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.1, .3))


Permanencia2Paralell<-att_gt(yname="Reelección",
                             tname="Año",
                             idname="Municipio",
                             gname = "AñoLlegada",
                             xformla = ~1,
                             allow_unbalanced_panel = TRUE,
                             clustervars='Municipio',
                             bstrap=TRUE,
                             control_group= "notyettreated",
                             est_method = "reg",
                             base_period = "universal",
                             data=opositor
                             
)
agg.Permanencia2Paralell<-aggte(Permanencia2Paralell,type="dynamic",na.rm=TRUE)
summary(agg.Permanencia2Paralell)

agg.Permanencia2Paralell <- data.frame(
  att_egt = agg.Permanencia2Paralell[["att.egt"]],
  egt = agg.Permanencia2Paralell[["egt"]],
  se_egt = agg.Permanencia2Paralell[["se.egt"]]
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
agg.Permanencia2Paralell <- agg.Permanencia2Paralell %>%
  mutate(
    conf.low.90 = att_egt - z_90 * se_egt,
    conf.high.90 = att_egt + z_90 * se_egt
  )

colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Callaway_Opo_R <- ggplot(data = agg.Permanencia2Paralell, mapping = aes(y = att_egt, x = egt)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("Reeleccion.Opositor.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Callaway_Opo_R<- Callaway_Opo_R +
  scale_x_continuous(breaks = seq(-10, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.4, .6))
#CALLAWAY Y SANTA ANNA MISMO PARTIDO------------
Permanencia2Paralell<-att_gt(yname="VotoOficial",
                             tname="Año",
                             idname="Municipio",
                             gname = "AñoLlegada",
                             xformla = ~1,
                             allow_unbalanced_panel = TRUE,
                             clustervars='Municipio',
                             bstrap=TRUE,
                             control_group= "notyettreated",
                             est_method = "reg",
                             base_period = "universal",
                             data=mismo
                             
)
agg.Permanencia2Paralell<-aggte(Permanencia2Paralell,type="dynamic",na.rm=TRUE)
summary(agg.Permanencia2Paralell)

agg.Permanencia2Paralell <- data.frame(
  att_egt = agg.Permanencia2Paralell[["att.egt"]],
  egt = agg.Permanencia2Paralell[["egt"]],
  se_egt = agg.Permanencia2Paralell[["se.egt"]]
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
agg.Permanencia2Paralell <- agg.Permanencia2Paralell %>%
  mutate(
    conf.low.90 = att_egt - z_90 * se_egt,
    conf.high.90 = att_egt + z_90 * se_egt
  )

colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Callaway_Mismo_VO <- ggplot(data = agg.Permanencia2Paralell, mapping = aes(y = att_egt, x = egt)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("VotoOficial.Mismo.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Callaway_Mismo_VO<- Callaway_Mismo_VO +
  scale_x_continuous(breaks = seq(-10, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.1, .3))


Permanencia2Paralell<-att_gt(yname="Reelección",
                             tname="Año",
                             idname="Municipio",
                             gname = "AñoLlegada",
                             xformla = ~1,
                             allow_unbalanced_panel = TRUE,
                             clustervars='Municipio',
                             bstrap=TRUE,
                             control_group= "notyettreated",
                             est_method = "reg",
                             base_period = "universal",
                             data=mismo
                             
)
agg.Permanencia2Paralell<-aggte(Permanencia2Paralell,type="dynamic",na.rm=TRUE)
summary(agg.Permanencia2Paralell)

agg.Permanencia2Paralell <- data.frame(
  att_egt = agg.Permanencia2Paralell[["att.egt"]],
  egt = agg.Permanencia2Paralell[["egt"]],
  se_egt = agg.Permanencia2Paralell[["se.egt"]]
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
agg.Permanencia2Paralell <- agg.Permanencia2Paralell %>%
  mutate(
    conf.low.90 = att_egt - z_90 * se_egt,
    conf.high.90 = att_egt + z_90 * se_egt
  )

colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Callaway_Mismo_R <- ggplot(data = agg.Permanencia2Paralell, mapping = aes(y = att_egt, x = egt)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("Reeleccion.Mismo.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = egt - 0.3, xend = egt + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Callaway_Mismo_R<- Callaway_Mismo_R +
  scale_x_continuous(breaks = seq(-10, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.2, .4))


#BORUSYAK----------------
Permanencia1Paralell<- did_imputation(data=tesis,yname="VotoOficial", gname="AñoLlegada", first_stage= ~ 0 |
                                        Año + Municipio, 
                                      tname="Año", idname="Municipio", pretrends=c(-17:-1), horizon=TRUE,
                                      cluster_var='Municipio')  %>% dplyr::mutate(term = as.numeric(term),
                                                                                  type = ifelse(term < 0, "Pre", "Post"))

Permanencia1Paralellaux_1 <- Permanencia1Paralell |> filter(term < 0)

Permanencia1Paralellaux_2 <- Permanencia1Paralell |> filter(term >= 0)

#Permanencia1Paralellaux_3 <- tibble(lhs = "PermanenciaOTAT", term = -1, estimate = 0, std.error = 0,
                                    
                                    #conf.low = 0, conf.high = 0, type = "Excluded")


Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2)
#Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2, Permanencia1Paralellaux_3)


#z <- 2.576  # Valor crítico para un nivel de confianza del 99%

#Permanencia11Paralell <- Permanencia11Paralell %>%
  #mutate(
   # conf.low = estimate - z * std.error,
    #conf.high = estimate + z * std.error
  #)


colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Borusyak_VO <- ggplot(data = Permanencia11Paralell, mapping = aes(y = estimate, x = term)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour = "black", size = 1.3) +
  geom_point(shape=18, color = "black", size = 2.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Borusyak_VO<- Borusyak_VO +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.1, .3))


Permanencia1Paralell<- did_imputation(data=tesis,yname="Reelección", gname="AñoLlegada", first_stage= ~ 0 |
                                        Año + Municipio, tname="Año", idname="Municipio", pretrends=c(-5:-1), horizon=TRUE,
                                      cluster_var='Municipio')  %>% dplyr::mutate(term = as.numeric(term),
                                                                                  type = ifelse(term < 0, "Pre", "Post"))

Permanencia1Paralellaux_1 <- Permanencia1Paralell |> filter(term < 0)

Permanencia1Paralellaux_2 <- Permanencia1Paralell |> filter(term >= 0)

#Permanencia1Paralellaux_3 <- tibble(lhs = "PermanenciaOTAT", term = -1, estimate = 0, std.error = 0,

#conf.low = 0, conf.high = 0, type = "Excluded")


Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2)
#Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2, Permanencia1Paralellaux_3)


z <- 1.96  

Permanencia11Paralell <- Permanencia11Paralell %>%
mutate(
 conf.low = estimate - z * std.error,
conf.high = estimate + z * std.error
)


colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Borusyak_R <- ggplot(data = Permanencia11Paralell, mapping = aes(y = estimate, x = term)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour = "black", size = 1.3) +
  geom_point(shape=18, color = "black", size = 2.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Borusyak_R<- Borusyak_R +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.2, .4))

#BORUSYAK OPOSITOR-----------
Permanencia1Paralell<- did_imputation(data=opositor,yname="VotoOficial", gname="AñoLlegada", first_stage= ~ 0 |
                                        Año + Municipio, 
                                      tname="Año", idname="Municipio", pretrends=c(-5:-1), horizon=TRUE,
                                      cluster_var='Municipio')  %>% dplyr::mutate(term = as.numeric(term),
                                                                                  type = ifelse(term < 0, "Pre", "Post"))

Permanencia1Paralellaux_1 <- Permanencia1Paralell |> filter(term < 0)

Permanencia1Paralellaux_2 <- Permanencia1Paralell |> filter(term >= 0)

#Permanencia1Paralellaux_3 <- tibble(lhs = "PermanenciaOTAT", term = -1, estimate = 0, std.error = 0,

#conf.low = 0, conf.high = 0, type = "Excluded")


Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2)
#Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2, Permanencia1Paralellaux_3)


#z <- 2.576  # Valor crítico para un nivel de confianza del 99%

#Permanencia11Paralell <- Permanencia11Paralell %>%
#mutate(
# conf.low = estimate - z * std.error,
#conf.high = estimate + z * std.error
#)


colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Borusyak_Opo_VO <- ggplot(data = Permanencia11Paralell, mapping = aes(y = estimate, x = term)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour = "black", size = 1.3) +
  geom_point(shape=18, color = "black", size = 2.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Borusyak_Opo_VO<- Borusyak_Opo_VO +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.1, .33))

Permanencia1Paralell<- did_imputation(data=opositor,yname="Reelección", gname="AñoLlegada", first_stage= ~ 0 |
                                        Año + Municipio, 
                                      tname="Año", idname="Municipio", pretrends=c(-5:-1), horizon=TRUE,
                                      cluster_var='Municipio')  %>% dplyr::mutate(term = as.numeric(term),
                                                                                  type = ifelse(term < 0, "Pre", "Post"))

Permanencia1Paralellaux_1 <- Permanencia1Paralell |> filter(term < 0)

Permanencia1Paralellaux_2 <- Permanencia1Paralell |> filter(term >= 0)

#Permanencia1Paralellaux_3 <- tibble(lhs = "PermanenciaOTAT", term = -1, estimate = 0, std.error = 0,

#conf.low = 0, conf.high = 0, type = "Excluded")


Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2)
#Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2, Permanencia1Paralellaux_3)


#z <- 2.576  # Valor crítico para un nivel de confianza del 99%

#Permanencia11Paralell <- Permanencia11Paralell %>%
#mutate(
# conf.low = estimate - z * std.error,
#conf.high = estimate + z * std.error
#)


colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Borusyak_Opo_R <- ggplot(data = Permanencia11Paralell, mapping = aes(y = estimate, x = term)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour = "black", size = 1.3) +
  geom_point(shape=18, color = "black", size = 2.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Borusyak_Opo_R<- Borusyak_Opo_R +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.4, .6))

#BORUSYAK MISMO-----------
Permanencia1Paralell<- did_imputation(data=mismo,yname="VotoOficial", gname="AñoLlegada", first_stage= ~ 0 |
                                        Año + Municipio, 
                                      tname="Año", idname="Municipio", pretrends=c(-5:-1), horizon=TRUE,
                                      cluster_var='Municipio')  %>% dplyr::mutate(term = as.numeric(term),
                                                                                  type = ifelse(term < 0, "Pre", "Post"))

Permanencia1Paralellaux_1 <- Permanencia1Paralell |> filter(term < 0)

Permanencia1Paralellaux_2 <- Permanencia1Paralell |> filter(term >= 0)

#Permanencia1Paralellaux_3 <- tibble(lhs = "PermanenciaOTAT", term = -1, estimate = 0, std.error = 0,

#conf.low = 0, conf.high = 0, type = "Excluded")


Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2)
#Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2, Permanencia1Paralellaux_3)


#z <- 2.576  # Valor crítico para un nivel de confianza del 99%

#Permanencia11Paralell <- Permanencia11Paralell %>%
#mutate(
# conf.low = estimate - z * std.error,
#conf.high = estimate + z * std.error
#)


colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Borusyak_Mismo_VO <- ggplot(data = Permanencia11Paralell, mapping = aes(y = estimate, x = term)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour = "black", size = 1.3) +
  geom_point(shape=18, color = "black", size = 2.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Borusyak_Mismo_VO<- Borusyak_Mismo_VO +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.1, .3))


Permanencia1Paralell<- did_imputation(data=mismo,yname="Reelección", gname="AñoLlegada", first_stage= ~ 0 |
                                        Año + Municipio, 
                                      tname="Año", idname="Municipio", pretrends=c(-10:-1), horizon=TRUE,
                                      cluster_var='Municipio')  %>% dplyr::mutate(term = as.numeric(term),
                                                                                  type = ifelse(term < 0, "Pre", "Post"))

Permanencia1Paralellaux_1 <- Permanencia1Paralell |> filter(term < 0)

Permanencia1Paralellaux_2 <- Permanencia1Paralell |> filter(term >= 0)

#Permanencia1Paralellaux_3 <- tibble(lhs = "PermanenciaOTAT", term = -1, estimate = 0, std.error = 0,

#conf.low = 0, conf.high = 0, type = "Excluded")


Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2)
#Permanencia11Paralell <- rbind(Permanencia1Paralellaux_1, Permanencia1Paralellaux_2, Permanencia1Paralellaux_3)


#z <- 2.576  # Valor crítico para un nivel de confianza del 99%

#Permanencia11Paralell <- Permanencia11Paralell %>%
#mutate(
# conf.low = estimate - z * std.error,
#conf.high = estimate + z * std.error
#)


colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
Borusyak_Mismo_R <- ggplot(data = Permanencia11Paralell, mapping = aes(y = estimate, x = term)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.8) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), colour = "black", size = 1.3) +
  geom_point(shape=18, color = "black", size = 2.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
Borusyak_Mismo_R<- Borusyak_Mismo_R +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-.3, .3))



#TWFE----
TWFE<- feols(VotoOficial ~ i(Time_to_Event, Treatment, ref = -1) | 
                       Municipio + Año, cluster = "Municipio", data = tesis)

{
items_values <- TWFE[["model_matrix_info"]][[1]]$items
items_values <- items_values[items_values != -1]
TWFE <- data.frame(
  coefficients = TWFE[["coefficients"]], 
  se = TWFE[["se"]],
  x_= items_values
)
z_90 <- 1.96

# Calcular los intervalos de confianza al 90%
TWFE <- TWFE %>%
  mutate(
    conf.low.90 = coefficients - z_90 * se,
    conf.high.90 = coefficients + z_90 * se
  )
colors <- c("#000000", "#FF0000", "#CCCCCC")

# Crear el gráfico base
TWFE_VO <- ggplot(data = TWFE, mapping = aes(y = coefficients, x = x_)) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
  geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
  geom_point(shape=18, color = "black", size = 2.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
  geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
               colour = "black", size = .7) +
  geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
               colour = "black", size = .7) +
  #geom_line(aes(group = 1), colour = "black", size = .5)+
  theme_bw() +
  ylab("|P de permanencia del mismo partido político\n En porciento") +
  xlab("Años desde que llegó el primer cártel") +
  scale_color_manual(name = "Periodo", values = colors) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
        axis.line.x = element_line(color = "black"), # Línea del eje x en negro
        axis.line.y = element_line(color = "black"), # Línea del eje y en negro
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 12), # Tamaño del título de los ejes
        axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
        axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
  )

# Establecer los intervalos de los ejes
TWFE_VO<- TWFE_VO +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
  coord_cartesian(xlim = c(-10, 20), ylim = c(-0.1, .3))
}

TWFE <- feols(Reelección ~ i(Time_to_Event, Treatment, ref = -1) | 
                   Municipio + Año, cluster = "Municipio", data = tesis)

{
  items_values <- TWFE[["model_matrix_info"]][[1]]$items
  items_values <- items_values[items_values != -1]
  TWFE <- data.frame(
    coefficients = TWFE[["coefficients"]], 
    se = TWFE[["se"]],
    x_= items_values
  )
  z_90 <- 1.96
  
  # Calcular los intervalos de confianza al 90%
  TWFE <- TWFE %>%
    mutate(
      conf.low.90 = coefficients - z_90 * se,
      conf.high.90 = coefficients + z_90 * se
    )
  colors <- c("#000000", "#FF0000", "#CCCCCC")
  
  # Crear el gráfico base
  TWFE_R <- ggplot(data = TWFE, mapping = aes(y = coefficients, x = x_)) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
    geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
    geom_point(shape=18, color = "black", size = 2.2) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
    #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
                 colour = "black", size = .7) +
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
                 colour = "black", size = .7) +
    #geom_line(aes(group = 1), colour = "black", size = .5)+
    theme_bw() +
    ylab("|P de permanencia del mismo partido político\n En porciento") +
    xlab("Años desde que llegó el primer cártel") +
    scale_color_manual(name = "Periodo", values = colors) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
          axis.line.x = element_line(color = "black"), # Línea del eje x en negro
          axis.line.y = element_line(color = "black"), # Línea del eje y en negro
          panel.border = element_blank(),
          plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
          axis.title = element_text(size = 12), # Tamaño del título de los ejes
          axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
          axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
    )
  
  # Establecer los intervalos de los ejes
  TWFE_R<- TWFE_R +
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    coord_cartesian(xlim = c(-10, 20), ylim = c(-0.2, .4))
}
#TWFE OPOSITOR----
TWFE <- feols(VotoOficial ~ i(Time_to_Event, Treatment, ref = -1) | 
                   Municipio + Año, cluster = "Municipio", data = opositor)

{
  items_values <- TWFE[["model_matrix_info"]][[1]]$items
  items_values <- items_values[items_values != -1]
  TWFE <- data.frame(
    coefficients = TWFE[["coefficients"]], 
    se = TWFE[["se"]],
    x_= items_values
  )
  z_90 <- 1.96
  
  # Calcular los intervalos de confianza al 90%
  TWFE <- TWFE %>%
    mutate(
      conf.low.90 = coefficients - z_90 * se,
      conf.high.90 = coefficients + z_90 * se
    )
  colors <- c("#000000", "#FF0000", "#CCCCCC")
  
  # Crear el gráfico base
  TWFE_Opo_VO <- ggplot(data = TWFE, mapping = aes(y = coefficients, x = x_)) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
    geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
    geom_point(shape=18, color = "black", size = 2.2) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
    #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
                 colour = "black", size = .7) +
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
                 colour = "black", size = .7) +
    #geom_line(aes(group = 1), colour = "black", size = .5)+
    theme_bw() +
    ylab("|P de permanencia del mismo partido político\n En porciento") +
    xlab("Años desde que llegó el primer cártel") +
    scale_color_manual(name = "Periodo", values = colors) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
          axis.line.x = element_line(color = "black"), # Línea del eje x en negro
          axis.line.y = element_line(color = "black"), # Línea del eje y en negro
          panel.border = element_blank(),
          plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
          axis.title = element_text(size = 12), # Tamaño del título de los ejes
          axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
          axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
    )
  
  # Establecer los intervalos de los ejes
  TWFE_Opo_VO<- TWFE_Opo_VO +
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    coord_cartesian(xlim = c(-10, 20), ylim = c(-0.1, .3))
}

TWFE<- feols(Reelección ~ i(Time_to_Event, Treatment, ref = -1) | 
                  Municipio + Año, cluster = "Municipio", data = opositor)

{
  items_values <- TWFE[["model_matrix_info"]][[1]]$items
  items_values <- items_values[items_values != -1]
  TWFE <- data.frame(
    coefficients = TWFE[["coefficients"]], 
    se = TWFE[["se"]],
    x_= items_values
  )
  z_90 <- 1.96
  
  # Calcular los intervalos de confianza al 90%
  TWFE <- TWFE %>%
    mutate(
      conf.low.90 = coefficients - z_90 * se,
      conf.high.90 = coefficients + z_90 * se
    )
  colors <- c("#000000", "#FF0000", "#CCCCCC")
  
  # Crear el gráfico base
  TWFE_Opo_R <- ggplot(data = TWFE, mapping = aes(y = coefficients, x = x_)) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
    geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
    geom_point(shape=18, color = "black", size = 2.2) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
    #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
                 colour = "black", size = .7) +
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
                 colour = "black", size = .7) +
    #geom_line(aes(group = 1), colour = "black", size = .5)+
    theme_bw() +
    ylab("|P de permanencia del mismo partido político\n En porciento") +
    xlab("Años desde que llegó el primer cártel") +
    scale_color_manual(name = "Periodo", values = colors) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
          axis.line.x = element_line(color = "black"), # Línea del eje x en negro
          axis.line.y = element_line(color = "black"), # Línea del eje y en negro
          panel.border = element_blank(),
          plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
          axis.title = element_text(size = 12), # Tamaño del título de los ejes
          axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
          axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
    )
  
  # Establecer los intervalos de los ejes
  TWFE_Opo_R<- TWFE_Opo_R +
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    coord_cartesian(xlim = c(-10, 20), ylim = c(-0.4, .6))
}
#TWFE MISMO----
TWFE <- feols(VotoOficial ~ i(Time_to_Event, Treatment, ref = -1) | 
                       Municipio + Año, cluster = "Municipio", data = mismo)

{
  items_values <- TWFE[["model_matrix_info"]][[1]]$items
  items_values <- items_values[items_values != -1]
  TWFE <- data.frame(
    coefficients = TWFE[["coefficients"]], 
    se = TWFE[["se"]],
    x_= items_values
  )
  z_90 <- 1.96
  
  # Calcular los intervalos de confianza al 90%
  TWFE <- TWFE %>%
    mutate(
      conf.low.90 = coefficients - z_90 * se,
      conf.high.90 = coefficients + z_90 * se
    )
  colors <- c("#000000", "#FF0000", "#CCCCCC")
  
  # Crear el gráfico base
  TWFE_Mismo_VO <- ggplot(data = TWFE, mapping = aes(y = coefficients, x = x_)) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
    geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
    geom_point(shape=18, color = "black", size = 2.2) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
    #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
                 colour = "black", size = .7) +
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
                 colour = "black", size = .7) +
    #geom_line(aes(group = 1), colour = "black", size = .5)+
    theme_bw() +
    ylab("|P de permanencia del mismo partido político\n En porciento") +
    xlab("Años desde que llegó el primer cártel") +
    scale_color_manual(name = "Periodo", values = colors) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
          axis.line.x = element_line(color = "black"), # Línea del eje x en negro
          axis.line.y = element_line(color = "black"), # Línea del eje y en negro
          panel.border = element_blank(),
          plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
          axis.title = element_text(size = 12), # Tamaño del título de los ejes
          axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
          axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
    )
  
  # Establecer los intervalos de los ejes
  TWFE_Mismo_VO<- TWFE_Mismo_VO +
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    coord_cartesian(xlim = c(-10, 20), ylim = c(-0.1, .3))
}

TWFE <- feols(Reelección ~ i(Time_to_Event, Treatment, ref = -2) | 
                      Municipio + Año, cluster = "Municipio", data = mismo)

{
  items_values <- TWFE[["model_matrix_info"]][[1]]$items
  items_values <- items_values[items_values != -1]
  TWFE <- data.frame(
    coefficients = TWFE[["coefficients"]], 
    se = TWFE[["se"]],
    x_= items_values
  )
  z_90 <- 1.96
  
  # Calcular los intervalos de confianza al 90%
  TWFE <- TWFE %>%
    mutate(
      conf.low.90 = coefficients - z_90 * se,
      conf.high.90 = coefficients + z_90 * se
    )
  colors <- c("#000000", "#FF0000", "#CCCCCC")
  
  # Crear el gráfico base
  TWFE_Mismo_R <- ggplot(data = TWFE, mapping = aes(y = coefficients, x = x_)) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
    geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
    geom_point(shape=18, color = "black", size = 2.2) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
    #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
                 colour = "black", size = .7) +
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
                 colour = "black", size = .7) +
    #geom_line(aes(group = 1), colour = "black", size = .5)+
    theme_bw() +
    ylab("|P de permanencia del mismo partido político\n En porciento") +
    xlab("Años desde que llegó el primer cártel") +
    scale_color_manual(name = "Periodo", values = colors) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
          axis.line.x = element_line(color = "black"), # Línea del eje x en negro
          axis.line.y = element_line(color = "black"), # Línea del eje y en negro
          panel.border = element_blank(),
          plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
          axis.title = element_text(size = 12), # Tamaño del título de los ejes
          axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
          axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
    )
  
  # Establecer los intervalos de los ejes
  TWFE_Mismo_R<- TWFE_Mismo_R +
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    coord_cartesian(xlim = c(-10, 20), ylim = c(-0.2, .4))
}

#GRAFICAS----------------------
TWFE_VO<-TWFE_VO+theme(plot.margin = margin(3, 3,0, 0))
Callaway_VO<-Callaway_VO+theme(plot.margin = margin(3, 3,0, 0))
Borusyak_VO<-Borusyak_VO+theme(plot.margin = margin(3, 3,0, 0))

TWFE_R<-TWFE_R+theme(plot.margin = margin(3, 3,0, 0))
Callaway_R<-Callaway_R+theme(plot.margin = margin(3, 3,0, 0))
Borusyak_R<-Borusyak_R+theme(plot.margin = margin(3, 3,0, 0))

TWFE_Opo_VO<-TWFE_Opo_VO+theme(plot.margin = margin(3, 3,0, 0))
Callaway_Opo_VO<- Callaway_Opo_VO+theme(plot.margin = margin(3, 3,0, 0))
Borusyak_Opo_VO<- Borusyak_Opo_VO+theme(plot.margin = margin(3, 3,0, 0))

TWFE_Opo_R<-TWFE_Opo_R+theme(plot.margin = margin(3, 3,0, 0))
Callaway_Opo_R<- Callaway_Opo_R+theme(plot.margin = margin(3, 3,0, 0))
Borusyak_Opo_R<-Borusyak_Opo_R+theme(plot.margin = margin(3, 3,0, 0))

TWFE_Mismo_VO<-TWFE_Mismo_VO+theme(plot.margin = margin(3, 3,0, 0))
Callaway_Mismo_VO<- Callaway_Mismo_VO+theme(plot.margin = margin(3, 3,0, 0))
Borusyak_Mismo_VO<-Borusyak_Mismo_VO+theme(plot.margin = margin(3, 3,0, 0))

TWFE_Mismo_R<-TWFE_Mismo_R+theme(plot.margin = margin(3, 3,0, 0))
Callaway_Mismo_R<-Callaway_Mismo_R+theme(plot.margin = margin(3, 3,0, 0))
Borusyak_Mismo_R<-Borusyak_Mismo_R+theme(plot.margin = margin(3, 3,0, 0))

grid.arrange(TWFE_VO,Callaway_VO, Borusyak_VO, ncol = 2, 
             top = "VO")
grid.arrange(TWFE_R,Callaway_R, Borusyak_R, ncol = 2, 
             top = "R")
grid.arrange(TWFE_Opo_VO,Callaway_Opo_VO, Borusyak_Opo_VO,ncol = 2, 
             top = "Opo_VO")
grid.arrange(TWFE_Mismo_VO,Callaway_Mismo_VO, Borusyak_Mismo_VO,ncol = 2, 
             top = "Mismo_VO")
grid.arrange(TWFE_Opo_R,Callaway_Opo_R, Borusyak_Opo_R, ncol = 2, 
             top = "Opo_R")
grid.arrange(TWFE_Mismo_R,Callaway_Mismo_R, Borusyak_Mismo_R, ncol = 2, 
             top = "Mismo_R")
#CHAISMARTIN---------------------
chais<-tesis%>%
  filter(Elecc_Val==1)
chais2<-tesis%>%
  filter(Elecc_Val==1,Ventana%in%c(3,20))
chaisplacebo<-tesis%>%
  filter(Elecc_Val==1,Ventana%in%c(3,20))%>%
  mutate(PresenciaPlacebo=if_else(lead(PresenciaOTAT)==1&PresenciaOTAT==0,1,Presencia),
         PresenciaPlaceboOTAT=if_else(lead(PresenciaOTAT)==1&PresenciaOTAT==0,1,PresenciaOTAT))
chaisplacebo <- chaisplacebo %>%
  arrange(Municipio, Año) %>%
  group_by(Municipio) %>%
  filter(grupo == 0 | lead(PresenciaPlaceboOTAT) != 1)
  
table(chais$Ventana)

efecto10_<-chais%>%
  filter(grupo==1&Año<=AñoLlegada,Ventana%in%c(1,2,4,5))
efecto10<-chais%>%
  filter((grupo==0|grupo==1)&Ventana%in%c(3,20))
efecto10 <- efecto10 %>%
  anti_join(efecto10_, by = "Municipio")

efecto10todo<-chais%>%
  filter((grupo==0|grupo==1)&Ventana%in%c(3,20))
table(chais$grupo)
efecto20_<-chais%>%
  filter(grupo==2&Año-1<=AñoLlegada,Ventana%in%c(1,2,4,5))
efecto20<-chais%>%
  filter((grupo==0|grupo==2)&Ventana%in%c(3,20))
efecto20 <- efecto20 %>%
  anti_join(efecto20_, by = "Municipio")

efecto20todo<-chais%>%
  filter((grupo==0|grupo==2)&Ventana%in%c(3,20))

efecto30_<-chais%>%
  filter(grupo==3&Año-2<=AñoLlegada,Ventana%in%c(1,2,4,5))
efecto30<-chais%>%
  filter((grupo==0|grupo==3)&Ventana%in%c(3,20))
efecto30 <- efecto30 %>%
  anti_join(efecto30_, by = "Municipio")

efecto30todo<-chais%>%
  filter((grupo==0|grupo==3)&Ventana%in%c(3,20))


tesis2<-chais %>%
  group_by(Año, Municipio) %>%
  filter(n() > 1)
temp2 <- did_multiplegt_dyn(efecto10todo, "VotoOficial", "Municipio", "Año", "Presencia", effects = 1,placebo=1,ci_level = 90)
temp <- did_multiplegt_dyn(tesis, "Reelección", "Municipio", "Año", "Presencia", effects = 10,placebo=6,ci_level = 95)
effects <- 4
table <- NULL
for (j in 1:3) {
  temp <- did_multiplegt_dyn(
    subset(tres, tres$grupo %in% c(0, j)), "Reelección", "Municipio", "Año", "PresenciaOTAT", 
    graph_off = TRUE, effects = effects, ci_level = 90)
  rownames(temp$results$Effects) <- 
    sapply(1:temp$results$N_Effects, function(x) paste0("Effect_",  j + (x-1) * 3))
  table <- rbind(table, temp$results$Effects)
}
rown <- unlist(strsplit(rownames(table), "_"))
table <- cbind(table, as.numeric(rown[rown != "Effect"]))
print(table[order(table[,ncol(table)]),1:(ncol(table)-1)])
library(ggplot2)
table <- table[order(table[,ncol(table)]), ]
table <- rbind(rep(0, ncol(table)), table)
colnames(table)[ncol(table)] <- "Time"
table <- as.data.frame(table)
out_plot <- ggplot(table, aes(x = .data$Time, y = .data$Estimate, group = 1)) + 
  #geom_line(colour = "blue") +
  geom_errorbar(data = ~dplyr::filter(.x, table$Estimate != 0), aes(ymin = .data[["LB CI"]], ymax = .data[["UB CI"]]), 
                position=position_dodge(0.05), width = 0.2, colour = "red") + 
  geom_point(colour = "blue") + 
  ggtitle("DID, from last period before treatment changes (t=0) to t") + 
  xlab("Relative time to last period before treatment changes (t=0)") +
  theme(plot.title = element_text(hjust = 0.5))
out_plot<- out_plot +
  scale_x_continuous(breaks = seq(0,15,by = 1)) +
  coord_cartesian(xlim = c(0,16), ylim = c(-0.3, 0.3))
print(out_plot)



placebos<-1
table2 <- NULL
for (j in 1:3) {
  temp <- did_multiplegt_dyn(
    subset(chaisplacebo, chaisplacebo$grupo %in% c(0, j)), "VotoOficial", "Municipio", "Año", "PresenciaPlacebo", 
    graph_off = TRUE, placebo=placebos, ci_level = 90)
  rownames(temp$results$Placebos) <- 
    sapply(1:temp$results$N_Placebos, function(x) paste0("Placebo_",  j + (x-1) * 3))
  table2 <- rbind(table2,temp$results$Placebos)
}
rown2 <- unlist(strsplit(rownames(table2), "_"))
numbers_placebo <- as.numeric(rown2[rown2 != "Placebo"])
numbers_placebo_adjusted <- -numbers_placebo
table2 <- cbind(table2, Placebo_Adjusted_Numbers = numbers_placebo_adjusted)
table2 <- table2[order(table2[,ncol(table2)]), ]
table2 <- rbind(rep(0, ncol(table2)), table2)
colnames(table2)[ncol(table2)] <- "Time"
table2 <- as.data.frame(table2)
out_plot <- ggplot(table2, aes(x = .data$Time, y = .data$Estimate, group = 1)) + 
  #geom_line(colour = "blue") +
  geom_errorbar(data = ~dplyr::filter(.x, table2$Estimate != 0), aes(ymin = .data[["LB CI"]], ymax = .data[["UB CI"]]), 
                position=position_dodge(0.05), width = 0.2, colour = "red") + 
  geom_point(colour = "blue") + 
  ggtitle("DID, from last period before treatment changes (t=0) to t") + 
  xlab("Relative time to last period before treatment changes (t=0)") +
  theme(plot.title = element_text(hjust = 0.5))
out_plot<- out_plot +
  scale_x_continuous(breaks = seq(0,15,by = 1)) +
  coord_cartesian(xlim = c(-5,0), ylim = c(-0.03, 0.03))
print(out_plot)
#SUN AND ABRAHAM------------------
TWFE<- feols(VotoOficial ~ sunab(AñoLlegadaChais,Año) | 
               Municipio + Año, cluster = "Municipio", data = tesis)
summary(TWFE)
{
  TWFE <- data.frame(
    TWFE[["coeftable"]]
  )
  seq_neg <- seq(-28, -2, by = 1)
  seq_zero <- seq(0, 28, by = 1)
  TWFE$x_ <- c(seq_neg, seq_zero)
  z_90 <- 1.96
  
  # Calcular los intervalos de confianza al 90%
  TWFE <- TWFE %>%
    mutate(
      conf.low.90 = Estimate - z_90 * Std..Error,
      conf.high.90 = Estimate + z_90 * Std..Error
    )
  colors <- c("#000000", "#FF0000", "#CCCCCC")
  
  # Crear el gráfico base
  TWFE_VO <- ggplot(data = TWFE, mapping = aes(y = Estimate, x = x_)) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.7) +
    geom_linerange(aes(ymin = conf.low.90, ymax = conf.high.90), colour = "black", size = .7) +
    geom_point(shape=18, color = "black", size = 2.2) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
    #ggtitle("VotoOficial.Separacion2.NotYetTreated. Universal. 95%")+
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.high.90, yend = conf.high.90), 
                 colour = "black", size = .7) +
    geom_segment(aes(x = x_ - 0.3, xend = x_ + 0.3, y = conf.low.90, yend = conf.low.90), 
                 colour = "black", size = .7) +
    #geom_line(aes(group = 1), colour = "black", size = .5)+
    theme_bw() +
    ylab("|P de permanencia del mismo partido político\n En porciento") +
    xlab("Años desde que llegó el primer cártel") +
    scale_color_manual(name = "Periodo", values = colors) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "#F2F2F2", linetype = "dashed", size=1), # Líneas punteadas horizontales
          axis.line.x = element_line(color = "black"), # Línea del eje x en negro
          axis.line.y = element_line(color = "black"), # Línea del eje y en negro
          panel.border = element_blank(),
          plot.margin = margin(t = 1, r = 4, b = 1, l = 4, unit = "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
          axis.title = element_text(size = 12), # Tamaño del título de los ejes
          axis.ticks.length = unit(0.1, "cm"),  # Longitud de las marcas de los ejes
          axis.ticks = element_line(color = "black", size = 1) # Grosor de las marcas de los ejes
    )
  
  # Establecer los intervalos de los ejes
  TWFE_VO<- TWFE_VO +
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    coord_cartesian(xlim = c(-10, 20), ylim = c(-0.1, .3))
}
