#============================================================================
#----------------------------------------------------------------------------
#primer punto
#El preprocesamiento y limpieza de los datos.
#----------------------------------------------------------------------------
#============================================================================

#Se debe instalar la libreria readxl
install.packages("readxl")
#Cargar libreria
library(readxl)

#Guardar tabla excel en una variable
my_data <- read_excel("paises.xls")

#Visualizar contenido de la variable
my_data

#Instalar librería especial para hacer carga automática de muchas librerias al mismo tiempo
install.packages("easypackages")  
#Cargar libreria
library("easypackages")

#Vector que contiene los nombres de muchas librerias
lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules", "corrplot")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)

#Cuenta la frecuencia de los registros en una columna
table(my_data$GRUPOS)

# Declaración de niveles correctos para las variables tipo Factor
level_grupos <- c(africa="africa", Africa="africa", AFRICA="africa", asia="asia", Asia="asia", ASIA="asia",
                  `Europa Oriental`="europa oriental", `EUROPA ORIENTAL`="europa oriental", 
                  iberoamerica="iberoamerica", Iberoamerica="iberoamerica", IBEROAMERICA="iberoamerica",
                  `ORIENTE MEDIO`="oriente medio", `EO-NA_JAPON_AUSTR_NZ`="eo-na_japon_austr_nz")

#Corrección de los registros de una columna
my_data <- transform(my_data,
                     GRUPOS=factor(dplyr::recode(GRUPOS, !!!level_grupos)))

#Cuenta la frecuencia de los registros en una columna
table(my_data$GRUPOS)

# Carga del archivo de reglas de validación
Rules <- editrules::editfile("consistencia.txt")
Rules

# Verificación de las reglas sobres los datos
editrules::violatedEdits(Rules, my_data) #Usa las reglas de validación en el archivo y las aplica
Valid_Data = editrules::violatedEdits(Rules, my_data)
summary(Valid_Data)

windows()
plot(Valid_Data)


# Función que evalua e identifica los datos faltantes por variable e individuo.
miss<-function(Datos,plot=T){  
  n=nrow(Datos);p=ncol(Datos)
  names.obs<-rownames(Datos)
  
  nobs.comp=sum(complete.cases(Datos))         # Cuenta los registros(filas) completos
  Obs.comp=which(complete.cases(Datos))        # Identifica las posiciones en las que hay registros completos
  nobs.miss = sum(!complete.cases(Datos))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(Datos))       # Identifica los registros con datos faltantes.
  
  Datos.NA<-is.na(Datos) #Arroja una tabla donde pone TRUE en los datos faltantes.
  Var_Num<- sort(colSums(Datos.NA),decreasing=T) #ColSums(tabla) -> Suma los valores por columna, TRUE=1 Y FALSE=0
  #sort -> ordena los valores de un arreglo de manera decreciente indicando decreasing=T
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(Datos.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p, n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    windows(height=10,width=15)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F) #barplot muestra la frecuencia absoluta.
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "Porcentaje de datos faltantes por variable")
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.8,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    #las = 0,1,2 para rotar los nombres de las etiquetas
    #at pone los numeros en el eje
    #pos ubica el eje en el número indicado
    #axis 1,2,3,4
    #axis(ubicación del eje,distancia entre las rayitas, valor que se le colocará a cada rayita, ubicación del eje)
    #axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    #cex.axis tamaño de los valores de las rayitas del eje
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "Porcentaje de datos faltantes por registro")
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}


#Función que detecta los valores faltantes en una tabla 
is.na(my_data)                                 
x11()
#Muestra datos faltantes por columna
visdat::vis_miss(my_data) 
#Uso de la función miss, se guarda el resultado en una variable
Summary.NA = miss(my_data,T) 


# Imputación por la media.
mean(my_data$Población..miles., na.rm=T)
mean(my_data$Esperanza.vida.mujer, na.rm=T)
mean(my_data$PNB, na.rm=T)

imputM = mice::mice(my_data, maxit = 1, method = "mean", seed = 2018, print=F)
my_data_ImputM = mice::complete(imputM)
windows(height=10,width=15); visdat::vis_miss(my_data_ImputM) 

#============================================================================
#----------------------------------------------------------------------------
#segundo punto
#Visualización de datos.
#----------------------------------------------------------------------------
#============================================================================

#2.1
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
install.packages("ggplot2")
library("ggplot2")

x11()
ggplot(my_data_ImputM, aes(x=Grupo)) + 
  ggtitle("Cantidad de países por grupo") +
  geom_bar(colour="black", fill=c("light blue","green","light blue","green","light blue","green")) +
  xlab("Grupos") +
  ylab("Cantidad") +
  xlim(c(0,7)) +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) 


#Según el gráfico la mayoría de países pertenecen al grupo 6, después a los grupos 3 y 5, 
#y el grupo 1 es el que está conformado por la menor cantidad de países 

#2.2
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
x11()
boxplot( my_data_ImputM$Tasa.natalidad, my_data_ImputM$Tasa.mortalidad, my_data_ImputM$Mortalidad.infantil, 
         main="Comparación de la diferentes tasas",
         names = c("Tasa de natalidad","Tasa de mortalidad","Mortalidad infantil"),
         border = c("purple","blue","dark green"),
         col = "white"
)


#2.3
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
my_new_data <- dplyr::mutate(my_data_ImputM,
                             #Si la edad es mayor a la media -> mayor
                             `PNB per cápita` = PNB/Población..miles.)

x11()
boxplot(my_new_data$`PNB per cápita`~my_new_data$Grupo,
        names=c("Europa Oriental","Iberoamérica","na_japon_austr_nz","Oriente Medio","Asia","Africa"),
        main="Evaluación de la variable PNB per cápita por grupo",
        ylab="PNB Per Cápita",
        xlab="Grupos",
        border = c("black","red"),
        col = "white"
)


#2.4
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
PNBPerCapita <- my_new_data$`PNB per cápita`

#Obteniendo los diferente países en los respectivos niveles
pos1 <- which(PNBPerCapita <= quantile(PNBPerCapita, prob=c(0.25)))
Bajo <- my_new_data$País[pos1]

pos2 <- which(PNBPerCapita > quantile(PNBPerCapita, prob=c(0.25)) & PNBPerCapita <= quantile(PNBPerCapita, prob=c(0.5)))
Medio_Bajo <- my_new_data$País[pos2]

pos3 <- which(PNBPerCapita > quantile(PNBPerCapita, prob=c(0.5)) & PNBPerCapita <=quantile(PNBPerCapita, prob=c(0.75)))
Medio_Alto <- my_new_data$País[pos3]

pos4 <- which(PNBPerCapita > quantile(PNBPerCapita, prob=c(0.75)))
Alto <- my_new_data$País[pos4]

#Cargar librería para poder filtrar y seleccionar
library(dplyr)

#Obteniendo los países de cada grupo
grupos <- dplyr::select(my_new_data, Grupo, País)
grupo1 <- dplyr::filter(grupos, Grupo == 1)
grupo2 <- dplyr::filter(grupos, Grupo == 2)
grupo3 <- dplyr::filter(grupos, Grupo == 3)
grupo4 <- dplyr::filter(grupos, Grupo == 4)
grupo5 <- dplyr::filter(grupos, Grupo == 5)
grupo6 <- dplyr::filter(grupos, Grupo == 6)

# --- Función que calcula la cantidad de valores iguales en dos arreglos ---
calcularNivel <- function(vectornivel,vectorgrupo){
  sum <- 0
  for (i in vectornivel){
    for(j in vectorgrupo){
      if(j == i){
        sum <- sum + 1
      }
    }
  }
  return(sum)
}


#-------------------------------------------------------------------------------------------------------

#Haciendo uso de la función establecida anteriormente para obtener el porcentaje de los 4 niveles en cada grupo
bajo1 <- calcularNivel (Bajo, grupo1$País)
medio_bajo1 <-calcularNivel(Medio_Bajo, grupo1$País)
medio_alto1 <- calcularNivel(Medio_Alto, grupo1$País)
alto1 <- calcularNivel(Alto, grupo1$País)

# Tabla de frecuencia
tf1 <- data.frame(nivel = c("Alto","Medio Alto","Medio Bajo", "Bajo"),
                  ni = c(alto1,medio_alto1,medio_bajo1,bajo1),
                  fi = c( (alto1/length(grupo1$País))*100,
                          (medio_alto1/length(grupo1$País))*100,
                          (medio_bajo1/length(grupo1$País))*100,
                          (bajo1/length(grupo1$País))*100),
                  Ni = c( alto1, alto1+medio_alto1, alto1+medio_alto1+medio_bajo1, alto1+medio_alto1+medio_bajo1+bajo1),
                  Fi = c( (alto1/length(grupo1$País))*100,
                          (alto1/length(grupo1$País))*100+(medio_alto1/length(grupo1$País))*100,
                          (alto1/length(grupo1$País))*100+(medio_alto1/length(grupo1$País))*100+(medio_bajo1/length(grupo1$País))*100,
                          (alto1/length(grupo1$País))*100+(medio_alto1/length(grupo1$País))*100+(medio_bajo1/length(grupo1$País))*100+(bajo1/length(grupo1$País))*100))

# Gráfico
x11()
ggplot(tf1, aes(x = nivel, y = fi)) +
  ggtitle("Grupo 1 - Europa Oriental") +
  geom_bar(stat = "identity", position = "dodge", colour="purple", fill=c("#b878f4","#cabbf2","#cabbf2","#b878f4")) +
  ylab("Porcentajes (%)") +
  xlab("Niveles") +
  geom_text(aes(label=paste0(round(fi,1),"%")),vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) 

#-------------------------------------------------------------------------------------------------------

bajo2 <- calcularNivel (Bajo, grupo2$País)
medio_bajo2 <-calcularNivel(Medio_Bajo, grupo2$País)
medio_alto2 <- calcularNivel(Medio_Alto, grupo2$País)
alto2 <- calcularNivel(Alto, grupo2$País)

# Tabla de frecuencia
tf2 <- data.frame(nivel = c("Alto","Medio Alto","Medio Bajo", "Bajo"),
                  ni = c(alto2,medio_alto2,medio_bajo2,bajo2),
                  fi = c( (alto2/length(grupo2$País))*100,
                          (medio_alto2/length(grupo2$País))*100,
                          (medio_bajo2/length(grupo2$País))*100,
                          (bajo2/length(grupo2$País))*100),
                  Ni = c( alto2, alto2+medio_alto2, alto2+medio_alto2+medio_bajo2, alto2+medio_alto2+medio_bajo2+bajo2),
                  Fi = c( (alto2/length(grupo2$País))*100,
                          (alto2/length(grupo2$País))*100+(medio_alto2/length(grupo2$País))*100,
                          (alto2/length(grupo2$País))*100+(medio_alto2/length(grupo2$País))*100+(medio_bajo2/length(grupo2$País))*100,
                          (alto2/length(grupo2$País))*100+(medio_alto2/length(grupo2$País))*100+(medio_bajo2/length(grupo2$País))*100+(bajo2/length(grupo2$País))*100))

# Gráfico
x11()
ggplot(tf2, aes(x = nivel, y = fi)) +
  ggtitle("Grupo 2 - Iberoamerica") +
  geom_bar(stat = "identity", position = "dodge", colour="#12694c", fill=c("#009966","#49ca9f","#49ca9f","#009966")) +
  ylab("Porcentajes (%)") +
  xlab("Niveles") +
  geom_text(aes(label=paste0(round(fi,1),"%")),vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) 

#-------------------------------------------------------------------------------------------------------

bajo3 <- calcularNivel(Bajo, grupo3$País)
medio_bajo3 <-calcularNivel(Medio_Bajo, grupo3$País)
medio_alto3 <- calcularNivel(Medio_Alto, grupo3$País)
alto3 <- calcularNivel(Alto, grupo3$País)

# Tabla de frecuencia
tf3 <- data.frame(nivel = c("Alto","Medio Alto","Medio Bajo", "Bajo"),
                  ni = c(alto3,medio_alto3,medio_bajo3,bajo3),
                  fi = c( (alto3/length(grupo3$País))*100,
                          (medio_alto3/length(grupo3$País))*100,
                          (medio_bajo3/length(grupo3$País))*100,
                          (bajo3/length(grupo3$País))*100),
                  Ni = c( alto3, alto3+medio_alto3, alto3+medio_alto3+medio_bajo3, alto3+medio_alto3+medio_bajo3+bajo3),
                  Fi = c( (alto3/length(grupo3$País))*100,
                          (alto3/length(grupo3$País))*100+(medio_alto3/length(grupo3$País))*100,
                          (alto3/length(grupo3$País))*100+(medio_alto3/length(grupo3$País))*100+(medio_bajo3/length(grupo3$País))*100,
                          (alto3/length(grupo3$País))*100+(medio_alto3/length(grupo3$País))*100+(medio_bajo3/length(grupo3$País))*100+(bajo3/length(grupo3$País))*100))

# Gráfico
x11()
ggplot(tf3, aes(x = nivel, y = fi)) +
  ggtitle("Grupo 3 - EO_NA_JAPON_AUSTR_NZ") +
  geom_bar(stat = "identity", position = "dodge", colour="purple", fill=c("#b878f4","#cabbf2","#cabbf2","#b878f4")) +
  ylab("Porcentajes (%)") +
  xlab("Niveles") +
  geom_text(aes(label=paste0(round(fi,1),"%")),vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

#-------------------------------------------------------------------------------------------------------

bajo4 <- calcularNivel(Bajo, grupo4$País)
medio_bajo4 <-calcularNivel(Medio_Bajo, grupo4$País)
medio_alto4 <- calcularNivel(Medio_Alto, grupo4$País)
alto4 <- calcularNivel(Alto, grupo4$País)

# Tabla de frecuencia
tf4 <- data.frame(nivel = c("Alto","Medio Alto","Medio Bajo", "Bajo"),
                  ni = c(alto4,medio_alto4,medio_bajo4,bajo4),
                  fi = c( (alto4/length(grupo4$País))*100,
                          (medio_alto4/length(grupo4$País))*100,
                          (medio_bajo4/length(grupo4$País))*100,
                          (bajo4/length(grupo4$País))*100),
                  Ni = c( alto4, alto4+medio_alto4, alto4+medio_alto4+medio_bajo4, alto4+medio_alto4+medio_bajo4+bajo4),
                  Fi = c( (alto4/length(grupo4$País))*100,
                          (alto4/length(grupo4$País))*100+(medio_alto4/length(grupo4$País))*100,
                          (alto4/length(grupo4$País))*100+(medio_alto4/length(grupo4$País))*100+(medio_bajo4/length(grupo4$País))*100,
                          (alto4/length(grupo4$País))*100+(medio_alto4/length(grupo4$País))*100+(medio_bajo4/length(grupo4$País))*100+(bajo4/length(grupo4$País))*100))

# Gráfico
x11()
ggplot(tf4, aes(x = nivel, y = fi)) +
  ggtitle("Grupo 4 - Oriente Medio") +
  geom_bar(stat = "identity", position = "dodge", colour="#12694c", fill=c("#009966","#49ca9f","#49ca9f","#009966")) +
  ylab("Porcentajes (%)") +
  xlab("Niveles") +
  geom_text(aes(label=paste0(round(fi,1),"%")),vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

#-------------------------------------------------------------------------------------------------------

bajo5 <- calcularNivel (Bajo, grupo5$País)
medio_bajo5 <-calcularNivel(Medio_Bajo, grupo5$País)
medio_alto5 <- calcularNivel(Medio_Alto, grupo5$País)
alto5 <- calcularNivel(Alto, grupo5$País)

# Tabla de frecuencia
tf5 <- data.frame(nivel = c("Alto","Medio Alto","Medio Bajo", "Bajo"),
                  ni = c(alto5,medio_alto5,medio_bajo5,bajo5),
                  fi = c( (alto5/length(grupo5$País))*100,
                          (medio_alto5/length(grupo5$País))*100,
                          (medio_bajo5/length(grupo5$País))*100,
                          (bajo5/length(grupo5$País))*100),
                  Ni = c( alto5, alto5+medio_alto5, alto5+medio_alto5+medio_bajo5, alto5+medio_alto5+medio_bajo5+bajo5),
                  Fi = c( (alto5/length(grupo5$País))*100,
                          (alto5/length(grupo5$País))*100+(medio_alto5/length(grupo5$País))*100,
                          (alto5/length(grupo5$País))*100+(medio_alto5/length(grupo5$País))*100+(medio_bajo5/length(grupo5$País))*100,
                          (alto5/length(grupo5$País))*100+(medio_alto5/length(grupo5$País))*100+(medio_bajo5/length(grupo5$País))*100+(bajo5/length(grupo5$País))*100))

# Gráfico
x11()
ggplot(tf5, aes(x = nivel, y = fi)) +
  ggtitle("Grupo 5 - Asia") +
  geom_bar(stat = "identity", position = "dodge", colour="purple", fill=c("#b878f4","#cabbf2","#cabbf2","#b878f4")) +
  ylab("Porcentajes (%)") +
  xlab("Niveles") +
  geom_text(aes(label=paste0(round(fi,1),"%")),vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

#-------------------------------------------------------------------------------------------------------

bajo6 <- calcularNivel(Bajo, grupo6$País)
medio_bajo6 <-calcularNivel(Medio_Bajo, grupo6$País)
medio_alto6 <- calcularNivel(Medio_Alto, grupo6$País)
alto6 <- calcularNivel(Alto, grupo6$País)

# Tabla de frecuencia
tf6 <- data.frame(nivel = c("Alto","Medio Alto","Medio Bajo", "Bajo"),
                  ni = c(alto6,medio_alto6,medio_bajo6,bajo6),
                  fi = c( (alto2/length(grupo6$País))*100,
                          (medio_alto6/length(grupo6$País))*100,
                          (medio_bajo6/length(grupo6$País))*100,
                          (bajo6/length(grupo6$País))*100),
                  Ni = c( alto6, alto6+medio_alto6, alto6+medio_alto6+medio_bajo6, alto6+medio_alto6+medio_bajo6+bajo6),
                  Fi = c( (alto6/length(grupo6$País))*100,
                          (alto6/length(grupo6$País))*100+(medio_alto6/length(grupo6$País))*100,
                          (alto6/length(grupo6$País))*100+(medio_alto6/length(grupo6$País))*100+(medio_bajo6/length(grupo6$País))*100,
                          (alto6/length(grupo6$País))*100+(medio_alto6/length(grupo6$País))*100+(medio_bajo6/length(grupo6$País))*100+(bajo6/length(grupo6$País))*100))

# Gráfico
x11()
ggplot(tf6, aes(x = nivel, y = fi)) +
  ggtitle("Grupo 6 - África") +
  geom_bar(stat = "identity", position = "dodge", colour="#12694c", fill=c("#009966","#49ca9f","#49ca9f","#009966")) +
  ylab("Porcentajes (%)") +
  xlab("Niveles") +
  geom_text(aes(label=paste0(round(fi,1),"%")),vjust=-0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
