# ---------------------------------------------------------------------------- #
#                            Procedimientos previos                            #
# ---------------------------------------------------------------------------- #

install.packages("easypackages")
library("easypackages")
lib_req <- c("ggplot2", "devtools", "readxl", "lubridate", "dplyr", "visdat", "missMDA", "mice", "DMwR2", "editrules", "corrplot") # Librerías requeridas
easypackages::packages(lib_req) # Instalación de librerías requeridas


# Lectura de archivo Excel---
library("readxl")
my_data <- read_excel("paises.xls", sheet = 1)

# ---------------------------------------------------------------------------- #
#                1. El preprocesamiento y limpieza de los datos                #
# ---------------------------------------------------------------------------- #

# Cuenta la frecuencia de los registros en una columna
table(my_data$GRUPOS)

# Declaración de niveles correctos para las variables tipo Factor
level_groups <- c(
                africa = "Africa", Africa = "Africa", AFRICA = "Africa",
                asia = "Asia", Asia = "Asia", ASIA = "Asia",
                `Europa Oriental` = "Europa Oriental", `EUROPA ORIENTAL` = "Europa Oriental",
                `ORIENTE MEDIO` = "Oriente Medio",
                iberoamerica = "Iberoamerica", Iberoamerica = "Iberoamerica", IBEROAMERICA = "Iberoamerica",
                `EO-NA_JAPON_AUSTR_NZ` = "eo-na_japon_austr_nz"
                )

# Actualizo los datos con los factores
my_data <- transform(my_data, GRUPOS = factor(dplyr::recode(GRUPOS, !!!level_groups)))

# Volvemos a verificar los grupos
table(my_data$GRUPOS)

# Carga del archivo de las reglas de validación
rules <- editrules::editfile("consistencia.txt")

# Verificación de las reglas sobres los datos
valid_data = editrules::violatedEdits(rules, my_data) # Usa las reglas de validación en el archivo y las verifica
summary(valid_data)

# Función que evalúa e identifica los datos faltantes por variable e individuo
miss <- function(data, plot = T) {
    n = nrow(data); p = ncol(data)
    names.obs <- rownames(data)

    nobs.comp = sum(complete.cases(data))   # Cuenta los registros(filas) completos
    obs.comp = which(complete.cases(data))  # Identifica las posiciones en las que hay registros completos
    nobs.miss = sum(!complete.cases(data))  # Identifica los registros con datos faltantes.
    obs.miss = which(!complete.cases(data)) # Identifica los registros con datos faltantes.

    data.NA <- is.na(data) # Arroja una matrix (registros x campos) donde pone TRUE en los datos faltantes
    var_num <- sort(colSums(data.NA), decreasing = T) # ColSums(tabla) -> suma los valores por columna, TRUE = 1 Y FALSE = 0
    # sort -> ordena los valores de un arreglo de manera decreciente indicando decreasing = T
    var_per <- round(var_num / n, 3)
    obs_num <- rowSums(data.NA)
    names(obs_num) <- names.obs
    obs_num <- sort(obs_num, decreasing = T)
    obs_per <- round(obs_num / p, 3)
    list <- list(
            n.row = n,
            n.col = p,
            n.comp = nobs.comp,
            obs.comp = obs.comp,
            n.miss = nobs.miss,
            obs.miss = obs.miss,
            var.n = var_num ,
            var.p = var_per,
            obs.n = obs_num,
            obs.per = obs_per
            )
    if(plot) {
        windows(height = 10,width = 15)
        par(mfrow = c(1,2))

        coord <- barplot(var_per, plot = F) # barplot muestra la frecuencia absoluta.
        barplot(
                var_per,
                xaxt = "n",
                horiz = T,
                yaxt = "n",
                xlim = c(-0.2,1),
                ylim = c(0, max(coord) + 1),
                main= "% Porcentaje de datos faltantes por variable"
                )
        axis(
            2,
            at = coord,
            labels = names(var_per),
            cex.axis = 0.8,
            pos = 0,
            las = 2
            )

        axis(1, seq(0, 1, 0.2), seq(0, 1, 0.2), pos = 0)
        # 0, 1, 0.2 para rotar los nombres de las etiquetas
        # "at" pone los números en el eje
        # "pos" ubica el eje en el número indicado
        # axis 1,2,3,4
        # axis(ubicación del eje,distancia entre las rayas, valor que se le colocará a cada raya)
        # cex.axis tamaño de los valores de las rayas del eje

        coord <- barplot(obs_per, plot = F)
        barplot(
            obs_per,
            xaxt = "n",
            horiz = T,
            yaxt = "n",
            xlim = c(-0.2, 1),
            ylim = c(0, max(coord) + 1),
            main= "Porcentaje de datos faltantes por registro"
            )

            axis(
                2,
                at = coord,
                labels = names(obs_per),
                cex.axis = 0.5,
                pos = 0,
                las = 2
                )

        axis(1, seq(0, 1, 0.2), seq(0, 1, 0.2))
    }
    return(invisible(list))
}

# Función que detecta los valores faltantes en una tabla
is.na(my_data)

# Muestra datos faltantes por columna
visdat::vis_miss(my_data)

# Uso de la función miss, se guarda el resultado en una variable
Summary.NA = miss(my_data, T)

# Imputación por la media---
mean(my_data$Población..miles., na.rm = T) # na.rm = TRUE ignora los datos faltantes
mean(my_data$Esperanza.vida.mujer, na.rm = T)
mean(my_data$PNB, na.rm = T)


imputed_mean = mice::mice(my_data, maxit = 1, method = "mean", seed = 2018, print = F)
my_data = mice::complete(imputed_mean) # data_imputed_mean
visdat::vis_miss(my_data)

save(my_data, file = "cleaned_data.RData")

# ---------------------------------------------------------------------------- #
#                           2. Visualización de datos                          #
# ---------------------------------------------------------------------------- #

# ------------------------------------ 2.1 ----------------------------------- #

# Para no hacer todo el proceso desde el principio
load("cleaned_data.RData")

library("ggplot2")

x11()
ggplot(
    my_data, aes(x = Grupo)) +
    ggtitle("Cantidad de países por grupo") +
    geom_bar(colour = "black", fill = c("light blue", "green", "light blue", "green", "light blue", "green")) +
    xlab("Grupos") +
    ylab("Cantidad") +
    xlim(c(0, 7)) +
    geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
)

# ------------------------------------ 2.2 ----------------------------------- #

x11()
boxplot(my_data$Tasa.natalidad, my_data$Tasa.mortalidad, my_data$Mortalidad.infantil,
        main = "Comparación de la diferentes tasas",
        names = c("Tasa de natalidad", "Tasa de mortalidad", "Mortalidad infantil"),
        border = c("purple", "blue", "dark green"),
        col = "white"
)

# ------------------------------------ 2.3 ----------------------------------- #

my_data <- dplyr::mutate(my_data,`PNB per cápita` = PNB / Población..miles.)

x11()
boxplot(my_data$`PNB per cápita`~my_data$Grupo,
        names = c("Europa Oriental", "Iberoamérica", "na_japon_austr_nz", "Oriente Medio","Asia","Africa"),
        main = "Evaluación de la variable PNB per cápita por grupo",
        ylab = "PNB Per Cápita",
        xlab = "Grupos",
        border = c("black","red"),
        col = "white"
)

# ------------------------------------ 2.4 ----------------------------------- #

PNB_per_capita <- my_data$`PNB per cápita`

# Obteniendo los diferente países en los respectivos niveles
pos1 <- which(PNB_per_capita <= quantile(PNB_per_capita, prob = c(0.25)))
bajo <- my_data$País[pos1]

pos2 <- which(PNB_per_capita > quantile(PNB_per_capita, prob = c(0.25)) & PNB_per_capita <= quantile(PNB_per_capita, prob = c(0.5)))
medio_bajo <- my_data$País[pos2]

pos3 <- which(PNB_per_capita > quantile(PNB_per_capita, prob = c(0.5)) & PNB_per_capita <= quantile(PNB_per_capita, prob = c(0.75)))
medio_alto <- my_data$País[pos3]

pos4 <- which(PNB_per_capita > quantile(PNB_per_capita, prob = c(0.75)))
alto <- my_data$País[pos4]

# Cargar librería para poder filtrar y seleccionar
library(dplyr)

# Obteniendo los países de cada grupo
grupos <- dplyr::select(my_data, Grupo, País)
grupo1 <- dplyr::filter(grupos, Grupo == 1) # Europa Oriental
grupo2 <- dplyr::filter(grupos, Grupo == 2) # Iberoamerica
grupo3 <- dplyr::filter(grupos, Grupo == 3) # eo-na_japon_austr_nz
grupo4 <- dplyr::filter(grupos, Grupo == 4) # Oriente Medio
grupo5 <- dplyr::filter(grupos, Grupo == 5) # Asia
grupo6 <- dplyr::filter(grupos, Grupo == 6) # Africa

# --------------------------------- Funciones -------------------------------- #

# --- Función que calcula la cantidad de valores iguales en dos arreglos ---
calcular_nivel <- function(vector_nivel, vector_grupo) {
    sum <- 0
    for(i in vector_nivel) {
        for(j in vector_grupo) {
            if(j == i)  sum <- sum + 1
        }
    }
    return (sum)
}

# --- Función que crea tablas de frecuencia para este caso en específico ---
crear_tabla_frecuencia <- function(alto, medio_alto, medio_bajo, bajo, grupo) {
    numero_paises <- length(grupo$País)
    tf <- data.frame(
                    nivel = c("Alto", "Medio alto", "Medio bajo", "Bajo"),
                    ni = c(alto, medio_alto, medio_bajo, bajo),
                    fi = c(
                        (alto / numero_paises) * 100,
                        (medio_alto / numero_paises) * 100,
                        (medio_bajo / numero_paises) * 100,
                        (bajo / numero_paises) * 100
                        ),
                    Ni = c(alto, alto + medio_alto, alto + medio_alto + medio_bajo, alto + medio_alto + medio_bajo + bajo),
                    Fi = c(
                        (alto / numero_paises) * 100,
                        (alto / numero_paises) * 100 + (medio_alto / numero_paises) * 100,
                        (alto / numero_paises) * 100 + (medio_alto / numero_paises) * 100 + (medio_bajo / numero_paises) * 100,
                        (alto / numero_paises) * 100 + (medio_alto / numero_paises) * 100 + (medio_bajo / numero_paises) * 100 + (bajo / numero_paises) * 100)
        )

    return(tf)
}

# --- Función que crea gráficos específicos para este proyecto ---
crear_grafico <- function(tf, color1, color2, color3, titulo) {
    g <- ggplot(tf, aes(x = nivel, y = ni)) +
                ggtitle(titulo) +
                geom_bar(stat = "identity", position = "dodge", colour = color3, fill = c(color2, color2, color1, color1)) +
                ylab("Cantidad") +
                xlab("Niveles") +
                scale_y_continuous(breaks = seq(0, 11, 1)) +
                geom_text(aes(label = paste0(round(fi, 1), "%")), vjust = -0.5) +
                theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
                )
    return (g)
}

# -------------------------------------------------------------------------- #

# --- Haciendo uso de la función establecida anteriormente para obtener el porcentaje de los 4 niveles en cada grupo
bajo1 <- calcular_nivel(bajo, grupo1$País)
medio_bajo1 <- calcular_nivel(medio_bajo, grupo1$País)
medio_alto1 <- calcular_nivel(medio_alto, grupo1$País)
alto1 <- calcular_nivel(alto, grupo1$País)

# Tabla de frecuencia
tf1 <- crear_tabla_frecuencia(alto1, medio_alto1, medio_bajo1, bajo1, grupo1)

# Gráfico
x11()
crear_grafico(tf1, "#b878f4", "#cabbf2", "purple", "Grupo 1 - Europa Oriental")

# -------------------------------------------------------------------------- #

bajo2 <- calcular_nivel(bajo, grupo2$País)
medio_bajo2 <- calcular_nivel(medio_bajo, grupo2$País)
medio_alto2 <- calcular_nivel(medio_alto, grupo2$País)
alto2 <- calcular_nivel(alto, grupo2$País)

# Tabla de frecuencia
tf2 <- crear_tabla_frecuencia(alto2, medio_alto2, medio_bajo2, bajo2, grupo2)

# Gráfico
x11()
crear_grafico(tf2, "#009966", "#49ca9f", "#12694c", "Grupo 2 - Iberoamérica")

#-------------------------------------------------------------------------------------------------------

bajo3 <- calcular_nivel(bajo, grupo3$País)
medio_bajo3 <- calcular_nivel(medio_bajo, grupo3$País)
medio_alto3 <- calcular_nivel(medio_alto, grupo3$País)
alto3 <- calcular_nivel(alto, grupo3$País)

# Tabla de frecuencia
tf3 <- crear_tabla_frecuencia(alto3, medio_alto3, medio_bajo3, bajo3, grupo3)

# Gráfico
x11()
crear_grafico(tf3,"#3d85c6","#67afef", "#236cae", "Grupo 3 - EO_NA_JAPON_AUSTR_NZ")

#-------------------------------------------------------------------------------------------------------

bajo4 <- calcular_nivel(bajo, grupo4$País)
medio_bajo4 <- calcular_nivel(medio_bajo, grupo4$País)
medio_alto4 <- calcular_nivel(medio_alto, grupo4$País)
alto4 <- calcular_nivel(alto, grupo4$País)

# Tabla de frecuencia
tf4 <- crear_tabla_frecuencia(alto4, medio_alto4, medio_bajo4, bajo4, grupo4)

# Gráfico
x11()
crear_grafico(tf4, "#009966", "#49ca9f", "#12694c", "Grupo 4 - Oriente Medio")

#-------------------------------------------------------------------------------------------------------

bajo5 <- calcular_nivel(bajo, grupo5$País)
medio_bajo5 <-calcular_nivel(medio_bajo, grupo5$País)
medio_alto5 <- calcular_nivel(medio_alto, grupo5$País)
alto5 <- calcular_nivel(alto, grupo5$País)

# Tabla de frecuencia
tf5 <- crear_tabla_frecuencia(alto5, medio_alto5, medio_bajo5, bajo5, grupo5)

# Gráfico
x11()
crear_grafico(tf5, "#b878f4", "#cabbf2", "purple", "Grupo 5 - Asia")

#-------------------------------------------------------------------------------------------------------

bajo6 <- calcular_nivel(bajo, grupo6$País)
medio_bajo6 <-calcular_nivel(medio_bajo, grupo6$País)
medio_alto6 <- calcular_nivel(medio_alto, grupo6$País)
alto6 <- calcular_nivel(alto, grupo6$País)

# Tabla de frecuencia
tf6 <- crear_tabla_frecuencia(alto6, medio_alto6, medio_bajo6, bajo6, grupo6)

# Gráfico
x11()
crear_grafico(tf6, "#3d85c6", "#67afef", "#236cae", "Grupo 6 - África")