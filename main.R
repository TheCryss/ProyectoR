install.packages("easypackages")
lib_req <- c("devtools", "readxl", "lubridate", "dplyr", "visdat", "missMDA", "mice", "DMwR2", "editrules", "corrplot") # Librerías requeridas
easypackages::packages(lib_req) # Instalación de librerías requeridas


# Lectura de archivo Excel---
library("readxl")
my_data <- read_excel("paises.xls", sheet = 1)

# --------------------------- Análisis pre-limpieza -------------------------- #


is.na(my_data) # For each element in "my_data" verifies if is NA
jpeg(filename = "images/1.jpeg", width = 800, height = 600) # Iniciar proceso de guardado
#x11()
visdat::vis_miss(my_data) # Función para visualizar datos faltantes
dev.off() # Terminar proceso de guardado

#-------------------------------------------------------------------------------
# ----------------------------NIVELES PARA FACTORES-----------------------------
#-------------------------------------------------------------------------------

library(dplyr)
table(my_data$GRUPOS)

# Declaro los niveles correctos
level_group <- c(
                africa = "Africa", Africa = "Africa", AFRICA = "Africa",
                asia = "Asia", Asia = "Asia", ASIA = "Asia",
                `Europa Oriental` = "Europa Oriental", `EUROPA ORIENTAL` = "Europa Oriental",
                `ORIENTE MEDIO` = "Oriente Medio",
                iberoamerica = "Iberoamerica", Iberoamerica = "Iberoamerica", IBEROAMERICA = "Iberoamerica",
                `EO-NA_JAPON_AUSTR_NZ` = "eo-na_japon_austr_nz"
                )

# Actualizo my_data
my_data <- transform(my_data, GRUPOS = factor(dplyr::recode(GRUPOS, !!!level_group)))
str(my_data)

# Cargo el archivo de las reglas
library(editrules)
rules <- editrules::editfile("consistencia.txt")
rules

#Ver como se relacionan las reglas
#x11()
#plot(Rules)
valid_data = editrules::violatedEdits(rules, my_data)
summary(valid_data) # Puesto que sale Null ningún valor viola las reglas
