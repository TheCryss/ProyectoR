
install.packages("readxl")
library("readxl")
my_data <- read_excel("paises.xls",sheet=1)
install.packages("easypackages")

install.packages("devtools")
lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules", "corrplot") #librerias que se requieren
easypackages::packages(lib_req) #se verifican las librerias instaladas

#-------------------------------------------------------------------------------
# ----------------------------NIVELES PARA FACTORES-----------------------------
#-------------------------------------------------------------------------------
install.packages("dplyr")
library(dplyr)
table(my_data$GRUPOS)
#Declaro los niveles correctos
level_group <- c(africa="africa",Africa="africa",AFRICA="africa",asia="asia"
                 ,Asia="asia",ASIA="asia",`Europa Oriental`="europa oriental"
                 ,`EUROPA ORIENTAL`="europa oriental",`ORIENTE MEDIO`="oriente medio"
                 ,iberoamerica="iberoamerica",Iberoamerica="iberoamerica",IBEROAMERICA="iberoamerica",
                 `EO-NA_JAPON_AUSTR_NZ`="eo-na_japon_austr_nz"
                 )

#Actualizo my_data

my_data <- transform(my_data,GRUPOS=factor(dplyr::recode(GRUPOS,!!!level_group)))
str(my_data)

#Reglas
install.packages("editrules")
library(editrules)

#Cargo el archivo de las reglas
Rules <-editrules::editfile("rules.txt")
Rules
#Ver como se relacionan las reglas
#x11()
#plot(Rules)
valid_data=editrules::violatedEdits(Rules, my_data)
summary(valid_data) #Puesto que sale Null ningun valor viola las reglas
