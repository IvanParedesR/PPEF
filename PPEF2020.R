library(readxl)
library(zoo)

#generar función para repetir nombres

last <- function (x){
  x[length(x)]
}    

fill.NAs <- function(isNA){
  if (isNA[1] == 1) {
    isNA[1:max({which(isNA==0)[1]-1},1)] <- 0 # first is NAs 
    # can't be forward filled
  }
  isNA.neg <- isNA.pos <- isNA.diff <- diff(isNA)
  isNA.pos[isNA.diff < 0] <- 0
  isNA.neg[isNA.diff > 0] <- 0
  which.isNA.neg <- which(as.logical(isNA.neg))
  if (length(which.isNA.neg)==0) return(NULL) # generates warnings later, but works
  which.isNA.pos <- which(as.logical(isNA.pos))
  which.isNA <- which(as.logical(isNA))
  if (length(which.isNA.neg)==length(which.isNA.pos)){
    replacement <- rep(which.isNA.pos[2:length(which.isNA.neg)], 
                       which.isNA.neg[2:max(length(which.isNA.neg)-1,2)] - 
                         which.isNA.pos[1:max(length(which.isNA.neg)-1,1)])      
    replacement <- c(replacement, rep(last(which.isNA.pos), last(which.isNA) - last(which.isNA.pos)))
  } else {
    replacement <- rep(which.isNA.pos[1:length(which.isNA.neg)], which.isNA.neg - which.isNA.pos[1:length(which.isNA.neg)])     
    replacement <- c(replacement, rep(last(which.isNA.pos), last(which.isNA) - last(which.isNA.pos)))
  }
  replacement
}

#############
#2015 - 2020
PEF2015 <- read_excel("2020-2016/ac01_ra_ur_og PEF2015.xlsx", col_names = TRUE, skip = 11)
PEF2016 <- read_excel("2020-2016/ac01_ra_ur_og PEF2016.xlsx", col_names = TRUE, skip = 11)
PEF2016 <- read_excel("2020-2016/ac01_ra_ur_og PEF2016.xlsx", col_names = TRUE, skip = 11)
PEF2017 <- read_excel("2020-2016/ac01_ra_ur_og PEF2017.xlsx", col_names = TRUE, skip = 11)
PEF2018 <- read_excel("2020-2016/ac01_ra_ur_og PEF2018.xlsx", col_names = TRUE, skip = 11)
PEF2019 <- read_excel("2020-2016/ac01_ra_ur_og PEF2019.xlsx", col_names = TRUE, skip = 11)
PEF2020 <- read_excel("2020-2016/ac01_ra_ur_og PEF2020.xlsx", col_names = TRUE, skip = 11)


#Sustituir NA con valorprevio en Ramo 2015
isNA <- as.numeric(is.na(PEF2015$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  PEF2015$RAMO[to.replace] <- PEF2015$RAMO[replacement]
} 


#Sustituir NA con valorprevio en Ramo 2016
isNA <- as.numeric(is.na(PEF2016$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  PEF2016$RAMO[to.replace] <- PEF2016$RAMO[replacement]
} 

#Sustituir NA con valorprevio en Ramo 2017
isNA <- as.numeric(is.na(PEF2017$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  PEF2017$RAMO[to.replace] <- PEF2017$RAMO[replacement]
} 


#Sustituir NA con valorprevio en Ramo 2018
isNA <- as.numeric(is.na(PEF2018$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  PEF2018$RAMO[to.replace] <- PEF2018$RAMO[replacement]
} 


#Sustituir NA con valorprevio en Ramo 2019
isNA <- as.numeric(is.na(PEF2019$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  PEF2019$RAMO[to.replace] <- PEF2019$RAMO[replacement]
} 


#Sustituir NA con valorprevio en Ramo 2020
isNA <- as.numeric(is.na(PEF2020$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  PEF2020$RAMO[to.replace] <- PEF2020$RAMO[replacement]
} 

View(PEF2016)
View(PEF2015)
View(PEF2017)
View(PEF2018)
View(PEF2019)
View(PEF2020)

#Sustituir NA con valorprevio en UR
isNA <- as.numeric(is.na(URBase$UR))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  URBase$UR[to.replace] <- URBase$UR[replacement]
} 

#Cargamos tidyverse
library(tidyverse)

#filtrar
Legis <- filter(URBase, RAMO == "01 Poder Legislativo")

#Eliminamos primeros renglones de cada uno, que son las sumas
Legis <- Legis[-c(1, 2), ]

#Ordenamos por UR
by_UR <- group_by(Legis, UR)
summarise(by_UR, delay = mean(Total, na.rm = TRUE))

############# Pendiente
isNA <- as.numeric(is.na(URBase$PARTIDA))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  URBase$PARTIDA[to.replace] <- URBase$PARTIDA[replacement]
} 

URBase <- URBase[-c(1, 2), ]

install.packages("writexl")
library("writexl")
write_xlsx(URBase,"ac01.xlsx")

sapply(URBase, function(x) sum(is.na(x)))

View(URBase)



#https://www.ppef.hacienda.gob.mx/work/models/PPEF2020/analiticosPresupuestarios/Proyecto/ac01_ra_ur_og.xlsx
#Análisis por Unidad Responsable
URBase <- read_excel("ac01_ra_ur_og.xlsx",
                     range = "A12:F86279")
View(URBase)