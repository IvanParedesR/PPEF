library(readxl)
#https://www.ppef.hacienda.gob.mx/work/models/PPEF2020/analiticosPresupuestarios/Proyecto/ac01_ra_ur_og.xlsx
#Análisis por Unidad Responsable
URBase <- read_excel("ac01_ra_ur_og.xlsx",
range = "A12:F86279")

library(zoo)
View(URBase)

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

isNA <- as.numeric(is.na(URBase$RAMO))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  URBase$RAMO[to.replace] <- URBase$RAMO[replacement]
} 

isNA <- as.numeric(is.na(URBase$UR))
replacement <- fill.NAs(isNA)
if (length(replacement)){
  which.isNA <- which(as.logical(isNA))
  to.replace <- which.isNA[which(isNA==0)[1]:length(which.isNA)]
  URBase$UR[to.replace] <- URBase$UR[replacement]
} 

URBase <- URBase[-c(1, 2), ]


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
