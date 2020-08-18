library(readxl)
library(zoo)

#############
#2015 - 2020
PEF2015 <- read_excel("2020-2016/ac01_ra_ur_og PEF2015.xlsx", col_names = TRUE, skip = 11)
PEF2016 <- read_excel("2020-2016/ac01_ra_ur_og PEF2016.xlsx", col_names = TRUE, skip = 11)
PEF2017 <- read_excel("2020-2016/ac01_ra_ur_og PEF2017.xlsx", col_names = TRUE, skip = 11)
PEF2018 <- read_excel("2020-2016/ac01_ra_ur_og PEF2018.xlsx", col_names = TRUE, skip = 11)
PEF2019 <- read_excel("2020-2016/ac01_ra_ur_og PEF2019.xlsx", col_names = TRUE, skip = 11)
PEF2020 <- read_excel("2020-2016/ac01_ra_ur_og PEF2020.xlsx", col_names = TRUE, skip = 11)


require(tidyverse) #fill is part of tidyr

PEF2015$RAMO <- na.locf(PEF2015$RAMO) # Apply na.locf function
PEF2015 <- PEF2015[-c(1, 1), ]
PEF2015$UR <- na.locf(PEF2015$UR)
PEF2015 <- PEF2015[-c(1, 1), ]

PEF2016$RAMO <- na.locf(PEF2016$RAMO) # Apply na.locf function
PEF2016 <- PEF2016[-c(1, 1), ]
PEF2016$UR <- na.locf(PEF2016$UR)
PEF2016 <- PEF2016[-c(1, 1), ]

PEF2017$RAMO <- na.locf(PEF2017$RAMO) # Apply na.locf function
PEF2017 <- PEF2017[-c(1, 1), ]
PEF2017$UR <- na.locf(PEF2017$UR)
PEF2017 <- PEF2017[-c(1, 1), ]

PEF2018$RAMO <- na.locf(PEF2018$RAMO) # Apply na.locf function
PEF2018 <- PEF2018[-c(1, 1), ]
PEF2018$UR <- na.locf(PEF2018$UR)
PEF2018 <- PEF2018[-c(1, 1), ]

PEF2019$RAMO <- na.locf(PEF2019$RAMO) # Apply na.locf function
PEF2019 <- PEF2019[-c(1, 1), ]
PEF2019$UR <- na.locf(PEF2019$UR)
PEF2019 <- PEF2019[-c(1, 1), ]

PEF2020$RAMO <- na.locf(PEF2020$RAMO) # Apply na.locf function
PEF2020 <- PEF2020[-c(1, 1), ]
PEF2020$UR <- na.locf(PEF2020$UR)
PEF2020 <- PEF2020[-c(1, 1), ]

View(PEF2016)
View(PEF2015)
View(PEF2017)
View(PEF2018)
View(PEF2019)
View(PEF2020)

#filtrar
COFECE2015 <- filter(PEF2015, RAMO == "41 Comisión Federal de Competencia Económica")
COFECE2016 <- filter(PEF2016, RAMO == "41 Comisión Federal de Competencia Económica")
COFECE2017 <- filter(PEF2017, RAMO == "41 Comisión Federal de Competencia Económica")
COFECE2018 <- filter(PEF2018, RAMO == "41 Comisión Federal de Competencia Económica")
COFECE2019 <- filter(PEF2019, RAMO == "41 Comisión Federal de Competencia Económica")
COFECE2020 <- filter(PEF2020, RAMO == "41 Comisión Federal de Competencia Económica")




#Ordenamos por UR
by_UR <- group_by(Legis, UR)
summarise(by_UR, delay = mean(Total, na.rm = TRUE))


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