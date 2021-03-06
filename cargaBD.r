library(RMySQL)
library(DBI)
library(stringr)
library(openxlsx)
#Establish an SQL conection Function
sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(),
                  user = "root",
                  password = "",
                  dbname = "parque_arauco2")
  #dbSendQuery(DB, 'set character set "utf8"')
  dbSendQuery(DB, 'SET NAMES utf8')
  # send Query to btain result set
  rs <- dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  # close db connection
  dbDisconnect(DB)
  # return the dataframe
  return(result)
}

facturas <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\facturas.csv")

rows_facturas <- NROW(facturas)

y <- c("ene. "="01","feb. "="02","mar. "="03","abr. "="04","may. "="05","jun. "="06","jul. "="07","ago. "="08","sept."="09","oct. "="10","nov. "="11","dic. "="12")
for(i in 1:12){
  facturas$Mes.de.facturaciÃ³n <- gsub(names(y[i]),y[[i]],facturas$Mes.de.facturaciÃ³n)
}
facturas$Fecha <- paste(str_sub(facturas$Mes.de.facturaciÃ³n ,-4),str_sub(facturas$Mes.de.facturaciÃ³n ,1,2),"01",sep = "/")

for (i in 1:rows_facturas) {
    sqlQuery(
      paste(
        "INSERT INTO `facturas`(`Numero de factura`, `PROVEEDOR`, `FECHA`, `Total sin impuestos`, `Total imp incluidos`)
        VALUES (",
        facturas$ï..NÃºmero.de.factura[i],
        ",",
        facturas$`Proveedor`[i],
        ",",
        facturas$Fecha[i],
        ",",
        facturas$Total.sin.impuestos[i],
        ",",
        facturas$Total.imp..incluidos[i],
        ")",
        sep = "'"
      )
    )
}


uso_por_acc_201701 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201701_uso_por_acc.csv")
uso_por_acc_201702 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201702_uso_por_acc.csv")
uso_por_acc_201703 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201703_uso_por_acc.csv")
uso_por_acc_201704 <- read.csv2("C:\\Users\\Neobis\\Downloads\\BI PA\\201704_uso_por_acc.csv")

rows_201701 <- NROW(uso_por_acc_201701)
rows_201702 <- NROW(uso_por_acc_201702)
rows_201703 <- NROW(uso_por_acc_201703)
rows_201704 <- NROW(uso_por_acc_201704)

y <- c("ene. "="01","feb. "="02","mar. "="03","abr. "="04","may. "="05","jun. "="06","jul. "="07","ago. "="08","sept."="09","oct. "="10","nov. "="11","dic. "="12")
for(i in 1:12){
  uso_por_acc_201704$PerÃ.odo.de <- gsub(names(y[i]),y[[i]],uso_por_acc_201704$PerÃ.odo.de)
}
uso_por_acc_201704$Fecha <- paste(str_sub(uso_por_acc_201704$PerÃ.odo.de ,-4),str_sub(uso_por_acc_201704$PerÃ.odo.de ,1,2),"01",sep = "/")


for (i in 1:rows_201701) {
  
    sqlQuery(
      paste(
        "INSERT INTO `uso_por_acceso`(`ACCESO`, `PROVEEDOR`, `PAIS`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N. Copias`, `N. Copias B/N`, `N. Copias Color`, `FECHA`)
        VALUES (",
        uso_por_acc_201701$ï..Acceso[i],
        ",",
        uso_por_acc_201701$Proveedor[i],
        ",",
        uso_por_acc_201701$PaÃ.s[i],
        ",",
        uso_por_acc_201701$Total..UF.[i],
        ",",
        uso_por_acc_201701$Plano.tarifario..UF.[i],
        ",",
        uso_por_acc_201701$Uso..UF.[i],
        ",",
        uso_por_acc_201701$Servicios..UF.[i],
        ",",
        uso_por_acc_201701$Descuentos..UF.[i],
        ",",
        uso_por_acc_201701$Voz..UF.[i],
        ",",
        uso_por_acc_201701$Voz.nacional..UF.[i],
        ",",
        uso_por_acc_201701$Voz.inter...UF.[i],
        ",",
        uso_por_acc_201701$Datos..UF.[i],
        ",",
        uso_por_acc_201701$Datos.nac...UF.[i],
        ",",
        uso_por_acc_201701$Datos.inter...UF.[i],
        ",",
        uso_por_acc_201701$SMS..UF.[i],
        ",",
        uso_por_acc_201701$MMS..UF[i],
        ",",
        uso_por_acc_201701$N.Â..Copias[i],
        ",",
        uso_por_acc_201701$N.Â..Copias.B.N[i],
        ",",
        uso_por_acc_201701$N.Â..Copias.Color[i],
        ",",
        uso_por_acc_201701$Fecha,
        ")",
        sep = "'"
        )
    )
}
for (i in 1:rows_201702) {
  
    sqlQuery(
      paste(
        "INSERT INTO `uso_por_acceso`(`ACCESO`, `PROVEEDOR`, `PAIS`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N. Copias`, `N. Copias B/N`, `N. Copias Color`, `FECHA`)
        VALUES (",
        uso_por_acc_201702$ï..Acceso[i],
        ",",
        uso_por_acc_201702$Proveedor[i],
        ",",
        uso_por_acc_201702$PaÃ.s[i],
        ",",
        uso_por_acc_201702$Total..UF.[i],
        ",",
        uso_por_acc_201702$Plano.tarifario..UF.[i],
        ",",
        uso_por_acc_201702$Uso..UF.[i],
        ",",
        uso_por_acc_201702$Servicios..UF.[i],
        ",",
        uso_por_acc_201702$Descuentos..UF.[i],
        ",",
        uso_por_acc_201702$Voz..UF.[i],
        ",",
        uso_por_acc_201702$Voz.nacional..UF.[i],
        ",",
        uso_por_acc_201702$Voz.inter...UF.[i],
        ",",
        uso_por_acc_201702$Datos..UF.[i],
        ",",
        uso_por_acc_201702$Datos.nac...UF.[i],
        ",",
        uso_por_acc_201702$Datos.inter...UF.[i],
        ",",
        uso_por_acc_201702$SMS..UF.[i],
        ",",
        uso_por_acc_201702$MMS..UF.[i],
        ",",
        uso_por_acc_201702$N.Â..Copias[i],
        ",",
        uso_por_acc_201702$N.Â..Copias.B.N[i],
        ",",
        uso_por_acc_201702$N.Â..Copias.Color[i],
        ",",
        uso_por_acc_201702$Fecha,
        ")",
        sep = "'"
        )
    )
}
for (i in 1:rows_201703) {
  
    sqlQuery(
      paste(
        "INSERT INTO `uso_por_acceso`(`ACCESO`, `PROVEEDOR`, `PAIS`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N. Copias`, `N. Copias B/N`, `N. Copias Color`, `FECHA`)
        VALUES (",
        uso_por_acc_201703$ï..Acceso[i],
        ",",
        uso_por_acc_201703$Proveedor[i],
        ",",
        uso_por_acc_201703$PaÃ.s[i],
        ",",
        uso_por_acc_201703$Total..UF.[i],
        ",",
        uso_por_acc_201703$Plano.tarifario..UF.[i],
        ",",
        uso_por_acc_201703$Uso..UF.[i],
        ",",
        uso_por_acc_201703$Servicios..UF.[i],
        ",",
        uso_por_acc_201703$Descuentos..UF.[i],
        ",",
        uso_por_acc_201703$Voz..UF.[i],
        ",",
        uso_por_acc_201703$Voz.nacional..UF.[i],
        ",",
        uso_por_acc_201703$Voz.inter...UF.[i],
        ",",
        uso_por_acc_201703$Datos..UF.[i],
        ",",
        uso_por_acc_201703$Datos.nac...UF.[i],
        ",",
        uso_por_acc_201703$Datos.inter...UF.[i],
        ",",
        uso_por_acc_201703$SMS..UF.[i],
        ",",
        uso_por_acc_201701$MMS..UF.[i],
        ",",
        uso_por_acc_201703$N.Â..Copias[i],
        ",",
        uso_por_acc_201703$N.Â..Copias.B.N[i],
        ",",
        uso_por_acc_201703$N.Â..Copias.Color[i],
        ",",
        uso_por_acc_201703$Fecha,
        ")",
        sep = "'"
        )
    )
}

for (i in 1:rows_201704) {
  
  sqlQuery(
    paste(
      "INSERT INTO `uso_por_acceso`(`ACCESO`, `PROVEEDOR`, `PAIS`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N. Copias`, `N. Copias B/N`, `N. Copias Color`, `FECHA`)
      VALUES (",
      uso_por_acc_201704$ï..Acceso[i],
      ",",
      uso_por_acc_201704$Proveedor[i],
      ",",
      uso_por_acc_201704$PaÃ.s[i],
      ",",
      uso_por_acc_201704$Total..UF.[i],
      ",",
      uso_por_acc_201704$Plano.tarifario..UF.[i],
      ",",
      uso_por_acc_201704$Uso..UF.[i],
      ",",
      uso_por_acc_201704$Servicios..UF.[i],
      ",",
      uso_por_acc_201704$Descuentos..UF.[i],
      ",",
      uso_por_acc_201704$Voz..UF.[i],
      ",",
      uso_por_acc_201704$Voz.nacional..UF.[i],
      ",",
      uso_por_acc_201704$Voz.inter...UF.[i],
      ",",
      uso_por_acc_201704$Datos..UF.[i],
      ",",
      uso_por_acc_201704$Datos.nac...UF.[i],
      ",",
      uso_por_acc_201704$Datos.inter...UF.[i],
      ",",
      uso_por_acc_201704$SMS..UF.[i],
      ",",
      uso_por_acc_201704$MMS..UF.[i],
      ",",
      uso_por_acc_201704$N.Â..Copias[i],
      ",",
      uso_por_acc_201704$N.Â..Copias.B.N[i],
      ",",
      uso_por_acc_201704$N.Â..Copias.Color[i],
      ",",
      uso_por_acc_201704$Fecha,
      ")",
      sep = "'"
      )
    )
}

sqlQuery("update `uso_por_acceso` set
         ACCESO = replace(ACCESO, ' ', '')")
sqlQuery("update `uso_por_acceso` set
        `N. Copias` = replace(`N. Copias`, '.', '')")
sqlQuery("update `uso_por_acceso` set
         `N. Copias B/N` = replace(`N. Copias B/N`, '.', '')")
sqlQuery("update `uso_por_acceso` set
         `N. Copias Color` = replace(`N. Copias Color`, '.', '')")



# Abrir y guardar Archivo export soluciona bug

file <- "C:\\Users\\Neobis\\Downloads\\BI PA\\Exp.xlsx"

USERS <- read.xlsx(
  file,
  sheet = "USERS",
  startRow = 1
)
#Eliminar comilla Simple de nombre de usuario
USERS <-  as.data.frame(lapply(USERS, function(x) {gsub("'", "", x)}))

ACCESSES <- read.xlsx(
  file,
  sheet = "ACCESSES",
  startRow = 1
)

ACCESSES$ACTIVATION.DATE <- as.Date(ACCESSES$ACTIVATION.DATE, origin = "1899-12-30")
ACCESSES$EXPIRATION.DATE <- as.Date(ACCESSES$EXPIRATION.DATE, origin = "1899-12-30")

DEVICES <- read.xlsx(
  file,
  sheet = "DEVICES",
  startRow = 1
)
ASSOCIATIONS <- read.xlsx(
  file,
  sheet = "ASSOCIATIONS",
  startRow = 1
)
PRODUCT_ASSOCIATIONS <- read.xlsx(
  file,
  sheet = "PRODUCT ASSOCIATIONS",
  startRow = 1
)

rows_USERS <- NROW(USERS)
rows_ACCESSES <-NROW(ACCESSES)
rows_DEVICES <-NROW(DEVICES)
rows_ASSOCIATIONS <-NROW(ASSOCIATIONS)
rows_PRODUCT_ASSOCIATIONS <-NROW(PRODUCT_ASSOCIATIONS)

# Eliminar filas Que uno quiera
ACCESSES <- ACCESSES[ACCESSES$ACCESS.NUMBER!="444444449",]


for (i in 1:rows_USERS) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_users`(`UUI`, `TIPO`, `NOMBRE`, `USER ID`, `ESTADO`, `MANAGEMENT_ORG_1`, `MANAGEMENT_ORG_2`, `MANAGEMENT_ORG_3`, `MANAGEMENT_ORG_4`, `SITE_ORG_1_1`, `SITE_ORG_1_2`)
        VALUES (",
        USERS$UUI[i],
        ",",
        USERS$TYPE[i],
        ",",
        USERS$LAST.NAME[i],
        ",",
        USERS$USER.ID[i],
        ",",
        USERS$STATUS[i],
        ",",
        USERS$MANAGEMENT_ORG.1[i],
        ",",
        USERS$MANAGEMENT_ORG.2[i],
        ",",
        USERS$MANAGEMENT_ORG.3[i],
        ",",
        USERS$MANAGEMENT_ORG.4[i],
        ",",
        USERS$SITE_ORG.1.1[i],
        ",",
        USERS$SITE_ORG.1.2[i],
        ")",
        sep = "'"
        )
    )
}

for (i in 1:rows_ACCESSES) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_access`(`ACCESO`, `TIPO`, `ESTADO`, `FECHA EXPIRACION`, `MANAGEMENT_ORG_1`, `MANAGEMENT_ORG_2`, `MANAGEMENT_ORG_3`, `MANAGEMENT_ORG_4`, `PROVEEDOR`, `CARRIER_ORG_2`, `CARRIER_ORG_3`, `FECHA ACTIVACION`)
        VALUES (",
        ACCESSES$ACCESS.NUMBER[i],
        ",",
        ACCESSES$TYPE[i],
        ",",
        ACCESSES$STATUS[i],
        ",",
        ACCESSES$EXPIRATION.DATE[i],
        ",",
        ACCESSES$`MANAGEMENT_ORG:1`[i],
        ",",
        ACCESSES$`MANAGEMENT_ORG:2`[i],
        ",",
        ACCESSES$`MANAGEMENT_ORG:3`[i],
        ",",
        ACCESSES$`MANAGEMENT_ORG:4`[i],
        ",",
        ACCESSES$`CARRIER_ORG:1`[i],
        ",",
        ACCESSES$`CARRIER_ORG:2`[i],
        ",",
        ACCESSES$`CARRIER_ORG:3`[i],
        ",",
        ACCESSES$ACTIVATION.DATE[i],
        ")",
        sep = "'"
        )
    )
}
for (i in 1:rows_DEVICES) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_devices`(`TIPO`, `REFNUM`, `IMEI`, `ESTADO`, `CUSTOM_FIELD_Marca`, `MANAGEMENT_ORG_1`, `MANAGEMENT_ORG_2`, `MANAGEMENT_ORG_3`, `MANAGEMENT_ORG_4`, `SITE_ORG_1_1`, `SITE_ORG_1_2`)
        VALUES (",
        DEVICES$TYPE[i],
        ",",
        DEVICES$REFNUM[i],
        ",",
        DEVICES$IMEI[i],
        ",",
        DEVICES$STATUS[i],
        ",",
        DEVICES$`CUSTOM_FIELD:Marca`[i],
        ",",
        DEVICES$`MANAGEMENT_ORG:1`[i],
        ",",
        DEVICES$`MANAGEMENT_ORG:2`[i],
        ",",
        DEVICES$`MANAGEMENT_ORG:3`[i],
        ",",
        DEVICES$`MANAGEMENT_ORG:4`[i],
        ",",
        DEVICES$`SITE_ORG:1.1`[i],
        ",",
        DEVICES$`SITE_ORG:1.2`[i],
        ")",
        sep = "'"
      )
    )
}
for (i in 1:rows_ASSOCIATIONS) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_associations`(`ACCESO`, `UUI`, `IMEI`, `REFNUM`)
        VALUES (",
        ASSOCIATIONS$ACCESS.NUMBER[i],
        ",",
        ASSOCIATIONS$UUI[i],
        ",",
        ASSOCIATIONS$IMEI[i],
        ",",
        ASSOCIATIONS$REFNUM[i],
        ")",
        sep = "'"
      )
    )
}
for (i in 1:rows_PRODUCT_ASSOCIATIONS) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_product_associations`(`ACCESO`, `PLAN`)
        VALUES (",
        PRODUCT_ASSOCIATIONS$ACCESS.NUMBER[i],
        ",",
        PRODUCT_ASSOCIATIONS$PRODUCT.NAME[i],
        ")",
        sep = "'"
      )
    )
}

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}
killDbConnections()
