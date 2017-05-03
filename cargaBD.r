library(RMySQL)

#Establish an SQL conection Function
sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(),
                  user = "root",
                  password = "",
                  dbname = "parque_arauco2")
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

for (i in 1:rows_facturas) {
    sqlQuery(
      paste(
        "INSERT INTO `facturas`(`Número de factura`,`Proveedor`,`date`,`Total sin impuestos`,`Total imp incluidos`)
        VALUES (",
        facturas$ï..NÃºmero.de.factura[i],
        ",",
        facturas$Proveedor[i],
        ",",
        facturas$Mes.de.facturaciÃ³n[i],
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

rows_201701 <- NROW(uso_por_acc_201701)
rows_201702 <- NROW(uso_por_acc_201702)
rows_201703 <- NROW(uso_por_acc_201703)

total<- rows_201701+rows_201702+rows_201703

date_201701 <- "2017-01-01"
date_201702 <- "2017-02-01"
date_201703 <- "2017-03-01"

for (i in 1:rows_201701) {
  
    sqlQuery(
      paste(
        "INSERT INTO `uso_por_acceso`(`Acceso`, `Proveedor`, `País`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, 
      `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, 
      `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N.° Copias`, `N.° Copias B/N`, `N.° Copias Color`, `Date`)
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
        date_201701,
        ")",
        sep = "'"
        )
    )
}
for (i in 1:rows_201702) {
  
    sqlQuery(
      paste(
        "INSERT INTO `uso_por_acceso`(`Acceso`, `Proveedor`, `País`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, 
      `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, 
       `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N.° Copias`, `N.° Copias B/N`, `N.° Copias Color`, `Date`)
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
        date_201702,
        ")",
        sep = "'"
        )
    )
}
for (i in 1:rows_201703) {
  
    sqlQuery(
      paste(
        "INSERT INTO `uso_por_acceso`(`Acceso`, `Proveedor`, `País`, `Total (UF)`, `Plano tarifario (UF)`, `Uso (UF)`, 
      `Servicios (UF)`, `Descuentos (UF)`, `Voz (UF)`, `Voz nacional (UF)`, `Voz inter. (UF)`, `Datos (UF)`, 
      `Datos nac. (UF)`, `Datos inter. (UF)`, `SMS (UF)`, `MMS (UF)`, `N.° Copias`, `N.° Copias B/N`, `N.° Copias Color`, `Date`)
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
        date_201703,
        ")",
        sep = "'"
        )
    )
}


library(openxlsx)

file <- "C:\\Users\\Neobis\\Downloads\\BI PA\\Exp.xlsx"

USERS <- read.xlsx(
  file,
  sheet = "USERS",
  startRow = 1
)

ACCESSES <- read.xlsx(
  file,
  sheet = "ACCESSES",
  startRow = 1
)
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

for (i in 1:rows_USERS) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_users`(`UUI`,`TYPE`,`LAST NAME`,`USER ID`,`STATUS`,
        `MANAGEMENT_ORG_1`,`MANAGEMENT_ORG_2`,`MANAGEMENT_ORG_3`,`MANAGEMENT_ORG_4`
        ,`SITE_ORG_1_1`,`SITE_ORG_1_2`)
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
        USERS$`MANAGEMENT_ORG:1`[i],
        ",",
        USERS$`MANAGEMENT_ORG:2`[i],
        ",",
        USERS$`MANAGEMENT_ORG:3`[i],
        ",",
        USERS$`MANAGEMENT_ORG:4`[i],
        ",",
        USERS$`SITE_ORG:1.1`[i],
        ",",
        USERS$`SITE_ORG:1.2`[i],
        ")",
        sep = "'"
        )
    )
}

for (i in 1:rows_ACCESSES) {
  
    sqlQuery(
      paste(
        "INSERT INTO `export_ACCESS`(`ACCESS NUMBER`,`TYPE`,`STATUS`,`EXPIRATION DATE`,`MANAGEMENT_ORG_1`,`MANAGEMENT_ORG_2`,
        `MANAGEMENT_ORG_3`,`MANAGEMENT_ORG_4`
        ,`CARRIER_ORG_1`,`CARRIER_ORG_2`,`CARRIER_ORG_3`,`ACTIVATION DATE`)
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
        "INSERT INTO `export_DEVICES`(`TYPE`,`REFNUM`,`IMEI`,`STATUS`,`CUSTOM_FIELD_Marca`,
        `MANAGEMENT_ORG_1`,`MANAGEMENT_ORG_2`,`MANAGEMENT_ORG_3`,`MANAGEMENT_ORG_4`
        ,`SITE_ORG_1_1`,`SITE_ORG_1_2`)
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
        "INSERT INTO `export_ASSOCIATIONS`(`ACCESS NUMBER`,`UUI`,`IMEI`,`REFNUM`)
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
        "INSERT INTO `export_PRODUCT_ASSOCIATIONS`(`ACCESS NUMBER`,`PRODUCT NAME`)
        VALUES (",
        PRODUCT_ASSOCIATIONS$ACCESS.NUMBER[i],
        ",",
        PRODUCT_ASSOCIATIONS$PRODUCT.NAME[i],
        ")",
        sep = "'"
      )
    )
}
