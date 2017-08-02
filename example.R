install.packages("shinyjs")
install.packages("V8")

.Machine$integer.max
library(shiny)
library(RMySQL)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(RMySQL)
library(openxlsx)
library(stringr)
library(readxl)
library(dplyr)

install.packages('devtools')

install.packages(c('shiny','RMySQL','shinyBS','shinyjs','shinythemes','RMySQL','openxlsx','stringr','readxl','rmarkdown','DBI','pool','V8','devtools','dplyr'))

devtools::install_github("rstats-db/DBI")
devtools::install_github("rstudio/pool")
devtools::install_github("rstudio/shiny")

?dbDisconnect()

rsconnect::setAccountInfo(name='neobis',
                          token='03FE7023BF0251720D4EC60D34AF01A1',
                          secret='lS1S7ETxBOG4i2cIxycOrbQUlQWP0ckcc31ftiip')

library(rsconnect)
rsconnect::deployApp('C:\\Users\\Neobis\\Documents\\RProject')

library(xlsx)
dat<-read.xlsx(file.choose(), 1,startRow = 1, endRow = 1, 
               header = FALSE, encoding = "UTF-8")
print(dat)


require(XLConnect)
wb <- loadWorkbook(system.file("demoFiles/mtcars.xlsx", package = "XLConnect"))
lst = readWorksheet(wb, sheet = getSheets(wb))


library(XLConnect)

filename <- file.choose()

  workbook <- loadWorkbook(filename)
  sheet_names <- getSheets(workbook)
  names(sheet_names) <- sheet_names
  a = XLConnect::readWorksheet(file.choose(), sheet = 1, startRow=1, endRow=2)
  print(sheet_names[1])
  
  library(RMySQL)  
  
  sqlQuery <- function (query) {
    # creating DB connection object with RMysql package
    DB <- dbConnect(MySQL(), user="root", password="",dbname="neobis")
    # send Query to btain result set
    rs <- dbSendQuery(DB, query)
    # get elements from result sets and convert to dataframe
    result <- fetch(rs, -1)
    # close db connection
    dbDisconnect(DB)
    # return the dataframe
    return(result)
  }
  sqlQuery("select * from item")
  
  install.packages("XLConnect", dependencies=TRUE)
  
  options(java.parameters = "-Xmx1024m")
  options(java.parameters = "-Xmx4g" )
  library(XLConnect)
  
  wb <-loadWorkbook(file.choose())
  col <- col2idx("a")
  a <- XLConnect::readWorksheet(wb, sheet = 1)
  countt <- nrow(XLConnect::readWorksheet(wb, sheet = 1))
  print(countt)
  
  cliente <- "Walmart"
  proveedor <- "HP"
  mesfact <- "201701"
  nofacture <- paste(cliente, "-",proveedor,"-",mesfact,sep = "")

  # for (i in 1:2){
  #   sqlQuery(sprintf("INSERT INTO `item`(`nofacture`) VALUES ('%s')",nofacture))
  
  for (i in 1:10000){
  sqlQuery(
    
    "INSERT INTO `item`(`moisfacturation`, `datefacturation`, `datefacture1`, 
`datefacture2`, `codedevise`, `idoperateur`, `nomcompte`, `centrefacturation`, `nofacture`, 
`m_total_facture`, `m_total_ttc_facture`, `noappel`, `libelle_charge`, `montant_charge`, `m_total`) 
VALUES ('Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701',
    'Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701',
    'Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701','Walmart-HP-201701')")
  }
  
  for (i in 1:10000){
    sqlQuery(
      
      "UPDATE `item` SET `moisfacturation`='1',`datefacturation`='1',`datefacture1`='1',`datefacture2`='1',
`codedevise`='1',`idoperateur`='1', `nomcompte`='1',`centrefacturation`='1',`m_total_facture`='1',
`m_total_ttc_facture`='1',`noappel`='1',`libelle_charge`='1',`montant_charge`='1',`m_total`='1',
      `m_hors_voix`='1',`m_hors_data`='1',`m_remises_nondefini`='1',`m_autre_nondefini`='1',`m_tva`='1' 
      WHERE `nofacture`='Walmart-HP-201701'")
  }
  
  
  
print(nofacture)

factureDate <- "dwf"
reangeDate <- "sfa"
factureName <- "dfa"

sqlQuery(paste("INSERT INTO `item`(`datefacturation`, `datefacture1`, `datefacture2`, `nofacture`) VALUES (",factureDate,",",factureDate,",",factureDate,",",factureName,")", sep="'"))
  

library(openxlsx)

df <- read.xlsx(file.choose(), sheet = i)

print(df['libelle_charge'])


library(tm)
doc <- readPDF(control = list(text = "-table"))(elem = list(uri = file.choose()),
                                                 language = "es",
                                                 id = "id1")

write.table(doc$content, "C:/Users/Neobis/Desktop/clarotable.txt", sep="\t")


print(dat)



file <- file.choose()
count <- length(getSheetNames(file))

dat<-NULL 
for (i in 1:count) { 
  dat[[i]] <- read.xlsx(file, sheet = i, rows = 1)
}

list(dat)
list(a)

dat[[1]][2]
dat
grep("PLAZO",dat[1])

read.xlsx()

df <- read.xlsx(file, sheet = 1, colNames = TRUE, cols = )

b<-read.xlsx(file, sheet = 1, rows = 1)
a <- getSheetNames(file)
names(a)
b
names(b)
a

is.data.frame(b)
is.vector(b)

test1 <- c('Base','Incorporaciones','Bajas', 'Hoja1','Hoja2','Hoja3')
test3 <- as.list(test1)
names(test3)

test1
is.array(test1)

as.list(setNames(x,x))


test2 <- as.data.frame(test1)
test2
names(test2)


a <- "2017-01-10"
moisfact <- strsplit(a, "-")
c<- b[[1]][1]
d<- b[[1]][2]
print(c(c,d))

cliente <- "falabella"
prove <- "adessa"
mois

hola <- list(cliente,"-",prove,"-",moisfact[[1]][1],"-",moisfact[[1]][2])

return(hola)

paste(cliente, "-", prove, "-",moisfact[[1]][1],moisfact[[1]][2], sep = "")


cliente <- "Walmart"
prove <- "Coasin"
camp <- sqlQuery(paste("SELECT campos.nombre
                       FROM campos
                       JOIN(SELECT campos_base.campos_id AS cid, d.ce1, d.ce2, d.ce3, d.ce4, d.ce5
                       FROM campos_base
                       JOIN(SELECT cliente_proveedor.tipo_proveedores_id AS tpid, cliente_proveedor.col_extra_1 AS ce1, cliente_proveedor.col_extra_2 AS ce2,
                       cliente_proveedor.col_extra_3 AS ce3, cliente_proveedor.col_extra_4 AS ce4, cliente_proveedor.col_extra_5 AS ce5
                       FROM cliente_proveedor
                       JOIN(SELECT proveedores.id as idproveedor, clientes.id as idcliente
                       FROM proveedores, clientes
                       WHERE proveedores.nombre LIKE ",prove," AND clientes.nombre LIKE ",cliente,") AS s
                       ON s.idproveedor=cliente_proveedor.proveedores_id AND s.idcliente=cliente_proveedor.clientes_id) AS d
                       ON campos_base.tipo_proveedores_id=d.tpid) AS a
                       WHERE a.cid=campos.id 
                       ORDER BY campos.nombre DESC", sep="'"))

a <- camp

c<- a$nombre

b <- "noappel" %in% c

e <- match('noappel',c)

if(isTRUE('noappel' %in% c)){
  print("hola")
}


dput(c)

c[1]

  prove <- "Coasin"
  idoperateur <- sqlQuery(paste("SELECT idoperateur FROM proveedores WHERE nombre LIKE ",prove,"", sep="'"))
  a<- idoperateur
  dput(a)
  str(a)
  
  file <- choose.files()
  
a <- read.xlsx(file, sheet = 1, rows = 1)
  
b <- match('libelle_charge',names(a))
  
c <- read.xlsx(file, sheet = 1, cols = c(1,2,3,4))

c$libelle_charge

library(RODBC)

myconn <-odbcConnect("localhost", uid="root", pwd="")
crimedat <- sqlFetch(myconn, "neobis")
pundat <- sqlQuery(myconn, "select * from item")
close(myconn)

m_total_facture_Data <- 123

m_total_ttc_facture_Data <- m_total_facture_Data*1.19
 print(m_total_ttc_facture_Data)
 
 
 fact_name <- "Falabella-AdessaPC-201703"
 csvfinal <-
   sqlQuery(
     paste(
       "SELECT `moisfacturation`, `datefacturation`,
          `datefacture1`, `datefacture2`, `idoperateur`,`nomcompte`,
          `centrefacturation`,`codedevise`,`nofacture`,`noappel`,
          `m_total`, `montant_charge`, `libelle_charge`,
          `m_total_facture`, `m_total_ttc_facture`
          FROM `item` WHERE nofacture = ",
       fact_name,"",
       sep = "'"
     )
   )
 rownames(csvfinal) <- NULL
 return(csvfinal)
 
 

 library(readxl)    
 read_excel_allsheets <- function(filename) {
   sheets <- readxl::excel_sheets(filename)
   x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
   names(x) <- sheets
   x
 }
 
 mysheets <- read_excel_allsheets("C:\\Users\\Neobis\\Desktop\\Archivo pruebas AUT\\PRUEBA HP.xlsx")
 
 print(mysheets)
 
 str(mysheets)
 
 mysheets[1,]
 
do.call(rbind, lapply(mysheets, head, 0))
   
mysheets$Hoja1$libelle_charge
 
mysheets[]

names(mysheets)

mysheets$Hoja1[0,]
lapply(mysheets, head, 0)

data2 <- data.frame(merge(mysheets$Hoja1, mysheets$Hoja2, by = "row.names"), row.names = 1)

mysheets[0,]

require(plyr)

df1 <- data.frame(mysheets$Hoja1)
df2 <- data.frame(mysheets$Hoja2)
  
df1$rn <- colnames(df1)
df2$rn <- colnames(df2)

df <- join_all(list(df1,df2), by = 'rn', type = 'full')

colnames(data)

colnames(mysheets$Hoja1)




print(paste(nameSheets[1]))
colnames(mysheets["Hoja1"])

sheet1 <- as.data.frame(mysheets[1])

colnames(mysheets[paste(nameSheets[1])])

cols <- as.data.frame(colnames(mysheets[nameSheets[1]]))

cols2 <- as.data.frame(paste("Hoja1", cols$cols, sep = "$"))

#############################################

library(readxl)    
library(openxlsx)

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) openxlsx::read.xlsx(filename, sheet = X))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("C:\\Users\\Neobis\\Desktop\\Archivo pruebas AUT\\PRUEBA Quintec.xlsx") #cargar archivo

namSheets <- names(mysheets) # obtain names of sheets

numSheets <- NROW(mysheets) # Number of sheets

if(numSheets >= 1) {
aut<- mysheets[namSheets[1]] # juntar hoja 1 a variable
aut <- do.call(rbind.data.frame, aut) # convertir hoja 1 a df
colnames(aut) <- paste(namSheets[1],names(aut),sep = " $ ") # cambiar nombre de columnas
new <- aut
}

if (numSheets >= 2) {
aut2<- mysheets[namSheets[2]] # juntar hoja 1 a variable
aut2 <- do.call(rbind.data.frame, aut2) # convertir hoja 1 a df
colnames(aut2) <- paste(namSheets[2],names(aut2),sep = " $ ") # cambiar nombre de columnas
new <- cbind(aut,aut2)
}

if (numSheets >= 3) {
aut3<- mysheets[namSheets[3]] # juntar hoja 1 a variable
aut3 <- do.call(rbind.data.frame, aut3) # convertir hoja 1 a df
colnames(aut3) <- paste(namSheets[3],names(aut3),sep = " $ ") # cambiar nombre de columnas
new <- cbind(aut,aut2,aut3)
}

if (numSheets >= 4) {
aut4<- mysheets[namSheets[4]] # juntar hoja 1 a variable
aut4 <- do.call(rbind.data.frame, aut4) # convertir hoja 1 a df
colnames(aut4) <- paste(namSheets[4],names(aut4),sep = " $ ") # cambiar nombre de columnas
new <- cbind(aut,aut2,aut3,aut4)
}

if (numSheets >= 5) {
aut5<- mysheets[namSheets[5]] # juntar hoja 1 a variable
aut5 <- do.call(rbind.data.frame, aut5) # convertir hoja 1 a df
colnames(aut5) <- paste(namSheets[5],names(aut5),sep = " $ ") # cambiar nombre de columnas
new <- cbind(aut,aut2,aut3,aut4,aut5)
}

if (numSheets >= 6) {
aut6<- mysheets[namSheets[6]] # juntar hoja 1 a variable
aut6 <- do.call(rbind.data.frame, aut6) # convertir hoja 1 a df
colnames(aut6) <- paste(namSheets[6],names(aut6),sep = " $ ") # cambiar nombre de columnas
new <- cbind(aut,aut2,aut3,aut4,aut5,aut6)
}

aut[18]+aut[19]


df <- read.xlsx(
  "C:\\Users\\Neobis\\Desktop\\Archivo pruebas AUT\\Recarga - Matriz de carga consumos 201702.xlsx",
  sheet = 2,
  startRow = 1,
  cols = c(9)
)
str(df)
sapply(df, class)
typeof(df)


insert_sql <- data.frame("moisfacturation", "datefacturation", "datefacture1", "datefacture2",
                         "codedevise", "idoperateur", "nomcompte", "centrefacturation", "nofacture", "noappel",
                         "libelle_charge", "montant_charge", "m_total", "m_total_facture", "m_total_ttc_facture")


rows <- 15000
moisfacturation <- 201703
for (i in 1:rows) {
  df_moisfacturation[i] <- moisfacturation
}
data <- data.frame(df_moisfacturation)

f4 <- function(n) {
  x <- numeric(n)
  y <- character(n)
  for (i in 1:n) {
    x[i] <- i
    y[i] <- i
  }
  data.frame(x, y, stringsAsFactors=FALSE)
}

fmil <- f4(1000)


library(readxl)    
test <- function(){
print(paste("8: ",Sys.time(),sep = ""))
file <- "C:\\Users\\Neobis\\Desktop\\Archivo pruebas AUT\\ENERO-Catalogo_Telecomunicaciones.xlsx"
file <- readxl::read_excel(file,sheet = 1)
print(paste("8: ",Sys.time(),sep = ""))
}

test()
concepto <- file["Concepto"]

install.packages("ggplot2")
install.packages("pryr")
install.packages("devtools")
devtools::install_github("hadley/lineprof")

library(pryr)
object_size(insert_sql)

library(devtools)
install_github("rstudio/shiny")
install_github("MangoTheCat/processx")
install_github("MangoTheCat/webdriver")
install_github("MangoTheCat/shinytest")

webdriver::install_phantomjs()

library(shinytest)

# Launch the target app (replace with the correct path)
recordTest("C:/Users/Neobis/workspace/R_automatizacion")


file <- "C:\\Users\\Neobis\\Desktop\\Plan Walmart\\Base.xlsx"
plandata <-
  read.xlsx(
    file,
    sheet = 1,
    startRow = 1,
    cols = c(
      1:6
    )
  )
RelojControl <- filter(plandata, plandata$Servicio  == "RelojControl", grepl("_", CECO))

d <- 5
for(i in 1:10) { 
  nam <- paste("A", i, sep = "")
  assign(nam, rnorm(3)+d)
}

x <-"201601"
substring(x,5)


x<- Seteadora

x[] <- lapply(x, function(x) str_replace(x, 'ñ', 'n'))

x[x$noappel=='pathfinderviña1',10] <- 'pathfindervina1'

write.table(rbind(ArriendoPC,Impresion,RelojControl,SoporteArriendo), "myDF.csv", sep = ";", col.names = TRUE, row.names = FALSE, append = T)
