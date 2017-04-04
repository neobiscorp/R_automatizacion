
for (i in 1:numSheets) {
  nam <- paste("aut", i, sep = "")
  assign(nam, mysheets[namSheets[i]])
  assign(nam, do.call(rbind.data.frame, get (paste0 ("aut", i))))
  get (paste0 ("aut", i)) <- do.call(rbind.data.frame, get (paste0 ("aut", i)))
  if(NCOL(paste("aut", i, sep = ""))==0){
    session$sendCustomMessage(
      type = 'testmessage',
      message = paste("La primera fila de la hoja `",namSheets[i],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
    )
  }
  colnames(paste("aut", i, sep = "")) <- paste(namSheets[i], names(paste("aut", i, sep = "")), sep = " $ ")
  new <- cbind(get (paste0 ("aut", i)))
}

if (numSheets >= 1) {
  aut <- mysheets[namSheets[1]] #Call the name of the first sheet and its columns
  aut <- do.call(rbind.data.frame, aut) 
  if (NCOL(aut) == 0) {
    session$sendCustomMessage( #Custom message in case the first rows of that sheet are empty
      type = 'testmessage',
      message = paste(
        "La primera fila de la hoja `",
        namSheets[1],
        "` del archivo esta vacia, borrar primeras lineas vacias y reintentar",
        sep = ""
      )
    )
  }
  colnames(aut) <-
    paste(namSheets[1], names(aut), sep = " $ ") #every column will be named after the name of the sheet
  new <- aut #bind the variables to an array
}
if (numSheets >= 2) {
  aut2 <- mysheets[namSheets[2]] 
  aut2 <-
    do.call(rbind.data.frame, aut2) 
  if (NCOL(aut[i]) == 0) {
    session$sendCustomMessage(
      type = 'testmessage',
      message = paste(
        "La primera fila de la hoja `",
        namSheets[i],
        "` del archivo esta vacia, borrar primeras lineas vacias y reintentar",
        sep = ""
      )
    )
  }
  colnames(aut2) <-
    paste(namSheets[2], names(aut2), sep = " $ ")
  new <- cbind(aut, aut2)
}
library(openxlsx) 

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      openxlsx::read.xlsx(filename, sheet = X, rows = 1))
  names(x) <- sheets
  x
}

mysheets <-
  read_excel_allsheets("C:\\Users\\Neobis\\Desktop\\Archivo pruebas AUT\\PRUEBA HP.xlsx")

##################
numSheets <- NROW(mysheets) # Number of sheets
namSheets <- names(mysheets) # obtain names of sheets
new <- data.frame()
aut <- character()

for (i in 1:numSheets) {
  aut[i]<- mysheets[namSheets[i]]
  if (length(aut[[i]]) == 0) {
    session$sendCustomMessage(
      type = 'testmessage',
      message = paste(
       "La primera fila de la hoja `",
                        namSheets[i],
                        "` del archivo esta vacia, borrar primeras lineas vacias y reintentar",
                        sep = ""
    )
    )
  }
  names(aut[[i]]) <-  paste(namSheets[i], names(aut[[i]]), sep = " $ ")
}
new <- cbind(aut[[ 1 ]])
new <- do.call(rbind.data.frame, aut) 
new <- cbind(aut[[1]],aut[[2]])
new <- cbind(paste(aut[[1:numSheets]]),sep = ",")

x<- 1:numSheets
new <- do.call("cbind", paste("aut[[", x,"]]", collapse = ", "))  

t <-paste0("aut[[",x,"]]")

get(paste(t,sep = ","))

new <- cbind(get(paste0("aut[[",x,"]]")))

get("aut")[[1]]

for (i in 1:numSheets) {
  X[i,] = get(paste0("aut[[",x,"]]"))
}


######################
new <- cbind(aut)

new <- rbind(aut)

names(new)

for (i in 1:numSheets) {
  aut <- character()
  aut[i]<- mysheets[namSheets[i]]
  
  
  nam <- paste("aut", i, sep = "")
  assign(nam, mysheets[namSheets[i]])
  assign(nam, do.call(rbind.data.frame, get (paste0 ("aut", i))))
  get (paste0 ("aut", i)) <- do.call(rbind.data.frame, get (paste0 ("aut", i)))
  if(NCOL(paste("aut", i, sep = ""))==0){
    session$sendCustomMessage(
      type = 'testmessage',
      message = paste("La primera fila de la hoja `",namSheets[i],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
    )
  }
  colnames(paste("aut", i, sep = "")) <- paste(namSheets[i], names(paste("aut", i, sep = "")), sep = " $ ")
  new <- cbind(get (paste0 ("aut", i)))
}