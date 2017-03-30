#Load Needed Libraries
library(shiny)
library(XLConnect)
library(RMySQL)
library(openxlsx)
library(shinyBS)
library(shinyjs)
library(stringr)
library(readxl)
library(DBI)
library(pool)

#Increase the maxium size of an uploaded file to 30mb
options(shiny.maxRequestSize = 45 * 1024 ^ 2)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  user='root', 
  password='', 
  dbname='neobis', 
  host='localhost')

#Establish an SQL conection Function
sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(),
                  user = "root",
                  password = "",
                  dbname = "neobis")
  # send Query to btain result set
  rs <- dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  # close db connection
  dbDisconnect(DB)
  # return the dataframe
  return(result)
}

#Function to get the providers of the client
ProvQuery <- function(cliente) {
  id_cliente <-
    sqlQuery(paste("SELECT id FROM clientes WHERE nombre LIKE ", cliente, "", sep = "'"))
  
  proveedores <-
    sqlQuery(
      paste(
        "SELECT proveedores.nombre as nombre
        FROM proveedores
        JOIN cliente_proveedor
        ON cliente_proveedor.proveedores_id = proveedores.id
        WHERE cliente_proveedor.clientes_id = ",
        id_cliente,
        "",
        sep = "'"
      )
    )
  return(proveedores[, 1])
}

#function to read all the sheets
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      openxlsx::read.xlsx(filename, sheet = X, rows = 1))
  names(x) <- sheets
  x
}

#Start shiny BackEnd Function
shinyServer(function(input, output, session) {
  output$image <- renderImage({
    return(list(
      src = "www/LOGONEOBISOK.jpeg",
      contentType = "image/jpeg",
      alt = "Neobis"
    ))
  }, deleteFile = FALSE)
  
  #Get the Dates and transform them to the needed format
  factmonth <- reactive({
    factmonth <- input$factmonth
    factmonth_fix <- strsplit(as.character(factmonth), "-")
    factmonth2 <-
      paste(factmonth_fix[[1]][1], factmonth_fix[[1]][2] , sep = "")
    return(factmonth2)
  })
  
  output$factmonth <- renderText({
    factmonth()
  })
  
  factdate <- reactive({
    factdate <- input$factdate
    factdate_fix <- strsplit(as.character(factdate), "-")
    factdate2 <-
      paste(factdate_fix[[1]][3], factdate_fix[[1]][2], factdate_fix[[1]][1], sep = "/")
    return(factdate2)
  })
  
  output$factdate <- renderText({
    factdate()
  })
  
  startdate <- reactive({
    period <- input$dateRange
    period_fix <- strsplit(as.character(period), " ")
    startdate_full <- strsplit(period_fix[[1]][1], "-")
    startdate <-
      paste(startdate_full[[1]][3],
            startdate_full[[1]][2],
            startdate_full[[1]][1],
            sep = "/")
    return(startdate)
  })
  
  output$startdate <- renderText({
    startdate()
  })
  
  endate <- reactive({
    period2 <- input$dateRange
    period_fix2 <- strsplit(as.character(period2), " ")
    endate_full <- strsplit(period_fix2[[2]][1], "-")
    endate <-
      paste(endate_full[[1]][3], endate_full[[1]][2], endate_full[[1]][1], sep = "/")
    return(endate)
  })
  
  output$endate <- renderText({
    endate()
  })
  
  #Get the id of the selected client
  client_prov <- reactive({
    ProvQuery(input$Client)
  })
  
  output$Provider <- renderUI({
    selectInput(
      "Prov",
      label = h4("Proveedor"),
      selected = "",
      as.list(client_prov())
    )##here we call the function test
  })
  
  #Get the selected fields
  output$cliente <- reactive({
    cliente <- input$Client
    
  })
  output$prove <- reactive({
    prove <- input$Prov
  })
  
  output$valorUF <- reactive({
    valorUF <- input$UF
    if (is.null(valorUF))
      return(NULL)
    return (valorUF)
  })
  
  #Get the name of the selected file
  file_name <- reactive({
    inFile <- input$files
    if (is.null(inFile))
      return(NULL)
    return (stringi::stri_extract_first(str = inFile$name, regex = ".*()"))
  })
  
  
  #Return the name of the selected file
  output$myFileName <- renderText({
    file_name()
  })
  
  #Create the name of the facture, or in case theres one selected by the user, use that
  facture_name <- reactive({
    cliente <- input$Client
    if (cliente == "-Seleccionar Cliente-") {
      return(NULL)
    }
    
    check_name <- input$fact
    fact_name <- input$facture
    if (check_name == FALSE) {
      cliente <- input$Client
      prove <- gsub(" ", "", input$Prov, fixed = TRUE)
      moisfact <- factmonth()
      facture_name <-
        paste(cliente, prove, moisfact, sep = "-")
      if (prove == "QuintecArriendo" | prove == "QuintecSoporte") {
        facture_name <-
          paste(cliente, prove, "(HOJA)", moisfact, sep = "-")
      }
      return(facture_name)
    }
    return(fact_name)
  })
  
  #return the name of the facture
  output$fact_name <- renderText({
    facture_name()
  })
  
  #Function to read the headers of the file
  contentsrea <- reactive({
    inFile <- input$files
    if (is.null(inFile)){
      return(NULL)}
    file.copy(inFile$datapath, paste(inFile$datapath, ".xlsx", sep = ""))
    mysheets <<-
      read_excel_allsheets(paste(inFile$datapath, ".xlsx", sep = ""))
    
    numSheets <- NROW(mysheets) # Number of sheets
    namSheets <- names(mysheets) # obtain names of sheets
    
    if (numSheets >= 1) {
      aut <- mysheets[namSheets[1]] # juntar hoja 1 a variable
      aut <- do.call(rbind.data.frame, aut) # convertir hoja 1 a df
      if(NCOL(aut)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[1],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut) <-
        paste(namSheets[1], names(aut), sep = " $ ") # cambiar nombre de columnas
      new <- aut
    }
    
    if (numSheets >= 2) {
      aut2 <- mysheets[namSheets[2]] # juntar hoja 2 a variable
      aut2 <-
        do.call(rbind.data.frame, aut2) # convertir hoja 2 a df
      if(NCOL(aut2)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[2],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut2) <-
        paste(namSheets[2], names(aut2), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2)
    }
    
    if (numSheets >= 3) {
      aut3 <- mysheets[namSheets[3]] # juntar hoja 3 a variable
      aut3 <-
        do.call(rbind.data.frame, aut3) # convertir hoja 3 a df
      if(NCOL(aut3)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[3],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut3) <-
        paste(namSheets[3], names(aut3), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3)
    }
    
    if (numSheets >= 4) {
      aut4 <- mysheets[namSheets[4]] # juntar hoja 4 a variable
      aut4 <-
        do.call(rbind.data.frame, aut4) # convertir hoja 4 a df
      if(NCOL(aut4)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[4],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut4) <-
        paste(namSheets[4], names(aut4), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3, aut4)
    }
    
    if (numSheets >= 5) {
      aut5 <- mysheets[namSheets[5]] # juntar hoja 5 a variable
      aut5 <-
        do.call(rbind.data.frame, aut5) # convertir hoja 5 a df
      if(NCOL(aut5)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[5],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut5) <-
        paste(namSheets[5], names(aut5), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3, aut4, aut5)
    }
    
    if (numSheets >= 6) {
      aut6 <- mysheets[namSheets[6]] # juntar hoja 6 a variable
      aut6 <-
        do.call(rbind.data.frame, aut6) # convertir hoja 6 a df
      if(NCOL(aut6)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[6],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut6) <-
        paste(namSheets[6], names(aut6), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3, aut4, aut5, aut6)
    }
    if (numSheets >= 7) {
      aut7 <- mysheets[namSheets[7]] # juntar hoja 7 a variable
      aut7 <-
        do.call(rbind.data.frame, aut7) # convertir hoja 7 a df
      if(NCOL(aut7)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[7],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut7) <-
        paste(namSheets[7], names(aut7), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3, aut4, aut5, aut6, aut7)
    }
    if (numSheets >= 8) {
      aut8 <- mysheets[namSheets[8]] # juntar hoja 8 a variable
      aut8 <-
        do.call(rbind.data.frame, aut8) # convertir hoja 8 a df
      if(NCOL(aut8)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[8],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut8) <-
        paste(namSheets[8], names(aut8), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3, aut4, aut5, aut6, aut7, aut8)
    }
    if (numSheets >= 9) {
      aut9 <- mysheets[namSheets[9]] # juntar hoja 9 a variable
      aut9 <-
        do.call(rbind.data.frame, aut9) # convertir hoja 9 a df
      if(NCOL(aut9)==0){
        session$sendCustomMessage(
          type = 'testmessage',
          message = paste("La primera fila de la hoja `",namSheets[9],"` del archivo esta vacia, borrar primeras lineas vacias y reintentar",sep = "")
        )
      }
      colnames(aut9) <-
        paste(namSheets[9], names(aut9), sep = " $ ") # cambiar nombre de columnas
      new <- cbind(aut, aut2, aut3, aut4, aut5, aut6, aut7, aut8, aut9)
    }
    new
  })
  
  #Get the needed fields that belong to the selected provider and client
  campos <- reactive({
    cliente <- input$Client
    prove <- input$Prov
    campos <- sqlQuery(
      paste(
        "SELECT campos.nombre
        FROM campos
        JOIN(SELECT campos_base.campos_id AS cid, d.ce1, d.ce2, d.ce3, d.ce4, d.ce5
        FROM campos_base
        JOIN(SELECT cliente_proveedor.tipo_proveedores_id AS tpid, cliente_proveedor.col_extra_1 AS ce1, cliente_proveedor.col_extra_2 AS ce2,
        cliente_proveedor.col_extra_3 AS ce3, cliente_proveedor.col_extra_4 AS ce4, cliente_proveedor.col_extra_5 AS ce5
        FROM cliente_proveedor
        JOIN(SELECT proveedores.id as idproveedor, clientes.id as idcliente
        FROM proveedores, clientes
        WHERE proveedores.nombre LIKE ",
        prove,
        " AND clientes.nombre LIKE ",
        cliente,
        ") AS s
        ON s.idproveedor=cliente_proveedor.proveedores_id AND s.idcliente=cliente_proveedor.clientes_id) AS d
        ON campos_base.tipo_proveedores_id=d.tpid) AS a
        WHERE a.cid=campos.id
        ORDER BY campos.nombre DESC",
        sep = "'"
        )
        )
    return(campos$nombre)
  })
  
  #Return the needed fields
  output$campos <- reactive({
    name_campos <- campos()
    return(name_campos)
  })
  
  #Get the ID of the provider
  idoperateur <- reactive({
    prove <- input$Prov
    idoperateur <-
      sqlQuery(paste(
        "SELECT idoperateur FROM proveedores WHERE nombre LIKE ",
        prove,
        "",
        sep = "'"
      ))
    return(idoperateur$idoperateur)
  })
  #Return the ID of the provider
  output$idoperateur <- renderText({
    idoperateur()
  })
  
  #Get the CECO of the provider
  ceco <- reactive({
    cliente <- input$Client
    prove <- input$Prov
    id <- sqlQuery(
      paste(
        "SELECT ceco_id
        FROM cliente_proveedor
        JOIN (
        SELECT clientes.id AS clienteid, proveedores.id AS proveedorid
        FROM clientes, proveedores
        WHERE clientes.nombre LIKE ",
        cliente,
        " AND proveedores.nombre LIKE ",
        prove,
        ") AS a
        ON proveedores_id = a.proveedorid AND clientes_id = a.clienteid
        ",
        sep = "'"
        )
      )
    ceco <-
      sqlQuery(paste("SELECT nombre FROM ceco WHERE id=", id, "", sep = "'"))
    return(ceco$nombre)
  })
  
  #Return the CECO of the provider
  output$ceco <- renderText({
    ceco()
  })
  
  #Get the nomcompte field
  nomcompte <- reactive({
    cliente <- input$Client
    prove <- input$Prov
    id <- sqlQuery(
      paste(
        "SELECT nomcompte_id
        FROM cliente_proveedor
        JOIN (
        SELECT clientes.id AS clienteid, proveedores.id AS proveedorid
        FROM clientes, proveedores
        WHERE clientes.nombre LIKE ",
        cliente,
        " AND proveedores.nombre LIKE ",
        prove,
        ") AS a
        ON proveedores_id = a.proveedorid AND clientes_id = a.clienteid
        ",
        sep = "'"
        )
      )
    nomcompte <-
      sqlQuery(paste("SELECT nombre FROM nomcompte WHERE id=", id, "", sep =
                       "'"))
    return(nomcompte$nombre)
  })
  
  #Return the nomcompte field
  output$nomcompte <- renderText({
    nomcompte()
  })
  
  #Get the Divise code of the selected provider
  codedevise <- reactive({
    cliente <- input$Client
    prove <- input$Prov
    
    id <- sqlQuery(
      paste(
        "SELECT ceco_id, nomcompte_id, codedevise_id
        FROM cliente_proveedor
        JOIN (
        SELECT clientes.id AS clienteid, proveedores.id AS proveedorid
        FROM clientes, proveedores
        WHERE clientes.nombre LIKE ",
        cliente,
        " AND proveedores.nombre LIKE ",
        prove,
        ") AS a
        ON proveedores_id = a.proveedorid AND clientes_id = a.clienteid",
        sep = "'"
        )
      )
    codedevise <-
      sqlQuery(paste(
        "SELECT nombre FROM codedevise WHERE id=",
        id$codedevise_id,
        "",
        sep =
          "'"
      ))
    return(codedevise$nombre)
  })
  
  #Return the Divise code of the selected provider
  output$codedevise <- renderText({
    codedevise()
  })
  
  #Update the following inputs
  observe({
    updateSelectizeInput(session, "noappel", choices = names(contentsrea()))
    updateSelectizeInput(session, "m_total", choices = names(contentsrea()))
    updateSelectizeInput(session, "montant_charge", choices = names(contentsrea()))
    updateSelectizeInput(session, "libelle_charge", choices = names(contentsrea()))
    updateSelectizeInput(session, "centrefacturation", choices = names(contentsrea()))
    updateSelectizeInput(session, "m_remises_nondefini", choices = names(contentsrea()))
    updateSelectizeInput(session, "m_autre_nondefini", choices = names(contentsrea()))
    updateSelectizeInput(session, "m_hors_voix", choices = names(contentsrea()))
    updateSelectizeInput(session, "m_hors_data", choices = names(contentsrea()))
    updateTextInput(session, "campos", value = campos())
    updateCheckboxInput(session, "fact", value = )
  })
  

  #Main Function to create the final output
  observeEvent(input$final_exec, {
    
    #call vars
    moisfacturation <- factmonth()
    datefacturation <- factdate()
    datefacture1 <- startdate()
    datefacture2 <- endate()
    codedevise <- codedevise()
    idoperateur <- idoperateur()
    nomcompte <- nomcompte()
    centrefacturation <- ceco()
    nofacture <- facture_name()
    valorUF <- as.numeric(input$UF)
    prove <- input$Prov
    noappel <- input$noappel
    m_tot <- input$m_total
    montant_char <- input$montant_charge
    libelle_charge <- input$libelle_charge
    
    inFile <- input$files
    
    #if any input is null, send error
    if (is.null(noappel) |
        is.null(m_tot) |
        is.null(montant_char) | is.null(libelle_charge)) {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Hay al menos un campo no seleccionado o vacio.")
    }
    
    #Get the columns and sheets of the selected fields of the file
    noappel <- strsplit(noappel, " \\$ ")
    noappelSheet <- noappel[[1]][1]
    noappel <- noappel[[1]][2]
    
    #if 1 or 2 fields were selected in m_total and montant_charge, get them
    if (NROW(m_tot) == 1) {
      m_total <- strsplit(m_tot, " \\$ ")
      m_totalSheet <- m_total[[1]][1]
      m_total <- m_total[[1]][2]
    }
    if (NROW(m_tot) == 2) {
      m_total <- strsplit(m_tot[1], " \\$ ")
      m_totalSheet <- m_total[[1]][1]
      m_total <- m_total[[1]][2]
      
      m_total2 <- strsplit(m_tot[2], " \\$ ")
      m_totalSheet2 <- m_total2[[1]][1]
      m_total2 <- m_total2[[1]][2]
    }
    
    if (NROW(montant_char) == 1) {
      montant_charge <- strsplit(montant_char, " \\$ ")
      montant_chargeSheet <- montant_charge[[1]][1]
      montant_charge <- montant_charge[[1]][2]
    }
    if (NROW(montant_char) == 2) {
      montant_charge <- strsplit(montant_char[1], " \\$ ")
      montant_chargeSheet <- montant_charge[[1]][1]
      montant_charge <- montant_charge[[1]][2]
      
      montant_charge2 <- strsplit(montant_char[2], " \\$ ")
      montant_chargeSheet2 <- montant_charge2[[1]][1]
      montant_charge2 <- montant_charge2[[1]][2]
    }
    
    libelle_charge <- strsplit(libelle_charge, " \\$ ")
    libelle_chargeSheet <- libelle_charge[[1]][1]
    libelle_charge <- libelle_charge[[1]][2]
    
    #if provider AdessaEnlaces get centrefacturation
    if (prove == "Adessa Enlaces") {
      centrefacturation <- input$centrefacturation
      centrefacturation <- strsplit(centrefacturation, " \\$ ")
      centrefacturationSheet <- centrefacturation[[1]][1]
      centrefacturation <- centrefacturation[[1]][2]
    }
    #get the headers of the selected fields
    if(noappelSheet == m_totalSheet && montant_chargeSheet == libelle_chargeSheet && noappelSheet == montant_chargeSheet
       && noappelSheet == libelle_chargeSheet){
      all_headers <- mysheets[noappelSheet]
      all_headers <- do.call(rbind.data.frame, all_headers)
      headers_noappel <- all_headers
      headers_m_total <- all_headers
      headers_montant_charge <- all_headers
      headers_libelle_charge <- all_headers
      
      if (prove == "Adessa Enlaces") {
        headers_centrefacturation <- all_headers
        col_centrefacturation <- match(centrefacturation, names(headers_centrefacturation))
      }
      }
    else{
    headers_noappel <-
      read.xlsx(inFile$datapath, sheet = noappelSheet, rows = 1)
    headers_m_total <-
      read.xlsx(inFile$datapath, sheet = m_totalSheet, rows = 1)
    headers_montant_charge <-
      read.xlsx(inFile$datapath, sheet = montant_chargeSheet, rows = 1)
    headers_libelle_charge <-
      read.xlsx(inFile$datapath, sheet = libelle_chargeSheet, rows = 1)
    
    if (prove == "Adessa Enlaces") {
      headers_centrefacturation <-
        read.xlsx(inFile$datapath, sheet = centrefacturationSheet, rows = 1)
      col_centrefacturation <-
        match(centrefacturation, names(headers_centrefacturation))
    }
    if (NROW(montant_char) == 2 && NROW(m_tot) == 2) {
      headers_m_total2 <-
        read.xlsx(inFile$datapath, sheet = m_totalSheet2, rows = 1)
      headers_montant_charge2 <-
        read.xlsx(inFile$datapath, sheet = montant_chargeSheet2, rows = 1)
      
      col_m_total2 <- match(m_total2, names(headers_m_total2))
      col_montant_charge2 <-
        match(montant_charge2, names(headers_montant_charge2))
      
      col_m_total[[2]] <- col_m_total2
      col_montant_charge[[2]] <- col_montant_charge2
    }
    }
    #get the column number of the selected field
    col_noappel <- match(noappel, names(headers_noappel))
    col_m_total <- match(m_total, names(headers_m_total))
    col_montant_charge <-
      match(montant_charge, names(headers_montant_charge))
    col_libelle_charge <-
      match(libelle_charge, names(headers_libelle_charge))
    if (NROW(montant_char) == 2 && NROW(m_tot) == 2 && m_totalSheet2==noappelSheet && montant_chargeSheet2==noappelSheet) {
      headers_m_total2 <- all_headers
      headers_montant_charge2 <- all_headers
      col_m_total2 <- match(m_total2, names(headers_m_total2))
      col_montant_charge2 <- match(montant_charge2, names(headers_montant_charge2))
      col_m_total[[2]] <- col_m_total2
      col_montant_charge[[2]] <- col_montant_charge2
    }
    #get the data from the file
    
    if(noappelSheet == m_totalSheet && montant_chargeSheet == libelle_chargeSheet && noappelSheet == montant_chargeSheet
       && noappelSheet == libelle_chargeSheet){
      if (prove == "Adessa Enlaces" && noappelSheet==centrefacturationSheet) {
        data <-
          read.xlsx(
            inFile$datapath,
            sheet = centrefacturationSheet,
            startRow = 1,
            cols = c(col_noappel,col_m_total,col_montant_charge,col_libelle_charge,col_centrefacturation)
          )
        rows <- nrow(data)
        noappel_Data <- data[noappel]
        m_total_Data <- data[m_total]
        montant_charge_Data <- data[montant_charge]
        libelle_charge_Data <- data[libelle_charge]
        centrefacturation_charge_Data <- data[centrefacturation]
      }
      if (NROW(montant_char) == 2 && NROW(m_tot) == 2 && m_totalSheet2==noappelSheet && montant_chargeSheet2==noappelSheet) {
        data <-
          read.xlsx(
            inFile$datapath,
            sheet = noappelSheet,
            startRow = 1,
            cols = c(col_noappel,col_libelle_charge)
          )
        m_total_Data <-
          read.xlsx(
            inFile$datapath,
            sheet = m_totalSheet,
            startRow = 1,
            cols = c(col_m_total)
          )
        montant_charge_Data <-
          read.xlsx(
            inFile$datapath,
            sheet = montant_chargeSheet,
            startRow = 1,
            cols = c(col_montant_charge)
          )
        rows <- nrow(data)
        noappel_Data <- data[noappel]
        libelle_charge_Data <- data[libelle_charge]
      }
      else{
      data <- read.xlsx(
        inFile$datapath,
        sheet = noappelSheet,
        startRow = 1,
        cols = c(col_noappel,col_m_total,col_montant_charge,col_libelle_charge)
      )
      rows <- nrow(data)
      noappel_Data <- data[noappel]
      m_total_Data <- data[m_total]
      montant_charge_Data <- data[montant_charge]
      libelle_charge_Data <- data[libelle_charge]
      }
    }
    
    if (prove == "Adessa Enlaces" && noappelSheet!=centrefacturationSheet) {
      centrefacturation_charge_Data <-
        read.xlsx(
          inFile$datapath,
          sheet = centrefacturationSheet,
          startRow = 1,
          cols = c(col_centrefacturation)
        )
      data <- read.xlsx(
        inFile$datapath,
        sheet = noappelSheet,
        startRow = 1,
        cols = c(col_noappel,col_m_total,col_montant_charge,col_libelle_charge)
      )
      rows <- nrow(data)
      noappel_Data <- data[noappel]
      m_total_Data <- data[m_total]
      montant_charge_Data <- data[montant_charge]
      libelle_charge_Data <- data[libelle_charge]
    }
    
    
   #if fields are not numeric throw error
    if (sapply(m_total_Data, class) != "numeric" |
        sapply(montant_charge_Data, class) != "numeric") {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Los campos seleccionados en m_total y/o montant_charge no son valores numericos.")
    }
    #round the data from m_total and montant_charge to 4 decimals
    m_total_Data[[1]] <- round(m_total_Data[[1]], 4)
    montant_charge_Data[[1]] <- round(montant_charge_Data[[1]], 4)
    #Custom functions for every provider
    if (prove == "Adessa PC") {
      libelle_charge_Data[] <-
        lapply(libelle_charge_Data, function(x)
          paste(x, ".AD", sep = ""))
    }
    if (prove == "Adessa IP") {
      libelle_charge_Data[] <-
        lapply(libelle_charge_Data, function(x)
          gsub("Telefonia ", "", x))
    }
    if (prove == "Coasin") {
      libelle_charge_Data[] <-
        lapply(libelle_charge_Data, function(x)
          paste("Coasin Soporte Dispositivos", sep = ""))
      y <- round(((0.104 * valorUF) / 1000), 4)
      m_total_Data[] <-
        lapply(m_total_Data, function(x)
          paste(y, sep = ""))
      montant_charge_Data[] <-
        lapply(montant_charge_Data, function(x)
          paste(y, sep = ""))
      m_total_facture_Data <- colSums(data.matrix(m_total_Data))
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19)
    }
    
    if (prove != "Coasin" && prove != "Quintec Soporte" && prove != "Quintec Arriendo") {
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <-
        round(m_total_facture_Data * 1.19, 4)
    }
    if (prove == "Adessa Enlaces") {
      centrefacturation_charge_Data[] <-
        lapply(centrefacturation_charge_Data,
               function(x)
                 gsub(" ", "", paste("Enlaces.", x, sep = "")))
    }
    
    if (prove == "Quintec Soporte" | prove == "Quintec Arriendo") {
      m_total_Data <- m_total_Data[1] + m_total_Data[2]
      montant_charge_Data <-
        montant_charge_Data[1] + montant_charge_Data[2]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <-
        round(m_total_facture_Data * 1.19, 4)
      nofacture_Quintec <- strsplit(nofacture, "-")
      nofacture <-
        paste(
          nofacture_Quintec[[1]][1],
          nofacture_Quintec[[1]][2],
          m_totalSheet,
          nofacture_Quintec[[1]][4],
          sep = "-"
        )
      if (prove == "Quintec Soporte") {
        libelle_charge_Data[] <-
          lapply(libelle_charge_Data,
                 function(x)
                   gsub(" ", "", paste("Soporte", sep = "")))
      }
    }
    
    df_moisfacturation <- character(rows)
    df_datefacturation <- character(rows)
    df_datefacture1 <- character(rows)
    df_datefacture2 <- character(rows)
    df_codedevise <- character(rows)
    df_idoperateur <- character(rows)
    df_nomcompte <- character(rows)
    df_centrefacturation_charge_Data <- character(rows)
    df_centrefacturation <- character(rows)
    df_nofacture <- character(rows)
    df_noappel_Data <- character(rows)
    df_libelle_charge_Data <- character(rows)
    df_montant_charge_Data <- numeric(rows)
    df_m_total_Data <- numeric(rows)
    df_m_total_facture_Data <- numeric(rows)
    df_m_total_ttc_facture_Data <- numeric(rows)
    
    #custom function to insert to the database the data from AdessaEnlaces that requires centrefacturation
    if (prove == "Adessa Enlaces") {
        # Number of times we'll go through the loop
        for (i in 1:rows) {
                df_moisfacturation[i] <- moisfacturation
                df_datefacturation[i] <- datefacturation
                df_datefacture1[i] <- datefacture1
                df_datefacture2[i] <- datefacture2
                df_codedevise[i] <- codedevise
                df_idoperateur[i] <- idoperateur
                df_nomcompte[i] <- nomcompte
                df_centrefacturation_charge_Data[i] <- centrefacturation_charge_Data[i,]
                df_nofacture[i] <- nofacture
                df_noappel_Data[i] <- noappel_Data[i,]
                df_libelle_charge_Data[i] <- libelle_charge_Data[i,]
                df_montant_charge_Data[i] <- montant_charge_Data[i,]
                df_m_total_Data[i] <- m_total_Data[i,]
                df_m_total_facture_Data[i] <- m_total_facture_Data
                df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
        }
        insert_sql <- data.frame(df_moisfacturation,df_datefacturation,df_datefacture1,df_datefacture2,df_codedevise,df_idoperateur,
                                 df_nomcompte,df_centrefacturation_charge_Data,df_nofacture,df_noappel_Data,df_libelle_charge_Data,
                                 df_montant_charge_Data,df_m_total_Data,df_m_total_facture_Data,df_m_total_ttc_facture_Data)
    
    }
    #Main function to add the data to the DB of the rest of the providers
    else
    {
        # Number of times we'll go through the loop
        for (i in 1:rows) {
          df_moisfacturation[i] <- moisfacturation
          df_datefacturation[i] <- datefacturation
          df_datefacture1[i] <- datefacture1
          df_datefacture2[i] <- datefacture2
          df_codedevise[i] <- codedevise
          df_idoperateur[i] <- idoperateur
          df_nomcompte[i] <- nomcompte
          df_centrefacturation[i] <- centrefacturation
          df_nofacture[i] <- nofacture
          df_noappel_Data[i] <- noappel_Data[i,]
          df_libelle_charge_Data[i] <- libelle_charge_Data[i,]
          df_montant_charge_Data[i] <- montant_charge_Data[i,]
          df_m_total_Data[i] <- m_total_Data[i,]
          df_m_total_facture_Data[i] <- m_total_facture_Data
          df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
          
        }
        insert_sql <- data.frame(df_moisfacturation,df_datefacturation,df_datefacture1,df_datefacture2,df_codedevise,df_idoperateur,
                                  df_nomcompte,df_centrefacturation,df_nofacture,df_noappel_Data,df_libelle_charge_Data,
                                  df_montant_charge_Data,df_m_total_Data,df_m_total_facture_Data,df_m_total_ttc_facture_Data)
    }
    names(insert_sql) <- c("moisfacturation","datefacturation","datefacture1","datefacture2","codedevise","idoperateur",
                   "nomcompte","centrefacturation","nofacture","noappel","libelle_charge",
                   "montant_charge","m_total","m_total_facture","m_total_ttc_facture")
    
    
    #update to set the decimal separator as comma not dot
    insert_sql$montant_charge[] <- lapply(insert_sql$montant_charge, function(x)
      gsub("\\.", ",", x))
    insert_sql$m_total[] <- lapply(insert_sql$m_total, function(x)
      gsub("\\.", ",", x))
    insert_sql$m_total_facture[] <- lapply(insert_sql$m_total_facture, function(x)
      gsub("\\.", ",", x))
    insert_sql$m_total_ttc_facture[] <- lapply(insert_sql$m_total_ttc_facture, function(x)
      gsub("\\.", ",", x))
    
    insert_sql <<- data.frame(lapply(insert_sql, as.character), stringsAsFactors=FALSE)
    
    #Update the button to show progress on the actions
    style = "success"
    icon = icon("check")
    updateButton(session, "final_exec", style = style, icon = icon)
    updateButton(session, "tab2", style = style, icon = icon)
    updateButton(session,
                 "tab3",
                 style = "default",
                 icon = icon("minus", lib = "glyphicon"))
  })
  
  #Render the final table
  output$tabla <- renderTable({
    return(head(insert_sql,10))
  })
  
  #Download the final csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      prove <- input$Prov
      m_tot <- input$m_total
      if (prove == "Quintec Arriendo" |
          prove == "Quintec Soporte") {
        nofacture2 <- facture_name()
        nofacture_Quintec <- strsplit(nofacture2, "-")
        m_total <- strsplit(m_tot, " \\$ ")
        m_totalSheet <- m_total[[1]][1]
        nofacture <-
          paste(
            nofacture_Quintec[[1]][1],
            nofacture_Quintec[[1]][2],
            m_totalSheet,
            nofacture_Quintec[[1]][4],
            sep = "-"
          )
        fact_name <- nofacture
      }
      else {
        fact_name <- facture_name()
      }
      
      paste(fact_name, ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(insert_sql, file, quote = FALSE, row.names = FALSE)
    }
  )
  
  
  #Alerts
  observeEvent(input$execute, {
    style = "default"
    icon = ""
    factdate <- strsplit(as.character(input$factdate), "-")
    factmonth <- strsplit(as.character(input$factmonth), "-")
    period_fix <- strsplit(as.character(input$dateRange), " ")
    startdate <- strsplit(period_fix[[1]][1], "-")
    period_fix2 <- strsplit(as.character(input$dateRange), " ")
    endate <- strsplit(period_fix2[[2]][1], "-")
    prove <- input$Prov
    cliente <- input$Client
    valorUF <- input$UF
    inFile <- input$files
    
    if (isTRUE(factdate[[1]][1] < startdate[[1]][1] |
               factdate[[1]][1] < endate[[1]][1])) {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "El año de facturacion es menor al año del periodo de facturacion.")
      
    }
    else if (isTRUE(factdate[[1]][2] < startdate[[1]][2] |
                    factdate[[1]][2] < endate[[1]][2])) {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "El mes de facturacion es menor al mes del periodo de facturacion.")
    }
    else if (isTRUE(startdate[[1]][2] > endate[[1]][2] &
                    startdate[[1]][1] == endate[[1]][1])) {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "El mes de inicio de periodo es mayor al mes de fin periodo de facturacion.")
    }
    else if (isTRUE(startdate[[1]][3] > endate[[1]][3] &
                    startdate[[1]][2] > endate[[1]][2] &
                    startdate[[1]][1] == endate[[1]][1])) {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "El dia de inicio de periodo es mayor al dia de fin periodo de facturacion.")
    }
    else if (is.null(inFile)) {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "Adjuntar un archivo xlsx.")
    }
    else if (is.null(valorUF) && prove == 'Coasin') {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "Ingrese un Valor en Valor UF.")
    }
    else if (cliente == '-Seleccionar Cliente-') {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "Seleccione un Cliente y proveedor")
    }
    else {
      style = "success"
      icon = icon("check")
      renderUI({
        
      })
    }
    updateButton(session, "execute", style = style, icon = icon)
    updateButton(session, "tab1", style = style, icon = icon)
    updateButton(session,
                 "tab2",
                 style = "default",
                 icon = icon("minus", lib = "glyphicon"))
  })
  
  #Alert in case there is no field selection in the base fields
  observeEvent(input$final_exec, {
    inFile <- input$files
    noappel <- input$noappel
    m_total <- input$m_total
    montant_charge <- input$montant_charge
    libelle_charge <- input$libelle_charge
    centrefacturation <- input$centrefacturation
    prove <- input$Prov
    if (!is.null(inFile)) {
      if (is.null(noappel)) {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Ingrese al menos un campo en noappel.")
      }
      if (is.null(m_total)) {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Ingrese al menos un campo en m_total.")
      }
      if (is.null(montant_charge)) {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Ingrese al menos un campo en montant_charge.")
      }
      if (is.null(libelle_charge)) {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Ingrese al menos un campo en libelle_charge.")
      }
      if (is.null(centrefacturation) && prove == 'Adessa Enlaces') {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Ingrese al menos un campo en centrefacturation.")
      }
    }
    
  })
  
  #Alert in case of empty rows
  observeEvent(input$empty_rows, {
    x <- insert_sql
    empty_rows <- sapply(x, function(x)any(is.na(x)))
    if (any(empty_rows)) {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "Existen filas vacias, Revisar archivo xlsx y corregir. Columnas vacias:")
    } else{
      style = "success"
      icon = icon("check")
    }
    updateButton(session, "empty_rows", style = style, icon = icon)
    updateButton(session, "tab3", style = style, icon = icon)
  })
  
  #Alert in case repeated noappels
  observeEvent(input$rep_noappel, {
    x <- insert_sql
    prov <- input$Prov
    if (any(duplicated(x[,10])) && prov != "Adessa Enlaces") {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(
        type = 'testmessage',
        message = "Existen noappels repetidas, Revisar archivo xlsx y corregir."
      )
    }
    else{
      style = "success"
      icon = icon("check")
    }
    updateButton(session, "rep_noappel", style = style, icon = icon)
    updateButton(session, "tab3", style = style, icon = icon)
    })
  
  #On Hover Tootltips
  addPopover(
    session,
    "factmonth",
    title = "Mes de Facturacion",
    content = "Ingrese el Mes de facturacion.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "factdate",
    title = "Fecha de Facturacion",
    content = "Ingrese la fecha de facturacion.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "dateRange",
    title = "Periodo de Facturacion",
    content = "Ingrese la fecha de Inicio y fin del periodo de Facturacion.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "Client",
    title = "Cliente",
    content = "Seleccione el cliente de la factura a ingresar y luego el proveedor correspondiente.",
    placement = "top",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "UF",
    title = "Valor UF",
    content = "Ingrese el valor UF que aparece en la factura o el valor UF que corresponda al dia.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  
  addPopover(
    session,
    "fact",
    title = "Ticket Factura opcional",
    content = "Haga click aca para ingresar un nombre de Factura personalizado. El nombre de factura por defecto es Cliente-Proveedor-AñoMes.",
    placement = "right",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "facture",
    title = "Ingresar nombre de Factura",
    content = "Ingrese el nombre de factura deseado, ese sera el que aparecera en el campo 'nofacture' y sera el nombre del archivo.
    Solo Utilizar caracteres AlfaNumericos, guiones o parentesis.",
    placement = "top",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "execute",
    title = "Verificar Datos Ingresados",
    content = "Comprobar automaticamente errores comunes en fechas ingresadas o datos faltantes.",
    placement = "top",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "refresh",
    title = "Reiniciar Aplicacion",
    content = "Volver a Iniciar la pagina con los valores por defecto.",
    placement = "top",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "final_exec",
    title = "Crear CSV",
    content = "El sistema utiliza todos los datos ingresados para crear el archivo CSV, en la pestaña Descarga CSV se visualiza y descarga.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "downloadData",
    title = "Descargar CSV",
    content = "Descargar el archivo CSV.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "empty_rows",
    title = "Buscar Filas Vacias",
    content = "Existen casos en que la factura ingresada tiene filas vacias y esto genera archivos CSV malos, utilizar esta opcion para comprobar integridad de datos.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "rep_noappel",
    title = "Buscar Series repetidas",
    content = "Existen casos en que la factura ingresada tiene series (noappel) Repetidas, si esto no debiese suceder, verificar con este boton.",
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "tab1",
    title = "Validacion",
    content = "En esta etapa valide la informacion que utilizo en los inputs de la izquierda y que correspondan a la factura que desea procesar.",
    placement = "bottom",
    trigger = "hover",
    options = list(container = "body")
  )
  addPopover(
    session,
    "tab2",
    title = "Campos",
    content = "En esta etapa seleccione las columnas del excel que corresponden a cada campo indicado, puede ingresar mas de una columna por campo de ser necesario.",
    placement = "bottom",
    trigger = "hover",
    options = list(container = "body")
  )
  addPopover(
    session,
    "tab3",
    title = "Descarga CSV",
    content = "En esta etapa descargue el Archivo CSV final, tambien puede buscar por errores comunes del archivo de entrada, como filas vacias o series repetidas.",
    placement = "bottom",
    trigger = "hover",
    options = list(container = "body")
  )
  #reload page button
  observeEvent(input$refresh, {
    js$refresh()
  })
  })