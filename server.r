#Load Needed Libraries
library(shiny)
library(RMySQL)
library(openxlsx)
library(shinyBS)
library(shinyjs)
library(stringr)
library(readxl)
library(dplyr)

#Increase the maxium size of an uploaded file to 30mb
options(shiny.maxRequestSize = 45 * 1024 ^ 2)

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
  result <- fetch(rs,-1)
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

#function to read all the sheets of the xlsx worksheet
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
    )
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
    if (is.null(inFile)) {
      return(NULL)
    }
    file.copy(inFile$datapath, paste(inFile$datapath, ".xlsx", sep = ""))
    mysheets <<-
      read_excel_allsheets(paste(inFile$datapath, ".xlsx", sep = ""))
    
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
    
    if (numSheets == 1) {
      new <- do.call(rbind.data.frame, aut) 
    }
    if (numSheets == 2) {
      new <- cbind(aut[[1]],aut[[2]])
    }
    if (numSheets == 3) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]])
    }
    if (numSheets == 4) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]])
    }
    if (numSheets == 5) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]],aut[[5]])
    }
    if (numSheets == 6) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]],aut[[5]],aut[[6]])
    }
    if (numSheets == 7) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]],aut[[5]],aut[[6]],aut[[7]])
    }
    if (numSheets == 8) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]],aut[[5]],aut[[6]],aut[[7]],aut[[8]])
    }
    if (numSheets == 9) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]],aut[[5]],aut[[6]],aut[[7]],aut[[8]],aut[[9]])
    }
    if (numSheets >= 10) {
      new <- cbind(aut[[1]],aut[[2]],aut[[3]],aut[[4]],aut[[5]],aut[[6]],aut[[7]],aut[[8]],aut[[9]],aut[[10]])
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
    updateSelectizeInput(session, "m_remise_forfait", choices = names(contentsrea()))
    updateSelectizeInput(session, "m_data_nondefini", choices = names(contentsrea()))
    updateTextInput(session, "campos", value = campos())
    updateCheckboxInput(session, "fact", value =)
  })
  
  
  #Main Function to create the final output
  observeEvent(input$final_exec, {
    #call variables
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
    m_remises_nondefini <- input$m_remises_nondefini
    m_autre_nondefini <- input$m_autre_nondefini
    m_hors_voix <- input$m_hors_voix
    m_hors_data <- input$m_hors_data
    m_remise_forfait <- input$m_remise_forfait
    m_data_nondefini <- input$m_data_nondefini
    
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
    
    #If 1 or 2 fields were selected in m_total and montant_charge, get them
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
    
    if (prove == "Claro") {
      m_autre_nondefini <- input$m_autre_nondefini
      m_autre_nondefini <- strsplit(m_autre_nondefini, " \\$ ")
      m_autre_nondefiniSheet <- m_autre_nondefini[[1]][1]
      m_autre_nondefini <- m_autre_nondefini[[1]][2]
      
      m_remise_forfait <- input$m_remise_forfait
      m_remise_forfait <- strsplit(m_remise_forfait, " \\$ ")
      m_remise_forfaitSheet <- m_remise_forfait[[1]][1]
      m_remise_forfait <- m_remise_forfait[[1]][2]
      
      m_data_nondefini <- input$m_data_nondefini
      m_data_nondefini <- strsplit(m_data_nondefini, " \\$ ")
      m_data_nondefiniSheet <- m_data_nondefini[[1]][1]
      m_data_nondefini <- m_data_nondefini[[1]][2]
      
    }
    
    #get the headers of the selected fields
    if (noappelSheet == m_totalSheet &&
        montant_chargeSheet == libelle_chargeSheet &&
        noappelSheet == montant_chargeSheet
        && noappelSheet == libelle_chargeSheet) {
      all_headers <- mysheets[noappelSheet]
      all_headers <- do.call(rbind.data.frame, all_headers)
      headers_noappel <- all_headers
      headers_m_total <- all_headers
      headers_montant_charge <- all_headers
      headers_libelle_charge <- all_headers
      
      if (prove == "Adessa Enlaces") {
        headers_centrefacturation <- all_headers
        col_centrefacturation <-  match(centrefacturation, names(headers_centrefacturation))
      }
      if (prove == "Claro") {
        headers_m_autre_nondefini <- all_headers
        headers_m_remise_forfait <- all_headers
        headers_m_data_nondefini <- all_headers
        col_m_autre_nondefini <-  match(m_autre_nondefini, names(headers_m_autre_nondefini))
        col_m_remise_forfait <-  match(m_remise_forfait, names(headers_m_remise_forfait))
        col_m_data_nondefini <-  match(m_data_nondefini, names(headers_m_data_nondefini))
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
      
      if (prove == "Claro") {
        headers_m_autre_nondefini <- read.xlsx(inFile$datapath, sheet = m_autre_nondefiniSheet, rows = 1)
        headers_m_remise_forfait <- read.xlsx(inFile$datapath, sheet = m_remise_forfaitSheet, rows = 1)
        headers_m_data_nondefini <- read.xlsx(inFile$datapath, sheet = m_data_nondefiniSheet, rows = 1)
        col_m_autre_nondefini <-  match(m_autre_nondefini, names(headers_m_autre_nondefini))
        col_m_remise_forfait <-  match(m_remise_forfait, names(headers_m_remise_forfait))
        col_m_data_nondefini <-  match(m_data_nondefini, names(headers_m_data_nondefini))
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
    if (NROW(montant_char) == 2 &&
        NROW(m_tot) == 2 &&
        m_totalSheet2 == noappelSheet &&
        montant_chargeSheet2 == noappelSheet) {
      headers_m_total2 <- all_headers
      headers_montant_charge2 <- all_headers
      col_m_total2 <- match(m_total2, names(headers_m_total2))
      col_montant_charge2 <-
        match(montant_charge2, names(headers_montant_charge2))
      col_m_total[[2]] <- col_m_total2
      col_montant_charge[[2]] <- col_montant_charge2
    }
    #get the data from the file
    if (noappelSheet == m_totalSheet &&
        montant_chargeSheet == libelle_chargeSheet &&
        noappelSheet == montant_chargeSheet
        && noappelSheet == libelle_chargeSheet) {
      if (prove == "Adessa Enlaces" &&
          noappelSheet == centrefacturationSheet) {
        data <-
          read.xlsx(
            inFile$datapath,
            sheet = centrefacturationSheet,
            startRow = 1,
            cols = c(
              col_noappel,
              col_m_total,
              col_montant_charge,
              col_libelle_charge,
              col_centrefacturation
            )
          )
        rows <- nrow(data)
        noappel_Data <- data[noappel]
        m_total_Data <- data[m_total]
        montant_charge_Data <- data[montant_charge]
        libelle_charge_Data <- data[libelle_charge]
        centrefacturation_charge_Data <- data[centrefacturation]
      }
      if (NROW(montant_char) == 2 &&
          NROW(m_tot) == 2 &&
          m_totalSheet2 == noappelSheet &&
          montant_chargeSheet2 == noappelSheet) {
        data <-
          read.xlsx(
            inFile$datapath,
            sheet = noappelSheet,
            startRow = 1,
            cols = c(col_noappel, col_libelle_charge)
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
      if (prove == "Claro" &&
          noappelSheet == m_autre_nondefiniSheet) {
        data <-
          read.xlsx(
            inFile$datapath,
            sheet = m_autre_nondefiniSheet,
            startRow = 1,
            cols = c(
              col_noappel,
              col_m_total,
              col_montant_charge,
              col_libelle_charge,
              col_m_remise_forfait,
              col_m_autre_nondefini,
              col_m_data_nondefini
              
            )
          )
        rows <- nrow(data)
        noappel_Data <- data[noappel]
        m_total_Data <- data[m_total]
        montant_charge_Data <- data[montant_charge]
        libelle_charge_Data <- data[libelle_charge]
        m_remise_forfait_Data <- data[m_remise_forfait]
        m_autre_nondefini_Data <- data[m_autre_nondefini]
        m_data_nondefini_Data <- data[m_data_nondefini]
      }
      else{
        data <- read.xlsx(
          inFile$datapath,
          sheet = noappelSheet,
          startRow = 1,
          cols = c(
            col_noappel,
            col_m_total,
            col_montant_charge,
            col_libelle_charge
          )
        )
        rows <- nrow(data)
        noappel_Data <- data[noappel]
        m_total_Data <- data[m_total]
        montant_charge_Data <- data[montant_charge]
        libelle_charge_Data <- data[libelle_charge]
      }
    }
    
    if (prove == "Adessa Enlaces" &&
        noappelSheet != centrefacturationSheet) {
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
        cols = c(
          col_noappel,
          col_m_total,
          col_montant_charge,
          col_libelle_charge
        )
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
                                message = "Los campos seleccionados en m_total y/o montant_charge no tiene solo valores numericos.")
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
    if (prove != "Coasin" &&
        prove != "Quintec Soporte" && prove != "Quintec Arriendo" && prove != "Adessa") {
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
    }
    
    
    
    if (prove == "Adessa"){
      
      Sabcito <- filter(data, Servicio == "Sabcito-Pistola de Calzado")
      rows <- nrow(Sabcito)
      noappel_Data <- Sabcito[noappel]
      m_total_Data <- Sabcito[m_total]
      montant_charge_Data <- Sabcito[montant_charge]
      libelle_charge_Data <- Sabcito[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "Sabcito"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "Sabcito",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      Sabcito <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(Sabcito) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      Sabcito$montant_charge[] <-
        lapply(Sabcito$montant_charge, function(x)
          gsub("\\.", ",", x))
      Sabcito$m_total[] <- lapply(Sabcito$m_total, function(x)
        gsub("\\.", ",", x))
      Sabcito$m_total_facture[] <-
        lapply(Sabcito$m_total_facture, function(x)
          gsub("\\.", ",", x))
      Sabcito$m_total_ttc_facture[] <-
        lapply(Sabcito$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      Sabcito <<-
        data.frame(lapply(Sabcito, as.character), stringsAsFactors = FALSE) 
      
      
      
      ##ImpresoraSabcito##
      ImpresoraSabcito <- filter(data, Servicio %in% c("Impresora sabcito","Impresora Sabcito"))
      rows <- nrow(ImpresoraSabcito)
      noappel_Data <- ImpresoraSabcito[noappel]
      m_total_Data <- ImpresoraSabcito[m_total]
      montant_charge_Data <- ImpresoraSabcito[montant_charge]
      libelle_charge_Data <- ImpresoraSabcito[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "Impresora.Sabcito"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "ImpresoraSabcito",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      ImpresoraSabcito <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(ImpresoraSabcito) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      ImpresoraSabcito$montant_charge[] <-
        lapply(ImpresoraSabcito$montant_charge, function(x)
          gsub("\\.", ",", x))
      ImpresoraSabcito$m_total[] <- lapply(ImpresoraSabcito$m_total, function(x)
        gsub("\\.", ",", x))
      ImpresoraSabcito$m_total_facture[] <-
        lapply(ImpresoraSabcito$m_total_facture, function(x)
          gsub("\\.", ",", x))
      ImpresoraSabcito$m_total_ttc_facture[] <-
        lapply(ImpresoraSabcito$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      ImpresoraSabcito <<-
        data.frame(lapply(ImpresoraSabcito, as.character), stringsAsFactors = FALSE) 
      
      
      ##iPad##
      iPad <- filter(data, Servicio %in% c("Arriendo IPAD","Arriendo IPAD 3","Arriendo IPAD Proyecto DIM","Arriendo IPAD Retail","Arriendo IPAD Retail 2"))
      rows <- nrow(iPad)
      noappel_Data <- iPad[noappel]
      m_total_Data <- iPad[m_total]
      montant_charge_Data <- iPad[montant_charge]
      libelle_charge_Data <- iPad[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "iPad"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "iPad",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      iPad <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(iPad) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      iPad$montant_charge[] <-
        lapply(iPad$montant_charge, function(x)
          gsub("\\.", ",", x))
      iPad$m_total[] <- lapply(iPad$m_total, function(x)
        gsub("\\.", ",", x))
      iPad$m_total_facture[] <-
        lapply(iPad$m_total_facture, function(x)
          gsub("\\.", ",", x))
      iPad$m_total_ttc_facture[] <-
        lapply(iPad$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      iPad <<-
        data.frame(lapply(iPad, as.character), stringsAsFactors = FALSE) 
      
      
      
      ##Kioscos##
      Kioscos <- filter(data, Servicio %in% c("Arriendo Kioscos Bco.       ","Arriendo Kioscos CMR","Arriendo Kioscos Novios","Arriendo Kioscos RRHH "))
      rows <- nrow(Kioscos)
      noappel_Data <- Kioscos[noappel]
      m_total_Data <- Kioscos[m_total]
      montant_charge_Data <- Kioscos[montant_charge]
      libelle_charge_Data <- Kioscos[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "Kioscos"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "Kioscos",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      Kioscos <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(Kioscos) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      Kioscos$montant_charge[] <-
        lapply(Kioscos$montant_charge, function(x)
          gsub("\\.", ",", x))
      Kioscos$m_total[] <- lapply(Kioscos$m_total, function(x)
        gsub("\\.", ",", x))
      Kioscos$m_total_facture[] <-
        lapply(Kioscos$m_total_facture, function(x)
          gsub("\\.", ",", x))
      Kioscos$m_total_ttc_facture[] <-
        lapply(Kioscos$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      Kioscos <<-
        data.frame(lapply(Kioscos, as.character), stringsAsFactors = FALSE) 
      
      
      
      ##MK590##
      MK590 <- filter(data, Servicio == "Arriendo de Consultores de precio")
      rows <- nrow(MK590)
      noappel_Data <- MK590[noappel]
      m_total_Data <- MK590[m_total]
      montant_charge_Data <- MK590[montant_charge]
      libelle_charge_Data <- MK590[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "MK590"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "MK590",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      MK590 <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(MK590) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      MK590$montant_charge[] <-
        lapply(MK590$montant_charge, function(x)
          gsub("\\.", ",", x))
      MK590$m_total[] <- lapply(MK590$m_total, function(x)
        gsub("\\.", ",", x))
      MK590$m_total_facture[] <-
        lapply(MK590$m_total_facture, function(x)
          gsub("\\.", ",", x))
      MK590$m_total_ttc_facture[] <-
        lapply(MK590$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      MK590 <<-
        data.frame(lapply(MK590, as.character), stringsAsFactors = FALSE) 
      
      
      ##MacMini##
      MacMini <- filter(data, Servicio == "Arriendo Mac Mini")
      rows <- nrow(MacMini)
      noappel_Data <- MacMini[noappel]
      m_total_Data <- MacMini[m_total]
      montant_charge_Data <- MacMini[montant_charge]
      libelle_charge_Data <- MacMini[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "MacMini"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "MacMini",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      MacMini <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(MacMini) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      MacMini$montant_charge[] <-
        lapply(MacMini$montant_charge, function(x)
          gsub("\\.", ",", x))
      MacMini$m_total[] <- lapply(MacMini$m_total, function(x)
        gsub("\\.", ",", x))
      MacMini$m_total_facture[] <-
        lapply(MacMini$m_total_facture, function(x)
          gsub("\\.", ",", x))
      MacMini$m_total_ttc_facture[] <-
        lapply(MacMini$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      MacMini <<-
        data.frame(lapply(MacMini, as.character), stringsAsFactors = FALSE) 
      
      
      
      ##MC2180""
      MC2180 <- filter(data, Servicio == "Arriendo MC2180")
      rows <- nrow(MC2180)
      noappel_Data <- MC2180[noappel]
      m_total_Data <- MC2180[m_total]
      montant_charge_Data <- MC2180[montant_charge]
      libelle_charge_Data <- MC2180[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "MC2180"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "MC2180",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      MC2180 <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(MC2180) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      MC2180$montant_charge[] <-
        lapply(MC2180$montant_charge, function(x)
          gsub("\\.", ",", x))
      MC2180$m_total[] <- lapply(MC2180$m_total, function(x)
        gsub("\\.", ",", x))
      MC2180$m_total_facture[] <-
        lapply(MC2180$m_total_facture, function(x)
          gsub("\\.", ",", x))
      MC2180$m_total_ttc_facture[] <-
        lapply(MC2180$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      MC2180 <<-
        data.frame(lapply(MC2180, as.character), stringsAsFactors = FALSE) 
      
      
      OKIPOS <- filter(data, Servicio == "Arriendo Impresora OKIPOS")
      rows <- nrow(OKIPOS)
      noappel_Data <- OKIPOS[noappel]
      m_total_Data <- OKIPOS[m_total]
      montant_charge_Data <- OKIPOS[montant_charge]
      libelle_charge_Data <- OKIPOS[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "OKIPOS"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "OKIPOS",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      OKIPOS <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(OKIPOS) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      OKIPOS$montant_charge[] <-
        lapply(OKIPOS$montant_charge, function(x)
          gsub("\\.", ",", x))
      OKIPOS$m_total[] <- lapply(OKIPOS$m_total, function(x)
        gsub("\\.", ",", x))
      OKIPOS$m_total_facture[] <-
        lapply(OKIPOS$m_total_facture, function(x)
          gsub("\\.", ",", x))
      OKIPOS$m_total_ttc_facture[] <-
        lapply(OKIPOS$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      OKIPOS <<-
        data.frame(lapply(OKIPOS, as.character), stringsAsFactors = FALSE)  
      
      
      ##Seteadora##
      Seteadora <- filter(data, Servicio == "Arriendo Pistolas Avery Pathfinder")
      rows <- nrow(Seteadora)
      noappel_Data <- Seteadora[noappel]
      m_total_Data <- Seteadora[m_total]
      montant_charge_Data <- Seteadora[montant_charge]
      libelle_charge_Data <- Seteadora[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "Seteadora"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "Seteadora",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      Seteadora <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(Seteadora) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      Seteadora$montant_charge[] <-
        lapply(Seteadora$montant_charge, function(x)
          gsub("\\.", ",", x))
      Seteadora$m_total[] <- lapply(Seteadora$m_total, function(x)
        gsub("\\.", ",", x))
      Seteadora$m_total_facture[] <-
        lapply(Seteadora$m_total_facture, function(x)
          gsub("\\.", ",", x))
      Seteadora$m_total_ttc_facture[] <-
        lapply(Seteadora$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      Seteadora <<-
        data.frame(lapply(Seteadora, as.character), stringsAsFactors = FALSE)      
      
      
      ##POS##
      POS <- filter(data, Servicio %in% c("Arriendo POS nuevos con licencias (tiendas nuevas)","Arriendo POS nuevos sin licencias (renovación)","Arriendo POS nuevos sin licencias (renovación) especial ALC","Soporte  POS de Propiedad de Falabella","Soporte  POS de Propiedad de Falabella ESPECIAL"))
      rows <- nrow(POS)
      noappel_Data <- POS[noappel]
      m_total_Data <- POS[m_total]
      montant_charge_Data <- POS[montant_charge]
      libelle_charge_Data <- POS[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "POS"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "POS",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
      }
      POS <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(POS) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      POS$montant_charge[] <-
        lapply(POS$montant_charge, function(x)
          gsub("\\.", ",", x))
      POS$m_total[] <- lapply(POS$m_total, function(x)
        gsub("\\.", ",", x))
      POS$m_total_facture[] <-
        lapply(POS$m_total_facture, function(x)
          gsub("\\.", ",", x))
      POS$m_total_ttc_facture[] <-
        lapply(POS$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      POS <<-
        data.frame(lapply(POS, as.character), stringsAsFactors = FALSE)
      
      
      ##STG##
      STG <- filter(data, Servicio %in% c("Arriendo STG LS3408","Arriendo STG MC9190","Arriendo STG MC92N0","Arriendo STG MK590","Arriendo STG RS419","Arriendo STG WT41N0","Arriendo etiquetadora Zebra"))
      rows <- nrow(STG)
      noappel_Data <- STG[noappel]
      m_total_Data <- STG[m_total]
      montant_charge_Data <- STG[montant_charge]
      libelle_charge_Data <- STG[libelle_charge]
      m_total_facture_Data <- round(colSums(m_total_Data), 4)
      m_total_ttc_facture_Data <- round(m_total_facture_Data * 1.19, 4)
      centrefacturation <- "STG"
      nofactu <- facture_name()
      nofacture_Adessa <- strsplit(nofactu, "-")
      nofacture <-
        paste(
          nofacture_Adessa[[1]][1],
          nofacture_Adessa[[1]][2],
          "STG",
          nofacture_Adessa[[1]][3],
          sep = "-"
        )
      
      df_moisfacturation <- character(rows)
      df_datefacturation <- character(rows)
      df_datefacture1 <- character(rows)
      df_datefacture2 <- character(rows)
      df_codedevise <- character(rows)
      df_idoperateur <- character(rows)
      df_nomcompte <- character(rows)
      df_centrefacturation <- character(rows)
      df_nofacture <- character(rows)
      df_noappel_Data <- character(rows)
      df_libelle_charge_Data <- character(rows)
      df_montant_charge_Data <- numeric(rows)
      df_m_total_Data <- numeric(rows)
      df_m_total_facture_Data <- numeric(rows)
      df_m_total_ttc_facture_Data <- numeric(rows)
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
          df_noappel_Data[i] <- noappel_Data[i, ]
          df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
          df_montant_charge_Data[i] <- montant_charge_Data[i, ]
          df_m_total_Data[i] <- m_total_Data[i, ]
          df_m_total_facture_Data[i] <- m_total_facture_Data
          df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
        }
        STG <-
          data.frame(
            df_moisfacturation,
            df_datefacturation,
            df_datefacture1,
            df_datefacture2,
            df_codedevise,
            df_idoperateur,
            df_nomcompte,
            df_centrefacturation,
            df_nofacture,
            df_noappel_Data,
            df_libelle_charge_Data,
            df_montant_charge_Data,
            df_m_total_Data,
            df_m_total_facture_Data,
            df_m_total_ttc_facture_Data
          )
      
      names(STG) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      STG$montant_charge[] <-
        lapply(STG$montant_charge, function(x)
          gsub("\\.", ",", x))
      STG$m_total[] <- lapply(STG$m_total, function(x)
        gsub("\\.", ",", x))
      STG$m_total_facture[] <-
        lapply(STG$m_total_facture, function(x)
          gsub("\\.", ",", x))
      STG$m_total_ttc_facture[] <-
        lapply(STG$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      
      STG <<-
        data.frame(lapply(STG, as.character), stringsAsFactors = FALSE)
    }
    
    
    #custom function to insert to the database the data from AdessaEnlaces that requires centrefacturation
    if (prove == "Adessa Enlaces") {
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
      # Number of times we'll go through the loop
      for (i in 1:rows) {
        df_moisfacturation[i] <- moisfacturation
        df_datefacturation[i] <- datefacturation
        df_datefacture1[i] <- datefacture1
        df_datefacture2[i] <- datefacture2
        df_codedevise[i] <- codedevise
        df_idoperateur[i] <- idoperateur
        df_nomcompte[i] <- nomcompte
        df_centrefacturation_charge_Data[i] <-
          centrefacturation_charge_Data[i, ]
        df_nofacture[i] <- nofacture
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <-
          m_total_ttc_facture_Data
      }
      insert_sql <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation_charge_Data,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(insert_sql) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      
      
      #update to set the decimal separator as comma not dot
      insert_sql$montant_charge[] <-
        lapply(insert_sql$montant_charge, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total[] <- lapply(insert_sql$m_total, function(x)
        gsub("\\.", ",", x))
      insert_sql$m_total_facture[] <-
        lapply(insert_sql$m_total_facture, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total_ttc_facture[] <-
        lapply(insert_sql$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      
      insert_sql <<-
        data.frame(lapply(insert_sql, as.character), stringsAsFactors = FALSE)
      
    }
    
    if (prove == "Claro"){
      
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
      df_m_autre_nondefini  <- numeric(rows)
      df_m_remise_forfait <- numeric(rows)
      df_m_data_nondefini <- numeric(rows)
      
      # Number of times we'll go through the loop
      for (i in 1:rows) {
        df_moisfacturation[i] <- moisfacturation
        df_datefacturation[i] <- datefacturation
        df_datefacture1[i] <- datefacture1
        df_datefacture2[i] <- datefacture2
        df_codedevise[i] <- codedevise
        df_idoperateur[i] <- idoperateur
        df_nomcompte[i] <- nomcompte
        df_centrefacturation <-
          centrefacturation
        df_nofacture[i] <- nofacture
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_autre_nondefini  <- m_autre_nondefini[i, ]
        df_m_remise_forfait <- m_autre_nondefini[i, ]
        df_m_data_nondefini <- m_autre_nondefini[i, ]
        df_m_total_ttc_facture_Data[i] <-
          m_total_ttc_facture_Data
        
      }
      insert_sql <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation_charge_Data,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_autre_nondefini,
          df_m_remise_forfait,
          df_m_data_nondefini,
          df_m_total_ttc_facture_Data
        )
      names(insert_sql) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "df_m_autre_nondefini",
          "df_m_remise_forfait",
          "df_m_data_nondefini",
          "m_total_ttc_facture"
        )
      
      
      #update to set the decimal separator as comma not dot
      insert_sql$montant_charge[] <-
        lapply(insert_sql$montant_charge, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total[] <- lapply(insert_sql$m_total, function(x)
        gsub("\\.", ",", x))
      insert_sql$m_total_facture[] <-
        lapply(insert_sql$m_total_facture, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total_ttc_facture[] <-
        lapply(insert_sql$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_autre_nondefini[] <-
        lapply(insert_sql$m_autre_nondefini, function(x)
          gsub("\\.", ",", x))
      insert_sql$df_m_remise_forfait[] <-
        lapply(insert_sql$df_m_remise_forfait, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total_ttc_facture[] <-
        lapply(insert_sql$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      
      insert_sql <<-
        data.frame(lapply(insert_sql, as.character), stringsAsFactors = FALSE)
    }
    
    
    #Main function to add the data to the DB of the rest of the providers
    if (prove != "Adessa Enlaces" | prove != "Adessa" | prove != "Claro")
    {
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
        df_noappel_Data[i] <- noappel_Data[i, ]
        df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
        df_montant_charge_Data[i] <- montant_charge_Data[i, ]
        df_m_total_Data[i] <- m_total_Data[i, ]
        df_m_total_facture_Data[i] <- m_total_facture_Data
        df_m_total_ttc_facture_Data[i] <- m_total_ttc_facture_Data
        
      }
      insert_sql <-
        data.frame(
          df_moisfacturation,
          df_datefacturation,
          df_datefacture1,
          df_datefacture2,
          df_codedevise,
          df_idoperateur,
          df_nomcompte,
          df_centrefacturation,
          df_nofacture,
          df_noappel_Data,
          df_libelle_charge_Data,
          df_montant_charge_Data,
          df_m_total_Data,
          df_m_total_facture_Data,
          df_m_total_ttc_facture_Data
        )
      names(insert_sql) <-
        c(
          "moisfacturation",
          "datefacturation",
          "datefacture1",
          "datefacture2",
          "codedevise",
          "idoperateur",
          "nomcompte",
          "centrefacturation",
          "nofacture",
          "noappel",
          "libelle_charge",
          "montant_charge",
          "m_total",
          "m_total_facture",
          "m_total_ttc_facture"
        )
      
      
      #update to set the decimal separator as comma not dot
      insert_sql$montant_charge[] <-
        lapply(insert_sql$montant_charge, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total[] <- lapply(insert_sql$m_total, function(x)
        gsub("\\.", ",", x))
      insert_sql$m_total_facture[] <-
        lapply(insert_sql$m_total_facture, function(x)
          gsub("\\.", ",", x))
      insert_sql$m_total_ttc_facture[] <-
        lapply(insert_sql$m_total_ttc_facture, function(x)
          gsub("\\.", ",", x))
      
      insert_sql <<-
        data.frame(lapply(insert_sql, as.character), stringsAsFactors = FALSE)
    }

    
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
    prove <- input$Prov
    if (prove == "Adessa"){
      return(head(STG, 10))
    }
    else{
    return(head(insert_sql, 10))}
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
  
  output$downloadDataAdessa <- downloadHandler(
    filename = 'Adessa.zip',
    content = function(fname) {
      factmonth <- input$factmonth
      factmonth_fix <- strsplit(as.character(factmonth), "-")
      factmonth2 <-
        paste0(factmonth_fix[[1]][1], factmonth_fix[[1]][2])
        
      setwd(getwd())
      
        fs <- c(paste(factmonth2,"FactCalzado.csv",sep = "_"), paste(factmonth2,"ImpreSabcito.csv",sep = "_"), paste(factmonth2,"Ipad.csv",sep = "_"),
                                                                     paste(factmonth2,"Kioscos.csv",sep = "_"), 
                paste(factmonth2,"LectorPrecio.csv",sep = "_"), paste(factmonth2,"MacMini.csv",sep = "_"), paste(factmonth2,"MC2180.csv",sep = "_"),
                paste(factmonth2,"OKIPOS.csv",sep = "_"), paste(factmonth2,"PATH.csv",sep = "_")
                ,paste(factmonth2,"POS.csv",sep = "_"),paste(factmonth2,"STG.csv",sep = "_"))
        
        write.csv2(Sabcito, file = paste(factmonth2,"FactCalzado.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(ImpresoraSabcito, file = paste(factmonth2,"ImpreSabcito.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(iPad, file = paste(factmonth2,"Ipad.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(Kioscos, file = paste(factmonth2,"Kioscos.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(MK590, file = paste(factmonth2,"LectorPrecio.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(MacMini, file = paste(factmonth2,"MacMini.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(MC2180, file = paste(factmonth2,"MC2180.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(OKIPOS, file = paste(factmonth2,"OKIPOS.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(Seteadora, file = paste(factmonth2,"PATH.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(POS, file = paste(factmonth2,"POS.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        write.csv2(STG, file = paste(factmonth2,"STG.csv",sep = "_"), quote = FALSE, row.names = FALSE)
        
        zip(zipfile=fname, files=fs)
        if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
      }
    ,contentType = "application/zip"
  )
  

  #In case a new provider was set change the verification button back to default
  observeEvent(input$Prov, {
    shinyBS::updateButton(session, "execute", style = "default", icon = "")
  })
  
  #Alerts after first inputs
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
      updateButton(session,
                   "tab2",
                   style = "default",
                   icon = icon("minus", lib = "glyphicon"))
      renderUI({
        
      })
    }
    updateButton(session, "execute", style = style, icon = icon)
    updateButton(session, "tab1", style = style, icon = icon)
    
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
    empty_rows <- sapply(x, function(x)
      any(is.na(x)))
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
    if (any(duplicated(x[, 10])) && prov != "Adessa Enlaces") {
      icon <- icon("ban")
      style = "primary"
      session$sendCustomMessage(type = 'testmessage',
                                message = "Existen noappels repetidas, Revisar archivo xlsx y corregir.")
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