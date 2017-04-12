#Load Needed Libraries
library(shiny)
library(RMySQL)
library(shinyBS)
library(shinyjs)
library(shinythemes)

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

#Query needed to get Clients
clientes <-
  sqlQuery("SELECT nombre FROM `clientes` ORDER BY id DESC")

#JS code to reset the page
jsResetCode <- "shinyjs.refresh = function() {history.go(0)}"

#Start Shiny User Interfase
shinyUI(
  fluidPage
  (
    #load the css theme of the shiny app
    theme = shinytheme("simplex"),
    #load ShinyJS
    useShinyjs(),
    extendShinyjs(text = jsResetCode),
    #Call Neobis img css
    tags$head(
      tags$style(type = "text/css",
                 "#image img {max-width: 100%; height: auto;}")
    ),
    #Sidebar Layout selected for this proyect
    sidebarLayout(
      #The folowing widgets are for the sidebar
      sidebarPanel(
        #Call Neobis img
        imageOutput("image", height = 150),
        #Create Dates of facturation and period
        div(style = "display: inline-block; width: 5%;", icon(
          "calendar", class = NULL, lib = "font-awesome"
        )),
        div(
          style = "display: inline-block; width: 45%;",
          dateInput(
            "factmonth",
            label = h4(
              "Mes de Facturación",
              tags$style(type = "text/css", "#qDateMonth {vertical-align: top;}"),
              bsButton(
                "qDateMonth",
                label = "",
                icon = icon("question"),
                style = "info",
                size = "extra-small"
              )
            ),
            startview = "year",
            format = "mm/yyyy",
            weekstart = 1,
            language = "es"
          ),
          bsPopover(
            id = "qDateMonth",
            title = "Mes de Facturación",
            content = "Ingrese el Mes de facturacion.",
            placement = "right",
            trigger = "hover"
          )
        ),
        div(
          style = "display: inline-block; width: 45%;",
          dateInput(
            "factdate",
            label = h4(
              "Fecha de Facturación",
              tags$style(type = "text/css", "#qDateFact {vertical-align: top;}"),
              bsButton(
                "qDateFact",
                label = "",
                icon = icon("question"),
                style = "info",
                size = "extra-small"
              )
            ),
            format = "dd/mm/yyyy",
            weekstart = 1,
            language = "es"
          ),
          bsPopover(
            id = "qDateFact",
            title = "Fecha de Facturación",
            content = "Ingrese la fecha de facturacion.",
            placement = "right",
            trigger = "hover"
          )
        ),
        div(style = "display: inline-block; width: 5%;", icon(
          "calendar", class = NULL, lib = "font-awesome"
        )),
        div(
          style = "display: inline-block; width: 91%;",
          dateRangeInput(
            'dateRange',
            label = h4(
              "Periodo de Facturación",
              tags$style(type = "text/css", "#qDatePer {vertical-align: top;}"),
              bsButton(
                "qDatePer",
                label = "",
                icon = icon("question"),
                style = "info",
                size = "extra-small"
              )
            ),
            format = "dd/mm/yyyy",
            weekstart = 1,
            language = "es",
            separator = " Hasta "
          ),
          bsPopover(
            id = "qDatePer",
            title = "Periodo de Facturacion",
            content = "Ingrese la fecha de Inicio y fin del periodo de Facturacion.",
            placement = "right",
            trigger = "hover"
          )
        ),
        #Creaete Inputs for client and provider
        
        div(
          style = "display: inline-block; width: 96%;",
          selectInput(
            "Client",
            label = h4(
              "Cliente",
              tags$style(type = "text/css", "#qclient {vertical-align: top;}"),
              bsButton(
                "qclient",
                label = "",
                icon = icon("question"),
                style = "info",
                size = "extra-small"
              )
            ),
            choices = clientes,
            selected = ""
          ),
          bsPopover(
            id = "qclient",
            title = "Cliente",
            content = "Seleccione el cliente de la factura a ingresar y luego el proveedor correspondiente.",
            placement = "right",
            trigger = "hover"
          )
        ),
        div(style = "display: inline-block; width: 96%;", uiOutput("Provider")),
        #Conditional for provider Coasin, Create numeric input for UF
        conditionalPanel(
          condition = "input.Prov == 'Coasin'",
          div(
            style = "display: inline-block; width: 96%;",
            numericInput(
              "UF",
              label = h4(
                "Ingrese Valor UF",
                tags$style(type = "text/css", "#qUF {vertical-align: top;}"),
                bsButton(
                  "qUF",
                  label = "",
                  icon = icon("question"),
                  style = "info",
                  size = "extra-small"
                )
              ),
              #Customizable values for UF
              value = 26444,
              min = 25000,
              max = 28000,
              step = 1
            ),
            bsPopover(
              id = "qUF",
              title = "Valor UF",
              content = "Ingrese el valor UF que aparece en la factura o el valor UF que corresponda al dia de la factura.",
              placement = "right",
              trigger = "hover"
            )
          )
        ),
        #File Input that predifines the needed file as xlsx
        div(style = "display: inline-block; width: 96%;",
            fileInput(
              "files",
              label = h4(
                "Subir Archivos Excel (.xlsx)",
                tags$style(type = "text/css", "#qfile {vertical-align: top;}"),
                bsButton(
                  "qfile",
                  label = "",
                  icon = icon("question"),
                  style = "info",
                  size = "extra-small"
                )
              ),
              accept = c(
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
              )
            )),
        bsPopover(
          id = "qfile",
          title = "Subir Factura",
          content = "Suba solo Archivos Excel que tengan una extencion .xlsx",
          placement = "right",
          trigger = "hover"
        ),
        #Optional Condition, if the checkbox is selected, the show a textInput to name the facture
        div(
          style = "display: inline-block; ",
          checkboxInput("fact", "Seleccionar nombre de la factura ")
        ),
        div(
          style = "display: inline-block;",
          bsButton(
            "qFactName",
            label = "",
            icon = icon("question"),
            style = "info",
            size = "extra-small"
          )
        ),
        bsPopover(
          id = "qFactName",
          title = "Ingresar nombre de Factura",
          content = "Ingrese el nombre de factura deseado, ese sera el que aparecera en el campo nofacture y sera el nombre del archivo.",
          placement = "right",
          trigger = "hover"
        )
        ,
        div(
          style = "display: inline-block; width: 96%;",
          conditionalPanel(condition = "input.fact == true",
                           textInput(
                             "facture",
                             label = h4("Nombre de factura personalizada")
                           ))
          
        ),
        #buttons to verify data and reboot app
        div(
          style = "text-align: center;",
          bsButton("execute", " Verificar Datos!"),
          actionButton(
            "refresh",
            "Reiniciar Aplicación!",
            icon = icon("off", lib = "glyphicon")
          )
        )
        
      ),
      #main Panel
      mainPanel(tabsetPanel(
        #First Validation Panel
        tabPanel(
          title = h4(
            "Validación ",
            tags$style(type = "text/css", "#tab1 {vertical-align: top;}"),
            bsButton(
              "tab1",
              label = "",
              icon = icon("minus", lib = "glyphicon"),
              style = "default",
              size = "extra-small"
            )
          ),
          h4("Visualización Datos ingresados"),
          div(style = "text-align: center", h4(strong(
            "Mes de Facturación: "
          )), textOutput("factmonth")),
          div(style = "text-align: center", h4(strong(
            "Fecha de Facturación: "
          )), textOutput("factdate")),
          div(style = "text-align: center",
              h4(
                strong("Inicio de Periodo de Facturación: ")
              ),
              textOutput("startdate")),
          div(style = "text-align: center",
              h4(strong(
                "Fin de periodo de Facturación: "
              )),
              textOutput("endate")),
          div(style = "text-align: center", h4(strong("Cliente: ")), textOutput("cliente")),
          div(style = "text-align: center", h4(strong("Proveedor: ")), textOutput("prove")),
          #Conditional Output, Depends of the selected Provider
          div(
            style = "text-align: center",
            conditionalPanel(condition = "input.Prov == 'Coasin'", h4(strong("Valor UF: ")), textOutput("valorUF"))
          ),
          div(style = "text-align: center", h4(strong("Archivo Subido: ")), textOutput("myFileName")),
          div(style = "text-align: center",
              h4(strong(
                "Nombre que tendra la factura: "
              )),
              textOutput("fact_name")),
          div(style = "text-align: center", h4(strong("ID Proveedor: ")), textOutput("idoperateur")),
          div(style = "text-align: center",
              h4(
                strong("Centro de Costo (centrefacturation): ")
              ),
              textOutput("ceco")),
          div(style = "text-align: center",
              h4(strong(
                "Nombre de la Cuenta (nomcompte): "
              )),
              textOutput("nomcompte")),
          div(style = "text-align: center", h4(strong(
            "Divisa del proveedor: "
          )), textOutput("codedevise")),
          div(style = "text-align: center", h4(strong("Campos a ingresar: ")), textOutput("campos")),
          br()
        ),
        #Excel File Field Selection Panel
        tabPanel(
          title = h4(
            "Campos ",
            tags$style(type = "text/css", "#tab2 {vertical-align: top;}"),
            bsButton(
              "tab2",
              label = "",
              icon = icon("ban"),
              style = "primary",
              size = "extra-small"
            )
          ),
          #The panels only appear if the user use the verify button and the fields belong to that provider
          conditionalPanel(condition = "input.execute", h4("Selección de Campos")),
          div(
            conditionalPanel(
              condition = "output.campos.includes('noappel')",
              column(
                3,
                selectizeInput(
                  "noappel",
                  label = h4(
                    "noappel: ",
                    tags$style(type = "text/css", "#q_noappel {vertical-align: top;}"),
                    bsButton(
                      "q_noappel",
                      label = "",
                      icon = icon("question"),
                      style = "info",
                      size = "extra-small"
                    )
                  ),
                  multiple = TRUE,
                  choices = "Sube un Archivo Primero",
                  options = list()
                ),
                bsPopover(
                  id = "q_noappel",
                  title = "Noappel",
                  content = "Numero de serie o linea",
                  placement = "top",
                  trigger = "hover"
                )
              ),
              conditionalPanel(condition = "output.campos.includes('m_total')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_total",
                                   label = h4(
                                     "m_total: ",
                                     tags$style(type = "text/css", "#q_m_total {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_total",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_total",
                                   title = "M total",
                                   content = "Valor total de la serie o linea",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('montant_charge')",
                               column(
                                 3,
                                 selectizeInput(
                                   "montant_charge",
                                   label = h4(
                                     "montant_charge: ",
                                     tags$style(type = "text/css", "#q_montant_charge {vertical-align: top;}"),
                                     bsButton(
                                       "q_montant_charge",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_montant_charge",
                                   title = "Montant Charge",
                                   content = "Monto del plan tarifario",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('libelle_charge')",
                               column(
                                 3,
                                 selectizeInput(
                                   "libelle_charge",
                                   label = h4(
                                     "libelle_charge: ",
                                     tags$style(type = "text/css", "#q_libelle_charge {vertical-align: top;}"),
                                     bsButton(
                                       "q_libelle_charge",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_libelle_charge",
                                   title = "Libelle_Charge",
                                   content = "Nombre del plan tarifario",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('centrefacturation')",
                               column(
                                 3,
                                 selectizeInput(
                                   "centrefacturation",
                                   label = h4(
                                     "centrefacturation: ",
                                     tags$style(type = "text/css", "#q_centrefacturation {vertical-align: top;}"),
                                     bsButton(
                                       "q_centrefacturation",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_centrefacturation",
                                   title = "Centrefacturation",
                                   content = "Centro de Facturacion. En Adessa Enlaces es Operador",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('m_remises_nondefini')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_remises_nondefini",
                                   label = h4(
                                     "m_remises_nondefini: ",
                                     tags$style(type = "text/css", "#q_m_remises_nondefini {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_remises_nondefini",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_remises_nondefini",
                                   title = "M_remises_nondefini",
                                   content = "Monto de descuentos",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('m_autre_nondefini')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_autre_nondefini",
                                   label = h4(
                                     "m_autre_nondefini: ",
                                     tags$style(type = "text/css", "#q_m_autre_nondefini {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_autre_nondefini",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_autre_nondefini",
                                   title = "M_autre_nondefini",
                                   content = "Monto de otros servicios",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('m_hors_voix')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_hors_voix",
                                   label = h4(
                                     "m_hors_voix: ",
                                     tags$style(type = "text/css", "#q_m_hors_voix {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_hors_voix",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_hors_voix",
                                   title = "M_hors_voix",
                                   content = "Monto de voz No Incluido Plan Tarifario",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              
              conditionalPanel(condition = "output.campos.includes('m_hors_data')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_hors_data",
                                   label = h4(
                                     "m_hors_data: ",
                                     tags$style(type = "text/css", "#q_m_hors_data {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_hors_data",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_hors_data",
                                   title = "M_hors_data",
                                   content = "Monto de datos No Incluido Plan Tarifario",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              conditionalPanel(condition = "output.campos.includes('m_remise_forfait')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_remise_forfait",
                                   label = h4(
                                     "M_remise_forfait: ",
                                     tags$style(type = "text/css", "#q_m_remise_forfait {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_remise_forfait",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_remise_forfait",
                                   title = "M_remise_forfait",
                                   content = "Paquete de descuento",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               )),
              conditionalPanel(condition = "output.campos.includes('m_data_nondefini')",
                               column(
                                 3,
                                 selectizeInput(
                                   "m_data_nondefini",
                                   label = h4(
                                     "m_data_nondefini: ",
                                     tags$style(type = "text/css", "#q_m_data_nondefini {vertical-align: top;}"),
                                     bsButton(
                                       "q_m_data_nondefini",
                                       label = "",
                                       icon = icon("question"),
                                       style = "info",
                                       size = "extra-small"
                                     )
                                   ),
                                   multiple = TRUE,
                                   choices = "Sube un Archivo Primero",
                                   options = list()
                                 ),
                                 bsPopover(
                                   id = "q_m_data_nondefini",
                                   title = "M_data_nondefini",
                                   content = "Datos no definidos o Servicio Roaming",
                                   placement = "top",
                                   trigger = "hover"
                                 )
                               ))
            ),
            br(),
            conditionalPanel(
              condition = "output.campos.includes('noappel')",
              div(style = "text-align: center; margin-top: 200px;", bsButton(
                "final_exec",
                " Crear CSV!",
                icon = icon("file-excel-o ", lib = "font-awesome")
              ))
            )
          )
        ),
        #Final Visualization of the file to download, and download link
        tabPanel(
          title = h4(
            "Descarga CSV ",
            tags$style(type = "text/css", "#tab3 {vertical-align: top;}"),
            bsButton(
              "tab3",
              label = "",
              icon = icon("ban"),
              style = "primary",
              size = "extra-small"
            )
          ),
          br(),
          #Buttons to download data and verify intefrity of the file
          conditionalPanel(
            condition = "input.final_exec",
            div(
              style = "text-align: center",conditionalPanel(
                condition = "input.Prov != 'Adessa'",
              downloadButton('downloadData', 'Descargar CSV')),
              conditionalPanel(
                condition = "input.Prov == 'Adessa'",
                downloadButton('downloadDataAdessa', 'Descargar CSV')),
              bsButton(
                "empty_rows",
                " Buscar si existen filas vacias",
                icon = icon("bars", lib = "font-awesome")
              ),
              bsButton(
                "rep_noappel",
                " Buscar si existen series repetidas",
                icon = icon("files-o", lib = "font-awesome")
              )
            )
            ,
            br(),
            #show the final visualization
            tableOutput('tabla')
          ),
          #include javascript file to create popup alerts
          singleton(tags$head(tags$script(src = "message-handler.js"))),
          singleton(tags$head(tags$script(src = "jquery-3.2.0.min.js"))),
          singleton(tags$head(tags$script(src = "bootstrap.min.js")))
        )
      ))
    )
  )
)
