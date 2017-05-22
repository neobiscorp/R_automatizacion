##Reloj Control##
if (PlanForecast == "Forecast"){
RelojControl <- filter(data, Servicio == "RelojControl", grepl('_', CECO))
rows <- nrow(RelojControl)
noappel_Dat <- filter(noappel_Data, Servicio == "RelojControl", grepl('_', CECO))
noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) != "01"){
  RelojControl <- filter(data, Servicio == "RelojControl", !grepl('_', CECO))
  rows <- nrow(RelojControl)
  noappel_Dat <- filter(noappel_Data, Servicio == "RelojControl", !grepl('_', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5)== "01"){
  RelojControl <- filter(data, Servicio == "RelojControl")
  rows <- nrow(RelojControl)
  noappel_Dat <- filter(noappel_Data, Servicio == "RelojControl")
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
}

m_total_Data <- abs(RelojControl[m_total])
montant_charge_Data <- abs(RelojControl[montant_charge])
libelle_charge_Data <- RelojControl[libelle_charge]

libelle_charge_Data[] <-
  lapply(libelle_charge_Data, function(x)
    paste("Plan", sep = ""))

m_total_facture_Data <- abs(round(colSums(m_total_Data), 4))
m_total_ttc_facture_Data <- abs(round(m_total_facture_Data * 1.19, 4))
centrefacturation <- "420901002RelojControl"
nofactu <- facture_name()
nofacture_Plan <- strsplit(nofactu, "-")
nofacture <-
  paste(
    nofacture_Plan[[1]][1],
    PlanForecast,
    "RelojControl",
    nofacture_Plan[[1]][3],
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
#df_m_total_Data <- numeric(rows)
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
  df_noappel_Data[i] <- noappel_Dat[i, ]
  df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
  df_montant_charge_Data[i] <- (montant_charge_Data[i, ])/1000
  #df_m_total_Data[i] <- (m_total_Data[i, ])/1000
  df_m_total_facture_Data[i] <- (m_total_facture_Data)/1000
  df_m_total_ttc_facture_Data[i] <- (m_total_ttc_facture_Data)/1000
}
RelojControl <-
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
    noappel_Dat,
    df_libelle_charge_Data,
    df_montant_charge_Data,
    #df_m_total_Data,
    df_m_total_facture_Data,
    df_m_total_ttc_facture_Data
  )
names(RelojControl) <-
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
    #"m_total",
    "m_total_facture",
    "m_total_ttc_facture"
  )
RelojControl$montant_charge[] <-
  lapply(RelojControl$montant_charge, function(x)
    gsub("\\.", ",", x))
#RelojControl$m_total[] <- lapply(RelojControl$m_total, function(x)
#  gsub("\\.", ",", x))
RelojControl$m_total_facture[] <-
  lapply(RelojControl$m_total_facture, function(x)
    gsub("\\.", ",", x))
RelojControl$m_total_ttc_facture[] <-
  lapply(RelojControl$m_total_ttc_facture, function(x)
    gsub("\\.", ",", x))
RelojControl <<-
  data.frame(lapply(RelojControl, as.character), stringsAsFactors = FALSE) 

##ServicioImpresion##
if(PlanForecast == "Forecast"){
  Impresion <- filter(data, Servicio == "ServicioImpresion", grepl('_', CECO))
  rows <- nrow(Impresion)
  noappel_Dat <- filter(noappel_Data, Servicio == "ServicioImpresion", grepl('_', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("Servicio", "", x)))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) != "01"){
  Impresion <- filter(data, Servicio == "ServicioImpresion", !grepl('_', CECO))
  rows <- nrow(Impresion)
  noappel_Dat <- filter(noappel_Data, Servicio == "ServicioImpresion", !grepl('_', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("Servicio", "", x)))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) == "01"){
  Impresion <- filter(data, Servicio == "ServicioImpresion")
  rows <- nrow(Impresion)
  noappel_Dat <- filter(noappel_Data, Servicio == "ServicioImpresion")
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("Servicio", "", x)))
}


m_total_Data <- abs(Impresion[m_total])
montant_charge_Data <- abs(Impresion[montant_charge])
libelle_charge_Data <- Impresion[libelle_charge]

libelle_charge_Data[] <-
  lapply(libelle_charge_Data, function(x)
    paste("Plan", sep = ""))

m_total_facture_Data <- abs(round(colSums(m_total_Data), 4))
m_total_ttc_facture_Data <- abs(round(m_total_facture_Data * 1.19, 4))
centrefacturation <- "420401009Impresion"
nofactu <- facture_name()
nofacture_Plan <- strsplit(nofactu, "-")
nofacture <-
  paste(
    nofacture_Plan[[1]][1],
    PlanForecast,
    "Impresion",
    nofacture_Plan[[1]][3],
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
#df_m_total_Data <- numeric(rows)
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
  df_noappel_Data[i] <- noappel_Dat[i, ]
  df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
  df_montant_charge_Data[i] <- (montant_charge_Data[i, ])/1000
  #df_m_total_Data[i] <- (m_total_Data[i, ])/1000
  df_m_total_facture_Data[i] <- (m_total_facture_Data)/1000
  df_m_total_ttc_facture_Data[i] <- (m_total_ttc_facture_Data)/1000
}
Impresion <-
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
    noappel_Dat,
    df_libelle_charge_Data,
    df_montant_charge_Data,
   # df_m_total_Data,
    df_m_total_facture_Data,
    df_m_total_ttc_facture_Data
  )
names(Impresion) <-
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
    #"m_total",
    "m_total_facture",
    "m_total_ttc_facture"
  )
Impresion$montant_charge[] <-
  lapply(Impresion$montant_charge, function(x)
    gsub("\\.", ",", x))
#Impresion$m_total[] <- lapply(Impresion$m_total, function(x)
#  gsub("\\.", ",", x))
Impresion$m_total_facture[] <-
  lapply(Impresion$m_total_facture, function(x)
    gsub("\\.", ",", x))
Impresion$m_total_ttc_facture[] <-
  lapply(Impresion$m_total_ttc_facture, function(x)
    gsub("\\.", ",", x))
Impresion <<-
  data.frame(lapply(Impresion, as.character), stringsAsFactors = FALSE) 

##SoporteArriendo##

if (PlanForecast == "Forecast"){
  SoporteArriendo <- filter(data, Servicio == "SoporteArriendo", grepl('_', CECO))
  rows <- nrow(SoporteArriendo)
  noappel_Dat <- filter(noappel_Data, Servicio == "SoporteArriendo", grepl('_', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("Soporte", "Sop", x)))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) != "01"){
  SoporteArriendo <- filter(data, Servicio == "SoporteArriendo", !grepl('_', CECO), !grepl('T1799010', CECO))
  rows <- nrow(SoporteArriendo)
  noappel_Dat <- filter(noappel_Data, Servicio == "SoporteArriendo", !grepl('_', CECO),!grepl('T1799010', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("Soporte", "Sop", x)))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) == "01"){
  SoporteArriendo <- filter(data, Servicio == "SoporteArriendo")
  rows <- nrow(SoporteArriendo)
  noappel_Dat <- filter(noappel_Data, Servicio == "SoporteArriendo")
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("Soporte", "Sop", x)))
}

m_total_Data <- abs(SoporteArriendo[m_total])
montant_charge_Data <- abs(SoporteArriendo[montant_charge])
libelle_charge_Data <- SoporteArriendo[libelle_charge]

libelle_charge_Data[] <-
  lapply(libelle_charge_Data, function(x)
    paste("Plan", sep = ""))

m_total_facture_Data <- abs(round(colSums(m_total_Data), 4))
m_total_ttc_facture_Data <- abs(round(m_total_facture_Data * 1.19, 4))
centrefacturation <- "420901002SoporteArriendo"
nofactu <- facture_name()
nofacture_Plan <- strsplit(nofactu, "-")
nofacture <-
  paste(
    nofacture_Plan[[1]][1],
    PlanForecast,
    "SoporteArriendo",
    nofacture_Plan[[1]][3],
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
#df_m_total_Data <- numeric(rows)
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
  df_noappel_Data[i] <- noappel_Dat[i, ]
  df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
  df_montant_charge_Data[i] <- (montant_charge_Data[i, ])/1000
 # df_m_total_Data[i] <- (m_total_Data[i, ])/1000
  df_m_total_facture_Data[i] <- (m_total_facture_Data)/1000
  df_m_total_ttc_facture_Data[i] <- (m_total_ttc_facture_Data)/1000
}
SoporteArriendo <-
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
    noappel_Dat,
    df_libelle_charge_Data,
    df_montant_charge_Data,
   # df_m_total_Data,
    df_m_total_facture_Data,
    df_m_total_ttc_facture_Data
  )
names(SoporteArriendo) <-
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
   # "m_total",
    "m_total_facture",
    "m_total_ttc_facture"
  )
SoporteArriendo$montant_charge[] <-
  lapply(SoporteArriendo$montant_charge, function(x)
    gsub("\\.", ",", x))
#SoporteArriendo$m_total[] <- lapply(SoporteArriendo$m_total, function(x)
#  gsub("\\.", ",", x))
SoporteArriendo$m_total_facture[] <-
  lapply(SoporteArriendo$m_total_facture, function(x)
    gsub("\\.", ",", x))
SoporteArriendo$m_total_ttc_facture[] <-
  lapply(SoporteArriendo$m_total_ttc_facture, function(x)
    gsub("\\.", ",", x))
SoporteArriendo <<-
  data.frame(lapply(SoporteArriendo, as.character), stringsAsFactors = FALSE) 


##PC/NOTEBOOKS##

if (PlanForecast == "Forecast"){
  ArriendoPC <- filter(data, Servicio == "ArriendoPC", grepl('_', CECO))
  rows <- nrow(ArriendoPC)
  noappel_Dat <- filter(noappel_Data, Servicio == "ArriendoPC", grepl('_', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("ArriendoPC", "PCLaptop", x)))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) != "01"){
  ArriendoPC <- filter(data, Servicio == "ArriendoPC", !grepl('_', CECO), !grepl('T1799010', CECO))
  rows <- nrow(ArriendoPC)
  noappel_Dat <- filter(noappel_Data, Servicio == "ArriendoPC", !grepl('_', CECO), !grepl('T1799010', CECO))
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("ArriendoPC", "PCLaptop", x)))
}
if (PlanForecast == "Plan" & substring(moisfacturation,5) == "01"){
  ArriendoPC <- filter(data, Servicio == "ArriendoPC")
  rows <- nrow(ArriendoPC)
  noappel_Dat <- filter(noappel_Data, Servicio == "ArriendoPC")
  noappel_Dat <- data.frame(do.call(paste0, noappel_Dat[c(2,1)]))
  noappel_Dat <- data.frame(lapply(noappel_Dat, function(x) gsub("ArriendoPC", "PCLaptop", x)))
}

m_total_Data <- abs(ArriendoPC[m_total])
montant_charge_Data <- abs(ArriendoPC[montant_charge])
libelle_charge_Data <- ArriendoPC[libelle_charge]

libelle_charge_Data[] <-
  lapply(libelle_charge_Data, function(x)
    paste("Plan", sep = ""))

m_total_facture_Data <- abs(round(colSums(m_total_Data), 4))
m_total_ttc_facture_Data <- abs(round(m_total_facture_Data * 1.19, 4))
centrefacturation <- "420901002PCLaptop"
nofactu <- facture_name()
nofacture_Plan <- strsplit(nofactu, "-")
nofacture <-
  paste(
    nofacture_Plan[[1]][1],
    PlanForecast,
    "ArriendoPC",
    nofacture_Plan[[1]][3],
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
#df_m_total_Data <- numeric(rows)
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
  df_noappel_Data[i] <- noappel_Dat[i, ]
  df_libelle_charge_Data[i] <- libelle_charge_Data[i, ]
  df_montant_charge_Data[i] <- (montant_charge_Data[i, ])/1000
 # df_m_total_Data[i] <- (m_total_Data[i, ])/1000
  df_m_total_facture_Data[i] <- (m_total_facture_Data)/1000
  df_m_total_ttc_facture_Data[i] <- (m_total_ttc_facture_Data)/1000
}
ArriendoPC <-
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
    noappel_Dat,
    df_libelle_charge_Data,
    df_montant_charge_Data,
   # df_m_total_Data,
    df_m_total_facture_Data,
    df_m_total_ttc_facture_Data
  )
names(ArriendoPC) <-
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
  #  "m_total",
    "m_total_facture",
    "m_total_ttc_facture"
  )
ArriendoPC$montant_charge[] <-
  lapply(ArriendoPC$montant_charge, function(x)
    gsub("\\.", ",", x))
#ArriendoPC$m_total[] <- lapply(ArriendoPC$m_total, function(x)
 # gsub("\\.", ",", x))
ArriendoPC$m_total_facture[] <-
  lapply(ArriendoPC$m_total_facture, function(x)
    gsub("\\.", ",", x))
ArriendoPC$m_total_ttc_facture[] <-
  lapply(ArriendoPC$m_total_ttc_facture, function(x)
    gsub("\\.", ",", x))
ArriendoPC <<-
  data.frame(lapply(ArriendoPC, as.character), stringsAsFactors = FALSE) 