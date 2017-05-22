##Sabcito##
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
centrefacturation <- "iPAD"
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
Kioscos <- filter(data, grepl('Kioscos', Servicio))
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
Seteadora[Seteadora$noappel=='pathfinderviña1',10] <- 'pathfindervina1'
Seteadora <<-
  data.frame(lapply(Seteadora, as.character), stringsAsFactors = FALSE)  


##POS##
POS <- filter(data, grepl(' POS ', Servicio))
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