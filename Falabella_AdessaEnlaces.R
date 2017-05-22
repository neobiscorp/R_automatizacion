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