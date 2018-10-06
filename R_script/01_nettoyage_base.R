

# 0 - Paramètres ----------------------------------------------------------
chemin_donnee <- "data/csv_datas_full.csv"


# 1 - Import des données --------------------------------------------------

data_tweets <-   read_delim(chemin_donnee,
                            "\t",
                            escape_double = FALSE)
