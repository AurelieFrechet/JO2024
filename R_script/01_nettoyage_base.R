

# 0 - Paramètres ----------------------------------------------------------
library(readr)   # Chargement des données
library(tm)      # Nettoyage du corpus
library(stringr) # Nettoyage des chaines de caractères
library(koRpus)  # Lemmatisation

chemin_donnee <- "data/csv_datas_full.csv"

source("R_functions/nettoyage_text.R")
source("R_functions/lemmatisation.R")

# 1 - Import des données --------------------------------------------------

data_tweets <-   read.table(file = chemin_donnee,
                            header = TRUE,
                            sep = "\t",
                            comment.char = "",
                            quote = "\"",
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8"
                            )


# 2 - Nettoyage de la ponctuation dans les hashtags et les mentions -------

data_tweets$hashtags <- gsub("[[:punct:]]", "", data_tweets$tweet_used_hashtags_list)
data_tweets$mentions <- gsub("[[:punct:]]", "", data_tweets$tweet_user_mentions_list)


# 3 - Nettoyage du contenu du tweet ---------------------------------------

data_tweets$corpus  <- nettoyage_text(data_tweets$tweet_text)
data_tweets$lemme <- lemmatisation(data_tweets$corpus)

saveRDS(data_tweets, "data/data_tweets1.RDS")
data_tweets <- readRDS("data/data_tweets1.RDS")

View(data_tweets[,c(5,54,55)])


# 4 - Nettoyage de la description des twittos -----------------------------


data_tweets$description <- nettoyage_text(data_tweets$publisher_description)
data_tweets$description2 <- lemmatisation(data_tweets$description)




# 5 - Sauvegarde de la bade de travail ------------------------------------

saveRDS(data_tweets, "data/data_tweets2.RDS")
data_tweets <- readRDS("data/data_tweets2.RDS")

