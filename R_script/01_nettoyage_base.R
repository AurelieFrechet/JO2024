

# 0 - Paramètres ----------------------------------------------------------
library(readr)   # Chargement des données
library(tm)      # Nettoyage du corpus
library(stringr) # Nettoyage des chaines de caractères
library(koRpus)

chemin_donnee <- "data/csv_datas_full.csv"


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
# source : https://rstudio-pubs-static.s3.amazonaws.com/93384_92b6fcd92c5642a2a52e97cd5694b6ab.html

# Mise en UTF-8
data_tweets$corpus <- nettoyage_text(data_tweets$tweet_text)

View(data_tweets[, c(5,54)])

# Nettoyage description du Twittos ----------------------------------------
data_tweets$description <- nettoyage_text(data_tweets$publisher_description)


# Recherche méthodo -------------------------------------------------------


library(quanteda)
txt <- c(doc1 = "Anne Hidalgo pourrait accepter les Jeux Olympiques à Paris @LaTribune #JO2024 @Anne_Hidalgo  http://t.co/yxhVf2AQ5h",
         doc2 = "#JO2024: 4,7 milliards de dollars de budget pour Bostonhttp://t.co/IXy4PQnkAX #ambitionolympique par @LesEchos http://t.co/YzjWgHKXmx",
         doc3 = "\"#GrandParis Amsalem : \"\"Paris n’est pas prêt pour 2024\"\" http://t.co/rx7tpuxhIc #JO2024\"")
tokens(txt)
# removing punctuation marks and lowecasing texts
file <- tokens(char_tolower(txt),
       remove_punct = TRUE,
       remove_separators =TRUE,
       remove_symbols = TRUE,
       remove_twitter= TRUE,
       remove_url = TRUE)


