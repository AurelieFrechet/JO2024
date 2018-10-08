

# 0 - Paramètres ----------------------------------------------------------
library(readr)   # Chargement des données
library(tm)      # Nettoyage du corpus
library(stringr) # Nettoyage des chaines de caractères

chemin_donnee <- "data/csv_datas_full.csv"


# 1 - Import des données --------------------------------------------------

data_tweets <-   read_delim(chemin_donnee,
                            "\t",
                            escape_double = FALSE
                            )


# 2 - Nettoyage de la ponctuation dans les hashtags et les mentions -------

data_tweets$hashtags <- gsub("[[:punct:]]", "", data_tweets$tweet_used_hashtags_list)
data_tweets$mentions <- gsub("[[:punct:]]", "", data_tweets$tweet_user_mentions_list)


# 3 - Nettoyage du contenu du tweet ---------------------------------------
# source : https://rstudio-pubs-static.s3.amazonaws.com/93384_92b6fcd92c5642a2a52e97cd5694b6ab.html

# Mise en UTF-8
data_tweets$corpus <-   enc2utf8(data_tweets$tweet_text)

# Retire les emojis
data_tweets$corpus <-
  str_replace_all(string = data_tweets$corpus,
                  pattern = "[^[:graph:]]",
                  replacement = " ")  

# Retire les accents
data_tweets$corpus <-
  iconv(x = data_tweets$corpus,
        from = "UTF-8",
        to = "ASCII//TRANSLIT") 

# Mise en minuscule
data_tweets$corpus <-
  tolower(data_tweets$corpus)  

# Retire les hashtags
data_tweets$corpus <-
  gsub(pattern = " #\\S*",
       replacement = "",
       x = data_tweets$corpus) 

# Retire les URLs (http, https, ftp)
data_tweets$corpus <-
  gsub(pattern = "(f|ht)(tp)(s?)(://)(\\S*)",
       replacement = "",
       x = data_tweets$corpus)     

# Retire les caractères spéciaux
data_tweets$corpus <-
  gsub(pattern = "[^0-9A-Za-z///' ]",
       replacement = "",
       x = data_tweets$corpus)     

# Enlever les espaces en trop
data_tweets$corpus <-
  str_replace(gsub(
    pattern = "\\s+",
    replacement = " ",
    x = str_trim(data_tweets$corpus)
  ), "B", "b")

data_tweets$tweet_text[1:10]
data_tweets$corpus[1:10] 

