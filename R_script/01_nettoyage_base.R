


# 0 - Paramètres ----------------------------------------------------------
library(readr)   # Chargement des données
library(R.utils)
library(dplyr)
library(tm)      # Nettoyage du corpus
library(stringr) # Nettoyage des chaines de caractères
library(koRpus)  # Lemmatisation
library(parallel)

chemin_donnee <- "data/csv_datas_full.csv"


sourceDirectory("R_functions/")

# 1 - Import des données --------------------------------------------------

base_depart <-   read.table(
  file = chemin_donnee,
  header = TRUE,
  sep = "\t",
  comment.char = "",
  quote = "\"",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)
str(base_depart) # beaucoup de variables inutiles
taux_remplissage(base_depart, colnames(base_depart))



# 2 - Création de la base des tweets --------------------------------------

# Selection des variables

data_tweets <- base_depart %>%
  select(
    tweet_id,
    tweet_creation_dt,
    tweet_text,
    tweet_quote_count,
    tweet_reply_count,
    tweet_retweet_count,
    tweet_favorite_count,
    tweet_used_hashtags_list,
    tweet_user_mentions_list,
    tweet_is_reply,
    tweet_is_quote_of_tweet,
    publisher_id
  )

# Réécriture des mentions et des hashtages

data_tweets$tweet_used_hashtags_list <-
  gsub("[[:punct:]]", "", data_tweets$tweet_used_hashtags_list)
data_tweets$tweet_user_mentions_list <-
  gsub("[[:punct:]]", "", data_tweets$tweet_user_mentions_list)

# Réécriture du corpus du texte

data_tweets$corpus  <- nettoyage_text(data_tweets$tweet_text)

# traduction en lemme via parralelisation
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(koRpus))
test <- parSapply(cl,
                  data_tweets$corpus,
                  lemmatisation)
stopCluster(cl)
data_tweets$lemme <- test
class(data_tweets$corpus)
# Sauvegarde

saveRDS(data_tweets, "data/data_tweets.RDS")
data_tweets <- readRDS("data/data_tweets.RDS")

#  Création de la base des publishers -------------------------------------

# Sélection des variables



data_publishers <- base_depart %>%
  group_by(
    publisher_id,
    publisher_name,
    publisher_nickname,
    publisher_description,
    publisher_account_creation_dt
  ) %>%
  summarise(
    jo2024_total_tweets    = n(),
    nb_followers           = max(publisher_followers_count, na.rm = T),
    nb_friends             = max(publisher_friends_count, na.rm = T),
    nb_listed              = max(publisher_listed_count, na.rm = T),
    nb_favorites           = max(publisher_favourites_count, na.rm = T),
    nb_tweets              = max(publisher_tweet_count, na.rm = T),
    jo2024_total_tweets    = n(),
    jo2024_total_reply     = sum(tweet_reply_count),
    jo2024_total_retweet   = sum(tweet_retweet_count),
    jo2024_total_favorites = sum(tweet_favorite_count)
  ) %>%
  mutate(anciennete = as.numeric(difftime(
    as.POSIXlt(
      "20/09/2018 00:00:00",
      tz = "GMT",
      format = ("%d/%m/%Y %H:%M:%S")
    ),
    as.POSIXlt(
      publisher_account_creation_dt,
      tz = "GMT",
      format = ("%d/%m/%Y %H:%M:%S")
    )
  ))) %>%
  ungroup() %>%
  as.data.frame()
str(data_publishers)

# Nettoyage de la description

data_publishers$description <-
  nettoyage_text(data_publishers$publisher_description)
data_publishers$nwords <-
  sapply(strsplit(data_publishers$description, " "), length)

# Gestion des descriptions vides
sum(est_vide(data_publishers$description))
data_publishers[est_vide(data_publishers$description), "description"] <- "vide"


# traduction en lemme via parralelisation
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(koRpus))
test <- parSapply(cl,
                  data_publishers$description,
                  lemmatisation)
stopCluster(cl)
data_publishers$lemme_des <- test


# Sauvegarde

saveRDS(data_publishers, "data/data_publishers.RDS")
data_publishers <- readRDS("data/data_publishers.RDS")
