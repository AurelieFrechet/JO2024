


# 0 - Paramètres ----------------------------------------------------------

# Library
library(dplyr)
library(Factoshiny)

# 1- Creation de la base d'étude ------------------------------------------

data_publishers <- data_tweets %>%
  group_by(
    publisher_id,
    publisher_name,
    publisher_nickname,
    publisher_location,
    publisher_description,
    description,
    publisher_declared_language,
    publisher_account_creation_dt
  ) %>%
  summarise(
    jo2024_total_tweets    = n(),
    nb_followers           = max(publisher_followers_count),
    nb_friends             = max(publisher_friends_count),
    nb_listed              = max(publisher_listed_count),
    nb_favorites           = max(publisher_favourites_count),
    nb_tweets              = max(publisher_favourites_count),
    jo2024_total_tweets    = n(),
    jo2024_total_reply     = sum(tweet_reply_count),
    jo2024_total_retweet   = sum(tweet_retweet_count),
    jo2024_total_favorites = sum(tweet_favorite_count)
  )


# 2 - L'analyse de la flemme ----------------------------------------------

PCAshiny(data_publishers)
individus_hors_normes <- c(7429, 478, 5392, 9071, 460, 6767)
View(data_publishers[individus_hors_normes, ]) # description = NA ? Impossible normalement =>> à vérifier

PCAshiny(data_publishers[-individus_hors_normes,])
individus_hors_normes2 <- c(9071, 460, 6767)


# 3 - Ajout de variables booléennes  --------------------------------------

# Présence du mot "officiel" dans la description
data_publishers$officiel <- FALSE
data_publishers[grep(pattern = "officiel", x = data_publishers$description), "officiel"] <- TRUE

# Présence du mot "actu" dans la description
data_publishers$actu <- FALSE
data_publishers[grep(pattern = "actu", x = data_publishers$description), "actu"] <- TRUE

# Présence du mot "sport" dans la description
data_publishers$sport <- FALSE
data_publishers[grep(pattern = "sport", x = data_publishers$description), "sport"] <- TRUE

# Présence du mot "journalist" dans la description
data_publishers$journalist <- FALSE
data_publishers[grep(pattern = "journalist", x = data_publishers$journalist), "journalist"] <- TRUE

