


# 0 - Paramètres ----------------------------------------------------------

# Library
library(dplyr)

data_publishers <- data_tweets %>%
  group_by(
    publisher_id,
    publisher_name,
    publisher_nickname,
    publisher_location,
    publisher_description,
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
