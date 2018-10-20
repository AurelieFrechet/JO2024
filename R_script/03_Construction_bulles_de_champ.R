
# 03_Construction_bulles_de_champ -----------------------------------------


# Création d’une table à la maille date -----------------------------------
data_tweets %>% glimpse()

base_tw = data_tweets %>% 
  select (tweet_id,tweet_creation_dt,tweet_text,tweet_retweet_count,tweet_favorite_count)

ech = base_tw[1:1000,]

# Retravailler la date ----------------------------------------------------
library(lubridate) 
base_tw$tweet_creation_dt %>% class()
base_tw$tweet_creation_dt = sort(as.POSIXlt(x = base_tw$tweet_creation_dt, tz = "GMT", format=("%d/%m/%Y %H:%M:%S")))
base_tw$tweet_creation_dt %>% class()

base_tw$tweet_date = as.Date(base_tw$tweet_creation_dt)

# Coordonnées du tweet ----------------------------------------------------
base_tw %>% glimpse()
# 1 - compter le nb de tweet par jour -------------------------------------
maille_jour = base_tw %>%
  select (tweet_date,tweet_id,tweet_text,tweet_retweet_count,tweet_favorite_count) %>% 
  group_by(tweet_date) %>% 
  mutate(nb_tw_par_jour =n()) %>% 
  ungroup()

str(maille_jour)
ech = maille_jour_tri[1:1000,]


# 2 - Trier par date et par retweet ----------------------------------------
maille_jour_tri = maille_jour %>% 
  arrange(tweet_date,tweet_retweet_count) 


# 3 - boucle pour ordonner les tweetos ------------------------------------
compteur = maille_jour_tri %>% 
  mutate()

maille_jour_tri$count = unlist(lapply(table(maille_jour_tri$tweet_date), function(x){1:x}))




