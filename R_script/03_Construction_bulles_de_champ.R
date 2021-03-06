###########################################################################
# Titre : Construction bulles de champ
# MAJ : 02-11-2018
###########################################################################


# 0 - Chargement données et packages --------------------------------------

# Packages
library(dplyr)
library(lubridate)


# Données 
data_tweets <- readRDS("data/data_tweets.RDS")


# 1 - Récupération du segment par twittos --------------------------------
# Présence du mot "officiel" dans la description
# data_tweets$officiel <- FALSE
# data_tweets[grep(pattern = "officiel", x = data_tweets$description), "officiel"] <- TRUE
# 
# # Présence du mot "actu" dans la description
# data_tweets$actu <- FALSE
# data_tweets[grep(pattern = "actu", x = data_tweets$description), "actu"] <- TRUE
# 
# # Présence du mot "sport" dans la description
# data_tweets$sport <- FALSE
# data_tweets[grep(pattern = "sport", x = data_tweets$description), "sport"] <- TRUE
# 
# # Présence du mot "journalist" dans la description
# data_tweets$journalist <- FALSE
# data_tweets[grep(pattern = "journalist", x = data_tweets$journalist), "journalist"] <- TRUE
# 
# # variable segment
# data_tweets = data_tweets %>% mutate(segment=ifelse(officiel==TRUE,"officiel",
#                                                     ifelse(actu==TRUE,"actu",
#                                                            ifelse(sport==TRUE,"sport",
#                                                                   ifelse(journalist==TRUE,"journalist","twittos")))))


# 2 - Création de variables pour le tracé des points ----------------------

# Création d’une table à la maille date -----------------------------------

data_tweets %>% glimpse()

base_tw = data_tweets %>% 
  select (tweet_id,tweet_creation_dt,tweet_text,tweet_retweet_count,tweet_favorite_count,segment)

ech = base_tw[1:1000,]

# Retravailler la date ----------------------------------------------------

base_tw <- readRDS("data/data_tweets.RDS")
base_tw$segment <- as.factor(rep(1:2, length = nrow(base_tw)))
base_tw$tweet_creation_dt %>% class()
base_tw$tweet_creation_dt = sort(as.POSIXlt(x = base_tw$tweet_creation_dt, tz = "GMT", format=("%d/%m/%Y %H:%M:%S")))
base_tw$tweet_creation_dt %>% class()

base_tw$tweet_date = as.Date(base_tw$tweet_creation_dt)

# Coordonnées du tweet ----------------------------------------------------
base_tw %>% glimpse()
# a - compter le nb de tweet par jour -------------------------------------
maille_jour = base_tw %>%
  select (tweet_date,tweet_id,tweet_text,tweet_retweet_count,tweet_favorite_count, segment) %>% 
  group_by(tweet_date) %>% 
  mutate(nb_tw_par_jour =n()) %>% 
  ungroup()

str(maille_jour)
ech = maille_jour_tri[1:1000,]


# b - Trier par date et par retweet ----------------------------------------
maille_jour_tri = maille_jour %>% 
  arrange(tweet_date,tweet_retweet_count) 


# c - boucle pour ordonner les tweetos ------------------------------------
maille_jour_tri$count = unlist(lapply(table(maille_jour_tri$tweet_date), function(x){1:x}))

#Taille et coordonnées des points
maille_jour_tri$size_point <-
  floor((maille_jour_tri$tweet_retweet_count) / max(maille_jour_tri$tweet_retweet_count) * 200)+ 2

maille_jour_tri$coord <- unlist(
  by(data = floor(maille_jour_tri$size_point/2)+1, maille_jour_tri$tweet_date, cumsum))

# 3 - Graphique -----------------------------------------------------------

# test graph ----------------------------------------------------------
# http://plotly-book.cpsievert.me/

# plot_ly(
#   x=c(1,2,3),
#   y=c(5,6,7),
#   type="scatter",
#   mode="markers",
#   size=c(1,5,10),
#   marker = list(
#     color=c("red","blue","green")
#   )
# )
# 
# 
# maille_jour_tri %>% 
#   ggplot(aes(x = tweet_date, y = count, col = segment)) + 
#   geom_point() 
# 
# ggplotly()
# 
# base = maille_jour_tri %>% 
#   dplyr::filter(tweet_date>= as.Date("2017-07-01") & tweet_date <= as.Date("2017-10-01"))
# 
# plot_ly(
#   x=maille_jour_tri$tweet_date,
#   y=maille_jour_tri$count,
#   text=maille_jour_tri$tweet_text,
#   type="scatter",
#   mode="markers",
#   marker = list(
#     color=c("red","blue","green")
#   )
# )


# Plotly ------------------------------------------------------------------

p <- plot_ly(maille_jour_tri, 
             x = ~tweet_date, 
             y = ~coord, 
             # text = ~tweet_text, 
             type = 'scattergl', 
             mode = 'markers',
             marker = list(size = ~size_point, 
                           opacity = 0.5,
                           color = 'none',
                           line = list(
                             color = ~segment,
                             width = 12
                           ))) %>%
  layout(title = 'Tweets émis sur les JO2024',
         xaxis = list(title = "",
                      showgrid = FALSE,
                      rangesilder = list(type = "date")),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      zeroline = FALSE,
                      showline = FALSE,
                      ticks = '',
                      showticklabels = FALSE))

p



# Sauvegarde des données pour appli ---------------------------------------

# Trace points :
# x : date du tweet
# y : coord du point
# segment : segment du twittos
# key : id du tweet

saveRDS(maille_jour_tri,  "data/trace_points.RDS")

# Trace nuage 
# key : id du tweet
# mots : lemmes du tweet
# tweet_html : tweet au format html



