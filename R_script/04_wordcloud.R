
# Packages et chargement des données --------------------------------------
library(tm)
library(wordcloud)

data_tweets <- readRDS("data/data_tweets.RDS")

colors_JO <- list(bleu = "#0085C7",
                  noir = "#000000",
                  jaune = "#F4C300",
                  vert = "#009F3D",
                  rouge = "#DF0024")

# 1 - Création de la matrice de terme -------------------------------------
txt <- data_tweets$lemme
txt <- gsub("Ãªtre", "être", txt)
mots <- unlist(strsplit(txt, split = " "))
mots <- as.data.frame(table(mots))
mots <- mots[order(mots$Freq, decreasing = TRUE),]
head(mots)

# 2 - Nuage de mots -------------------------------------------------------

wordcloud(
  words = mots$mots, 
  freq = mots$Freq,
  max.words = 100,
  random.order = FALSE,
  random.color = TRUE,
  colors = unlist(colors_JO)
)
