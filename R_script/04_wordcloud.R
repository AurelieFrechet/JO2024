
# Packages et chargement des données --------------------------------------
library(tm)
library(wordcloud)
library(wordcloud2)

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

unwanted_array = list(
  'Š' = 'S',
  'š' = 's',
  'Ž' = 'Z',
  'ž' = 'z',
  'À' = 'A',
  'Á' = 'A',
  ' ' = 'A',
  'Ã' = 'A',
  'Ä' = 'A',
  'Å' = 'A',
  'Æ' = 'A',
  'Ç' = 'C',
  'È' = 'E',
  'É' = 'E',
  'Ê' = 'E',
  'Ë' = 'E',
  'Ì' = 'I',
  'Í' = 'I',
  'Î' = 'I',
  'Ï' = 'I',
  'Ñ' = 'N',
  'Ò' = 'O',
  'Ó' = 'O',
  'Ô' = 'O',
  'Õ' = 'O',
  'Ö' = 'O',
  'Ø' = 'O',
  'Ù' = 'U',
  'Ú' = 'U',
  'Û' = 'U',
  'Ü' = 'U',
  'Ý' = 'Y',
  'Þ' = 'B',
  'ß' = 'S',
  'à' = 'a',
  'á' = 'a',
  'â' = 'a',
  'ã' = 'a',
  'ä' = 'a',
  'å' = 'a',
  'æ' = 'a',
  'ç' = 'c',
  'è' = 'e',
  'é' = 'e',
  'ê' = 'e',
  'ë' = 'e',
  'ì' = 'i',
  'í' = 'i',
  'î' = 'i',
  'ï' = 'i',
  'ð' = 'o',
  'ñ' = 'n',
  'ò' = 'o',
  'ó' = 'o',
  'ô' = 'o',
  'õ' = 'o',
  'ö' = 'o',
  'ø' = 'o',
  'ù' = 'u',
  'ú' = 'u',
  'û' = 'u',
  'ý' = 'y',
  'ý' = 'y',
  'þ' = 'b',
  'ÿ' = 'y'
)

lettres_avec_accents = paste(names(unwanted_array), collapse = ' ')
lettres_sans_accents = paste(unwanted_array, collapse = ' ')
txt <- chartr(lettres_avec_accents, lettres_sans_accents, txt)
corpus <- Corpus(x = VectorSource(x = txt))

corpus <- tm_map(
  x = corpus,
  FUN = removeWords,
  words = c(
    stopwords("fr"),
    "etre",
    "tre",
    "for",
    "the",
    "and",
    "avoir",
    "deja",
    "paris2024",
    "jo2024"
  )
)

dtm_tf <- DocumentTermMatrix(x = corpus,
                             control = list(weighting = weightTf))
inspect(dtm_tf) #26761 termes

# Nettoyage matrice
dtm_tf <- removeSparseTerms(dtm_tf, sparse = 0.99)
words_tf<- sort(apply(dtm_tf, 2, sum), decreasing = T)


# 2 - Nuage de mots -------------------------------------------------------

wordcloud(
  words = names(words_tf), 
  freq = words_tf,
  max.words = 100,
  random.order = FALSE,
  random.color = TRUE,
  colors = unlist(colors_JO)
)


wordcloud2(mots[1:1000,])

