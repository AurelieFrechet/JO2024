
# 0 - Paramètres et packages ----------------------------------------------
library(tm)


data_publishers <- readRDS("data/data_publishers.RDS")

# 1 - Conversion en DocumentTermMatrix ------------------------------------


# Isole les descriptions vides
data_publishers$nwords <- as.numeric(data_publishers$nwords)
no_description <-
  which(data_publishers$lemme_des %in% c("none", "vide", ""))
length(no_description)
data_publishers[no_description, "lemme_des"] <- "NA"
data_publishers$description_NA <-
  ifelse(data_publishers$lemme_des == "NA", 1, 0)

# Remplacer les accents avant la mise en matrice
unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', ' '='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A',
                          'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N',
                          'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 'Ü'='U',
                          'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n',
                          'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u',
                          'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

lettres_avec_accents=paste(names(unwanted_array), collapse=' ')
lettres_sans_accents=paste(unwanted_array, collapse=' ')

txt <- chartr(lettres_avec_accents,lettres_sans_accents,data_publishers$lemme_des)
head(txt)


# Mise en Matrice terme * document
corpus <- Corpus(x = VectorSource(x = txt))
for(i in 1:length(corpus)){
  attr(corpus[[i]], "ID") <- data_publishers$publisher_id[i]
}
corpus <- tm_map(x = corpus,
                 FUN = removeWords,
                 words = c(stopwords("fr"),
                           "etre",
                           "for",
                           "the",
                           "and",
                           "avoir",
                           "deja"))

dtm_tf <- DocumentTermMatrix(x = corpus,
                                control = list(weighting = weightTf))
inspect(dtm_tf) #26081 termes

# Nettoyage matrice 
dtm_tf <- removeSparseTerms(dtm_tf, sparse = 0.98)
sort(apply(dtm_tf, 2, sum), decreasing = T)

dtm_tf <- as.matrix(dtm_tf)
library(FactoMineR)
pca_tf <- PCA(dtm_tf)


df_dtm_tf$nb_mots_cles <- apply(dtm_tf, 1, sum)
df_dtm_tf$nb_mots_cles_NA <- ifelse(df_dtm_tf$nb_mots_cles == 0, 1, 0)
summary(df_dtm_tf)
PCAshiny(df_dtm_tf)

tentative <- cbind(data_publishers, dtm_tf)
str(tentative)


# 3 - Topic modeling ------------------------------------------------------
rowTotals <- apply(dtm_tf2, 1, sum)

dtm.new <- dtm_tf[rowTotals > 0,]
ap_lda <- LDA(dtm.new, k = 3, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents


