

# 0 - Paramètres et packages ----------------------------------------------
library(tm)
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tidytext)
library(topicmodels)

data_publishers <- readRDS("data/data_publishers.RDS")
str(data_publishers)


# 1 - Conversion en DocumentTermMatrix ------------------------------------


# Isole les descriptions vides

no_description <-
  which(data_publishers$lemme_des %in% c("none", "vide", ""))
length(no_description)
data_publishers[no_description, "lemme_des"] <- "NA"
data_publishers$description_NA <-
  ifelse(data_publishers$lemme_des == "NA", 1, 0)

# Remplacer les accents avant la mise en matrice

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
txt <- gsub("Ãª", "e", data_publishers$lemme_des)
txt <- chartr(lettres_avec_accents, lettres_sans_accents, txt)
head(data_publishers$lemme_des, 20)


# Mise en Matrice terme * document
corpus <- Corpus(x = VectorSource(x = txt))
for (i in 1:length(corpus)) {
  attr(corpus[[i]], "ID") <- data_publishers$publisher_id[i]
}
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
    "deja"
  )
)

dtm_tf <- DocumentTermMatrix(x = corpus,
                             control = list(weighting = weightTf))
inspect(dtm_tf) #26081 termes

# Nettoyage matrice
dtm_tf <- removeSparseTerms(dtm_tf, sparse = 0.98)
sort(apply(dtm_tf, 2, sum), decreasing = T)

# 2 - Topic modeling ------------------------------------------------------
rowTotals <- apply(dtm_tf, 1, sum)

dtm.new <- dtm_tf[rowTotals > 0, ]
ap_lda <- LDA(dtm.new, k = 3, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ topic, scales = "free") +
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents



# 3 - Analyse en composantes principales ----------------------------------

# Rapide PCA pour voir sur le texte
dtm_tf <- as.matrix(dtm_tf)
pca_tf <- PCA(dtm_tf)

# Creation des variables
infos_publishers <- cbind(data_publishers, dtm_tf)
infos_publishers$termes_NA <- ifelse(rowSums(dtm_tf) == 0, 1, 0)
str(infos_publishers)

variables_termes    <- colnames(dtm_tf)
variables_base      <-
  c("nb_followers",
    "nb_friends",
    "nb_listed",
    "nb_favorites",
    "nb_tweets")
variables_calculees <- c("jo2024_total_tweets",
                         "jo2024_total_reply",
                         "jo2024_total_retweet",
                         "jo2024_total_favorites",
                         "anciennete",
                         "nwords",
                         "description_NA",
                         "termes_NA")

# Première analyse avec toutes les variables
variables_PCA <- c(variables_termes, variables_base, variables_calculees)
pca_publishers <- PCA(infos_publishers[, variables_PCA])

#3 outliners =>les organisateurs
View(infos_publishers[c(5392, 478, 7429),])

# Nombre de dimensions 
fviz_eig(pca_publishers, addlabels = TRUE)
# Attention,beaucoup trop de variables, dimensions de faible inertie

# Contribution des variables
# Dim 1 
sort(pca_publishers$var$contrib[,1], decreasing = T)
fviz_contrib(pca_publishers, choice = "var", axes = 1, top = 10) 
fviz_contrib(pca_publishers, choice = "var", axes = 2, top = 10) 
# Contribution des individus


df_dtm_tf$nb_mots_cles <- apply(dtm_tf, 1, sum)
df_dtm_tf$nb_mots_cles_NA <-
  ifelse(df_dtm_tf$nb_mots_cles == 0, 1, 0)
summary(df_dtm_tf)
PCAshiny(df_dtm_tf)

tentative <- cbind(data_publishers, dtm_tf)
str(tentative)


# Reprise CUT -------------------------------------------------------------
data_publishers %>% glimpse()
infos_publishers %>% glimpse()


# Sélection des variables -------------------------------------------------
# base = infos_publishers %>% 
#   select(-publisher_name,-publisher_nickname,-publisher_description,-publisher_account_creation_dt,
#          -description,-nouveau,-suivre,-faire,-aussi,-tweets,-bien,-twitter,-french,-autre,-grand,
#          -monde,-vie,-compter,-citoyen,-tout,-aimer,-passionne,-plus,-compte,-groupe,-francais,
#          -jo2024_total_reply,-jo2024_total_retweet,-jo2024_total_favorites,
#          -nb_listed)
base = infos_publishers %>% 
  select(-publisher_nickname,-publisher_description,-publisher_account_creation_dt,
         -description,-suivre,-france,-faire,-grand,-vie,-plus,-compte,-nb_listed)
str(base)

res_PCA<-PCA(select(base,-c(publisher_id,publisher_name,lemme_des,nwords,termes_NA)), scale.unit = TRUE, ncp = 10, graph = FALSE)


# Eigen values
fviz_eig(res_PCA, addlabels = TRUE ,ncp=30)

# Nuages des variables
fviz_pca_var(res_PCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)

# Nuages des variables
fviz_pca_var(res_PCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             axes=c(1,3),
             repel = TRUE, 
             ggtheme = theme_minimal()
)

res_PCA_coord<-data.frame(res_PCA$ind$coord)

plot(res_PCA_coord$Dim.1,res_PCA_coord$Dim.2)
abline(v=c(0), col=c("black"), lty=c(1), lwd=c(1))
abline(h=c(0), col=c("black"), lty=c(1), lwd=c(1))

plot(res_PCA_coord$Dim.1,res_PCA_coord$Dim.3)
abline(v=c(0), col=c("black"), lty=c(1), lwd=c(1))
abline(h=c(0), col=c("black"), lty=c(1), lwd=c(1))

# peu de corrélation entre nb de tweet et caractéristiques du tweetos


# Contribution des variables aux axes -------------------------------------
var1 <- facto_summarize(res_PCA, "var", result = c("coord",
                                                   "cos2", "contrib"), axes = 1)
var2 <-facto_summarize(res_PCA, "var", result = c("coord",
                                                  "cos2", "contrib"), axes = 2)
var3 <- facto_summarize(res_PCA, "var", result = c("coord",
                                                   "cos2", "contrib"), axes = 3)
var4 <-facto_summarize(res_PCA, "var", result = c("coord",
                                                  "cos2", "contrib"), axes = 4)

VAR<-cbind(var1,var2,var3,var4)
VAR

# coordonnées des individus sur les axes
PCA_ind<-res_PCA$ind$coord


# KMeans + CAH pour connaitre le nb de classes ----------------------------
groupe1<-kmeans(PCA_ind[,1:10],centers=500)

barycentres=groupe1$centers

mat_dist<-dist(barycentres,method="euclidean")

cah<-hclust(mat_dist,method="ward.D",members=NULL)


<<<<<<< HEAD
# choix du nombre de classes ?
=======


>>>>>>> 8a022c82655e3dc7c944be1045f62ac0f36e4b88
inertie <- sort(cah$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", 
     ylab = "Inertie")

plot(cah)

library(RColorBrewer)
labelColors = brewer.pal(n = 7, name = "Spectral")
barplot(c(1,2,3,4,5,6,7), col=labelColors)
# cut dendrogram in 7 clusters
clusMember = cutree(cah, 7)
table(clusMember)

hcd = as.dendrogram(cah)

# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
plot(clusDendro)


# Lancement des Kmeans ----------------------------------------------------
km <- kmeans(PCA_ind[,1:10],centers=7 ,nstart=15)$cluster

res_fin <-cbind(km,base)

names(res_fin)[1] <- 'CLUSTER'

vol<-data.frame(table(res_fin$CLUSTER))
vol

# cluster 4 -> 3 individus :
CL4<-filter(res_fin,CLUSTER==4)
# Ce sont les 3 organisateurs qui ont bcp tweeté

# résultat par cluster
base %>% glimpse()
z1<-res_fin %>% 
  group_by(CLUSTER) %>% 
  summarise_each (funs(mean), 
                  jo2024_total_tweets,
                  nb_followers,
                  nb_friends,
                  nb_favorites,
                  nb_tweets,
                  jo2024_total_reply,
                  jo2024_total_retweet,
                  jo2024_total_favorites,
                  anciennete,
                  nwords,
                  lemme_des,
                  description_NA,
                  media,
                  paris,
                  communication,
                  politique,
                  sport,
                  digital,
                  journaliste,
                  marketing,
                  social,
                  directeur,
                  president,
                  tweets,
                  innovation,
                  engager,
                  actualite,
                  monde,
                  team,
                  officiel,
                  sportif,
                  termes_NA
  )
View(z1)


z1_global<-res_fin %>% 
  summarise_each (funs(mean), 
                  jo2024_total_tweets,
                  nb_followers,
                  nb_friends,
                  nb_favorites,
                  nb_tweets,
                  jo2024_total_reply,
                  jo2024_total_retweet,
                  jo2024_total_favorites,
                  anciennete,
                  nwords,
                  lemme_des,
                  description_NA,
                  media,
                  paris,
                  communication,
                  politique,
                  sport,
                  digital,
                  journaliste,
                  marketing,
                  social,
                  directeur,
                  president,
                  tweets,
                  innovation,
                  engager,
                  actualite,
                  monde,
                  team,
                  officiel,
                  sportif,
                  termes_NA
  )
write.csv2(z1, file="data/cluster2.csv")
write.csv2(z1_global, file="data/cluster2_.csv")


# Identification des cluster ----------------------------------------------
# Faire un heatmap avec les variables les + pertinentes
z1_var = z1 %>% select(#CLUSTER,
                       description_NA,
                       media,
                       paris,
                       communication,
                       politique,
                       sport,
                       digital,
                       journaliste,
                       marketing,
                       social,
                       # directeur,
                       president,
                       tweets,
                       # innovation,
                       engager,
                       # actualite,
                       # monde,
                       # team,
                       officiel,
                       sportif
                       # termes_NA
                       )

colfunc <- colorRampPalette(c("white", "blue"))
heatmap(as.matrix(z1_var),scale = "column",col = colfunc(15), main="caracteristique cluster",
        Rowv = NA,Colv = NA)

# Zoom sur certains cluster
CL6 = res_fin %>% 
  select(CLUSTER,publisher_id,publisher_name,lemme_des) %>% 
  filter(CLUSTER==6)
View(CL6)  

CL5 = res_fin %>% 
  select(CLUSTER,publisher_id,publisher_name,lemme_des) %>% 
  filter(CLUSTER==5)
View(CL5)  
View(z1)

#avec ce tableau, faire pour chaque variable pour chaque classe , 
#l'écart à la moyenne.
#ca permet d'idenetifier les variables discriminantes
# + regrouper des cluster
res_fin$CL_1<- ifelse(res_fin$CLUSTER == 1 | res_fin$CLUSTER == 6, 1, 0) #Média, digital, comm
res_fin$CL_2<- ifelse(res_fin$CLUSTER == 2 , 1, 0) # Les officiels (maires, ...)
res_fin$CL_3<- ifelse(res_fin$CLUSTER == 3 , 1, 0) # Les twittos
res_fin$CL_4<- ifelse(res_fin$CLUSTER == 4 , 1, 0) # Les organisateurs
res_fin$CL_5<- ifelse(res_fin$CLUSTER == 5 | res_fin$CLUSTER == 7, 1, 0) # Les influenceurs

# Combien d'individus par cluster :
res_fin %>% count(CL_1) #1 309
res_fin %>% count(CL_2) #1 608
res_fin %>% count(CL_3) #10 261
res_fin %>% count(CL_4) #3
res_fin %>% count(CL_5) #1 760

library(rpart)
library(rpart.plot)
#permet de voir d'une autre façon les variables
#discriminant chaque classe
z1 %>% glimpse()
mod_arbre <- rpart(CL_4~
                     jo2024_total_tweets+
                   nb_followers+
                   nb_friends+
                   nb_favorites+
                   nb_tweets+
                   jo2024_total_reply +
                   jo2024_total_retweet+
                   jo2024_total_favorites+
                   anciennete+
                   description_NA+
                   media+
                   paris+
                   communication+
                   politique+
                   sport+
                   digital+
                   journaliste+
                   marketing+
                   social+
                   directeur+
                   president+
                   tweets+
                   innovation+
                   engager+
                   actualite+
                   monde+
                   # team+
                   officiel+
                   sportif
                   # termes_NA
                   ,
                   res_fin)

plot_arbre <- rpart.plot(mod_arbre,extra=1 , fallen.leaves=T)

