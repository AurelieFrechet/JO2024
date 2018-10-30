


# 0 - Paramètres ----------------------------------------------------------

# Library
library(dplyr)
library(Factoshiny)
library(FactoMineR)
library(HDclassif)
library(plotly)
library(topicmodels)

# 1- Creation de la base d'étude ------------------------------------------

data_publishers <- readRDS("data/data_publishers.RDS")
str(data_publishers)

# 2 - L'analyse de la flemme ----------------------------------------------

PCAshiny(data_publishers)
individus_hors_normes <- c(7429, 478, 5392, 9071, 460)
View(data_publishers[individus_hors_normes, ]) # description = NA ? Impossible normalement =>> à vérifier


res.pca <- PCA(X = data_publishers[-individus_hors_normes,-c(1:6,16)])
coord_ind <- as.data.frame(res.pca$ind$coord)
coord_ind$ind<- data_publishers$publisher_nickname[-individus_hors_normes]

plot_ly(data = coord_ind,
        x = ~Dim.3,
        y = ~Dim.2,
        text = ~ind)

