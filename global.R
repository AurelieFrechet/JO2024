
# Packages ----------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(lubridate) 
library(plotly)
library(tm)
library(wordcloud)
library(wordcloud2)
library(memoise)


# Chargement des données --------------------------------------------------

trace_points <- readRDS("data/trace_points.RDS") 


# Couleurs et paramètres graphiques  --------------------------------------

colors_JO <- list(bleu = "#0085C7",
                  noir = "#000000",
                  jaune = "#F4C300",
                  vert = "#009F3D",
                  rouge = "#DF0024")

trace_points$segment_col <- sapply(
  trace_points$segment,
  FUN = function(x) {
    switch(
      x,
      "organisateurs" = colors_JO$bleu,
      "lambdas" = colors_JO$noir,
      "politiques" = colors_JO$rouge,
      "influenceurs" = colors_JO$jaune,
      "medias" = colors_JO$vert
    )
  }
)

# Ajouts de texte
text <- c(
  "23 juin 2015 - Paris annonce sa candidature aux JO 2024",
  "15 septembre 2015 - Dépôt des candidatures : Paris, Los Angeles",
  "9 février 2016 - Logo dévoilé sur l'Arc de Triomphe",
  "3 février 2017 - Slogan dévoilé sur la Tour Eiffel",
  "22 mars 2017 - Régions de France apporte leur soutien à la candidature",
  "13-17 mai 2017 - La comission d'évaluation du CIO visite Paris",
  "23-24 juin 2017 - Paris célèbre la journée internationale Olympique",
  "11 juillet 2017 - La double attribution votée : Paris aura les Jeux",
  "13 septembre 2017 - L'annonce officielle à Lima"
)


x <- as.Date(
  c(
    "2015-06-23",
    "2015-09-15",
    "2016-02-09",
    "2017-02-03",
    "2017-03-22",
    "2017-05-13",
    "2017-06-23",
    "2017-07-11",
    "2017-09-13"
  )
)
class(x)
y <- rep(0, length(text))
ax = c()
ay = c(60, 40, 20, 80, 100, 80, 60, 40, 20)
xanchor = c("left",
            "left",
            "left",
            "right",
            "left",
            "left",
            "left",
            "left",
            "left")