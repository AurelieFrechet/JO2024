nettoyage_text <- function(text) {
  

# Nettoyage du tweet ------------------------------------------------------

  # Retire les emojis
  corpus <-
    str_replace_all(string = text,
                    pattern = "[^[:graph:]]",
                    replacement = " ")
  
  # # Retire les accents
  # corpus <-
  #   iconv(x = corpus,
  #         from = "UTF-8",
  #         to = "ASCII//TRANSLIT")
  
  # Séparation des hashtags par Majuscule (VenezPartager)
  # corpus <-
  #   gsub('([[:upper:]])', ' \\1',
  #        x = corpus)
  
  # Retire les hashtags
  corpus <-
    gsub(pattern = "#",
         replacement = "",
         x = corpus)
  
  # Retire les mentions
  corpus <-
    gsub(pattern = "@([a-zA-Z0-9]|[_])*",
         replacement = "",
         x = corpus)

  
  # Retire les URLs (http, https, ftp)
  corpus <-
    gsub(pattern = "(f|ht)(tp)(s?)(://)(\\S*)",
         replacement = "",
         x = corpus)
  
  
  # Retire les retweets
  corpus <- gsub(pattern = "^RT :",
                 replacement = "",
                 x = corpus)
  # Retire l'unicode
  corpus <-
    gsub(pattern = "[^[:alpha:][:digit:] \t\n\r\f\v'-]",
         replacement = " ",
         x = corpus)
  
  # Retire les apostrphes, mots taille 1 et la ponctuation
  corpus <- gsub("[[:punct:]]", " ", corpus)
  corpus <- gsub("\\d", "", corpus)
  corpus <- gsub("d'|l'|j'|t'|n'|s'|c'|qu'", "", corpus)
  corpus <- gsub('\\b\\w{1,2}\\s', '', corpus)
  
  # Enlever les espaces en trop
  corpus <-
    str_replace(gsub(
      pattern = "\\s+",
      replacement = " ",
      x = str_trim(corpus)
    ), "B", "b")
  
  # Mise en minuscule
  corpus <-
    tolower(corpus)

    return(corpus)
}

lemmatisation <- function(corpus){
  
  # Suppression des signes de ponctuations
  x <- gsub("[[:punct:]]", " ", corpus)
  
  # On transforme les commentaires en un seul vecteur character pour faire un seul appel à TreeTagger
  x <- paste(x, collapse = ". ¤ ")
  x <- gsub("\\d", "", x)
  x <- gsub("d'|l'|j'|t'|n'|s'|c'|qu'", "", x)
  x <- gsub('\\b\\w{1,2}\\s', '', x)
  
  # Appel a TreeTagger
  suppressMessages({
    tagged.results <- treetag(
      file = x,
      treetagger = "manual",
      format = "obj",
      TT.tknz = FALSE ,
      lang = "fr",
      TT.options = list(path = "TreeTagger", preset = "fr")
    )
  })
  # Tableau des resultat
  res <- tagged.results@TT.res
  # Classes de mots que l'on souhaite supprimer
  wclass2suppr <-
    c(
      "preposition",
      "article",
      "punctuation",
      "conjunction",
      "fullstop",
      "pronoun",
      "http",
      "www",
      "@card@"
    )
  res <- res[!res$wclass %in% wclass2suppr, ]
  # traitements supp et remise en forme
  res <-
    res[!(res$lttr <= 2 & res$token != "¤" & res$wclass != "verb"), ]
  res[res$lemma == "<unknown>", 3] <-
    res[res$lemma == "<unknown>", 1]
  lemma <- gsub("^(.+?)\\|.+$", "\\1", res$lemma)
  lemma <- lemma[(nchar(lemma) > 2 | lemma == "¤")]
  vecteur <- paste(lemma, collapse = " ")
  vecteur <- as.list(unlist(strsplit(vecteur, split = "¤")))
  result <- gsub("NA", "", vecteur, fixed = TRUE)
  result <- gsub("^\\s*|\\s*$", "", result)
  return(result)
}