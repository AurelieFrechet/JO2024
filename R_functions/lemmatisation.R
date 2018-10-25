# Attention : nécessite le package koRpus + l'installation de TreeTager
# TreeTagger : http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# Lien utile : https://tribulationsdumonde.wordpress.com/2016/02/22/installer-treetagger-correctement-sous-windows/
lemmatisation <- function(corpus) {
  # On transforme les commentaires en un seul vecteur character pour faire un seul appel à TreeTagger
  x <- paste(corpus, collapse = ". ¤ ")
  
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
      "www"
    )
  res <- res[!res$wclass %in% wclass2suppr,]
  
  # traitements supp et remise en forme
  res <- res[!(res$lttr <= 1
               & res$token != "¤" 
               & res$wclass != "verb"),]
  res[res$lemma == "<unknown>", 3] <- res[res$lemma == "<unknown>", 1]
  res[res$lemma == "@card@", 3]    <- res[res$lemma == "@card@", 1]
  res[res$lemma == "Ãªtre", 3] <- "être"
  # Noms propres
  res[res$token == "paris", 3]     <- "paris"
  
  lemma <- gsub("^(.+?)\\|.+$", "\\1", res$lemma)
  lemma <- lemma[(nchar(lemma) > 1 | lemma == "¤")]
  
  vecteur <- paste(lemma, collapse = " ")
  vecteur <- as.list(unlist(strsplit(vecteur, split = "¤")))
  
  result <- gsub("NA", "", vecteur, fixed = TRUE)
  result <- gsub("^\\s*|\\s*$", "", result)
  return(result)
}