nettoyage_text <- function(text) {
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
  
  # Séparation par Majuscule (VenezPartager)
  corpus <-
    gsub('(#)([A-Za-z]+?)([A-Z][a-z])', '\\2 \\3',
         x = corpus)
  
  # Retire les hashtags
  corpus <-
    gsub(pattern = "#",
         replacement = "",
         x = corpus)
  
  
  # Retire les retweets
  corpus <- gsub(pattern = "^RT :",
                 replacement = "",
                 x = corpus)

  # Retire l'unicode
  corpus <-
    gsub(pattern = "[^[:alpha:][:digit:] \t\n\r\f\v'-]",
         replacement = "",
         x = corpus)
  
  # Retire les apostrphes, mots taille 1 et la ponctuation
  corpus <- gsub("[[:punct:]]", " ", corpus)
  corpus <- gsub("d'|l'|j'|t'|n'|s'|c'|qu'", "", corpus)
  corpus <- gsub('\\b\\w{1}\\s', '', corpus)
  
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

