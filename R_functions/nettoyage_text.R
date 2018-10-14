nettoyage_text <- function(text) {
  corpus <-   enc2utf8(text)
  
  # Retire les emojis
  corpus <-
    str_replace_all(string = corpus,
                    pattern = "[^[:graph:]]",
                    replacement = " ")
  
  # Retire les accents
  corpus <-
    iconv(x = corpus,
          from = "UTF-8",
          to = "ASCII//TRANSLIT")
  
  # Séparation des mots collés par Majuscule (VenezPartager)
  corpus <-
    gsub('(#[[:upper:]])', ' \\1',
         x = corpus)
  
  # Mise en minuscule
  corpus <-
    tolower(corpus)
  
  # Retire les URLs (http, https, ftp)
  corpus <-
    gsub(pattern = "(f|ht)(tp)(s?)(://)(\\S*)",
         replacement = "",
         x = corpus)
  
  # Retire les caractères spéciaux
  corpus <-
    gsub(pattern = "[^0-9A-Za-z///' ]",
         replacement = "",
         x = corpus)
  
  # Enlever les espaces en trop
  corpus <-
    str_replace(gsub(
      pattern = "\\s+",
      replacement = " ",
      x = str_trim(corpus)
    ), "B", "b")
  return(corpus)
}