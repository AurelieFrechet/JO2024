#' Indique si un objet est vide
#' @title est vide
#' @param x : vecteur de n'importe quel type (character, numeric)
#'
#' @return TRUE si x est "vide" : na ou égal à "VIDE", "", " "
#'
#' @export
#'
#' @examples
#' x = c("chat", "VIDE", "", " ", "perli")
#' est_vide(x)
#' # [1] FALSE  TRUE  TRUE  TRUE FALSE
#'
#' y = NA
#' est_vide(y)
#' # [1] TRUE
#'
est_vide <- function(x) {
  x %in% c("VIDE", "", " ") |  is.na(x) == TRUE
}




