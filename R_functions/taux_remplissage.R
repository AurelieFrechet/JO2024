
#' taux_remplissage
#' Renvoie le nombre de lignes saisies et le taux associé
#' pour une liste de variables issues d'un même dataframe
#'
#' @param df : data.frame à analyser
#' @param variables : liste des variables concernées
#'
#' @return une dataframe indicée par le nom des variables
#' nb_plein : nombre de lignes saisie
#' taux : taux associé
#'
#' @examples
#' #' # Don't run
#' taux_remplissage(iris, colnames(iris))
#' #              nb_plein taux
#' #  Sepal.Length      150    100,00
#' #  Sepal.Width       150    100,00
#' #  Petal.Length      150    100,00
#' #  Petal.Width       150    100,00
#' #  Species           150    100,00
taux_remplissage <- function(df, variables) {
  var_absentes_df <- sapply(
    X = variables,
    FUN = function(x)
      ! (x %in% colnames(df))
  )
  var_presentes_df <- sapply(
    X = variables,
    FUN = function(x)
      (x %in% colnames(df))
  )
  nb_plein <- colSums(!est_vide(as.matrix(df[, var_presentes_df])))
  
  result <- data.frame(nb_plein,
                       taux = round(nb_plein / nrow(df) * 100, 2))
  
  if (sum(var_absentes_df) > 0) {
    warning(paste(variables[var_absentes_df],
                  "are not in dataframe"))
  }
  return(result[order(result$nb_plein, decreasing = T),])
}