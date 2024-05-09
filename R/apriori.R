#' @name apriori
#' @title Apriori
#' @description Implementacja algorytmu Apriori dla reguł asocjacyjnych
#' @param X Zbior ze zmiennymi binarnymi
#' @param min_supp Próg odcięcia zbiorów częstych
#' @return A list
#' @details TODO:...
#' @export
apriori <- function(X, min_supp = 0.5){
  # zakładamy, że struktura X jest nastepujaca:
  # każda kolumna oznacza jeden obiekt
  # kazdy wiersz jedna tranzakcję
  C <- as.list(colnames(X))
  FI <- list() # narazie pusta
  while (TRUE) {
     sapply(C, function(c, X, min_supp){
       colMeans(X[, c]) > min_supp
     }, X, min_supp) -> idx
    # zapamietujemy czeste podzbiory
    FI <- c(FI, C[idx]) # rozszerzamy :(
    # konstruujemy nowych kadydatow na podzbiory czeste
    # (i1, i2, i3), (i1, i2, i3, i5) -> roznia sie tylko jednym
    #C <- # TODO:...
    invisible(NULL)
  }
}