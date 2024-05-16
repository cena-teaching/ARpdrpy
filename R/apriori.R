#' @name apriori
#' @title Apriori
#' @description Implementacja algorytmu Apriori dla reguł asocjacyjnych
#' @param X Zbior ze zmiennymi binarnymi
#' @param min_supp Próg odcięcia zbiorów częstych; (0, 1)
#' @return A list
#' @details TODO:...
#' @export
apriori <- function(X, min_supp = 0.5){
  # zakładamy, że struktura X jest nastepujaca:
  # każda kolumna oznacza jeden obiekt
  # kazdy wiersz jedna tranzakcję
  # C <- list( list(c(1, 5), iloczyn_skalarny) )
  # zbior itemsets
  C <- lapply(1:ncol(X), function(n, X) list(n, X[, n]), X) 
  sapply(C, function(c, min_supp){
    mean(c[[2]]) > min_supp
  }, min_supp) -> idx
  mapowanie <- which(idx) # zapamietujemy orginalne numery
  C <- C[idx]
  N <- length(C)
  # zostawiamy tylko czeste itemsety 1 elementowe
  Y <- vector(mode = 'list', length = N)
  Y[[1]] <- C
  iter <- 1
  while (TRUE) {
    print(iter)
    Y[[iter+1]] <- list()
    # konstruujemy nowych kadydatow na podzbiory czeste
    for( i in 1:length(Y[[iter]]) ){
      curr_set <- Y[[iter]][[i]]
      for( j in (max(curr_set[[1]])+1):N){
        if (j > N) next
         dot <- curr_set[[2]] * C[[j]][[2]]
         if( mean(dot) > min_supp ){
            Y[[iter+1]][[length(Y[[iter+1]])+1]] <- list(
              unique(c(curr_set[[1]], C[[j]][[1]])),
              dot 
            )
         }
      }
    }
    if (is.null(Y[[iter+1]])) break
    iter <- iter + 1
  }
  Y
}
