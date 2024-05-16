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
  # kazdy wiersz jedna tranzakcj
  # zbior itemsets poczatkowy
  C <- lapply(1:ncol(X), function(n, X) list(n, X[, n]), X) 
  # szukamy odpowiednio czestych itemset 1-elementowych
  sapply(C, function(c, min_supp){
    mean(c[[2]]) > min_supp
  }, min_supp) -> idx
  mapowanie <- which(idx) # zapamietujemy orginalne numery
  # zostawiamy tylko czeste itemsety 1 elementowe
  C <- C[idx]
  N <- length(C)
  
  Y <- vector(mode = 'list', length = N)
  Y[[1]] <- C
  iter <- 1
  while (TRUE) {
    Y[[iter+1]] <- list()
    # konstruujemy nowych kadydatow na podzbiory czeste
    if(length(Y[[iter]]) == 0) break
    for( i in 1:length(Y[[iter]]) ){
      curr_set <- Y[[iter]][[i]]
      curr_items <- curr_set[[1]]
      for( j in (max(curr_items)+1):N){
         if (j > N) break
         possible_add <- C[[j]]
         if (possible_add[[1]] %in% curr_items){
            break
         }
         dot <- curr_set[[2]] * possible_add[[2]]
         if( mean(dot) > min_supp ){
            n_itemsets <- length(Y[[iter+1]])
            Y[[iter+1]][[n_itemsets + 1]] <- list(
              c(curr_items, possible_add[[1]]),
              dot 
            )
         }
      }
    }
    if (length(Y[[iter+1]]) == 0) break
    iter <- iter + 1
  }
  # zamiast całych wektorów wystarczy srednia
  # == czyli czestosc jego wystepopwania
  frequent <- unlist(Y, recursive = FALSE)
  t <- lapply(frequent, function(x) list(x[[1]], 
                                         mean(x[[2]])))
  names(t) <- sapply(t, function(x) paste(x[[1]], collapse = ' '))
  list(t, mapowanie)
}
