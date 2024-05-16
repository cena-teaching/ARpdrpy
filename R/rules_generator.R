#' @name rules_generator
#' @title Associacion Rules Generator
#' @description Implementacja generatora reguł asocjacyjnych
#' @param X Zbior ze zmiennymi binarnymi
#' @param min_supp Próg odcięcia zbiorów częstych; (0, 1)
#' @param min_conf Próg odcięcia dla regul (0, 1)
#' @return A list
#' @details TODO:...
#' @export
rules_generator <- function(X, min_supp = 0.5, min_conf = 0.5){
  Y <- apriori(X, min_supp = min_supp)
  freq_itemses <- Y[[1]]
  codes <- Y[[2]]
  N <- length(freq_itemses)
  rules <- list()
  for(i in 1:N){
    itemset <- freq_itemses[[i]][[1]]
    supp <- freq_itemses[[i]][[2]]
    n_items <- length(itemset)
    if(n_items < 2) next
    
    for(k in 1:n_items){
      name <- paste(itemset[-k], collapse = ' ')
      supp_anc <- freq_itemses[[name]][[2]]
      conf <- supp/supp_anc
      if( conf >= min_conf){
        rules[[length(rules)+1]] <- list(
          poprzednik = itemset[-k],
          nastepnik = itemset[k],
          confidence = conf)
      }
    }
  }
  rules
}
