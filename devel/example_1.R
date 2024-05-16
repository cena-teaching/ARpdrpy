# Przykład działania algorytmu apriori

# Generowanie danych 1:
N <- 100
M <- 5
X <- sample(c(0, 1), N*M, replace = TRUE, prob = c(0.3, 0.7))
dim(X) <- c(N, M)

X[, 5] <- ifelse(X[, 4]*X[, 3] == 1, 1, X[, 5])

# Generowanie danych 2:
N <- 250
M <- 10
X <- sample(c(0, 1), N*M, replace = TRUE, prob = c(0.3, 0.7))
dim(X) <- c(N, M)

X[, M] <- ifelse(X[, M-1]*X[, M-2] == 1, 1, X[, M])


# Parametry:
min_supp <- 0.5

# Frequent itemsets:
ret <- apriori(X, 0.5)
str(ret)

# Results:
do.call(rbind, lapply(ret[[1]], function(z) 
  list(paste(z[[1]], collapse = '; '), z[[2]])))

# Example 2:
N <- 250
M <- 10
X <- sample(c(0, 1), N*M, replace = TRUE, prob = c(0.3, 0.7))
dim(X) <- c(N, M)

X[, M] <- ifelse(X[, M-1]*X[, M-2] == 1, 1, X[, M])

X <- cbind(X[, 1:5], rep(c(1, 0), c(2, 248)), rep(c(1, 0), c(2, 248)),
           X[, 6:M])

# reguły asocjacyjne:
r <- rules_generator(X)
str(apriori(X))
str(r)

