# robusna funkcija med_impute #

med_impute <- function(x, y) {
  
  # trebam y vektor s razrednim sredinama izračunatim iz kategorija
  # kategorije moraju biti oblika BROJnekiznakBROJ
  
  
  
  for (i in (seq_along(1:length(unique(x))))) {
    x[x == unique(x)[i]] <- y[i]
  }
  as.numeric(x)
}



