# robusna funkcija med_impute #
# prima ordered.factor ili character, obavezno u long (Untable'd) formatu
# kategorije moraju biti oblika BROJnekiznakBROJ
# širina je definirana navedenim kategorijama

library(stringr)

med_impute <- function(x,...){
  UseMethod("med_impute")
} 

med_impute.character <- function(x) {
  
  nejmz.brojke <- str_extract_all(unique(x), "[0-9]+")
  
  lower <- lapply(nejmz.brojke, function(x) x[1]) %>% unlist %>% as.numeric()
  upper <- lapply(nejmz.brojke, function(x) x[2]) %>% unlist %>% as.numeric()
  
  if (is.unsorted(lower) || (is.unsorted(upper))) {
    print(lower)
    print(upper)
    stop("intervali nisu u rastućem nizu")
  }
  
  print(lower) # ovo zakomentiraj nakon malo dužeg testiranja
  print(upper)
  
  add.to.lower <- unique(upper - lower) / 2
  
  if (identical(length(add.to.lower), 1L)) {
    sredina <- lower + add.to.lower
  } else stop("intervali nisu jednaki")
  
  for (i in (seq_along(1:length(unique(x))))) {
    x[x == unique(x)[i]] <- sredina[i]
  }
  as.numeric(x)
}

# # primjer:
# x <- readxl::read_excel("/IvanP/R/testovi_related/HR_medijani.xlsx", sheet = "primjer")
# x_impute <- DescTools::Untable(data.frame(x)) %>% med_impute
# table(x_impute)
# psych::interp.median(x_impute, w = 5)

  
# testiraj #
# med.HR.greska <- readxl::read_excel("/IvanP/R/testovi_related/HR_medijani.xlsx",
#                              sheet = "greska.1")
# x <- Untable(data.frame(med.HR.greska)); med_impute(x)
# med.HR.greska <- readxl::read_excel("/IvanP/R/testovi_related/HR_medijani.xlsx",
#                                     sheet = "greska.2")
# x <- Untable(data.frame(med.HR.greska)); med_impute(x)


med_impute.ordered <- function(x) {
  
  nejmz.brojke <- str_extract_all(levels(x), "[0-9]+")
  
  lower <- lapply(nejmz.brojke, function(x) x[1]) %>% unlist %>% as.numeric()
  upper <- lapply(nejmz.brojke, function(x) x[2]) %>% unlist %>% as.numeric()
  
  if (is.unsorted(lower) || (is.unsorted(upper))) {
    print(lower)
    print(upper)
    stop("intervali nisu u rastućem nizu")
  }
  
  print(lower) # ovo zakomentiraj nakon malo dužeg testiranja
  print(upper)
  
  add.to.lower <- unique(upper - lower) / 2
  
  if (identical(length(add.to.lower), 1L)) {
    sredina <- lower + add.to.lower
  } else stop("intervali nisu jednaki")
  
  for (i in (seq_along(1:length(levels(x))))) {
    levels(x)[i] <- sredina[i]
  }
  as.numeric(as.character(x))
}

# # primjer:
# x <- readxl::read_excel("/IvanP/R/testovi_related/HR_medijani.xlsx", sheet = "primjer")
# x_impute <- DescTools::Untable(data.frame(x)) %>% as.ordered %>% med_impute
# table(x_impute)
# psych::interp.median(x_impute, w = 5)

