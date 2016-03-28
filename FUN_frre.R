# frre - method fensijana totalna

frre <- function(x,...){
  UseMethod("frre")
} 

# labelled ----
frre.labelled <- function (x, varLabDuljina = 40, valLabDuljina = 35, ime="", N = FALSE, kablica = TRUE, ...) {
  varlab <- attributes(x)[["label"]]
  if (nchar(ime) > 0) {
    varlab <- ime
    if (varLabDuljina == 40) varLabDuljina <- 200
    }
  if (identical(varlab, attributes(x)[["labels"]]))
    stop("vaR lab i vaL lab su isti - vjerojatno nepostojeći")
  lejbld <- labelled::as_factor(x, levels = "prefixed")
  levels(lejbld) <- strtrim(levels(lejbld), valLabDuljina)
  gnjec.df <- merge.data.frame(as.data.frame(table(lejbld)),
                               as.data.frame(prop.table(table(lejbld))),
                               by = "lejbld")
  if (!is.null(varlab)) {
    names(gnjec.df)[1] <- strtrim(varlab, varLabDuljina)
    } else names(gnjec.df)[1] <- deparse(substitute(x))
  names(gnjec.df)[2] <- "Counts"
  names(gnjec.df)[3] <- "Percents"
  if (N) {
    print(paste("valid =", sum(!is.na(x)),
              " missing =", sum(is.na(x))))
    }
  if (kablica) {
    knitr::kable(gnjec.df, digits = 2, ...)
    } else {
    gnjec.df$Percents <- round(gnjec.df$Percents * 100)
    return(gnjec.df) 
    }
  }

# PRIMJERI
s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
label(s2) <- "A vector to label. Must be either numeric or character."

frre(s2)
frre(s2, var = 60) # duže ime
frre(s2, # custom ime ostaje dugo
     ime = "when coercing a labelled character vector to a factor")
frre(s2, var = 50, # ...ostaje dugo ako se ne specifira non-default
     ime = "when coercing a labelled character vector to a factor") 
frre(jezgra$vrij6, N = TRUE)
frre(jezgra$vrij6, kablica = FALSE) # ružno, ali ima svojih čari
frre(jezgra$vrij6, digits = 3) # treba implementirati
                               # defaultni argument koji se može mijenjati u kable
frre(jezgra$vrij6, 
     caption = "naslov koji se ne vidi, ali je valjda tu") # argument proslijeđen u kable





# factor ----
frre.factor <- function (x, varLabDuljina = 40, valLabDuljina = 35) {
  levels(x) <- strtrim(levels(x), valLabDuljina)
  gnjec.df <- merge.data.frame(as.data.frame(table(x)),
                               as.data.frame(prop.table(table(x))),
                               by = "x")
  if (!is.null(attr(x, "label"))) names(gnjec.df)[1] <- strtrim(attr(x, "label"), varLabDuljina)
  names(gnjec.df)[2] <- "Counts"
  names(gnjec.df)[3] <- "Percents"
  gnjec.df$Percents <- round(gnjec.df$Percents * 100)
  return(gnjec.df) }

# numeric ----
frre.numeric <- function (x, vaRlabDuljina = 40, vaLlabDuljina = 35) {
  njumerik <- labelled::as_factor(x, levels = "prefixed")
  levels(njumerik) <- strtrim(levels(njumerik), vaLlabDuljina)
  gnjec.df <- merge.data.frame(as.data.frame(table(njumerik)),
                               as.data.frame(prop.table(table(njumerik))),
                               by = "njumerik")
  if (!is.null(attr(x, "label"))) names(gnjec.df)[1] <- strtrim(attr(x, "label"), varLabDuljina)
  names(gnjec.df)[2] <- "Counts"
  names(gnjec.df)[3] <- "Percents"
  gnjec.df$Percents <- round(gnjec.df$Percents * 100)
  return(gnjec.df) }


# TODO ### ubaci %>% kable ###
kable(frre(jezgra$vrij6), caption = label(jezgra$vrij6)) %>% cat(sep = "\n")

str(jezgra$vrij6)
frre(jezgra$dmg3) # OK

jezgra$obraz <- recode(jezgra$dmg3, " 1:3 = 1; 4:5 = 2; 6:9 = 3; 98 = NA")
jezgra$obraz %<>% factor(labels = c("Osnovna", "Srednja", "Visoka"))
frre(jezgra$obraz) # OK

rnorm(nrow(jezgra))
??kable

length(jezgra$vrij6)
nrow(jezgra)

frre(jezgra$vrij6) # labelled = OK

frre(jezgra$obraz) # faktor = OK; faktor s labelom još i bolji
attr(jezgra$obraz, "label") <- "obrazovanje ispitanika" ; frre(jezgra$obraz)

jezgra$njum <- c(1:455)
frre(jezgra$njum) # NE RADI NJUM

attr(jezgra$vrij6, "label") <- "vrij6"
attributes(jezgra$vrij6) <- NULL

summary(jezgra$vrij6)

frre(jezgra$obraz) # tweakati da ne pada kad je label = 0
?kable
attr(jezgra$vrij6, "label")
??"strip"
