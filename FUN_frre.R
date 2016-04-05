# frre - dispatch method fensijana totalna

frre <- function(x,...){
  UseMethod("frre")
} 

# labelled ----
frre.labelled <- function (x, varLabDuljina = 40, valLabDuljina = 35, ime="", 
                           N = FALSE, kablica = TRUE, digits = 2, ...) {
  varlab <- attributes(x)[["label"]]
  if (nchar(ime) > 0) {
    varlab <- ime
    if (varLabDuljina == 40) varLabDuljina <- 200
    }
  if (identical(varlab, attributes(x)[["labels"]]))
    stop("vaR lab i vaL lab su isti - vjerojatno nepostojeći")
  lejbld <- labelled::as_factor(x, levels = "prefixed", sort_levels = "values")
  levels(lejbld) <- strtrim(levels(lejbld), valLabDuljina)
  gnjec.df <- merge.data.frame(as.data.frame(table(lejbld)),
                               as.data.frame(prop.table(table(lejbld))),
                               by = "lejbld", sort = FALSE)
  if (!is.null(varlab)) {
    names(gnjec.df)[1] <- strtrim(varlab, varLabDuljina)
    } else names(gnjec.df)[1] <- deparse(substitute(x))
  names(gnjec.df)[2] <- "Counts"
  names(gnjec.df)[3] <- "Percents"
  if (N) {
    cat("valid =", sum(!is.na(x)),
              " missing =", sum(is.na(x)))
    }
  if (kablica) {
    knitr::kable(gnjec.df, digits = digits, ...)
    } else {
    gnjec.df$Percents <- round(gnjec.df$Percents * 100)
    return(gnjec.df) 
    }
  }

# PRIMJERI # labelled ----
# s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
# s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
# label(s2) <- "A vector to label. Must be either numeric or character."
# 
# frre(s1)
# frre(s2, var = 60) # duže ime
# frre(s2, # custom ime ostaje dugo
#      ime = "when coercing a labelled character vector to a factor")
# frre(s1, var = 10, # ...ostaje dugo ako se ne specifira non-default
#      ime = "when coercing a labelled character vector to a factor") 
# frre(s1, N = TRUE)
# frre(s2, kablica = FALSE) # ružno, ali ima svojih čari
# frre(jezgra$vrij6, digits = 3) # TODO treba implementirati
#                                # defaultni argument koji se može mijenjati u kable
# frre(jezgra$vrij6, 
#      caption = "naslov koji se ne vidi, ali je valjda tu") # argument proslijeđen u kable
# ====

# factor v 2 ----
frre.factor <- function (x, varLabDuljina = 40, valLabDuljina = 35, 
                         ime="", N = FALSE, kablica = TRUE, ...) {
  varlab <- attributes(x)[["label"]]
  if (nchar(ime) > 0) {
    varlab <- ime
    if (varLabDuljina == 40) varLabDuljina <- 200
  }
  
  nejm <- deparse(substitute(x))
  
  if (!identical(levels(x), 
                 levels(droplevels(x)))) warning("levels dropped", call. = FALSE)
  
  levels(x) <- strtrim(levels(x), valLabDuljina)
  x <- droplevels(x)
  
  gnjec.df <- merge.data.frame(as.data.frame(table(x)),
                               as.data.frame(prop.table(table(x))),
                               by = "x", sort = FALSE)
  if (!is.null(varlab)) {
    names(gnjec.df)[1] <- strtrim(varlab, varLabDuljina)
  } else names(gnjec.df)[1] <- nejm
  names(gnjec.df)[2] <- "Counts"
  names(gnjec.df)[3] <- "Percents"
  if (N) {
    cat("valid =", sum(!is.na(x)),
        " missing =", sum(is.na(x)))
  }
  if (kablica) {
    knitr::kable(gnjec.df, digits = 2, ...)
  } else {
    gnjec.df$Percents <- round(gnjec.df$Percents * 100)
    return(gnjec.df) 
  }
}

# PRIMJERI # factor ----
# s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
# s1f <- labelled::as_factor(s1)
# frre(s1); frre(s1f)
# #
# frre(diamonds$cut, ime = "Cut of the diamonds", N = TRUE)
# #
# jezgra$obraz <- recode(jezgra$dmg3, " 1:3 = 1; 4:5 = 2; 6:9 = 3; 98 = NA")
# jezgra$obraz %<>% factor(labels = c("Osnovna", "Srednja", "Visoka")) 
# # NE OVAKO label(jezgra$obraz) <- label(jezgra$dmg3) ### NI OVAKO label(jezgra$obraz) <- "obrazovanje ispitanika"
# attributes(jezgra$obraz)[["label"]] <- attributes(jezgra$dmg3)[["label"]] # OK
# frre(jezgra$obraz, var = 30)
# ====

# numeric ----
frre.numeric <- function (x, varLabDuljina = 40, ime="", 
                          prosjekN = FALSE, kablica = TRUE, ...) {
  varlab <- attributes(x)[["label"]]
  if (nchar(ime) > 0) {
    varlab <- ime
    if (varLabDuljina == 40) varLabDuljina <- 200
  }
  nejm <- deparse(substitute(x))
  gnjec.df <- merge.data.frame(as.data.frame(table(x)),
                               as.data.frame(prop.table(table(x))),
                               by = "x", sort = FALSE)
  if (!is.null(varlab)) {
    names(gnjec.df)[1] <- strtrim(varlab, varLabDuljina)
  } else names(gnjec.df)[1] <- nejm
  names(gnjec.df)[2] <- "Counts"
  names(gnjec.df)[3] <- "Percents"
  
  if (prosjekN) {
    cat("valid =", sum(!is.na(x)),
                " missing =", sum(is.na(x)),
                "mean =", round(mean(x, na.rm = TRUE), digits = 2))
  }
  if (kablica) {
    knitr::kable(gnjec.df, digits = 2, ...)
  } else {
    gnjec.df$Percents <- round(gnjec.df$Percents * 100)
    return(gnjec.df) 
  }
}

# PRIMJERI # numeric ----
# frre(mtcars$cyl, prosjek = TRUE, moj_summary = TRUE)
# frre(diamonds$carat, prosjek = TRUE) # bzvz
# hist(diamonds$carat); summary(diamonds$carat) # klasika je bolja
# ====