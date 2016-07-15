# rekreirati SPSS-ov FREQUENCIES

frre <- function(x,...){
  UseMethod("frre")
} 

# labelled ----
frre.labelled <- function (x, lab.duljina = 40, ime="", 
                           N = TRUE, 
                           levels = "prefixed", sort_levels = "auto", drop = TRUE, 
                           kablica = TRUE, digits = 2, ...) {
  
  varlab <- attributes(x)[["label"]]
  
  if (nchar(ime) > 0 ) {
    varlab <- ime
    if (lab.duljina == 40) lab.duljina <- 200
  }
  
  if (identical(varlab, attributes(x)[["labels"]])) {
    stop("vaR lab i vaL lab su isti - vjerojatno nepostojeći")
  }
  
  
  
  lejbld <- labelled::to_factor(x, levels = levels, 
                                sort_levels = sort_levels, 
                                drop_unused_labels = drop)

  levels(lejbld) <- strtrim(levels(lejbld), lab.duljina - 3)
  
  if (drop && !identical(
      levels(to_factor(x)),
      levels(to_factor(x, drop_unused_labels = TRUE))
    )) {
    no_of_dropped <- 
      length(levels(to_factor(x))) - length(levels(to_factor(x, drop_unused_labels = TRUE)))
    warning(paste(no_of_dropped, "level(s) dropped"), call. = FALSE)
    }
  
  gnjec.df <- merge.data.frame(as.data.frame(table(lejbld)),
                               as.data.frame(prop.table(table(lejbld))),
                               by = "lejbld", sort = FALSE)
  
  if (!is.null(varlab)) {
    names(gnjec.df)[1] <- strtrim(varlab, lab.duljina)
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
    gnjec.df
    }
  }


# LABELLED primjeri --------------------------------------------------------
# s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
# s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
# var_label(s2) <- "A vector to label. Must be either numeric or character."
# 
# frre(s2)
# frre(s2, lab.duljina = 60) # duže ime
# frre(s1, # custom ime ostaje dugo
#      ime = "when coercing a labelled character vector to a factor")
# frre(s1, lab.duljina = 10, # ...ostaje dugo ako se ne specifira non-default
#      ime = "when coercing a labelled character vector to a factor")
# frre(s2, N = FALSE)
# frre(s1, N = FALSE, kablica = FALSE) # ružno, ali ima svojih čari
# frre(s1, digits = 3) # digits : proslijeđen u kable, def = 2
# frre(s2,  # argument proslijeđen u kable /// ne printa se u konzoli
#      caption = "naslov koji se ne vidi, ali je valjda tu") 


frre.factor <- function (x, lab.duljina = 40, ime="", 
                         N = TRUE, drop = TRUE, kablica = TRUE, digits = 2, ...) {
  
  varlab <- attributes(x)[["label"]]
  
  if (nchar(ime) > 0) {
    varlab <- ime
    if (lab.duljina == 40) lab.duljina <- 200
  }
  
  if (drop && !identical(levels(x), levels(droplevels(x)))) {
    no_of_dropped <- 
      length(levels(x)) - length(levels(droplevels(x)))
    warning(paste(no_of_dropped, "level(s) dropped"), call. = FALSE)
  }
  
  nejm <- deparse(substitute(x))
  
  if (drop) x <- droplevels(x)
  levels(x) <- strtrim(levels(x), lab.duljina - 3)
  
  gnjec.df <- merge.data.frame(as.data.frame(table(x)),
                               as.data.frame(prop.table(table(x))),
                               by = "x", sort = FALSE)
  
  if (!is.null(varlab)) {
    names(gnjec.df)[1] <- strtrim(varlab, lab.duljina)
  } else {
    names(gnjec.df)[1] <- nejm
  }
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
    gnjec.df
  }
}

# PRIMJERI # factor ----
# s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
# s1f <- to_factor(s1)
# var_label(s1f) <- "neko izrazito dugačko i nespretno ime velike dužine"
# frre(s1f)
# 
# sx <- to_factor(scopes.2015$p38)
# frre(sx, drop = TRUE)
# 
# frre(diamonds$cut, ime = "Cut of the diamonds", N = FALSE)
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
    gnjec.df
  }
}

# PRIMJERI # numeric ----
# frre(mtcars$cyl, prosjek = TRUE)
# frre(diamonds$carat, prosjek = TRUE) # bzvz
# hist(diamonds$carat); summary(diamonds$carat) # klasika je bolja
# ====