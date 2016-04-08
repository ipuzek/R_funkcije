# rekreirati SPSS-ove crosstable ###

frre_by <- function(redak, stupac, useNA = c("no", "ifany", "always"),
                    digits = 2, N = FALSE, vrati = "kable", ...) {
  
  if (is.factor(redak) & is.factor(stupac)) {
    if (!identical(levels(redak), 
                   levels(droplevels(redak)))) warning("redak levels dropped", call. = FALSE)
    if (!identical(levels(stupac), 
                   levels(droplevels(stupac)))) warning("stupac levels dropped", call. = FALSE)
    redak <- droplevels(redak); stupac <- droplevels(stupac)
  }
  
  t2 <- table(redak, stupac, useNA = useNA) # osnovna tablica
  t2.marg <- addmargins(t2, margin = 2)
  t2.prop <- prop.table(t2.marg, margin = 2)  # tu je sve potrebno...
  
  t2.df <- as.data.frame.matrix(t2.prop)      # ...mali housekeeping...
  rownames(t2.df)[is.na(rownames(t2.df))] <- "NA" 
  
  # dulj <- seq(1, length(t2.df) - 1)      # ...dalje je rearrange
  # t2.df.arr <- data.frame(Uzorak = t2.df$Sum,
  #                         t2.df[dulj] )
  
  # N
  if (N) {
    tdim <- addmargins(t2)
    if (any(is.na(rownames(tdim)))) {
      print(tdim[c(nrow(tdim), nrow(tdim) - 1),]) } else {
        print(tdim[nrow(tdim),])
      }
  }
  
  # što vraća?
  if (vrati == "long") {
    t2.long <- t2.df
    # t2.long <- t2.df.arr
    t2.long$kategorije <- factor(rownames(t2.long), levels = rownames(t2.long))
    rownames(t2.long) <- NULL
    gather_vars <- names(t2.long)[-length(names(t2.long))]
    tplot.long <- tidyr::gather_(t2.long, "legenda", "udio", gather_vars, factor_key = TRUE)
    return(tplot.long)
  }
  
  if (vrati == "df") {
    t2.df$kategorije <- factor(rownames(t2.df), levels = rownames(t2.df))
    rownames(t2.df) <- NULL
    return(t2.df)
  }
  
  if (vrati == "kable") {
    return(knitr::kable(t2.df, digits = digits, ...))
  }
}


# # testiranje / primjeri / mtcars -----------------------------
# frre_by(mtcars$cyl, mtcars$vs, N = TRUE)
# frre_by(mtcars$cyl, mtcars$vs, vrati = "df")
# frre_by(mtcars$cyl, mtcars$vs, vrati = "long")
# 
# # missinzi u recima
# mtcars$cylNA <- mtcars$cyl; mtcars$cylNA[mtcars$cylNA == 8] <- NA
# frre_by(mtcars$cylNA, mtcars$am) %>% kable
# frre_by(mtcars$cylNA, mtcars$am, N = TRUE) # default bez missinga
# frre_by(mtcars$cylNA, mtcars$am, useNA = "ifany", N = TRUE) # miss
# # missinzi u stupcima
# frre_by(mtcars$am, mtcars$cylNA, useNA = "ifany") %>% kable
# frre_by(mtcars$am, mtcars$cylNA, useNA = "ifany", N = TRUE)
# ====
# 
# # testiranje / primjeri / jezgra -----------------------------
# library(dplyr); library(labelled)
# jez <- jezgra %>% transmute(
#   gen.pov = factor(vrij6, labels = c("Vjerovati", "Oprez", "NZ", "BO")),
#   seljenje = factor(qol3, labels = c("Ne", "Split", "Dalmacija", "Hrvatska", "Izvan Hrvatske", "NZBO")),
#   spol = to_factor(dmg1),
#   zad.kvart = qol7 )
# 
# frre_by(jez$gen.pov, jez$spol, N = TRUE)
# frre_by(jez$seljenje, jez$gen.pov, digits = 3, N = TRUE)
# 
# # makni sve NA-ove iz prošle verzije # missing redak i stupac
# jez$gen.povNA <- jez$gen.pov
# jez$gen.povNA[jez$gen.povNA == "NZ"] <- NA ; jez$gen.povNA[jez$gen.povNA == "BO"] <- NA
# jez$seljenjeNA <- jez$seljenje
# jez$seljenjeNA[jez$seljenjeNA == "NZBO"] <- NA
# frre_by(jez$seljenjeNA, jez$gen.povNA, digits = 3)
# # 
# # elegantni graf uz pomoć opcije long
# frre_by(jez$seljenjeNA, jez$gen.povNA, vrati = "long") %>%
#   filter(legenda != "Sum") %>%
#   ggplot(aes(x = kategorije, y = udio, fill = legenda)) +
#   geom_bar(stat = "identity", position = "fill") +
#   geom_hline(aes(yintercept = .5), colour = "grey")
# # ====

# bar - nekad opcija, zasad ne!
# if (bar) {
#   t2.plot <- t2.df
#   # t2.plot <- t2.df.arr
#   t2.plot$kategorije <- factor(rownames(t2.plot), levels = rownames(t2.plot))
#   rownames(t2.plot) <- NULL
#   gather_vars <- names(t2.plot)[-length(names(t2.plot))]
#   t2.plot.long <- gather_(t2.plot, "legenda", "udio", gather_vars, factor_key = TRUE)
#   ggplot(t2.plot.long, aes(x = kategorije, y = udio, fill = legenda)) +
#     geom_bar(stat = "identity", position = "dodge")
# }