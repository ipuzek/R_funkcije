# rekreirati SPSS ###
library(knitr)
library(ggplot2)
library(tidyr)

# one liner
table(jez$gen.pov, jez$spol) %>% addmargins(2) %>% prop.table(2) %>% round(2)

# nova superkul TODODO! ----
frre_by <- function(redak, stupac, useNA = c("no", "ifany", "always"),
                    digits = 2,  N = FALSE, long = FALSE, bar = FALSE) {
  
  if (is.factor(redak) & is.factor(stupac)) {
    if (!identical(levels(redak), 
                 levels(droplevels(redak)))) cat("redak levels dropped")
    if (!identical(levels(stupac), 
                 levels(droplevels(stupac)))) cat("stupac levels dropped")
    redak <- droplevels(redak); stupac <- droplevels(stupac)
  }
  
  t2 <- table(redak, stupac, useNA = useNA) # osnovna tablica
  t2.marg <- addmargins(t2, margin = 2)
  t2.prop <- prop.table(t2.marg, margin = 2)  # tu je sve potrebno...
  t2.df <- as.data.frame.matrix(t2.prop)
  rownames(t2.df)[is.na(rownames(t2.df))] <- "NA" # dalje je rearrange
  
  dulj <- seq(1, length(t2.df) - 1)      # sve osim zadnjeg elementa
  t2.df.arr <- data.frame(Uzorak = t2.df$Sum,
                          t2.df[dulj]
  )
  # kable(t2.df.arr, digits = 2)

  print(kable(t2.df, digits = digits))

  # N
  if (N) {
    tdim <- addmargins(t2)
    if (any(is.na(rownames(tdim)))) {
      print(tdim[c(nrow(tdim), nrow(tdim) - 1),]) } else {
        print(tdim[nrow(tdim),])
      }
  }
  
  # vraća DF pogodan za ggplottanje
  if (long) {
    t2.long <- t2.df
    # t2.long <- t2.df.arr
    t2.long$kategorije <- factor(rownames(t2.long), levels = rownames(t2.long))
    rownames(t2.long) <- NULL
    gather_vars <- names(t2.long)[-length(names(t2.long))]
    tplot.long <- gather_(t2.long, "legenda", "udio", gather_vars, factor_key = TRUE)
    return(tplot.long)
  }
  
  # bar
  if (bar) {
    t2.plot <- t2.df
    # t2.plot <- t2.df.arr
    t2.plot$kategorije <- factor(rownames(t2.plot), levels = rownames(t2.plot))
    rownames(t2.plot) <- NULL
    gather_vars <- names(t2.plot)[-length(names(t2.plot))]
    t2.plot.long <- gather_(t2.plot, "legenda", "udio", gather_vars, factor_key = TRUE)
    ggplot(t2.plot.long, aes(x = kategorije, y = udio, fill = legenda)) +
    geom_bar(stat = "identity", position = "dodge")
  }
}


# testiranje / primjeri -----------------------------
frre_by(mtcars$cyl, mtcars$vs)
frre_by(mtcars$cyl, mtcars$am, N = TRUE, bar = TRUE)
# missinzi u recima
mtcars$cylNA <- mtcars$cyl; mtcars$cylNA[mtcars$cylNA == 8] <- NA
frre_by(mtcars$cylNA, mtcars$am, N = TRUE, bar = TRUE) # default bez missinga
frre_by(mtcars$cylNA, mtcars$am, useNA = "ifany", N = TRUE, bar = TRUE) # miss
# missinzi u stupcima
frre_by(mtcars$am, mtcars$cylNA, useNA = "ifany")
frre_by(mtcars$am, mtcars$cylNA, useNA = "ifany", N = TRUE, bar = TRUE)

# podaci ----
library(dplyr); library(labelled)
source("R/FUN_frre.R")
jez <- jezgra %>% transmute(
  gen.pov = factor(vrij6, labels = c("Vjerovati", "Oprez", "NZ", "BO")),
  seljenje = factor(qol3, labels = c("Ne", "Split", "Dalmacija", "Hrvatska", "Izvan Hrvatske", "NZBO")),
  spol = as_factor(dmg1),
  zad.kvart = qol7 )

frre(jezgra$vrij6); frre(jez$gen.pov); frre(jez$spol)
frre_by(jez$gen.pov, jez$spol, N = TRUE, bar = TRUE)
frre_by(jez$seljenje, jez$gen.pov, digits = 3, N = TRUE, bar = TRUE)

# missing stupac
jez$gen.povNA <- jez$gen.pov
jez$gen.povNA[jez$gen.povNA == "NZ"] <- NA ; jez$gen.povNA[jez$gen.povNA == "BO"] <- NA
frre(jez$gen.povNA, N = TRUE)
frre_by(jez$seljenje, jez$gen.povNA, digits = 3, N = TRUE, bar = TRUE)

# missing redak i stupac
jez$seljenjeNA <- jez$seljenje
jez$seljenjeNA[jez$seljenjeNA == "NZBO"] <- NA
frre_by(jez$seljenjeNA, jez$gen.povNA, digits = 3, N = TRUE, bar = TRUE)


# ====

# prototip ----
# prototip 2

mtcars$cylNA <- mtcars$cyl; mtcars$cylNA[mtcars$cylNA == 8] <- NA
mtcars$cyl.fac <- as.factor(mtcars$cyl)
mtcars$cyl.facNA <- mtcars$cyl.fac
mtcars$cyl.facNA[mtcars$cyl.facNA == 8] <- NA
summary(mtcars$cyl.facNA)

if (!identical(levels(redak), 
               levels(droplevels(redak)))) warning("levels dropped")
if (!identical(levels(stupac), 
               levels(droplevels(stupac)))) warning("levels dropped")
redak <- droplevels(redak); stupac <- droplevels(stupac)

t2 <- table(mtcars$cylNA, mtcars$am, useNA = "ifany") # osnovna tablica
t2.marg <- addmargins(t2, margin = 2)
t2.prop <- prop.table(t2.marg, margin = 2)  # tu je sve potrebno...
t2.df <- as.data.frame.matrix(t2.prop) 
rownames(t2.df)[is.na(rownames(t2.df))] <- "NA" # dalje je rearrange

# dulj <- seq(1, length(t2.df) - 1)      # sve osim zadnjeg elementa
# t2.df.arr <- data.frame(Uzorak = t2.df$Sum,
#                     t2.df[dulj] )
# kable(t2.df.arr, digits = 2)

kable(t2.df, digits = 2)

df <- TRUE
# vraća DF pogodan za ggplottanje # OK
if (df) {
  t2.tidy <- t2.df
  # t2.tidy <- t2.df.arr
  t2.tidy$kategorije <- factor(rownames(t2.tidy), levels = rownames(t2.tidy))
  rownames(t2.tidy) <- NULL
  return(t2.tidy)
  }

N <- TRUE
if (N) {
  tdim <- addmargins(t2)
  if (any(is.na(rownames(tdim)))) {
    tdim[c(nrow(tdim), nrow(tdim) - 1),] } else {
      print(tdim[nrow(tdim),])
    }
}

bar <- TRUE # OK
if (bar) {
  t2.plot <- t2.df
  # t2.plot <- t2.df.arr
  t2.plot$kategorije <- factor(rownames(t2.plot), levels = rownames(t2.plot))
  rownames(t2.plot) <- NULL
  gather_vars <- names(t2.plot)[-length(names(t2.plot))]
  t2.plot.long <- gather_(t2.plot, "legenda", "udio", gather_vars, factor_key = TRUE)
  ggplot(t2.plot.long, aes(x = kategorije, y = udio, fill = legenda)) +
    geom_bar(stat = "identity", position = "dodge")
  }
# ----

# prvi prototip za frre_by - obsolete ----
t1 <- table(jez$gen.pov, jez$spol) # osnovna tablica
t1.prop <- prop.table(t1, margin = 2) # postoci stupca 
t1.prop <- round(t1.prop, digits = 2)
t2 <- table(jez$gen.pov,
            rep(1, times = length(jez$gen.pov)))
t2.prop <- prop.table(t2, margin = 2)
t2.prop <- round(t2.prop, digits = 2)
tt <- cbind(t2.prop, t1.prop)
dimnames(tt)[[2]][1] <- "Uzorak"
tt.df <- as.data.frame.matrix(tt)
kable(tt.df)
# ====

# STARA dobra tblca ----
tblca <- function (redak, stupac, file = "") {
  t1 <- table(redak, stupac) # osnovna tablica
  t1.prop <- prop.table(t1, 2)
  t2 <- table(redak, # redak s ukupnim proporcijama
              rep(1, times = length(redak)))
  t2.prop <- prop.table(t2, 2)
  t1.add <- addmargins(t1) # N
  t1.add.nrow <- t1.add[nrow(t1.add),]
  prva <- cbind(t1.prop, t2.prop) # složi tablicu u 2 koraka
  druga <- rbind(N = t1.add.nrow, prva)
  druga.round <- round(druga,2) # zaokruži
  print(druga.round) # ispiši
  if (length(file) > 1) write.csv2(druga.round, file) # zapiši
}
# ----


# =======================================


# nova tablica pomoću dplyr i kable
# komplikovano - vidi primjer dolje ----
mtcars %>%
  group_by(am, gear) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
# ----
# Primjer sa SO: tablica s više nezavisnih varijabli u križanju via dplyr ----

library(tidyr)
library(dplyr)

m_mtcars <- melt(mtcars,measure.vars=c("gear","carb","cyl"))

res <- m_mtcars %>%
  group_by(am, variable, value) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

data.unt.2011 <- temp.2011.unt %>%
  gather(key = spol, value = frek, dob__00_04 : dob__85plus)

# Building on this, the desired output can be obtained using more reshaping and some string formatting

#make an 'export' variable
res$export <- with(res, sprintf("%i (%.1f%%)", n, freq*100))

#reshape again
output <- dcast(variable+value~am, value.var="export", data=res, fill="missing") #use drop=F to prevent silent missings 
#'silent missings'
output$variable <- as.character(output$variable)
#make 'empty lines' 
empties <- data.frame(variable=unique(output$variable), stringsAsFactors=F)
empties[,colnames(output)[-1]] <- ""

#bind them together
output2 <- rbind(empties,output)
output2 <- output2[order(output2$variable,output2$value),]

#optional: 'remove' variable if value present

output2$variable[output2$value!=""] <- ""
kable(output)
# END primjer SO : komplicirano ----


### detaljno komentiran postupak za funkciju # iskoristi uncomment lines
# 
# 1 # osnovna tablica proporcija stupca
t1 <- table(jezz$gen.pov, jezz$spol)
t1.prop <- prop.table(t1, 2)
t1.prop

# 2 # cijeli uzorak - postoci stupca pomoću trikova
t2 <- table(jezz$gen.pov,
            rep(1, times = length(jezz$gen.pov)))
t2.prop <- prop.table(t2, 2)

# # # margin table # besmislen za proporcije # a i inače za njubare
# margin.table(t1, 2)

### addmargins # bezbolno složi prvi redak s N-ovima
# ekvivalent ptot = N
t1.add <- addmargins(t1)
t1.add.nrow <- t1.add[nrow(t1.add),]

# složi tablicu u 2 koraka
prva <- cbind(t1.prop, t2.prop)
druga <- rbind(N = t1.add.5, prva)
# pa zaokruži
druga.r <- round(druga,2)
# i zapiši
write.csv2(druga.r, "tablicica.csv")


#### ----




