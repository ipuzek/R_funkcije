# rekreirati SPSS ###
library(knitr)
library(ggplot2)
library(tidyr)


# nova superkul TODODO! ----
frre_by <- function(redak, stupac, N = FALSE, df = FALSE, digits = 2, bar = FALSE) {
  t1 <- table(redak, stupac) # osnovna tablica
  t1.prop <- prop.table(t1, margin = 2) # postoci stupca 
  t1.prop <- round(t1.prop, digits = digits)
  t2 <- table(redak,
              rep(1, times = length(redak)))
  t2.prop <- prop.table(t2, margin = 2)
  t2.prop <- round(t2.prop, digits = digits)
  tt <- cbind(t2.prop, t1.prop)
  dimnames(tt)[[2]][1] <- "Uzorak"
  tt.df <- as.data.frame.matrix(tt)
  knitr::kable(tt.df)
}
# ----


# podaci ----
jez <- jezgra %>% transmute(
  gen.pov = factor(vrij6, labels = c("Vjerovati", "Oprez", "NZ", "BO")),
  seljenje = factor(qol3, labels = c("Ne", "Split", "Dalmacija", "Hrvatska", "Izvan Hrvatske", "NZBO")),
  spol = as_factor(dmg1),
  zad.kvart = qol7 )
# ----

# prototip ----
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

df <- TRUE
# return tidy DF  # radi ali testiraj
if (df) {
  tt.tidy <- tt.df
  tt.tidy$Kategorije <- factor(rownames(tt.tidy), levels = rownames(tt.tidy))
  rownames(tt.tidy) <- NULL
  return(tt.tidy)
  }

if (N) {} # implementiraj!!!

bar <- TRUE # debugiraj
if (bar) {
  tt.df$Kategorije <- factor(rownames(tt.df), levels = rownames(tt.df))
  rownames(tt.df) <- NULL
  gather_vars <- names(tt.df)[-length(names(tt.df))]
  tt.df.gather <- gather_(tt.df, "legenda", "udio", gather_vars, factor_key = TRUE)
  ggplot(aes(x = Kategorije, y = udio, fill = legenda)) +
    geom_bar(stat = "identity", position = "dodge")
  }
# ----

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




