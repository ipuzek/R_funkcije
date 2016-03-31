# rekreirati SPSS ###

### funkcija

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



(t1 <- table(jezgra$gen.pov, jezgra$spol)) # osnovna tablica
(t1.prop <- prop.table(t1, 2) %>% round(2))

t2 <- table(jezgra$gen.pov,
            rep(1, times = length(jezgra$gen.pov))) %>% print
t2.prop <- prop.table(t2, 2) %>% print

tt <- cbind(t1.prop, suma = t2.prop)


t1.prop.df <- as.data.frame.matrix(tt) %>% print












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




