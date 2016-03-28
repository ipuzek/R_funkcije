
# FUNKCIJA ZA ISCRTAVANJE VLASTITIH KRIVULJA U GGPLOTU

# generalna custom logit funkcija
# argumenti: x , model
stari.logit <- function(x, model) {invlogit(coef(model)[1] + coef(model)[2]*x)}
# in a nutshell: stat_function(fun = logit.func, args = list(model = privremeni.model))
# in a nutshell 2: uglavnom nepotrebna jer ggplot to radi sam


# generalna funkcija za iscrtavanje krivulje za fiksnu vrijednost prediktora
# argumenti: x , model, vrijednost
curve.logit <- function(x, model, vrijednost) {
  invlogit(cbind(1, x, vrijednost) %*% coef(model)) }
# in a nutshell: stat_function(fun = curve.logit, args = list(model = XXX, vrijednost = XXX))


primjer korištenja : stari.logit
## usporedba logit i probit (ggplot) vs logit (moja custom funkcija)
## vidi se i zašto je nepotrebna, ali možda dobro dođe jednom
# library(ggplot2)
# privremeni.model <- glm(vs ~ mpg, data = mtcars, family = binomial(link = "logit"))
# ggplot(mtcars, aes(x=mpg, y=vs)) + geom_point() +
#   stat_smooth(size = 2, alpha = .8, se = FALSE,
#               method="glm",
#               method.args = list(family = binomial(link = "logit"))) +
#   stat_smooth(size = 1, se = FALSE, colour = "red",
#               method="glm",
#               method.args = list(family = binomial(link = "probit"))) +
#   stat_function(fun = stari.logit, args = list(model = privremeni.model))


primjer korištenja : curve.logit
# tmp.model <- glm(vs ~ mpg + cyl, data = mtcars, family = binomial)
# ggplot(mtcars, aes(x=mpg, y=vs)) + geom_point() +
#   stat_function(colour = "red", fun = curve.logit, args = list(model = tmp.model, vrijednost = 4)) +
#   stat_function(colour = "green", fun = curve.logit, args = list(model = tmp.model, vrijednost = 6)) +
#   stat_function(colour = "blue", fun = curve.logit, args = list(model = tmp.model, vrijednost = 8))


# privremeni.model <- glm(vs ~ mpg, data = mtcars, family = binomial(link = "logit"))
# ggplot(mtcars, aes(x=mpg, y=vs)) + geom_point() +
#   stat_smooth(aes(colour = as.factor(cyl)), alpha = .8, se = FALSE,
#               method="glm",
#               method.args = list(family = binomial(link = "logit")))


