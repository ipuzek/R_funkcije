library(purrr); library(dplyr)

x <- c(1,2,3,4)

as.list(x) # ovo radi lapply kad mu je input atomic vector
list(x) # A NE OVO!

mean(x)

map(x, mean)

map_dbl(x, mean)
sapply(x, mean)

map_df(x, mean) # ne dela




map(1:5, runif) # vektor 1:5 je argument runif-u...zašto?

map(1:5, rnorm)
map(1:5, rnorm, n = 500) %>% map(mean)
map(1:5, rnorm, n = 500, mean = 1) %>% map(sd)
map(1:5, rnorm, n = 500, mean = 1, sd = 1) # unused argument

-2:2 %>% map(rnorm, n = 5)
as.list(-2:2) %>% map(rnorm, n = 5)


# exercise
col_summary <- function(df, FUN) {
  
  is_numeric <- function(df) map_lgl(df, is.numeric)
  
  out <- vector("numeric", length(df[is_numeric(df)]))
  for (i in seq_along(df[is_numeric(df)])) {
    out[i] <- FUN(df[is_numeric(df)][[i]])
  }
  out
}

# test
col_summary(mtcars, mean)
mutate_at(mtcars, vars(mpg:drat), as.character) %>% 
  col_summary(mean)


map(-1:1, mean, x = 1:4, trim = 1) # ili ovo

map(list(-1:1), mean, x = 1:4, trim = 1) # na.rm warning

map(list("x", "y"), mean, x = 1:4, trim = 1, na.rm = TRUE) # no warning




map_dbl(-2:2, rnorm, n = 5) # ne šljaka jer mora vratiti 
                            # vektor iste duljine kao input
map_df(-2:2, rnorm, n = 5) # ni ovo ne šljaka

sapply(-2:2, rnorm, n = 5) # vraća matricu




# do vs map

by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 2))

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
summarise(models, rsq = summary(mod)$r.squared)

mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ disp, data = .)) %>% 
  map(summary)