f1 <- factor(letters)
levels(f1) <- rev(levels(f1))

x <- c(2.1, 4.2, 3.3, 5.4)

order(x) # ludi koncept

x[order(x)]
#sort(x)

x[c(-1,1)]
x[c(-1,0)] # isto kao x[c(-1)]

y <- setNames(x, letters[1:4])
y[c("a", "d")]

#exact matching with []
z <- c(abc = 1, def = 2)
z[c("a", "d")]

### matrices

a <-matrix(1:9, nrow = 3)
colnames(a) <-c("A", "B", "C")

a[1:2, ]
a[c(TRUE, FALSE, TRUE), c("B", "A")]
a[0, -2] %>% str # simplified result because of []

###DFs

df <-data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[df$x == 2, ]
df[df$x == 2] # whooooopsss --- ne kužim

df[c(1,3), ]

df[(c("x", "z"))] # like a list
df[, (c("x", "z"))] # like a matrix

df[, "x"] # like a matrix --- simplification!
df["x"] # like a list --- NO simplification.

###exercises
xx <- 1:5; xx[NA]
xx <- 1:5; xx[NA_real_] #a bo?





