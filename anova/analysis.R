dat <- read.csv('anova.csv')
Fvalue <- matrix(rep(0,49), 7, 7)
significance <- matrix(rep(0,49), 7, 7)

for (row in 1:7) {
  for (col in 1:7) {
    fit <- aov(Y[,(row-1)*7+col]~ dat[,50])
    Fvalue[row, col] <- summary(fit)[[1]][["F value"]][1]
    significance[row, col] <- summary(fit)[[1]][["Pr(>F)"]][1] < 0.01
  }
}
write(Fvalue, file = "fvalue.txt", ncolumns = 7, sep = " ")
write(significance, file = "significance.txt", ncolumns = 7, sep = " ")


