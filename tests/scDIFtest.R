library(mirt)
library(strucchange)
library(scDIFtest)

### data and model
dat <- expand.table(LSAT7)
nObs <- dim(dat)[1]
mod <- mirt(dat, 2, itemtype = "3PL", constr = list(c(2, 1), c(18, 4)))

logical <- sample(size= nObs, c(TRUE, FALSE), replace = TRUE)
metric <- rnorm(nObs)
ordered <- ordered(sample(1:5, size = nObs, replace = TRUE))
factor <- factor(sample(1:2, size = nObs, replace = TRUE))

test <- scDIFtest(mod, order_by = metric)
test
dim(test$gefp$process)

scDIFtest(mod, order_by = logical)
scDIFtest(mod, order_by = ordered)
scDIFtest(mod, order_by = factor)

scDIFtest(mod, order_by = metric, functional = "maxmosum")
scDIFtest(mod, order_by = metric, functional = "suplm")



print(test, item_selection = c(2, 5))
print(test, item_selection = c(2, 5), digits = 5)
print(test, digits = 3)

# plot(test, item_selection = c(3:5))



