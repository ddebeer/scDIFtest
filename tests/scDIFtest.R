library(mirt)
library(strucchange)
library(scDIFtest)

### data and model
dat <- expand.table(LSAT7)
nObs <- dim(dat)[1]
mod <- mirt(dat, 1, itemtype = "2PL", constr = list(c(2, 1)))

logical <- sample(size= nObs, c(TRUE, FALSE), replace = TRUE)
metric <- rnorm(nObs)
ordered <- ordered(sample(1:5, size = nObs, replace = TRUE))
factor <- factor(sample(1:2, size = nObs, replace = TRUE))

test <- scDIFtest(mod, DIF_covariate = metric)
test
dim(test$gefp$process)

scDIFtest(mod, DIF_covariate = logical)
scDIFtest(mod, DIF_covariate = ordered)
scDIFtest(mod, DIF_covariate = factor)

scDIFtest(mod, DIF_covariate = metric, functional = "maxmosum")
scDIFtest(mod, DIF_covariate = metric, functional = "suplm")



print(test, item_selection = c(2, 5))
print(test, item_selection = c(2, 5), digits = 5)
print(test, digits = 3)

# plot(test, item_selection = c(3:5))


# library(TAM)
#
# data("data.timssAusTwn.scored")
# summary(data.timssAusTwn.scored)
# mod <- mirt(model = 1, data = data.timssAusTwn.scored[, -c(12:14)])
#
# mod
# test <- scDIFtest(mod, DIF_covariate = factor(data.timssAusTwn.scored$ITSEX))
# test
# print(test, item_selection = 4)
