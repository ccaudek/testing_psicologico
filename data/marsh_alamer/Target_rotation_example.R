 library(lavaan)

# data preparation
ex5_25 = read.table("ex5.25.dat")
names(ex5_25) = paste0("y",1:12) 

model <- '
    # efa block 
    efa("efa1")*f1 + 
    efa("efa1")*f2 =~ y1 + y2 + y3 + y4 + y5 + y6

    # cfa block
    f3 =~ y7 + y8 + y9
    f4 =~ y10 + y11 + y12

    # regressions
    f3 ~ f1 + f2
    f4 ~ f3
'

# create 0/1 matrix reflecting
# the expecting factor pattern:
TARGET <- matrix(0, 6, 2)
TARGET[1:3, 1] <- 1
TARGET[4:6, 2] <- 1
TARGET

fit <- sem(model = model, data = ex5_25, rotation = "target",
           rotation.args = list(target = TARGET))
summary(fit)

