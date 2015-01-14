source("1-setup.R")
library(psych)

attach(anscombe)
a1 <- data.frame(x1,y1)
a2 <- data.frame(x2,y2)
a3 <- data.frame(x3,y3)
a4 <- data.frame(x4,y4)

describe(a1)
describe(a2)
describe(a3)
describe(a4)

# create 4 plots
par(mfrow=c(2,2))

plot(a1,pch=15,bg="blue")
abline(lm(a1$y1~a1$x1),col="red")
plot(a2,pch=16,bg="blue")
abline(lm(a2$y2~a2$x2),col="red")
plot(a3,pch=17,bg="blue")
abline(lm(a3$y3~a3$x3),col="red")
plot(a4,pch=18,bg="blue")
abline(lm(a3$y3~a3$x3),col="red")
#title("Anscombe's Quartet",outer = TRUE)

