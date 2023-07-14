
data(mtcars)

summary(mtcars)

hist(mtcars$mpg, breaks = 10,
     xlab = "Miles per gallon",
     main = "Histogram of frequency vs miles per gallon of car",
     xlim = range(10:35),
     col = "yellow")


plot(mtcars$mpg ~ as.factor(mtcars$am), mtcars, 
     xlab = "Transmission type", 
     ylab="Miles per gallon", 
     main="Boxplot of MPG by transmission type",
     col = topo.colors(factor(mtcars$am)))
legend("topleft", inset = 0.02,
       title = "Transmission Type",
       c("automatic","manual"),
       fill = topo.colors(factor(mtcars$am)),
       horiz = TRUE,
       cex = 0.8)


plot(x = mtcars$am,
     y = mtcars$mpg,
     pch = 19,
     xlab = "miles per gallon",
     ylab = "transmission type",
     col = factor(mtcars$am))

# linear regression.

fit1 <- lm( mpg ~ as.factor(am), 
            data = mtcars)
abline(reg = fit1, col = "green")

legend(0.1,34, legend = c("automatic","manual"),
       pch = 19,
       col = c("black", factor(mtcars$am)),
       title = "transmission type",)

summary(fit1)$coef

boxplot(mpg~am, data = mtcars,
        xlab = "Transmission",
        ylab = "Miles per Gallon",
        main = "MPG by Transmission Type",
        col = topo.colors(factor(mtcars$am)))




