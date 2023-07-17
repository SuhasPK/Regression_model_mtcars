
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


library(ggplot2)
library(ggdark)
histplot <- ggplot(data = mtcars,
                   aes(x = mtcars$mpg)) + geom_histogram(color = "black",fill = "lightgreen") +
    xlab("miles per gallon")+dark_theme_light()
histplot

levels(factor(mtcars$am))[2]

scttrplot <- ggplot(data = mtcars,
                    aes(x = am, y = mpg, color = factor(am)))+ 
    geom_point(size = 2)+geom_smooth(method=lm, color = "yellow")

scttrplot +
    scale_colour_discrete(
        name = "Transmission",
        limits = c("0","1"),
        labels = c("Automatic",
                   "Manual")
    )+dark_theme_light()

ggplot(mtcars, aes(y = mpg, x = factor(am, labels = c("automatic", "manual")), fill = factor(am))) +
    geom_boxplot(colour = "black", size = 1) + scale_colour_discrete(
        name = "Transmission",
        limits = c("0","1"),
        labels = c("Automatic",
                   "Manual"))+ 
    xlab("Transmission") + ylab("mpg") 






bxplot <- ggplot(mtcars, aes(x=factor(am),y = mpg, color = factor(am)))+
    geom_boxplot() +
    geom_point(stat = "summary",
              fun = "median",
              color = "white")
bxplot +
    scale_colour_discrete(
    name = "Tranmission",
    limits = c("0","1"),
    labels = c("Automatic","Manual")
) + dark_theme_light()


df <- data.frame(
    x = factor(c(1, 1, 2, 2)),
    y = c(1, 3, 2, 1),
    grp = c("a", "b", "a", "b")
)
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col(aes(fill = grp), position = "dodge") +
    geom_text(aes(label = y), position = position_dodge(0.9))


