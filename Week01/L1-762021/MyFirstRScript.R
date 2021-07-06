# install.packages("readr")

library(readr)
iris <- read_csv("iris.csv", col_names = FALSE)
iris$X5 <- as.factor(iris$X5)

colnames(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
table(iris$Species)


View(iris)

# install.packages("dplyr")
library(dplyr)
iris %>% group_by(Species) %>% summarize(min.Sepal.Length=min(Sepal.Length, na.rm=T),
                                         max.Sepal.Length=max(Sepal.Length, na.rm=T))

