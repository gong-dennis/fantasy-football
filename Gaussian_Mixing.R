#Implementation of Gaussian Mixture Model

library(mclust)

top_150

test <- Mclust(top_150[,4], G = 7, model = "V")
summary(test)
