x <- iris
summary(x)
names(iris)
names(x)
pairs(x[1:4],col=as.numeric(x$Species))  #pairwise take the first 2 columns and understand the scatter of these variables
head(x)
irisdat <- x[1:4]
iriscov <- cov(irisdat)
iriscov
iriseig <- eigen(iriscov)
iriseig
irispca <- princomp(irisdat,cor="False") #cor equals false will do the PCA on the covariance and not the correlation matrix. This function will do all the first 4 lines again
irispca
summary(irispca)
screeplot(irispca)
irispca$loadings
summary(irispca)
irispca

#Run Naive Bayes classifier to Iris data
