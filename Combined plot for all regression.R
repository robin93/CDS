require (MASS)
data<-Boston[,c(6,14)]

Batch_gradient_descent(data,alpha,precision)
stochastic_gradient_descent(data,alpha,precision)
Normal_Equations(data)
Locally_Weighted_Regression(data,tau)

#Plotting all lines and plots on one graph
plot (data$rm ,data$medv,xlab="Avg. number of rooms per dwelling",ylab = "Median value of homes",main="Regression line v/s Input data")
par(new=TRUE)
plot(Query,Out_valyes,col=2,pch=15,axes=FALSE,ann = FALSE)
abline(Batch_grad_theta,col=2,lwd = 3)
abline(stochastic_theta, col=3, lwd = 3)
abline(Normal_Equations_theta,col=5,lwd = 3)

