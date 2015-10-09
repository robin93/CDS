Batch_gradient_descent <- function(data,alpha,precision){
  para_vect_op <- as.matrix(cbind(Xo=1,data))
  coeff_matrix<- matrix(c(0,0,-1),nrow=1)   #initiating coefficient matrix
  cols <- ncol(coeff_matrix)-1
  del_theta_matrix <- matrix(0, nrow = 1, ncol = cols) #initiating the partial derivative matrix for cost function
  k <-nrow(para_vect_op)
  init_cost <- sum((para_vect_op%*%t(coeff_matrix))^2)/(2*k)
  cost <- c(0,init_cost)  #initializing cost value vector
  i <- 1
  
  #implementing gradient descent update algorithm
  while (abs(cost[i+1]-cost[i])>precision){
    i <- i + 1
    print(cost[i])
    del_theta_mat <- (t(para_vect_op[,1:cols])%*%(para_vect_op%*%t(coeff_matrix)))/k
    coeff_matrix[1:cols] <- coeff_matrix[1:cols] - alpha*del_theta_mat
    cost[i+1] <- sum((para_vect_op%*%t(coeff_matrix))^2)/(2*k)
  }
  return(coeff_matrix[,1:cols])
  #returning the coefficient matrix 
}

stochastic_gradient_descent<-function(data, alpha, precision)
{
  m<-nrow(data)
  n<-ncol(data)-1
  x<-cbind(rep(1,m),as.matrix(data[,1:n])) #matrix of the explanatory variables
  y<-as.matrix(data[,n+1])  #matrix for the output vector
  coeff_matrix<-as.matrix((rep(0,n+1)))
  init_cost <- (1 / (2 * m)) * sum(((x %*% coeff_matrix) - y)^2) #initial value of the cost function
  cost <- c(0,init_cost)
  i <- 1
  
  while(abs(cost[i+1]-cost[i])>precision){ 
  i<-i+1
  #implementing the update of theta by every data set
  for (j in 1:m){
    h=x[j,]%*% coeff_matrix
    coeff_matrix <- coeff_matrix - alpha*(x[j,]%*%(h-y[j]))
  }
  cost[i+1] <- ((1 / (2 * m)) * sum(((x %*% coeff_matrix) - y)^2))
  print(cost[i])
  }
  return (coeff_matrix)
}  

require (MASS)
data<-Boston[,c(6,14)]
Batch_grad_theta <- Batch_gradient_descent(data,0.001,0.0000001)
print(Batch_grad_theta)
stochastic_theta<-stochastic_gradient_descent(data,.0001,.000001)
print(stochastic_theta)

plot (data$rm,data$medv)
fit<-lm(medv~rm,data)
abline(fit,col=2)
abline(stochastic_theta, col=3)

plot(data$rm,data$medv)
fit <- lm(medv~rm,data)
abline(fit,col=2)
abline(Batch_grad_theta,col=3)

#cost_values <- cost[c(2:10,100,1000,10000)]
#iterations <- c(2:10,100,1000,10000)
#plot(cost_values,type = "o",col ="blue",axes= false)

# y <- cost[c(2:length(cost))]
# x <- c(2:length(cost))
# plot(x,y,log = "x")

#cost_0.01 <- cost
cost_0.001 <- cost

y1 <- cost_0.01[c(2:100)]
y2 <- cost_0.001[c(2:100)]
x <- c(2:100)
plot(x,y1,col="blue",type="o")
lines(x,y2,col="green",type ="o")
