training_data <- Boston
summary(training_data)
head(training_data)
#scaled_t_data_pre <- apply(training_data,MARGIN = 2, FUN = function(X)(X-min(X))/diff(range(X)))
scaled_t_data_pre <- training_data
scaled_t_data <-cbind(Xo=1,scaled_t_data_pre)
#parameter_matrix <- scaled_t_data[,c("rm")]
#output_values_vector <- scaled_t_data[,c("medv")]
para_vect_op <- scaled_t_data[,c("Xo","rm","medv")]

max <-apply(training_data,2,max)
min <-apply(training_data,2,min)
rescaling_matrix <- max - min

alpha <- 0.045
k <- 1/nrow(para_vect_op)

coeff_matrix <- c(rep(0,2))

#function to iterate over the rows of the matrix and calculate the cost function
f<- function(x,coeff_m){
  (x[1:2]%*%coeff_m - x[3])^2
}

f_1 <- function(x,coeff_m){
  (x[1:2]%*%coeff_m - x[3])*x[1]
}

f_2 <- function(x,coeff_m){
  (x[1:2]%*%coeff_m - x[3])*x[2]
}

cost_1 <- k*sum(apply(para_vect_op,1,f,coeff_matrix))/2
cost_2 <- 0
while (abs(cost_2-cost_1)>0.0000001){
  cost_1 <- cost_2
  print(cost_2)
  del_theta_0 <- sum(apply(para_vect_op,1,f_1,coeff_matrix))
  del_theta_1 <- sum(apply(para_vect_op,1,f_2,coeff_matrix))
  coeff_matrix[1] <- coeff_matrix[1] - alpha*k*del_theta_0
  coeff_matrix[2] <- coeff_matrix[2] - alpha*k*del_theta_1
  cost_2 <- k*sum(apply(para_vect_op,1,f,coeff_matrix))/2
}

coeff_matrix

