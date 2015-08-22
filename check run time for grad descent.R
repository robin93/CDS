training_data <- Boston
summary(training_data)
head(training_data)
#scaled_t_data_pre <- apply(training_data,MARGIN = 2, FUN = function(X)(X-min(X))/diff(range(X)))
scaled_t_data_pre <- training_data
scaled_t_data <- as.matrix(cbind(Xo=1,scaled_t_data_pre))
para_vect_op <- scaled_t_data[,c("Xo","rm","medv")]
# max <-apply(training_data,2,max)
# min <-apply(training_data,2,min)
# rescaling_matrix <- max - min

alpha <- 0.01
k <- 1/nrow(para_vect_op)

coeff_matrix<- matrix(c(0,0,-1),nrow=1)

cost_1 <- k*sum((para_vect_op%*%t(coeff_matrix))^2)/2
cost_2 <- 0
while (abs(cost_2-cost_1)>0.0000001){
  cost_1 <- cost_2
  print(cost_2)
  #del_theta_1 <- sum((para_vect_op[1]*coeff_matrix[1] + para_vect_op[2]*coeff_matrix[2] - para_vect_op[3])*para_vect_op[1])
  #del_theta_2 <- sum((para_vect_op[1]*coeff_matrix[1] + para_vect_op[2]*coeff_matrix[2] - para_vect_op[3])*para_vect_op[2])
  del_theta_1 <- sum((para_vect_op%*%t(coeff_matrix)*para_vect_op[,1]))
  #print(del_theta_1)
  del_theta_2 <- sum((para_vect_op%*%t(coeff_matrix)*para_vect_op[,2]))
  #print(del_theta_2)
  coeff_matrix[1,1] <- coeff_matrix[1,1] - (alpha*k)*del_theta_1
  coeff_matrix[1,2] <- coeff_matrix[1,2] - (alpha*k)*del_theta_2
  #print(coeff_matrix)
  cost_2 <- k*sum((para_vect_op%*%t(coeff_matrix))^2)/2
}

coeff_matrix
proc.time()
