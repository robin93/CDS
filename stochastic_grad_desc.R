training_data <- Boston
scaled_t_data_pre <- training_data
scaled_t_data <- as.matrix(cbind(Xo=1,scaled_t_data_pre))
para_vect_op <- scaled_t_data[,c("Xo","rm","medv")]


alpha <- 0.0001
k <- 1/nrow(para_vect_op)

coeff_matrix<- matrix(c(0,0,-1),nrow=1)
cost_1 <- k*sum((para_vect_op%*%t(coeff_matrix))^2)/2
cost_2 <- 0
# iterations_cost <- c(0,cost_1)
# loop <- 0
while (abs(cost_2-cost_1)>0.000001){
  cost_1 <- cost_2
  print(cost_2)
  #print(coeff_matrix)
  for (i in 1:nrow(para_vect_op)){
    #print(i)
    del_theta_1 <- para_vect_op[i,]%*%t(coeff_matrix)*para_vect_op[i,1]
    del_theta_2 <- para_vect_op[i,]%*%t(coeff_matrix)*para_vect_op[i,2]
    #print(del_theta_1,del_theta_2)
    coeff_matrix[1,1] <- coeff_matrix[1,1] - alpha*del_theta_1
    coeff_matrix[1,2] <- coeff_matrix[1,2] - alpha*del_theta_2
  }
  cost_2 <- k*sum((para_vect_op%*%t(coeff_matrix))^2)/2
#   loop <- loop + 1
#   iterations_cost <- cbind(iterations_cost,c(loop,cost_2))
}

coeff_matrix

