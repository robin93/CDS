training_data <- Boston
scaled_t_data_pre <- training_data
scaled_t_data <- as.matrix(cbind(Xo=1,scaled_t_data_pre))
X_vect_op <- as.matrix(scaled_t_data[,c("Xo","rm")])
Y_vect_op <- as.matrix(scaled_t_data[,c("medv")])
theta <- as.matrix(c(rep(0,2)))
a <- t(X_vect_op)%*%X_vect_op
b <- t(X_vect_op)%*%Y_vect_op
theta <- solve(a,b)
