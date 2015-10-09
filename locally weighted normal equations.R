training_data <- Boston
scaled_t_data_pre <- training_data
scaled_t_data <- as.matrix(cbind(Xo=1,scaled_t_data_pre))
X_vect_op <- as.matrix(scaled_t_data[,c("Xo","rm")])
len <- nrow(X_vect_op)
rm_values <- as.matrix(scaled_t_data[,c("rm")])
w_values <- apply(rm_values,MARGIN = 2, FUN = function(X)exp((-1*((X-mean(X))^2))/diff(range(X))))
Weight_mat <- diag(c(w_values),len,len)
Y_vect_op <- as.matrix(scaled_t_data[,c("medv")])
theta <- as.matrix(c(rep(0,2)))
a <- t(X_vect_op)%*%Weight_mat%*%X_vect_op
b <- t(X_vect_op)%*%Weight_mat%*%Y_vect_op
theta <- solve(a,b)

