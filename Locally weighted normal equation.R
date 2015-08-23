#Locally Weighted Regression function takes as input training data set and the value of tau from the user
Locally_Weighted_Regression<-function(data,tau)
{
  #To check for valid input
  if (is.null(nrow(data))) {
    print("Please enter a valid dataset")
    } 
  else{
    
    #Generate the parameter and output vectors
    x<-data[,1:(ncol(data)-1)]
    y<-data[,ncol(data)]
    
    # Generate query vectors by dividing the input dataset into 100 points
    k<-(max(range(x))-min(range(x)))/100
    Query <- seq(min(range(x))+k,max(range(x)),by = k)
    
    #Adding constant column with value 1 corresponding to theta(0)
    R<-nrow(data)
    x0<-matrix(1,R,1)
    x<-as.matrix(cbind(x0,x))
    
    x_transpose <- t(x)
    Out_values<-vector("numeric",100)
    #Prediction corresponding to each query vector Q[m] is being calculated
    for(m in 1:100)
    {
      weight <-matrix(0,R,R)  #creating the diagonal matrix for weight
      for (i in 1:R)  #calculating weights for rows in the training set
      {
        weight[i,i]<-exp(-(((t(x[i,2]-Query[m]))%*%(x[i,2]-Query[m]))/(2*(tau)^2)))
      }
      #solving for theta values for ongoing query vector using normal equations
      a<-(x_transpose%*%weight%*%x)
      b<-(x_transpose%*%weight%*%y)
      theta<-solve(a)%*%b
      
      #Storing values of output 
      Out_values[m]<-t(theta)%*%rbind(1,Query[m]) 
    }
    plot(Query,Out_values,
         xlab="Query vector value",
         ylab="Predicted value",
         main="Locally weighted Linear Regression")
  }
}

#sample data set
require (MASS)
data<-Boston[,c(6,14)]

Locally_Weighted_Regression(data,0.1)
