#Linear regression using normal equations 
Normal_Equations <-function(data){
  #check for valid input
  if (is.null(nrow(data)) ){print("Please Enter a valid Dataset")} 
  else {
    
    #Main program starts
    X<-data[,1:(ncol(data)-1)]
    
    #Output vector
    Y<-data[,ncol(data)]
    
    #Added a column matrix X0 corresponding to theta(0)
    X0<-matrix(1,R,1)
    R<-nrow(data) 
    X<-as.matrix(cbind(x0,X))
    
    #using matrix solver to solve for theta
    a<-(t(X)%*%X)
    b<-(t(X)%*%Y)
    theta<-solve(a,b)
    
    print(theta)
  }
}

#sample output file
require (MASS)
data<-Boston[,c(6,14)]

Normal_Equations(data)