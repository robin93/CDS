#predict_y function takes as input the training data set the value of tau from the user
require (MASS)
data<-Boston[,c(6,14)]
R<-nrow(data)
#x<-data[,1:(ncol(data)-1)]
x<-data[,1]
y<-data[,ncol(data)]
    #In order to create a set of query vectors,divide the input dataset into 100 points 
    k<-(max(range(x))-min(range(x)))/100
    tau = 1
    Q2 <- seq(min(range(x))+k,max(range(x)),by = k)
    #An extra column,x0=(1xlength(data))should be added to the input x,
    #which corresponds to theta(0)
    x0<-matrix(1,R,1)
    X<-as.matrix(cbind(x0,x))#This is the input matrix X
    Xt<-t(X)
    Y2<-vector("numeric",100)
    for(m in 1:100){
      W<-matrix(0,R,R)
      for (i in 1:R){
        W[i,i]<-exp(-(((t(X[i,2]-Q2[m]))%*%(X[i,2]-Q2[m]))/(2*(tau)^2)))
      }
      a<-(Xt%*%W%*%X)
      b<-(Xt%*%W%*%y)
      theta<-solve(a)%*%b
      Y2[m]<-t(theta)%*%rbind(1,Q2[m])
    }
    plot(Q2,Y2,xlab="Query vector value",ylab="Output",main="Locally weighted Linear Regression line")

