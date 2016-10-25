#########################################################
#########################################################
# The function "fastgrad" quickly computes parameters for
# linear regression models for large datasets.
# Note: this function is not useful for inference as the Variance-Covariance
# matrix is not computed
# Lefteris Anastasopoulos
# Email: j.andronici@gmail.com
#

fastgrad<-function(X,y,alpha,iter){

# Reformat the data by adding a row of 1's for the constant
  X = cbind(
    rep(1,dim(X)[1]),X
    )
  
  n = dim(X)[1]

  theta<-matrix(rep(0,dim(X)[2]),
                nrow=dim(X)[2],ncol=1)
  

# Max # of iterations that gradient descent will run.
thetaiter<-list(theta)
costiter<-c()

# Computer the cost function
cost<-function(theta,X,n,y){
    J<-(1/n)*sum((X%*%theta - y)^2)
    return(J)
}


for(i in 2:iter){
  costiter[1]<-cost(theta = thetaiter[[1]],X=X, y=y, n=n)
  thetaiter[[i]]<-thetaiter[[i-1]] - (alpha/n)*t(t(X%*%thetaiter[[i-1]] - y)%*%X)
  costiter[i]<-cost(theta = thetaiter[[i]],X=X, y=y, n=n)
  if(costiter[i]>=costiter[i-1]){
    break
  }
}

iteration<-1:length(costiter)

finaltheta<-thetaiter[[length(thetaiter)]]

return(finaltheta)
return(costiter)
return(iteration)
}

