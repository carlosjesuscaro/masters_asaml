# Title     : TODO
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-06-19

library(rpart)
data(iris)
Tree1=rpart(iris$Species~.,data=iris[,-5])
Tree1
Tree2=rpart(iris$Species~.,data=iris[,-5],control=rpart.control(cp=10^-9,minsplit=2)
            printcp(Tree2)
            A=printcp(Tree2)
            alpha=A[,1]
            K=length(alpha)
            for (i in 1:(K-1))
              {
              T=prune(Tree2,cp=alpha[K+1-i])
              plot(T)
              #text(T)
            }


            TT1=rpart(iris$Species~.,data=iris[,-5],control=rpart.control(cp=10^-9,minsplit=2))
            TT2=rpart(iris$Species~.,data=iris[,-5],control=rpart.control(cp=10^-9,minsplit=2))
            TT3=rpart(iris$Species~.,data=iris[,-5],control=rpart.control(cp=10^-9,minsplit=2))


            cart<-function(learningX,learningY)
              {
              T<-rpart(learningY~.,data=learningX,control=rpart.control(cp=10^-10,minsplit=2))  #construction of the maximal tree
              A=printcp(T)
              b=which(A[,4]==min(A[,4]))   #we are looking where the minimum of the crossvalidation error is
              s=A[b,4]+A[b,5]         #the threshold do to the 1SE rule
              s=unique(min(s))        #to prevent the fct that b may be a vector
              w=1*(A[,4]<=s)
              v=which(w==1)
              r=v[1]
              cp=A[r,1]
              finalT=prune(T,cp=cp)
              cart=finalT
            }



            u=sample(1:150,120)
            learning=iris[u,]
            test=iris[-u,]
            Tree=rpart(learning[,5]~.,data=learning[,-5],cp=0.02,minsplit=2)
            predict(Tree)
            predict(Tree,type='class')
            Treer
            predict(Treer)



            u=sample(1:150,140)
            learning=iris[u,]
            test=iris[-u,]
            Tree=rpart(learning[,5]~.,data=learning[,-5],cp=10^-9,minsplit=2)

            #########################

            X1=runif(500,-4,4)
            X2=rnorm(500,2,1)
            X3=rnorm(500,-2,1)
            X4=rexp(500,4)
            X5=rexp(500,1/4)
            X6=rt(500,7)
            X0=rbinom(500,1,0.5)
            Y=(2+3*X2-2*X4)*(X0==0)+(-2-4*X3+2*X5)*(X0==1)+rnorm(500,0,1)
            X=cbind(X0,X1,X2,X3,X4,X5,X6)
            dim(X)
            T=rpart(Y~.,data=as.data.frame(X),cp=10^-10,minsplit=2)
