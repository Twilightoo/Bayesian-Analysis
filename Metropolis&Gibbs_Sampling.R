#1
#定义后验分布
f<-function(theta,x,sigma,tao,mu){
  return(exp(-(theta-x)^2/(2*sigma^2))/(tao^2+(theta-mu)^2))
}
#随机游动Metropolis算法
rw.Metropolis<-function(theta0,x,sigma,tao,mu,yita,N){
  theta<-numeric(N)
  theta[1]<-theta0
  u<-runif(N)
  k<-0
  for(i in 2:N){
    y<-rnorm(1,theta[i-1],yita)
    if(u[i]<=(f(y,x,sigma,tao,mu)/f(theta[i-1],x,sigma,tao,mu)))
      theta[i]<-y
    else{
      theta[i]<-theta[i+1]
      k=k+1
    }
  }
  return(list(theta=theta,k=k))
}
#给初值和参数赋值
x<-0
theta0<-10
sigma<-1
tao<-5
mu<-0
yita<-c(0.05,1,2,16)
N<-2000
#运行随机游动Metropolis算法得到四条链
rw1<-rw.Metropolis(theta0,x,sigma,tao,mu,yita[1],N)
rw2<-rw.Metropolis(theta0,x,sigma,tao,mu,yita[2],N)
rw3<-rw.Metropolis(theta0,x,sigma,tao,mu,yita[3],N)
rw4<-rw.Metropolis(theta0,x,sigma,tao,mu,yita[4],N)
#计算拒绝率并画出链的轨迹图
print(c(rw1$k,rw2$k,rw3$k,rw4$k)/N)
par(mfrow=c(2,2))
rw<-cbind(rw1$theta,rw2$theta,rw3$theta,rw4$theta)
for(j in 1:4){
  plot(rw[,j],type='l',xlab=bquote(yita==.(round(yita[j],3))),ylab='theta|x')
}
par(mfrow=c(1,1))
#去掉预烧期，求得第二条链的样本均值和样本方差
burn=500
sample<-rw[(burn+1):N,2]
plot(sample,type='l',xlab=bquote(yita==1),ylab='theta|x')
mean<-mean(sample)
var<-var(sample)
mean
var



#2
#给初值和参数赋值
k<-10
x<-c(3,1,4,2,5,3,2,2,0,4)
N<-5000
burn<-1000
theta<-matrix(10,N,k)
lambda<-numeric(N)
lambda[1]<-5
#Gibbs抽样
for(i in 2:N){
  sum<-0
  for(j in 1:k){
    theta[i,j]<-rgamma(1,x[j]+1,lambda[i-1]+1)
    sum<-sum+theta[i,j]
  }
  lambda[i]<-rgamma(1,k+1,sum+1)
}
#画出theta1和lambda的轨迹图
plot(theta[,1],type='l',ylab='theta1')
plot(lambda,type='l',ylab='lambda')
#去掉预烧期得到新的链
b=burn+1
newtheta<-theta[b:N,]
newlambda<-lambda[b:N]
#画出频率直方图
hist(newtheta[,1],freq=FALSE,breaks=100)
hist(newlambda,freq=FALSE,breaks=100)
#求后验中位数和后验众数
mediantheta1<-median(newtheta[,1])
medianlambda<-median(newlambda)
modetheta1<-which.max(table(round(newtheta[,1],3)))
modelambda<-which.max(table(round(newlambda,3)))
mediantheta1
medianlambda
modetheta1
modelambda
#求后验期望、后验协方差阵和后验相关系数阵
thetalambda<-cbind(newtheta,newlambda)
mean<-apply(thetalambda,2,mean)
cov<-cov(thetalambda)
cor<-cor(thetalambda)
mean
cov
cor







  





  




