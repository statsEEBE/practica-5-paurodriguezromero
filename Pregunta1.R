#Dibujamos la distribución poblacional de nuestro caso, es una normal
mu<-95.3
sigma<-5.7
curve(dnorm(x,mean=mu,sd=sigma),xlim=c(80,120),col="orange")


Y<-function(i){sum(rnorm(4,mu,sigma))}
mean(sapply(1:100000,Y)))
4*mu #Valor teorico

Y<-function(i){sum(rnorm(100,mu,sigma))}
var(sapply(1:100000,Y))
100*sigma^2 #Valor teorico

1-pnorm(103,mu,sigma)

Y<-function(i){mean(rnorm(4,mu,sigma))}
Ybar<-sapply(1:100000,Y)
mean(Ybar<98)
pnorm(98,mu,sigma/sqrt(4)) #Valor teorico

Y<-function(i){var(rnorm(100,mu,sigma))}
Ybar<-sapply(1:100000,Y)
mean(Ybar>32)
#Valor teorico
w<-32*(100-1)/sigma^2
1-pchisq(w,100-1)
