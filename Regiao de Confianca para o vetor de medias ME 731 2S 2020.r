
library(ellipse)
library(MASS)
library(xtable)

########################
# Elipsoide de confiançaa
# Dados simulados
n<-1000
# vetor de médias
mu <- c(10,5)
# matriz de covariâncias
rho<-0.9
var1<-100
var2<-25
Sigma<- rbind(cbind(var1,sqrt(var1*var2)*rho),cbind(sqrt(var1*var2)*rho,var2))
# gerando uma amostra
mx <- mvrnorm(n, mu, Sigma)
plot(mx[,1],mx[,2],xlab="variável 1",ylab="variável 2")
# estimativas de média, correlação e variâncias
ma <- apply(mx,2,mean)
rhoa <- cor(mx)[1,2]
vara1<-cov(mx)[1,1]
vara2<-cov(mx)[2,2]
# elipse com parâmetros estimados
lines(ellipse(rhoa, scale = c(sqrt(vara1), sqrt(vara2)), 
              centre = rbind(ma[1], ma[2]), level = 0.95))

###############
# Dados da Íris
iris<-as.matrix(iris[,1:4])
inames <- c("comprimento da sépala","largura da sépala","comprimento da pétala",
            "largura da pétala")
#colnames(iris)<-c("comprimento da sépala","largura da sépala",
#"comprimento da pétala","largura da pétala")

# Diagrama de dispersão
pairs(iris,pch=19,cex.labels=1.4)
# Boxplot
boxplot(iris,cex=1.2,cex.axis=1.2,cex.lab=1.2,names=inames)

# Histogramas
par(mfrow=c(2,2))
hist(iris[,1],xlab=inames[1],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="")
hist(iris[,2],xlab=inames[2],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="")
hist(iris[,3],xlab=inames[3],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="")
hist(iris[,4],xlab=inames[4],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="")

# Matriz de correlações
cor(iris)
xtable(cor(iris))
rho <-  cor(iris)#lowerTriangle((cor(iris)))
xb <- apply(iris,2,mean)
s2<-cov(iris)
vara <-diag(s2)
nvar <- 4

# Elipsóides de Confiança
par(mfrow=c(2,3))
for (i in 1:(nvar-1)){
  for (j in (i+1):nvar){
    plot(ellipse(rho[i,j], scale = c(sqrt(vara[i]), sqrt(vara[j])), 
                 centre = rbind(xb[i], xb[j]), level = 0.95),type="l",lwd=2,col=2,
         xlab=inames[i],ylab=inames[j])
    lines(iris[,i],iris[,j],pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2,type="p",col=4)
    #plot(iris[,i],iris[,j],pch=19,xlab=inames[i],ylab=inames[j],cex=1.2,
    #  cex.lab=1.2,cex.axis=1.2)
    #lines(ellipse(rho[i,j], scale = c(sqrt(vara[i]), sqrt(vara[j])), 
    #  centre = rbind(xb[i], xb[j]), level = 0.95))
  }
}
