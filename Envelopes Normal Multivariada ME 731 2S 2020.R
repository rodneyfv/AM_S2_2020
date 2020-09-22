# Envelopes para cada variável
library(car)
n <- nrow(iris)
mx <- as.matrix(iris[,1:4])
vmu <- apply(mx,2,mean) # vetor de médias
s2 <- cov(mx) # matriz de covariâncias
vvar <- diag(s2) # vetor de variâncias
nvar <- 4  # número de variáveis
inames <- c("comprimento da sépala","largura da sépala","comprimento da pétala",
            "largura da pétala")
# gráfico de quantis-quantis das variáveis
par(mfrow=c(2,2)) 
for (j in 1:nvar)
{
  qqPlot(scale(mx[,j]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantil da N(0,1)",ylab=inames[j],cex=1.2)
}
# teste de Kolmogorov-Smirnov para normalidade das variáveis
ks.test(scale(mx[,1]),"pnorm",0,1)
ks.test(scale(mx[,2]),"pnorm",0,1)
ks.test(scale(mx[,3]),"pnorm",0,1)
ks.test(scale(mx[,4]),"pnorm",0,1)




# Envelopes para a forma quadrática

par(mfrow=c(1,1))
#mmu <- t(matrix(t(vmu),nvar,n))
#vF <- apply(((mx-vmu)*(mx-vmu)%*%solve(s2)),1,sum)
#vF<- (n-nvar)*vF/((n-1)*nvar)
#vQ <- n*apply(((mx-vmu)*(mx-vmu)%*%solve(s2)),1,sum)

# distância de Mahalanobis
vQ <- n*mahalanobis(mx,center=vmu,cov=s2)
#qqPlot(vF,dist="f",df1=nvar,df2=n-nvar,col.lines=1,grid="FALSE",xlab="quantil da distribui??o F",ylab="forma quadr?tica",cex=1.2,id.cex=1.2)

# gráfico de quantis-quantis de vQ para a distribuição qui-quadrado
qqPlot(vQ,dist="chisq",df=nvar,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",ylab="forma quadrática",cex=1.2)




