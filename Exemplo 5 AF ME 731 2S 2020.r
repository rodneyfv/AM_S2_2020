library(xtable)
library(MASS)
library(car)
library(corrplot)
set.seed(33)

# limpar todas as vari�veis 
rm(list = ls(all.names = TRUE))


# Para gerar um conjunto de dados a partir da matriz de correla��es apresentada
# em Johnson \& Wichern

mcor <- rbind(cbind(1,0.439,0.410,0.288,0.329,0.248),
              cbind(0.439,1,0.351,0.354,0.320,0.329),
              cbind(0.410,0.351,1,0.164,0.190,0.181),
              cbind(0.288,0.354,0.164,1,0.595,0.470),
              cbind(0.329,0.320,0.190,0.595,1,0.464),
              cbind(0.248,0.329,0.181,0.470,0.464,1))
# xtable(mcor,digits=3)

# vetor de m�dias fict�cio
vmu<-rbind(8.2,8.0,7.6,9.1,9.4,8.9)
vvar <- rbind(1,1.5,0.8,1.3,1.7,2)
mvar <- diag(sqrt(c(vvar)))
n<-220
mcov <- mvar%*%mcor%*%mvar
nvar <- 6
#
# Simulando e salvando a matriz de dados
# mx<-mvrnorm(n, c(vmu), mcov)
# mcovs <- cov(mx); mx <- (mx%*%solve(chol(mcovs)))%*%chol(mcov)
# mx <- 
# write(t(round(mx,2)), file = "./DadosIntel.txt",ncolumns = 6, sep=" ")

# Carregando os dados
mx<- read.table(file = "./Dados/DadosIntel.txt")
colnames(mx) <-c("Ga�lico","Ingl�s","Hist�ria","Aritm�tica","�lgebra","Geometria") 
inames<-c("Ga�lico","Ingl�s","Hist�ria","Aritm�tica","�lgebra","Geometria") 

# Diagrama de dispers�o
plot(mx,pch=19)
# Boxplot
boxplot(mx)
# Envelopes
par(mfrow=c(2,3)) 
for (j in 1:nvar)
{
  qqPlot(scale(mx[,j]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantil da N(0,1)",ylab=inames[j],cex=1.2)
}
#
vmu<-apply(mx,2,mean)
s2<-cov(mx)
amx <- as.matrix(mx)
par(mfrow=c(1,1))
mmu <- t(matrix(t(vmu),nvar,n))
#vF <- apply(((amx-vmu)*(amx-vmu)%*%solve(s2)),1,sum)
#vF<- n*vF#(n-nvar)*vF/((n-1)*nvar)
vF<- n*mahalanobis(amx,center=vmu,cov=s2)
qqPlot(vF,dist="f",df1=nvar,df2=n-nvar,col.lines=1,grid="FALSE",
       xlab="quantil da distribui��o qui-quadrado",
       ylab="forma quadr�tica",cex=1.2)

# Matriz de correla��es
corrplot(cor(mx), order = "hclust", tl.col='black', tl.cex=.75) 

# Medidas resumo

# M�todo das componentes principais
p <- ncol(mx)
m.cor<-cor(mx)
aut.val <-  eigen(m.cor)$values
aut.vec <- (eigen(m.cor)$vectors)
m.aut.val <- t(matrix(((aut.val)),p,p))
mcarga <- sqrt(m.aut.val)*(-aut.vec)
100*aut.val/nvar
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,1-vcomunal)
xtable(result,digits=3)
LLt <- mcarga[,1:2]%*%t(mcarga[,1:2])

# Diferen�a entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferen�a entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(1-vcomunal),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(1-vcomunal)))


# M�todo de M�xima verossimilhan�a

# Matriz de correla��es
#factanal(covmat=cov(mx),nobs=220,factors=2,rotation="none")
# Matriz de dados
resultAF<-factanal(x=mx,factors=2,rotation="none")
mcarga <- resultAF$loadings[1:6,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,resultAF$uniquenesses)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)

# Diferen�a entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferen�a entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(resultAF$uniquenesses),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(resultAF$uniquenesses)))

# Escores fatoriais m�todo de m�nimos quadrados ponderados
resultAFMQP<-factanal(x=mx,factors=2,rotation="none",scores = c("Bartlett"))
resultAFMR<-factanal(x=mx,factors=2,rotation="none",scores = c("regression"))
escfatMQP <- resultAFMQP$scores
escfatMR <- resultAFMR$scores
colnames(escfatMQP)<-c("fator 1","fator 2")
colnames(escfatMR)<-c("fator 1","fator 2")

#Medidas-resumo
#
medados1<- rbind(apply(escfatMQP,2,mean),apply(escfatMQP,2,var),
                 apply(escfatMQP,2,sd),apply(escfatMQP,2,min),
                 apply(escfatMQP,2,quantile,0.5),apply(escfatMQP,2,max))
rownames(medados1)<-c("M�dia","Var.","DP","M�nimo","Mediana","M�ximo")
xtable(medados1)
medados2<- rbind(apply(escfatMR,2,mean),apply(escfatMR,2,var),
                 apply(escfatMR,2,sd),apply(escfatMR,2,min),
                 apply(escfatMR,2,quantile,0.5),apply(escfatMR,2,max))
rownames(medados2)<-c("M�dia","Var.","DP","M�nimo","Mediana","M�ximo")
xtable(medados2)
xtable(cbind(medados1,medados2))

# Gr�fico de dispers�o 2

par(mfrow=c(1,2))
plot(escfatMQP[,1],escfatMR[,1],xlab="fator 1 - MQP",ylab="fator 1 - MR",
     cex=1.2,cex.lab=1.2,pch=19)
abline(0,1)
plot(escfatMQP[,2],escfatMR[,2],xlab="fator 2 - MQP",ylab="fator 2 - MR",
     cex=1.2,cex.lab=1.2,pch=19)
abline(0,1)

# Gr�fico de dispers�o 1

par(mfrow=c(1,2))
plot(escfatMQP[,1],escfatMQP[,2],xlab="fator 1",ylab="fator 2",main="MQP",
     cex=1.2,cex.lab=1.2,pch=19)
plot(escfatMR[,1],escfatMR[,2],xlab="fator 1",ylab="fator 2",main="Regress�o",
     cex=1.2,cex.lab=1.2,pch=19)

# Boxplot
par(mfrow=c(1,2))
boxplot(escfatMQP,cex=1.2,cex.lab=1.2,xlab="fator",main="MQP",ylim=c(-4,4))
boxplot(escfatMR,cex=1.2,cex.lab=1.2,xlab="fator",main="Regress�o",ylim=c(-4,4))

# Histograma
par(mfrow=c(2,2))
hist(escfatMQP[,1],probability=TRUE,main="fator 1 - MQP",xlab="",ylab="")
hist(escfatMQP[,2],probability=TRUE,main="fator 2 - MQP",xlab="",ylab="")
hist(escfatMR[,1],probability=TRUE,main="fator 1 - MR",xlab="",ylab="")
hist(escfatMR[,2],probability=TRUE,main="fator 2 - MR",xlab="",ylab="")

# qqplots
#
par(mfrow=c(2,2))
qqPlot(scale(escfatMQP[,1]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
       xlab="quantil da N(0,1)",ylab="Fator 1 - MQP",cex=1.2)
qqPlot(scale(escfatMQP[,2]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
       xlab="quantil da N(0,1)",ylab="Fator 2 - MQP",cex=1.2)
qqPlot(scale(escfatMR[,1]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
       xlab="quantil da N(0,1)",ylab="Fator 1 - MR",cex=1.2)
qqPlot(scale(escfatMR[,2]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
       xlab="quantil da N(0,1)",ylab="Fator 2 - MR",cex=1.2)

#
par(mfrow=c(1,2))
nvar <-2
vmu<-apply(escfatMQP,2,mean)
s2<-cov(escfatMQP)
amx <- as.matrix(escfatMQP)
mmu <- t(matrix(t(vmu),nvar,n))
#vF <- apply(((amx-vmu)*(amx-vmu)%*%solve(s2)),1,sum)
#vF<- n*vF#(n-nvar)*vF/((n-1)*nvar)
vF <- n*mahalanobis(amx,center=vmu,cov=s2)
qqPlot(vF,dist="f",df1=nvar,df2=n-nvar,col.lines=1,grid="FALSE",
       xlab="quantil da distribui��o qui-quadrado",
       ylab="forma quadr�tica - MQP",cex=1.2)

#
vmu<-apply(escfatMR,2,mean)
s2<-cov(escfatMR)
amx <- as.matrix(escfatMR)
mmu <- t(matrix(t(vmu),nvar,n))
#vF <- apply(((amx-vmu)*(amx-vmu)%*%solve(s2)),1,sum)
#vF<- n*vF#(n-nvar)*vF/((n-1)*nvar)
vF <- n*mahalanobis(amx,center=vmu,cov=s2)
qqPlot(vF,dist="f",df1=nvar,df2=n-nvar,col.lines=1,grid="FALSE",
       xlab="quantil da distribui��o qui-quadrado",
       ylab="forma quadr�tica - Regress�o",cex=1.2)


########################
########################
# Rota��o

# dispers�o entre as cargas fatoriais obtidas pelo m�todo de m�xima 
# verossimilhan�a
par(mfrow=c(1,1))
#mcarga <- [1:6,]
plot(mcarga[,1],mcarga[,2],xlim=c(0,1),ylim=c(-0.6,0.8),xlab="Fator 1",
     ylab="Fator 2",pch=19,cex=1.2)
text(mcarga[,1],mcarga[,2],inames)
abline(0,0)
arrows(0,0,0.30,0.62,col=2)
arrows(0,0,0.8,-0.45,col=2)
arrows(0,0,0.60,0.45,col=3)
arrows(0,0,0.8,-0.35,col=3)
text(0.05,0.4,"F2")
text(0.8,0.1,"F1")
text(0.2,0.4,"F2",col=2)
text(0.6,-0.4,"F1",col=2)
text(0.4,0.2,"F2",col=3)
text(0.6,-0.2,"F1",col=3)
legend(0.2,-0.4,col=c(2,3),lwd=c(1,1),
       legend=c("Rota��o ortogonal","Rota��o obl�qua"),bty="n",cex=1.3)

# Implementando a rota��o (sentido hor�rio)
phi<-0.349066 # 20 graus em radianos
mT <- rbind(cbind(cos(phi),sin(phi)),cbind(-sin(phi),cos(phi)))
mcarga%*%mT
mcargarot <- mcarga%*%mT

# Solu��o varimax e fatores
resultAF<-factanal(x=mx,factors=2,rotation="varimax")
mcarga <- resultAF$loadings[1:6,]
mcargaMVV <- mcarga
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,resultAF$uniquenesses)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)


# Predi��o dos fatores usando a rota��o VARIMAX

resultAFMQP<-factanal(x=mx,factors=2,rotation="varimax",scores = c("Bartlett"))
resultAFMR<-factanal(x=mx,factors=2,rotation="varimax",scores = c("regression"))
escfatMQPV <- resultAFMQP$scores
escfatMRV <- resultAFMR$scores
colnames(escfatMQPV)<-c("fator 1","fator 2")
colnames(escfatMRV)<-c("fator 1","fator 2")

#
# Solu��o promax 
resultAF<-factanal(x=mx,factors=2,rotation="promax")
mcarga <- resultAF$loadings[1:6,]
mcargaMVP <- mcarga
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,resultAF$uniquenesses)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)

# Compara��o entre as cargas fatoriais

cargacomp <- cbind(mcargaMV[,1],mcargaMV[,2],mcargaMVV[,1],
                   mcargaMVV[,2],mcargaMVP[,1],mcargaMVP[,2])
xtable(cargacomp,digits=3)
##################################


# Predi��o dos fatores usando a rota��o PROMAX

resultAFMQP<-factanal(x=mx,factors=2,rotation="promax",scores = c("Bartlett"))
resultAFMR<-factanal(x=mx,factors=2,rotation="promax",scores = c("regression"))
escfatMQPP <- resultAFMQP$scores
escfatMRP <- resultAFMR$scores
colnames(escfatMQPP)<-c("fator 1","fator 2")
colnames(escfatMRP)<-c("fator 1","fator 2")
#

# Compara��o entre os escores fatoriais

par(mfrow=c(2,2))

plot(escfatMQPV[,1],escfatMQPV[,2],xlab="fator 1",ylab="fator 2",
     main="MQP - Varimax",cex=1.2,cex.lab=1.2,pch=19)
plot(escfatMRV[,1],escfatMRV[,2],xlab="fator 1",ylab="fator 2",
     main="Regress�o - Varimax",cex=1.2,cex.lab=1.2,pch=19)

plot(escfatMQPP[,1],escfatMQPP[,2],xlab="fator 1",ylab="fator 2",
     main="MQP - Promax",cex=1.2,cex.lab=1.2,pch=19)
plot(escfatMRP[,1],escfatMRP[,2],xlab="fator 1",ylab="fator 2",
     main="Regress�o - Promax",cex=1.2,cex.lab=1.2,pch=19)
