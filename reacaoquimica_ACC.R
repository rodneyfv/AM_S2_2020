
# bibliotecas necess�rias
library(tidyverse)
library(MASS)
library(CCA)
library(xtable)
library(car)
library(plyr)
library(corrplot)

medidasResumo <- function(x){
  medados<- rbind(apply(x,2,mean),
                  apply(x,2,var),
                  apply(x,2,sd),
                  apply(x,2,min),
                  apply(x,2,quantile,0.5),
                  apply(x,2,max))
  rownames(medados)<-c("M�dia","Var.","DP","M�nimo","Mediana","M�ximo")
  return(medados)
}


# Dados fornecidos em Rencher (Methods of multivariate analysis,
# 2002, p. 365) acerca de um experimento envolvendo uma rea��o
# qu�mica. As vari�veis s�o
# x1: temperatura
# x2: concentra��o
# x3: tempo
# y1: porcentagem de material inicial inalterado
# y2: porcentagem convertida para o produto desejado
# y3: porcentagem de um derivado n�o desejado

dados <- read.table('Dados/reacaoquimica.txt',head=TRUE)

m.Y <- dados %>% dplyr::select(y1, y2, y3)
m.X <- dados %>% dplyr::select(x1, x2, x3)

medados <- medidasResumo(dados)
xtable(medados)

# Matrizes de dispers�o
pairs(cbind(m.X,m.Y),pch=19,cex.labels=1.8)

# Gr�fico de correla��es
corrplot(cor(dados), order = "hclust", tl.col='black', tl.cex=.75) 
cor(dados)

# Boxplot
par(mfrow=c(2,3))
for (i in 1:3){
  boxplot(m.X[,i],xlab=c(names(m.X)[i]),cex=1.4,cex.lab=1.4)
}
for (i in 1:3){
  boxplot(m.Y[,i],xlab=c(colnames(m.Y)[i]),cex=1.4,cex.lab=1.4)
}

# Histograma
par(mfrow=c(2,3))
for (i in 1:3){
  hist(m.X[,i],probability=TRUE,main=c(names(m.X)[i]),cex=1.4,cex.lab=1.4,
       ylab="densidade",xlab="valores")
}
for (i in 1:3){
  hist(m.Y[,i],probability=TRUE,main=c(colnames(m.Y)[i]),cex=1.4,cex.lab=1.4,
       ylab="densidade",xlab="valores")
}

# qqplots
par(mfrow=c(2,3))
for (i in 1:3){
  qqPlot(scale(m.X[,i]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantil da N(0,1)",ylab="quantil da distribui��o de interesse",
         main=c(names(m.X)[i]),cex=1.4)
}
for (i in 1:3){
  qqPlot(scale(m.Y[,i]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantil da N(0,1)",ylab="quantil da distribui��o de interesse",
         main=c(colnames(m.Y)[i]),cex=1.4)
}


# An�lise de correla��o can�nica

m.X <- scale(m.X)
m.Y <- scale(m.Y)
result <- cc(m.X,m.Y)

# Correla��es can�nicas
result$cor

# Coeficientes das combina��es lineares. Cada linha corresponde a
# uma vari�vel can�nica
#(U)
mA <- t(result$xcoef)
#(V)
mB <- t(result$ycoef)

# Vari�veis can�nicas

vU1  <-  result$scores$xscores[,1]
vU2  <-  result$scores$xscores[,2]
vV1  <-  result$scores$yscores[,1]
vV2  <-  result$scores$yscores[,2]

# Correla��es entre as vari�veis can�nicas e as vari�veis originais
# (U,X(1))
corUX1 <- (result$scores$corr.X.xscores); corUX1
t(cor(t(mA%*%t(m.X)),m.X))
round(cor(vU1,m.X),2)
round(cor(vU2,m.X),2)
round(cor(vU1,m.Y),2)
round(cor(vU2,m.Y),2)
# (V,X(2))
corVX2 <- (result$scores$corr.Y.yscores); corVX2
t(cor(t(mB%*%t(m.Y)),m.Y))
round(cor(vV1,m.Y),2)
round(cor(vV2,m.Y),2)
round(cor(vV1,m.X),2)
round(cor(vV2,m.X),2)

# correla��o das primeiras vari�veis can�nicas com os dados originais
xtable(t(rbind(cor(vU1,m.X), cor(vV1,m.X),cor(vU1,m.Y), cor(vV1,m.Y))))


# Representa��o das matrizes de correla��o (com um �nico par)
imA <- solve(mA)
imB <- solve(mB)
#
cor(m.X) - cbind(imA[,1])%*%rbind(imA[,1])
mat1 <- cbind(imA[,1])%*%rbind(imA[,1]) + cbind(imA[,2])%*%rbind(imA[,2])
cor(m.X) - mat1
cor(m.Y) - cbind(imB[,1])%*%rbind(imB[,1])
mat2 <- cbind(imB[,1])%*%rbind(imB[,1]) + cbind(imB[,2])%*%rbind(imB[,2])
cor(m.Y) - mat2
#xtable(mat1)
#xtable(mat2)

#
# percentual da soma das vari�ncias representada pelo 
# primeiro par de var. can�nicas
sum(diag(cbind(imA[,1])%*%rbind(imA[,1])))/sum(diag(cor(m.X)))
sum(diag(cbind(imB[,1])%*%rbind(imB[,1])))/sum(diag(cor(m.Y)))
# dois primeiros pares de var. can�nicas
sum(diag(mat1))/sum(diag(cor(m.X)))
sum(diag(mat2))/sum(diag(cor(m.Y)))

par(mfrow=c(1,2),mar=c(5,5,3,3))
plot(vU1,vV1,xlab=expression(hat(U)[1]),ylab=expression(hat(V)[1]),
     cex.lab=1.2,cex.axis=1.2)
plot(vU2,vV2,xlab=expression(hat(U)[2]),ylab=expression(hat(V)[2]),
     cex.lab=1.2,cex.axis=1.2)

plt.cc(result)
plt.var(result,d1=1,d2=2,var.label=FALSE)
plt.indiv(result,d1=1,d2=2)



