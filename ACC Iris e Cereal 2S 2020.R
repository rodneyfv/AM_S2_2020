library(MASS)
library(CCA)
library(xtable)
library(car)
library(plyr)
library(corrplot)

set.seed(33)

# limpar todas as vari�veis 
rm(list = ls(all.names = TRUE))

# Iris de Fisher
especie <- iris[,5]
especieaux<-revalue(especie,c("setosa"="S","versicolor"="VE","virginica" = "VI"))

inames <- c("comprimento da s�pala","largura da s�pala","comprimento da p�tala","largura da p�tala")
irisd <- iris[,1:4]
colnames(irisd)<-c("CS","LS","CP","LP")

m.X <- irisd[,1:2] # s�pala
m.Y <- irisd[,3:4] # p�tala
corY <- cor(m.Y)
corX <- cor(m.X)
corXY <- cor(cbind(m.X),cbind(m.Y))
#
result <- cc(scale(m.X),scale(m.Y))

# Gr�fico de correla��es
corrplot(cor(irisd), order = "hclust", tl.col='black', tl.cex=.75) 

# Correla��es can�nicas
result$cor

# Coeficientes
#(U)
mA <- cbind(result$xcoef)
#(V)
mB <- cbind(result$ycoef)

# Correla��es entre as vari�veis can�nicas e as vari�veis originais
# (U,X(1))
corUX1<-(result$scores$corr.X.xscores)
# (V,X(2))
corVX2<-(result$scores$corr.Y.yscores)


xtable(cbind(mA[,1],corUX1[,1],mA[,2],corUX1[,2]))
xtable(cbind(mB[,1],corVX2[,1],mB[,2],corVX2[,2]))


# Representa��o das matrizes de correla��o (com um �nico par)
imA <-solve(t(mA))
imB <-solve(t(mB))
#
mat1<-corX -cbind(imA[,1])%*%rbind(imA[,1])
mat2<-corY -cbind(imB[,1])%*%rbind(imB[,1])
mat3<-corXY - result$cor[1]*(cbind(imA[,1])%*%rbind(imB[,1]))
xtable(mat1)
xtable(mat2)
xtable(mat3)
#xtable(cbind(mat1,mat2,mat3))

#
# percentual da soma das vari�ncias representada pelo primeiro par de var. can�nicas
sum(diag(cbind(imA[,1])%*%rbind(imA[,1])))/sum(diag(corX))
sum(diag(cbind(imB[,1])%*%rbind(imB[,1])))/sum(diag(corY))

# Vari�veis can�nicas

vU1  <-  result$scores$xscores[,1]
vU2  <-  result$scores$xscores[,2]
vV1  <-  result$scores$yscores[,1]
vV2  <-  result$scores$yscores[,2]

plot(vU1,vV1,xlab="primeira vari�vel can�nica",ylab="segunda vari�vel can�nica",type="n",cex.lab=1.2,cex.axis=1.2)
text(vU1,vV1,labels=especieaux,col=as.numeric(especie))

plt.cc(result,var.label=TRUE,ind.names=especieaux)
plt.var(result,d1=1,d2=2,var.label=TRUE)
plt.indiv(result,d1=1,d2=2,ind.names=especieaux)

# plt.var
# S�o usadas as correla��es entre as mesmas variaveis originais e as can�nicas
# mesmas vari�veis pois assim pode-se avaliar a estrura de correla��o entre elas
# plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),xlab = paste("Dimension ", 1), ylab = paste("Dimension ",2))
#
# result$scores$corr.X.xscores: correla��o entre a componente U e as vari�veis X
# result$scores$corr.X.xscores[, i] - U_i e X
# result$scores$corr.Y.xscores: correla��o entre a componente V e as vari�veis X
# result$scores$corr.Y.xscores[, j] - V_i e X
# points(result$scores$corr.X.xscores[, 1], result$scores$corr.X.xscores[,2], pch = 20, cex = 1.2, col = "red")
# points(result$scores$corr.Y.xscores[, 1], result$scores$corr.Y.xscores[,2], pch = 24, cex = 0.7, col = "blue")
# points(result$scores$corr.X.xscores[, d1], result$scores$corr.X.xscores[,d2], pch = 20, cex = 1.2, col = "red")
# points(result$scores$corr.Y.xscores[, d1], result$scores$corr.Y.xscores[,d2], pch = 24, cex = 0.7, col = "blue")
#
#
# plt.indiv
# dispers�o entre as as primeiras vari�veis can�nicas (U1,U2)
# plot(result$scores$xscores[, 1], result$scores$xscores[, 2], type = "n", main = "", xlab = paste("Dimension ", 1),ylab = paste("Dimension ", 2))
# text(result$scores$xscores[, 1], result$scores$xscores[, 2], especieaux)
# plot(result$scores$xscores[, d1], result$scores$xscores[, d2], type = "n", main = "", xlab = paste("Dimension ", d1),ylab = paste("Dimension ", d2))



#fit.model<-lm(vU1~vV1)
#source("E:\\windows\\Unicamp\\Disciplinas\\2_semestre_2015\\ME 731\\Programas\\diag_norm.txt")
#par(mfrow=c(1,2))
#boxplot(tsi,ylab="Res�duo Studentizado",cex=1.1,cex.axis=1.1,cex.lab=1.1)
#source("E:\\windows\\Unicamp\\Disciplinas\\2_semestre_2015\\ME 731\\Programas\\envel_norm.txt")


# Dados de cereais

UScereal

marca <- UScereal[,1]

# separando as matrizes
#m.X <- cbind(UScereal[,2:4],UScereal[,6])
#colnames(m.X)<- rbind("calorias","proteina","gordura","fibra")
m.X <- cbind(UScereal[,2:4],UScereal[,6:8])
colnames(m.X)<- rbind("calorias","proteina","gordura","fibra","carboidrato","a�ucar")
#m.Y <- cbind(UScereal[,5],UScereal[,7:8],UScereal[,10])
#colnames(m.Y) <- rbind("sodio","carboidrato","acucar","potassio")
m.Y <- cbind(UScereal[,5],UScereal[,10])
colnames(m.Y) <- rbind("sodio","potassio")

# An�lise descritiva
medados<- rbind(apply(m.X,2,mean),apply(m.X,2,var),apply(m.X,2,sd),100*apply(m.X,2,sd)/apply(m.X,2,mean),apply(m.X,2,min),apply(m.X,2,quantile,0.5),apply(m.X,2,max))
rownames(medados)<-c("M�dia","Var.","DP","CV(%)","M�nimo","Mediana","M�ximo")
xtable(medados)
#
medados<- rbind(apply(m.Y,2,mean),apply(m.Y,2,var),apply(m.Y,2,sd),100*apply(m.Y,2,sd)/apply(m.Y,2,mean),apply(m.Y,2,min),apply(m.Y,2,quantile,0.5),apply(m.Y,2,max))
rownames(medados)<-c("M�dia","Var.","DP","CV(%)","M�nimo","Mediana","M�ximo")
xtable(medados)

# Gr�fico de correla��es
corrplot(cor(cbind(m.X,m.Y)), order = "hclust", tl.col='black', tl.cex=.75) 

# An�lise descritiva por fabricante

#datadados<-data.frame(mX,marca)
#medados<-ddply(datadados,.(marca),summarise,media=mean(LP),dp=sqrt(var(LP)),vari=var(LP),cv=100*((sqrt(var(LP))/mean(LP))),minimo=min(LP),mediana=quantile(LP,0.5),maximo=max(LP),n=length(LP))
#xtable(medados)


#
# Matrizes de dispers�o
#
#plot(cbind(m.X,m.Y),pch=19)
pairs(cbind(m.X,m.Y),pch=19,cex.labels=1.8)

#
plot(cbind(m.X),pch=19)
#
plot(cbind(m.Y),pch=19)
#
#Boxplot
par(mfrow=c(2,3))
for (i in 1:6)
{
  boxplot(m.X[,i],xlab=c(names(m.X)[i]),cex=1.4,cex.lab=1.4)
}
#
par(mfrow=c(1,2))
for (i in 1:2)
{
  boxplot(m.Y[,i],xlab=c(colnames(m.Y)[i]),cex=1.4,cex.lab=1.4)
}
#Histograma
par(mfrow=c(2,3))
for (i in 1:6)
{
  hist(m.X[,i],probability=TRUE,main=c(names(m.X)[i]),cex=1.4,cex.lab=1.4,ylab="densidade",xlab="valores")
}
#
par(mfrow=c(1,2))
for (i in 1:2)
{
  hist(m.Y[,i],probability=TRUE,main=c(colnames(m.Y)[i]),cex=1.4,cex.lab=1.4,ylab="densidade",xlab="valores")
}

# Qqplot
# qqplots
#
par(mfrow=c(2,3))
for (i in 1:6)
{
  qqPlot(scale(m.X[,i]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="quantil da distribui��o de interesse",main=c(names(m.X)[i]),cex=1.4)
}
#
par(mfrow=c(1,2))
for (i in 1:2)
{
  qqPlot(scale(m.Y[,i]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="quantil da distribui��o de interesse",main=c(colnames(m.Y)[i]),cex=1.4)
}



par(mfrow=c(1,1))
#
nvar<-8
mXmY <-as.matrix(cbind(m.X,m.Y))
vmu <- c(apply(mXmY,2,mean))
s2 <- cov(mXmY)
n0<-nrow(mXmY)
mmu <- t(matrix(t(vmu),nvar,n0))
#vQ <- apply(((mXmY-vmu)*(mXmY-vmu)%*%solve(s2)),1,sum)
#vQ<- n0*vF#(n0-nvar)*vF/((n0-1)*nvar)
#qqPlot(vF,dist="f",df1=nvar,df2=n0-nvar,col.lines=1,grid="FALSE",xlab="quantil da distribui��o F",ylab="quantil da forma quadr�tica",cex=1.2,id.cex=1.2)
#qqPlot(vF,dist="chisq",df=nvar,col.lines=1,grid="FALSE",xlab="quantil da distribui��o qui-quadrado",ylab="quantil da forma quadr�tica",cex=1.2,id.cex=1.2)
# vQ1 <- as.numeric(n0*apply((mXmY-mmu)*((mXmY-mmu)%*%solve(s2)),1,sum))
vQ<- n0*mahalanobis(mXmY,center=vmu,cov=s2)
qqPlot(vQ,dist="chisq",df=nvar,col.lines=1,grid="FALSE",xlab="quantil da distribui��o qui-quadrado",ylab="quantil da forma quadr�tica",cex=1.2)

# An�lise descritiva por fabricante

datadados<-data.frame(m.X,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(calorias),dp=sqrt(var(calorias)),vari=var(calorias),cv=100*((sqrt(var(calorias))/mean(calorias))),minimo=min(calorias),mediana=quantile(calorias,0.5),maximo=max(calorias),n=length(calorias))
xtable(medados)
#
datadados<-data.frame(m.X,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(proteina),dp=sqrt(var(proteina)),vari=var(proteina),cv=100*((sqrt(var(proteina))/mean(proteina))),minimo=min(proteina),mediana=quantile(proteina,0.5),maximo=max(proteina),n=length(proteina))
xtable(medados)
#
datadados<-data.frame(m.X,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(gordura),dp=sqrt(var(gordura)),vari=var(gordura),cv=100*((sqrt(var(gordura))/mean(gordura))),minimo=min(gordura),mediana=quantile(gordura,0.5),maximo=max(gordura),n=length(gordura))
xtable(medados)
#
datadados<-data.frame(m.X,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(fibra),dp=sqrt(var(fibra)),vari=var(fibra),cv=100*((sqrt(var(fibra))/mean(fibra))),minimo=min(fibra),mediana=quantile(fibra,0.5),maximo=max(fibra),n=length(fibra))
xtable(medados)
#
datadados<-data.frame(m.X,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(carboidrato),dp=sqrt(var(carboidrato)),vari=var(carboidrato),cv=100*((sqrt(var(carboidrato))/mean(carboidrato))),minimo=min(carboidrato),mediana=quantile(carboidrato,0.5),maximo=max(carboidrato),n=length(carboidrato))
xtable(medados)
#
datadados<-data.frame(m.X,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(a�ucar),dp=sqrt(var(a�ucar)),vari=var(a�ucar),cv=100*((sqrt(var(a�ucar))/mean(a�ucar))),minimo=min(a�ucar),mediana=quantile(a�ucar,0.5),maximo=max(a�ucar),n=length(a�ucar))
xtable(medados)
#
datadados<-data.frame(m.Y,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(sodio),dp=sqrt(var(sodio)),vari=var(sodio),cv=100*((sqrt(var(sodio))/mean(sodio))),minimo=min(sodio),mediana=quantile(sodio,0.5),maximo=max(sodio),n=length(sodio))
xtable(medados)
#
datadados<-data.frame(m.Y,marca)
medados<-ddply(datadados,.(marca),summarise,media=mean(potassio),dp=sqrt(var(potassio)),vari=var(potassio),cv=100*((sqrt(var(potassio))/mean(potassio))),minimo=min(potassio),mediana=quantile(potassio,0.5),maximo=max(potassio),n=length(potassio))
xtable(medados)


# An�lise de correla��o can�nica

result<-cc(scale(m.X),scale(m.Y))

# Correla��es can�nicas
result$cor

# Coeficientes
#(U)
mA <- cbind(result$xcoef[,1])
#(V)
mB <- cbind(result$ycoef[,2])


# Diferen�a entre as matrizes de correla��o observadas e estimadas pelas vari�veis can�nicas
#mcorX <- cor(m.X)
#mcorY <- cor(m.Y)
#mcorA <- mA%*%t(mA)
#mcorB <- mB%*%t(mB)
#
#mcorX - mcorA
#mcorY - mcorB

# Percentual da soma das vari�ncias representado pelas vari�veis can�nicas


# Correla��es entre as vari�veis can�nicas e as vari�veis originais
# (U,X(1))
corUX1<-(result$scores$corr.X.xscores)[,1]
# (V,X(2))
corUX2<-(result$scores$corr.Y.yscores)[,1]

xtable(cbind(mA,corUX1))
xtable(cbind(mB,corUX2))

# Vari�veis can�nicas

vU1  <-  result$scores$xscores[,1]
vU2  <-  result$scores$xscores[,2]
vV1  <-  result$scores$yscores[,1]
vV2  <-  result$scores$yscores[,2]

plot(vU1,vV1,xlab="primeira vari�vel can�nica",ylab="segunda vari�vel can�nica",type="n",cex.lab=1.2,cex.axis=1.2)
text(vU1,vV1,labels=marca,col=as.numeric(marca))

plt.cc(result,var.label=TRUE,ind.names=marca)
plt.var(result,d1=1,d2=2,var.label=TRUE)
plt.indiv(result,d1=1,d2=2,ind.names=marca)

#biplot(cbind(vU1,vV1),cbind(mA,mB),xlabs=marca,xlab="Vari�vel Can�nica 1",ylab="Vari�vel can�nica 2")

#par(mfrow=c(1,1))
#biplot(cbind(vU1,vU2),cbind(cbind(result$xcoef[,1]),cbind(result$xcoef[,2])),xlabs=marca,xlab="Vari�vel Can�nica 1",ylab="Vari�vel can�nica 2")

#biplot(cbind(vU1,vU2),cbind(mA[,1],mA[,2]),xlabs=marca,xlab="Vari�vel Can�nica 1",ylab="Vari�vel can�nica 2")

#par(mfrow=c(1,1))
#biplot(cbind(vV1,vV2),cbind(mB[,1],mB[,2]),xlabs=marca,xlab="Vari�vel Can�nica 1",ylab="Vari�vel can�nica 2")
#biplot(CCorA(m.X, m.Y, stand.Y=TRUE, stand.X=TRUE),xlabs=marca)
