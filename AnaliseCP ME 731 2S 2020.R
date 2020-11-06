# Pacotes
library(MASS)
library(car)
library(xtable)
#
# Analisando o comportamento das componentes principais
par(mfrow=c(2,2))
m.X <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1,0,0, 1), 2))
plot(m.X,xlim=c(-4,4),ylim=c(-4,4),xlab="variável 1",ylab="variável 2",cex=1.2,cex.lab=1.2,cex.main=1.2,main="Correlação = 0")
m.Y <- princomp(m.X,cor=T)$scores
plot(m.Y,xlim=c(-4,4),ylim=c(-4,4),xlab="componente principal 1",ylab="componente principal 2",cex=1.2,main="Correlação = 0")
m.X <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1,0.3,0.3, 1), 2))
plot(m.X,xlim=c(-4,4),ylim=c(-4,4),xlab="variável 1",ylab="variável 2",cex=1.2,cex.lab=1.2,cex.main=1.2,main="Correlação = 0,3")
m.Y <- princomp(m.X,cor=T)$scores
plot(m.Y,xlim=c(-4,4),ylim=c(-4,4),xlab="componente principal 1",ylab="componente principal 2",cex=1.2,main="Correlação = 0,3")
#
par(mfrow=c(2,2))
m.X <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1,0.7,0.7, 1), 2))
plot(m.X,xlim=c(-4,4),ylim=c(-4,4),xlab="variável 1",ylab="variável 2",cex=1.2,cex.lab=1.2,cex.main=1.2,main="Correlação = 0,7")
m.Y <- princomp(m.X,cor=T)$scores
plot(m.Y,xlim=c(-4,4),ylim=c(-4,4),xlab="componente principal 1",ylab="componente principal 2",cex=1.2,main="Correlação = 0,7")
m.X <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1,0.9,0.9, 1), 2))
plot(m.X,xlim=c(-4,4),ylim=c(-4,4),xlab="variável 1",ylab="variável 2",cex=1.2,cex.lab=1.2,cex.main=1.2,main="Correlação = 0,9")
m.Y <- princomp(m.X,cor=T)$scores
plot(m.Y,xlim=c(-4,4),ylim=c(-4,4),xlab="componente principal 1",ylab="componente principal 2",cex=1.2,main="Correlação = 0,9")
#
par(mfrow=c(2,2))
m.X <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1,-0.7,-0.7, 1), 2))
plot(m.X,xlim=c(-4,4),ylim=c(-4,4),xlab="variável 1",ylab="variável 2",cex=1.2,cex.lab=1.2,cex.main=1.2,main="Correlação = -0,7")
m.Y <- princomp(m.X,cor=T)$scores
plot(m.Y,xlim=c(-4,4),ylim=c(-4,4),xlab="componente principal 1",ylab="componente principal 2",cex=1.2,main="Correlação = -0,7")
m.X <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1,-0.9,-0.9, 1), 2))
plot(m.X,xlim=c(-4,4),ylim=c(-4,4),xlab="variável 1",ylab="variável 2",cex=1.2,cex.lab=1.2,cex.main=1.2,main="Correlação = -0,9")
m.Y <- princomp(m.X,cor=T)$scores
plot(m.Y,xlim=c(-4,4),ylim=c(-4,4),xlab="componente principal 1",ylab="componente principal 2",cex=1.2,main="Correlação = -0,9")

#
USArrests # conjunto de dados
colnames(USArrests)<-c("Assassinato","Assalto","PMAU","VS")
#
# Análise descritiva
#
#Medidas-resumo
#
medados<- rbind(apply(USArrests,2,mean),apply(USArrests,2,var),apply(USArrests,2,sd),100*apply(USArrests,2,sd)/apply(USArrests,2,mean),apply(USArrests,2,min),apply(USArrests,2,quantile,0.5),apply(USArrests,2,max))
rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")
xtable(medados)
# Diagrama de dispersão
plot(USArrests,cex=1.2)
# Matriz de covariâncias
#
cov(USArrests)
#
# Matriz de correlações
#
cor(USArrests)
#
# Boxplot
#
par(mfrow=c(1,1))
boxplot(USArrests,cex=1.2,cex.lab=1.2,xlab="variável")
#
# Histograma
#
par(mfrow=c(2,2))
hist(USArrests[,1],probability=TRUE,main="Assassinato",xlab="",ylab="")
hist(USArrests[,2],probability=TRUE,main="Assalto",xlab="",ylab="")
hist(USArrests[,3],probability=TRUE,main="PMAU",xlab="",ylab="")
hist(USArrests[,4],probability=TRUE,main="VS",xlab="",ylab="")
#
# qqplots
#
par(mfrow=c(2,2))
qqPlot(scale(USArrests[,1]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="Assassinato",cex=1.2,id.cex=1.2)
qqPlot(scale(USArrests[,2]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="Assalto",cex=1.2,id.cex=1.2)
qqPlot(scale(USArrests[,3]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="PMAU",cex=1.2,id.cex=1.2)
qqPlot(scale(USArrests[,4]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="VS",cex=1.2,id.cex=1.2)



###############################
# ACP via Matriz de covariância
p <- ncol(USArrests)
m.cov<-cov(USArrests)
aut.val <-  eigen(m.cov)$values
aut.vec <- -(eigen(m.cov)$vectors)
m.aut.val <- t(matrix(((aut.val)),p,p))
m.dp.var <-  (matrix(diag(sqrt(m.cov)),p,p))
result.cp.cov <- princomp(USArrests,cor=FALSE)
corr.cp.var <- aut.vec*sqrt(m.aut.val)/m.dp.var
summary(result.cp.cov)
par(mfrow=c(1,1))
screeplot(result.cp.cov,type=c("lines"),main="autovalores",cex=1.2,cex.lab=1.2,cex.main=1.2)
#
# ACP via  Matriz de Correlações
m.cor<-cor(USArrests)
aut.val <-  eigen(m.cor)$values
aut.vec <- -(eigen(m.cor)$vectors)
m.aut.val <- t(matrix(((aut.val)),p,p))
result.cp.cor <- princomp(USArrests,cor=TRUE)
corr.cp.var <- aut.vec*sqrt(m.aut.val)
summary(result.cp.cor)
screeplot(result.cp.cor,type=c("lines"),main="autovalores",cex=1.2,cex.lab=1.2,cex.main=1.2)
#
#comp.princ <- t((t(aut.vec))%*%t(USArrests))
cp1 <-  -cbind((result.cp.cor$scores)[,1])
cp2 <-  -cbind((result.cp.cor$scores)[,2])
#
# Dispersão entre as componentes
par(mfrow=c(1,1))
plot(cp1,cp2,cex=1.2,ylim=c(-3,3),xlim=c(-3,3))
#
par(mfrow=c(2,2))
boxplot(cbind(cp1,cp2),cex=1.2,cex.lab=1.2,xlab="CP")
hist(cp1,probability=TRUE,main="CP1")
hist(cp2,probability=TRUE,main="CP2")
#
par(mfrow=c(1,2))
qqPlot(scale(cp1),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP1",cex=1.2)
qqPlot(scale(cp2),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP2",cex=1.2)

# Dispersão entre as componentes
# Por estado
estados <- rownames(USArrests)
par(mfrow=c(1,1))
plot(cp1,cp2,cex=1.2)
#identify(cp1,cp2,n=50,labels=estados)
text(cp1, cp2, labels=estados, cex= 0.7)
abline(v=0.5)
abline(-0.7,0)
# Por região
par(mfrow=c(1,1))
plot(cp1,cp2,cex=1.2)
abline(v=0.5)
abline(-0.7,0)
regiao<-c("Sul","Oeste","Oeste","Sul",
           "Oeste","Oeste","Nordeste","Sul",
	   "Sul","Sul","Oeste","Oeste",
	   "Centro-Oeste","Centro-Oeste","Centro-Oeste","Centro-Oeste",
	   "Sul","Sul","Nordeste","Sul",
	   "Nordeste","Centro-Oeste","Centro-Oeste","Sul",
	   "Centro-Oeste","Oeste","Centro-Oeste","Oeste",
	   "Nordeste","Nordeste","Oeste","Nordeste",
	   "Sul","Centro-Oeste","Centro-Oeste","Sul",
	   "Oeste","Nordeste","Nordeste","Sul",
	   "Centro-Oeste","Sul","Sul","Oeste",
	   "Nordeste","Sul","Oeste","Sul",
	   "Centro-Oeste","Oeste")
#identify(cp1,cp2,n=50,labels=regiao)
text(cp1, cp2, labels=regiao, cex= 0.7)

#
par(mfrow=c(1,1))
biplot(result.cp.cor,xlim=c(-0.3,0.4))
