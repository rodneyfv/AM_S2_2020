######################
# Pacotes necessários
library(car)
library(xtable)
library(plyr)
#
###############
# Dados da iris
especie <- iris[,5]
n <- nrow(iris)
mx<-as.matrix(iris[,1:4])
nvar<-4
inames <- c("comprimento da sépala","largura da sépala","comprimento da pétala","largura da pétala")
irisd <- iris[,1:4]
colnames(irisd)<-c("CS","LS","CP","LP")
irisaux <- iris
colnames(irisaux)<-c("CS","LS","CP","LP","Especie")

# programas de diagnóstico para modelos de regressão normal lineares
source("./diag_norm.r", encoding = "windows-1252")
source("./envel_norm.r", encoding = "windows-1252")
# Teste CB = M
source("./Testes Cbeta.r", encoding = "windows-1252")

#rownames(irisd) <- as.vector(iris[,5])

scatterplotMatrix(~ CS + LS + CP + LP | Especie, data=irisaux, smooth=FALSE, 
                  reg.line=FALSE, ellipse=FALSE,by.groups=TRUE, diagonal="none",cex=1.2)

#
#
#
###############################
# ACP via Matriz de correlações
p <- ncol(irisd)
m.cor<-cor(irisd)
aut.val <-  eigen(m.cor)$values
aut.vec <- (eigen(m.cor)$vectors)
m.aut.val <- t(matrix(((aut.val)),p,p))
result.cp.cor <- princomp(irisd,cor=TRUE)
corr.cp.cor <- aut.vec*sqrt(m.aut.val)
summary(result.cp.cor)
screeplot(result.cp.cor,type=c("lines"),main="autovalores",cex=1.2,cex.lab=1.2,cex.main=1.2)
#
cp1 <-  cbind((result.cp.cor$scores)[,1])
cp2 <-  cbind((result.cp.cor$scores)[,2])
#
# Dispersão entre as componentes
#
especie <- as.character(irisaux[,5]) 
par(mfrow=c(1,1))
plot(cp1,cp2,cex=1.2,col = c(irisaux[,5]),pch=19)
legend(-1,-1,col=c(1,2,3),pch=c(19,19,19),legend=c("setosa","versicolor","virginica"),bty="n",cex=1.2)
#
# Por grupo
par(mfrow=c(1,2))
boxplot(cp1~especie,cex=1.2,cex.lab=1.2,xlab="CP1")
boxplot(cp2~especie,cex=1.2,cex.lab=1.2,xlab="CP2")
par(mfrow=c(2,2))
hist(cp1[especie=="setosa"],probability=TRUE,main="CP1",xlab="",ylab="")
hist(cp1[especie=="versicolor"],probability=TRUE,main="CP1",xlab="",ylab="")
hist(cp1[especie=="virginica"],probability=TRUE,main="CP1",xlab="",ylab="")
par(mfrow=c(2,2))
hist(cp2[especie=="setosa"],probability=TRUE,main="CP2",xlab="",ylab="")
hist(cp2[especie=="versicolor"],probability=TRUE,main="CP2",xlab="",ylab="")
hist(cp2[especie=="virginica"],probability=TRUE,main="CP2",xlab="",ylab="")
#
par(mfrow=c(2,2))
qqPlot(scale(cp1[especie=="setosa"]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP1",cex=1.2,main="setosa")
qqPlot(scale(cp1[especie=="versicolor"]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP1",cex=1.2,main="versicolor")
qqPlot(scale(cp1[especie=="virginica"]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP1",cex=1.2,main="virginica")
par(mfrow=c(2,2))
qqPlot(scale(cp2[especie=="setosa"]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP2",cex=1.2,main="setosa")
qqPlot(scale(cp2[especie=="versicolor"]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP2",cex=1.2,main="versicolor")
qqPlot(scale(cp2[especie=="virginica"]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab="CP2",cex=1.2,main="virginica")
#
#
par(mfrow=c(1,1))
biplot(result.cp.cor,xlabs=iris[,5])

#biplot.psych {psych}
especief<-iris[,5]
# Componente 1
fit.model<- lm(cp1~especief)
tsi<-diagnorm(fit.model)

par(mfrow=c(1,2))
boxplot(tsi,ylab="Resíduo Studentizado",cex=1.1,cex.axis=1.1,cex.lab=1.1)
envelnorm(fit.model)
summary(fit.model)
xtable(summary(fit.model))
#
m.C <- cbind(0,1,-1)
m.M <-0
testeF.CBM(fit.model,m.C,m.M)
# Médias preditas pelo modelo
vbeta<- cbind(as.numeric(coef(fit.model)))
mcovbeta <- vcov(fit.model)
mCm <- rbind(cbind(1,0,0),cbind(1,1,0),cbind(1,0,1)) 
mmedia <- mCm%*%vbeta
epmedia <- cbind(sqrt(diag(mCm%*%mcovbeta%*%t(mCm))))
quant <-qt(0.975,df=150-3)
ICmedia <- cbind(mmedia-quant*epmedia,mmedia+quant*epmedia)
mresult <- cbind(mmedia,epmedia,ICmedia)
xtable(mresult)

#
# Componente 2
fit.model<- lm(cp2~especief)
tsi<-diagnorm(fit.model)
par(mfrow=c(1,2))
boxplot(tsi,ylab="Resíduo Studentizado",cex=1.1,cex.axis=1.1,cex.lab=1.1)
envelnorm(fit.model)
summary(fit.model)
xtable(summary(fit.model))
# Modelo reduzido
# Componente 2
especief2 <- especie
especief2[especie=="setosa"]<-"setosa-virginica"
especief2[especie=="virginica"]<-"setosa-virginica"
especief2 <-as.factor(especief2)
#
fit.model<- lm(cp2~especief2)
tsi<-diagnorm(fit.model)

par(mfrow=c(1,2))
boxplot(tsi,ylab="Resíduo Studentizado",cex=1.1,cex.axis=1.1,cex.lab=1.1)
#source("C:\\Users\\cnaber\\Trabalho\\windows\\Unicamp\\Disciplinas\\2_semestre_2017\\ME 731\\Programas\\envel_norm.txt")
envelnorm(fit.model)
summary(fit.model)
xtable(summary(fit.model))
#
# Médias preditas pelo modelo
vbeta<- cbind(as.numeric(coef(fit.model)))
mcovbeta <- vcov(fit.model)
mCm <- rbind(cbind(1,0),cbind(1,1)) 
mmedia <- mCm%*%vbeta
epmedia <- cbind(sqrt(diag(mCm%*%mcovbeta%*%t(mCm))))
quant <-qt(0.975,df=150-3)
ICmedia <- cbind(mmedia-quant*epmedia,mmedia+quant*epmedia)
mresult <- cbind(mmedia,epmedia,ICmedia)
xtable(mresult)
