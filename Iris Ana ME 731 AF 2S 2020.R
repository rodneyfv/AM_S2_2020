######################
# Pacotes necessários
library(car)
library(xtable)
library(plyr)
library(plotrix)
library(corrplot)

# limpar todas as variáveis 
rm(list = ls(all.names = TRUE))


#
###############
# Dados da iris
especie <- iris[,5]
n <- nrow(iris)
mx<-as.matrix(iris[,1:4])
nvar<-4
inames <- c("comprimento da sépala","largura da sépala","comprimento da pétala",
            "largura da pétala")
irisd <- iris[,1:4]
colnames(irisd)<-c("CS","LS","CP","LP")
irisaux <- iris
colnames(irisaux)<-c("CS","LS","CP","LP","Especie")

# Matriz de correlações
corrplot(cor(irisd), order = "hclust", tl.col='black', tl.cex=.75) 


# Solução e componentes principais
# Método das componentes principais
p <- ncol(irisd)
m.cor<-cor(irisd)
aut.val <-  eigen(m.cor)$values
aut.vec <- (eigen(m.cor)$vectors)
m.aut.val <- t(matrix(((aut.val)),p,p))
mcarga <- sqrt(m.aut.val)*(aut.vec)
100*aut.val/nvar
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,1-vcomunal)
xtable(result,digits=3)
LLt <- mcarga[,1:2]%*%t(mcarga[,1:2])

# Diferença entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferença entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(1-vcomunal),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(1-vcomunal)))

# Matriz de correlações
# Sem rotação

resultAF<-factanal(x=irisd,factors=1,rotation="none")
mcarga <- cbind(resultAF$loadings[1:4,])
vcomunal <- diag(mcarga%*%t(mcarga))
result <- cbind(mcarga,vcomunal,resultAF$uniquenesses)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)

# Diferença entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferença entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(resultAF$uniquenesses),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(resultAF$uniquenesses)))

# Escores fatoriais 
resultAFMQP<-factanal(x=mx,factors=1,rotation="none",scores = c("Bartlett"))
resultAFMR<-factanal(x=mx,factors=1,rotation="none",scores = c("regression"))
escfatMQP <- resultAFMQP$scores
escfatMR <- resultAFMR$scores

# Dispersão entre os escores fatoriais
plot(escfatMQP,escfatMR,xlab="MQP",ylab="MR",cex=1.2,cex.lab=1.2,pch=19)
abline(0,1)

# medidas descritivas
datadados<-data.frame(escfatMQP,especie)
colnames(datadados)<-c("Fator","especie")
medados<-ddply(datadados,.(especie),summarise,media=mean(Fator),
               dp=sqrt(var(Fator)),vari=var(Fator),
               cv=100*((sqrt(var(Fator))/mean(Fator))),minimo=min(Fator),
               mediana=quantile(Fator,0.5),maximo=max(Fator),n=length(Fator))
xtable(medados)

# Dispersão por grupo
plot(escfatMQP,pch=19,cex=1.2,col=especie,xlab="índice",ylab = "escore fatorial")
legend(1,0.5,pch=c(19,19,19),col=c(1,2,3),
       legend=c("setosa","versicolor","virginica"),bty="n",cex=1.2)

# Boxplot
par(mfrow=c(1,1))
boxplot(escfatMQP~especie,cex=1.2,cex.lab=1.2,xlab="espécie",
        ylab="escore fatorial")

# Histograma
par(mfrow=c(2,2))
hist(escfatMQP[especie=="setosa"],probability=TRUE,cex.lab=1.2,
     xlab="escore fatorial",ylab="densidade",main="setosa")
hist(escfatMQP[especie=="versicolor"],probability=TRUE,cex.lab=1.2,
     xlab="escore fatorial",ylab="densidade",main="versicolor")
hist(escfatMQP[especie=="virginica"],probability=TRUE,cex.lab=1.2,
     xlab="escore fatorial",ylab="densidade",main="virginica")

# Densidades suavizadas
par(mfrow=c(1,1))
plot(density(escfatMQP[especie=="setosa"]),lwd=2,xlim=c(-1.9,2.2),
     xlab="escore fatorial",ylab="densidade",main="")
lines(density(escfatMQP[especie=="versicolor"]),col=2,lwd=2)
lines(density(escfatMQP[especie=="virginica"]),col=3,lwd=2)
legend(1,3,lwd=c(2,2,2),col=c(1,2,3),
       legend=c("setosa","versicolor","virginica"),bty="n",cex=1.2)


# qqplots
#
par(mfrow=c(2,2))
qqPlot(scale(escfatMQP[especie=="setosa"]),dist="norm",mean=0,sd=1,col.lines=1,
       grid="FALSE",xlab="quantil da N(0,1)",ylab="Escore fatorial",
       cex=1.2,main="setosa")
qqPlot(scale(escfatMQP[especie=="versicolor"]),dist="norm",mean=0,sd=1,
       col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",
       ylab="Escore fatorial",cex=1.2,main="versicolor")
qqPlot(scale(escfatMQP[especie=="virginica"]),dist="norm",mean=0,sd=1,
       col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",
       ylab="Escore fatorial",cex=1.2,main="virginica")

# Modelo linear para comparar os grupos
source("./diag_norm.r", encoding = "windows-1252")
source("./envel_norm.r", encoding = "windows-1252")
source("./Testes Cbeta.r", encoding = "windows-1252")

fit.model <-lm(escfatMQP~especie)
summary(fit.model)
tsi<-diagnorm(fit.model)
par(mfrow=c(1,2))
boxplot(tsi,ylab="Resíduo Studentizado",cex=1.1,cex.axis=1.1,cex.lab=1.1)
envelnorm(fit.model)
#summary(fit.model)
xtable(summary(fit.model))
# Teste CB = M
m.C <- cbind(0,1,-1)
m.M <-0
testeF.CBM(fit.model,m.C,m.M)
#
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
par(mfrow=c(1,1))
plotCI(mmedia,ui=ICmedia[,2],li=ICmedia[,1],axes=FALSE,xlab="espécie",
       ylab="média",pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2)
axis(2,seq(-2,2,0.2),cex.axis=1.2)
axis(1,1:3,c("setosa","versicolor","virginica"),cex.axis=1.2)


