
# bibliotecas necessárias
library(tidyverse)
library(Matrix)
library(xtable)

medidasResumo <- function(x){
  medados<- rbind(apply(x,2,mean),
                  apply(x,2,var),
                  apply(x,2,sd),
                  apply(x,2,min),
                  apply(x,2,quantile,0.5),
                  apply(x,2,max))
  rownames(medados)<-c("Média","Var.","DP","Mínimo","Mediana","Máximo")
  return(medados)
}


data("diamonds")
head(diamonds)
dim(diamonds)
n <- nrow(diamonds)

dados <- diamonds %>% select(carat, depth, table, price, x, y, z)
colnames(dados) <- c("quilate", "profundidade", "tabela",
                     "preço","x","y","z")

medados<- rbind(round(apply(dados,2,mean),2),round(apply(dados,2,var),2),
                round(apply(dados,2,sd),2),
                round(100*apply(dados,2,sd)/apply(dados,2,mean),2),
                apply(dados,2,min),apply(dados,2,quantile,0.5),
                apply(dados,2,max))
rownames(medados)<-c("Média","Var.","DP","CV(%)","Mínimo","Mediana","Máximo")
t(medados)

# Diagrama de dispersão
plot(dados[sample(1:n,5000,replace = FALSE),],cex=.5,cex.lab=1)
# Boxplot
par(mfrow=c(2,2),mar=c(4,2,2,2))
for(i in 1:4){
  boxplot(dados[,i],cex=1.2,cex.lab=1.2,xlab=colnames(dados)[i])
}
par(mfrow=c(2,2),mar=c(4,2,2,2))
for(i in 5:7){
  boxplot(dados[,i],xlab=colnames(dados)[i])
}

# Histograma
par(mfrow=c(4,2),mar=c(4,2,2,2))
for(i in 1:7){
  hist(dados[[i]],probability=TRUE,main=colnames(dados)[i],xlab="",ylab="")
}

# Matriz de covariâncias
round(cov(dados),2)
# Matriz de correlações
round(cor(dados),2)

round(triu(cov(dados)) + tril(cor(dados) - diag(rep(1,7))),2)


# Método das componentes principais
p <- ncol(dados)
m.cor <- cor(dados)
aut.val <- eigen(m.cor)$values
aut.vec <- (eigen(m.cor)$vectors)
m.aut.val <- t(matrix(((aut.val)),p,p))
mcarga <- sqrt(m.aut.val)*(-aut.vec)
round(aut.val,2)
round(100*aut.val/p,2)
round(cumsum(100*aut.val/p),2)

# número de fatores considerado
m <- 2
vcomunal <- diag(mcarga[,1:m]%*%t(mcarga[,1:m]))
result <- cbind(mcarga[,1:m],vcomunal,1-vcomunal)
rownames(result) <- names(dados)
round(result,3)
#xtable(result,digits=3)

LLt <- mcarga[,1:m]%*%t(mcarga[,1:m])

# Diferença entre Sigma - LL' + Psi
round(m.cor -  LLt -  diag(1-vcomunal),3)
#xtable(m.cor -  LLt -  diag(1-vcomunal),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(1-vcomunal)))


# Método de Máxima verossimilhança

# Matriz de correlações
m <- 2
resultAF<-factanal(x=dados,factors=m,rotation="none")
mcarga <- resultAF$loadings[1:p,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:m]%*%t(mcarga[,1:m]))
result <- cbind(mcarga[,1:m],vcomunal,resultAF$uniquenesses)
round(result, 3)
#xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)
apply(mcarga^2,2,sum)
100*apply(mcarga^2,2,sum)/p
cumsum(100*apply(mcarga^2,2,sum))/p

# Diferença entre Sigma - LL' + Psi
round(m.cor -  LLt -  diag(resultAF$uniquenesses), 3)
#xtable(m.cor -  LLt -  diag(resultAF$uniquenesses),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(resultAF$uniquenesses)))


# Usando a rotação varimax
m <- 2
resultAF<-factanal(x=dados,factors=m,rotation="varimax")
mcarga <- resultAF$loadings[1:p,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:m]%*%t(mcarga[,1:m]))
result <- cbind(mcarga[,1:m],vcomunal,resultAF$uniquenesses)
round(result, 3)
#xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)
apply(mcarga^2,2,sum)
100*apply(mcarga^2,2,sum)/p
cumsum(100*apply(mcarga^2,2,sum))/p

# Diferença entre Sigma - LL' + Psi
round(m.cor -  LLt -  diag(resultAF$uniquenesses), 3)
#xtable(m.cor -  LLt -  diag(resultAF$uniquenesses),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(resultAF$uniquenesses)))


# Usando a rotação promax
m <- 2
resultAF<-factanal(x=dados,factors=m,rotation="promax")
mcarga <- resultAF$loadings[1:p,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:m]%*%t(mcarga[,1:m]))
result <- cbind(mcarga[,1:m],vcomunal,resultAF$uniquenesses)
round(result, 3)
#xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)
apply(mcarga^2,2,sum)
100*apply(mcarga^2,2,sum)/p
cumsum(100*apply(mcarga^2,2,sum))/p

# Diferença entre Sigma - LL' + Psi
round(m.cor -  LLt -  diag(resultAF$uniquenesses), 3)
#xtable(m.cor -  LLt -  diag(resultAF$uniquenesses),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(resultAF$uniquenesses)))


# Escores Fatoriais
resultAFMQP<-factanal(x=dados,factors=m,rotation="none",scores = c("Bartlett"))
resultAFMR<-factanal(x=dados,factors=m,rotation="none",scores = c("regression"))
escfatMQP <- resultAFMQP$scores
escfatMR <- resultAFMR$scores
colnames(escfatMQP)<-c("fator 1","fator 2")
colnames(escfatMR)<-c("fator 1","fator 2")

# Gráfico de dispersão 2
par(mfrow=c(1,2))
plot(escfatMQP[,1],escfatMR[,1],xlab="fator 1 - MQP",ylab="fator 1 - MR",
     cex=1.2,cex.lab=1.2)
abline(0,1)
plot(escfatMQP[,2],escfatMR[,2],xlab="fator 2 - MQP",ylab="fator 2 - MR",
     cex=1.2,cex.lab=1.2)
abline(0,1)

par(mfrow=c(1,1))
plot(escfatMQP[,1],escfatMQP[,2],xlab="fator 1",ylab="fator 2",
     col = c(labels=diamonds$cut),pch=19,ylim=c(-14,14))
legend("bottomleft",col=c(1:5),pch=rep(19,5),
       legend=c("Médio","Bom","Muito bom","Premium","Ideal"),
       bty="n",cex=.8,title = paste("Qualidade \n do corte"))


medados1 <- medidasResumo(escfatMQP[diamonds$cut=="Fair",])
medados2 <- medidasResumo(escfatMQP[diamonds$cut=="Good",])
medados3 <- medidasResumo(escfatMQP[diamonds$cut=="Very Good",])
medados4 <- medidasResumo(escfatMQP[diamonds$cut=="Premium",])
medados5 <- medidasResumo(escfatMQP[diamonds$cut=="Ideal",])

medados <- cbind(medados1,medados2,medados3,medados4,medados5)
round(medados,2)
xtable(medados, digits = 2)


