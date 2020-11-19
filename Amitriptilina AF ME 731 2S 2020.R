library(car)
library(xtable)
library(corrplot)

#
dados<-read.table(file="./Dados/Amitripilina.txt")
colnames(dados) <- c("Tot","Ami","Gen","Amt","Pr","Diap","Qrs")
dadosdf <- data.frame(dados)
nvar<-6
#genero <- as.factor(dados$Gen)
genero <- dados$Gen
genero[genero==1] <- "feminino"
genero[genero==0] <- "masculino"
genero <- as.factor(genero)
#

# Matriz de correlações
corrplot(cor(dados[,-3]), order = "hclust", tl.col='black', tl.cex=.75) 

#
# Método das componentes principais
mx <- dadosdf[,c(1,2,4,5,6,7)]
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

# Diferença entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferença entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(1-vcomunal),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(1-vcomunal)))


# Método de Máxima verossimilhança

# Matriz de correlações
# Matriz de dados
resultAF<-factanal(x=mx,factors=2,rotation="none")
mcarga <- resultAF$loadings[1:6,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,resultAF$uniquenesses)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)
apply(mcarga^2,2,sum)

# Diferença entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferença entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(resultAF$uniquenesses),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(resultAF$uniquenesses)))

# Matriz de correlações
#factanal(covmat=cov(mx),nobs=220,factors=2,rotation="none")
# Matriz de dados
resultAF<-factanal(x=mx,factors=2,rotation="varimax")
mcarga <- resultAF$loadings[1:6,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,1-vcomunal)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)
apply(mcarga^2,2,sum)

# Diferença entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferença entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(1-vcomunal),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(1-vcomunal)))

# Matriz de correlações
#factanal(covmat=cov(mx),nobs=220,factors=2,rotation="none")
# Matriz de dados
resultAF<-factanal(x=mx,factors=2,rotation="promax")
mcarga <- resultAF$loadings[1:6,]
mcargaMV <- mcarga
vcomunal <- diag(mcarga[,1:2]%*%t(mcarga[,1:2]))
result <- cbind(mcarga[,1:2],vcomunal,1-vcomunal)
xtable(result,digits=3)
LLt <- mcarga%*%t(mcarga)
apply(mcarga^2,2,sum)

# Diferença entre Sigma - LL' 
xtable(m.cor -  LLt,digits=3)
Qmres <- mean(abs(m.cor -  LLt))
#
# Diferença entre Sigma - LL' + Psi
xtable(m.cor -  LLt -  diag(1-vcomunal),digits=3)
Qmres <- mean(abs(m.cor -  LLt -  diag(1-vcomunal)))

# Escores Fatoriais
# Escores fatoriais métoo de mínimos quadrados ponderados
resultAFMQP<-factanal(x=mx,factors=2,rotation="none",scores = c("Bartlett"))
resultAFMR<-factanal(x=mx,factors=2,rotation="none",scores = c("regression"))
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

#
par(mfrow=c(1,1))
plot(escfatMQP[,1],escfatMQP[,2],xlab="fator 1",ylab="fator 2",cex=1.2,
     cex.lab=1.2,col=genero,pch=19)
legend(2,2,col=c(1,2),pch=c(19,19),pt.bg=c(1,2),
       legend=c("Feminino","Masculino"),bty="n",cex=1.5)

# Medidas resumo
medados1<- rbind(apply(escfatMQP[genero=="feminino",],2,mean),
                 apply(escfatMQP[genero=="feminino",],2,var),
                 apply(escfatMQP[genero=="feminino",],2,sd),
                 apply(escfatMQP[genero=="feminino",],2,min),
                 apply(escfatMQP[genero=="feminino",],2,quantile,0.5),
                 apply(escfatMQP[genero=="feminino",],2,max))
medados2<- rbind(apply(escfatMQP[genero=="masculino",],2,mean),
                 apply(escfatMQP[genero=="masculino",],2,var),
                 apply(escfatMQP[genero=="masculino",],2,sd),
                 apply(escfatMQP[genero=="masculino",],2,min),
                 apply(escfatMQP[genero=="masculino",],2,quantile,0.5),
                 apply(escfatMQP[genero=="masculino",],2,max))
medados <- cbind(medados1,medados2)
rownames(medados)<-c("Média","Var.","DP","Mínimo","Mediana","Máximo")
xtable(medados)

# Biplot

biplot(resultAFMQP$scores,resultAFMQP$loadings,xlabs=genero,
       xlab="Fator 1",ylab="Fator 2")



