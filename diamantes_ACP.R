
# bibliotecas necessárias
library(tidyverse)
library(Matrix)

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


# ACP via Matriz de covariância
p <- ncol(dados)
m.cov <- cov(dados)
aut.val <-  eigen(m.cov)$values
round(aut.val,2)
aut.vec <- -(eigen(m.cov)$vectors)
xtable(round(aut.vec,2))
m.aut.val <- t(matrix(((aut.val)),p,p))
m.dp.var <-  (matrix(sqrt(diag(m.cov)),p,p))
result.cp.cov <- princomp(dados,cor=FALSE)
corr.cp.var <- aut.vec*sqrt(m.aut.val)/m.dp.var
summary(result.cp.cov)
par(mfrow=c(1,1))
screeplot(result.cp.cov,type=c("lines"),main="autovalores")

# ACP via  Matriz de Correlações
m.cor<-cor(dados)
aut.val <-  eigen(m.cor)$values
round(aut.val,2)
aut.vec <- -(eigen(m.cor)$vectors)
xtable(round(aut.vec,2))
m.aut.val <- t(matrix(((aut.val)),p,p))
result.cp.cor <- princomp(dados,cor=TRUE)
corr.cp.var <- aut.vec*sqrt(m.aut.val)
summary(result.cp.cor)
screeplot(result.cp.cor,type=c("lines"),main="autovalores",cex=1.2,cex.lab=1.2,cex.main=1.2)
t(round(aut.vec[,1:2],2))
t(round(corr.cp.var[,1:2],2))
dim(result.cp.cor$scores)

cp1 <-  -cbind((result.cp.cor$scores)[,1])
cp2 <-  -cbind((result.cp.cor$scores)[,2])

boxplot(cbind(cp1,cp2),cex.lab=1.2,xlab="CP")
par(mfrow=c(1,2))
hist(cp1,probability=TRUE, xlab = "CP1", ylab = "Densidade", main = "")
hist(cp2,probability=TRUE, xlab = "CP2", ylab = "Densidade", main = "")
# Dispersão entre as componentes levando em consideração a
# variável qualidade do corte
par(mfrow=c(1,1))
plot(cp1,cp2,cex=.2,ylim=c(-6,6),xlim=c(-8,6),col = c(labels=diamonds$cut))
legend(3,-2,col=c(1:5),pch=rep(19,5),
       legend=c("Médio","Bom","Muito bom","Premium","Ideal"),
       bty="n",cex=1,title = paste("Qualidade \n do corte"))

par(mfrow=c(1,1))
biplot(result.cp.cor,xlim=c(-0.02,0.05),cex=.8)

