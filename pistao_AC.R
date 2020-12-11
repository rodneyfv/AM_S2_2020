
# bibliotecas necessárias
library(tidyverse)
library(Matrix)
library(xtable)
library(ca)

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

# dados presentes na tabela 15.5 do livro Rencher (Methods of multivariate
# analysis, p. 518). Os dados correspondem ao número de falhas em
# anéis de pistão em cada uma de três posições do pistão (norte,
# centro e sul) para quatro tipos de compressores encontrados na
# mesma construção (C1, C2, C3 e C4)

m.X <- matrix(c(17,11,11,14,
           17,9,8,7,
           12,13,19,28),nrow = 4,ncol = 3)
dimnames(m.X) <- list(c("C1","C2","C3","C4"),
                      c("Norte", "Centro", "Sul"))
names(dimnames(m.X)) <- c("Compressor","posição")

# frequências totais
m.X
#xtable(m.X)
apply(m.X,1,sum)
apply(m.X,2,sum)
sum(m.X)
# total das frequências na matriz m.X
n <- sum(apply(m.X, 1, sum))
nI <- nrow(m.X)
nJ <- ncol(m.X)
# matriz de proporções
m.P <- m.X/n; m.P
#xtable(m.P)
# soma das linhas
v.Pr <- c(m.P%*%rep(1,nJ))
round(v.Pr,2)
m.Dr <- diag(v.Pr)
# soma das colunas
v.Pc <- c(t(m.P)%*%rep(1,nI))
round(v.Pc,2)
m.Dc <- diag(v.Pc)

# perfil das linhas
m.R <- solve(m.Dr)%*%m.P
rownames(m.R) <- rownames(m.X)
xtable(m.R*100)
# perfil das colunas
m.C <- solve(m.Dc)%*%t(m.P)
rownames(m.C) <- colnames(m.X)
xtable(t(m.C)*100)

# teste de chi-quadrado
chisq.test(m.X)

# Inércia
resultCA <- ca(m.X)
inercia <- summary(resultCA)$scree # names(summary(resultCA))
xtable(cbind(inercia[,2], inercia[,2]^2,inercia[,3],cumsum(inercia[,3])),digits=4)

# Componentes
resultFCA <- plot(resultCA,xlab="componente 1",ylab="componente 2",cex=1.2)
xtable(resultFCA$rows,digits=4)
xtable(resultFCA$cols,digits=4)
biplot(resultFCA$rows,resultFCA$cols,var.axes=FALSE,xlab="componente 1", 
       ylab="componente 2",cex=1.2,xlim=c(-0.9,0.5))
abline(0,0,lty=2)
abline(v=0,lty=2)

source("./ACaux.r", encoding = "windows-1252")

# Perfis das linhas e perfis das colunas

# a função abaixo calcula a inércia e perfis das linhas e
# das colunas para uma análise de correspondência
resultaux<-ACaux(m.X)
m.R<-resultaux$m.R
m.C<-resultaux$m.C
xtable(100*m.R,digits=2)
xtable(100*m.C,digits=2)

