
# bibliotecas necess�rias
library(tidyverse)
library(Matrix)
library(xtable)
library(rARPACK)

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

# dados sobre neuroblastoma
# dispon�vel em: 
# "https://orfe.princeton.edu/~jqfan/fan/classes/525/DataSets/Neuroblastoma/neuroblastoma.csv"

neuroblastoma <- read.table("neuroblastoma.csv",header = TRUE, sep = ",")
sobrevivencia <- neuroblastoma$Event.free
dados <- neuroblastoma %>% select(-c("X","Event.free"))
rm(neuroblastoma)
dim(dados)
# n�mero de pacientes
n <- nrow(dados)
# n�mero de locais onde a express�o gen�tica foi medida
p <- ncol(dados)
# informa��o de sobreviv�ncia dos pacientes
# 0: positivo, 1: negativo
table(sobrevivencia)

dados[1:5,1:10]


# M�todo das componentes principais
m.cor <- cor(dados)

# n�mero de fatores considerado
m <- 2
decEspec <- eigs(m.cor, m, which = "LM")
rm(m.cor)
aut.val <- decEspec$values
aut.vec <- decEspec$vectors
mcarga <- cbind(sqrt(aut.val[1])*(-aut.vec[,1]),
                sqrt(aut.val[2])*(-aut.vec[,2]))
round(aut.val,2)
round(100*aut.val/p,2)
round(cumsum(100*aut.val/p),2)

vcomunal <- diag(mcarga[,1:m]%*%t(mcarga[,1:m]))
result <- cbind(mcarga[,1:m],vcomunal,1-vcomunal)
rownames(result) <- names(dados)
round(result,3)
#xtable(result,digits=3)

