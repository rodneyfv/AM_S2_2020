ACC.mult <- function()
{
c.p <- ncol(m.X) # variaveis matriz 1
c.q <- ncol(m.Y) # variaveis matriz 2
m.corX <- cor(m.X)
m.corY <- cor(m.Y)
m.corXY1 <- cor(m.X,m.Y)
m.corXY2 <- t(m.corXY1)

# matriz rq  para X
aux.1 <- eigen(m.corX)
aux.eigen.1 <- aux.1$values
aux.vector.1 <- aux.1$vectors
m.rqcorX <- aux.vector.1%*%((diag(sqrt(aux.eigen.1),c.p,c.p)))%*%(t(aux.vector.1))
m.irqcorX<-  aux.vector.1%*%(solve((diag(sqrt(aux.eigen.1),c.q,c.q))))%*%(t(aux.vector.1))

# matriz rq  para Y
aux.2 <- eigen(m.corY)
aux.eigen.2 <- aux.2$values
aux.vector.2 <- aux.2$vectors
m.rqcorY <- aux.vector.2%*%((diag(sqrt(aux.eigen.2),c.p,c.p)))%*%(t(aux.vector.2))
m.irqcorY<-  aux.vector.2%*%(solve((diag(sqrt(aux.eigen.2),c.q,c.q))))%*%(t(aux.vector.2))

# Matriz para se extrair as correlações canônicas
m.aux.CC.1 <-   m.irqcorX%*%m.corXY1%*%solve(m.corY)%*%m.corXY2%*%m.irqcorX  # X
m.aux.CC.2 <-   m.irqcorY%*%m.corXY2%*%solve(m.corX)%*%m.corXY1%*%m.irqcorY  # Y
aux.eigen.CC.1 <- eigen(m.aux.CC.1)
aux.eigen.CC.2 <- eigen(m.aux.CC.2)
v.aux.eigen <- cbind((aux.eigen.CC.1)$values)
v.aux.vector.1 <- aux.eigen.CC.1$vectors   # vetores por colunas
v.aux.vector.2 <- -aux.eigen.CC.2$vectors
v.CC <- sqrt(v.aux.eigen)
m.A<-m.CC.1 <- sinal1*(t(v.aux.vector.1))%*%m.irqcorX   # m.A
m.B <- m.CC.2 <- sinal2*(t(v.aux.vector.2))%*%m.irqcorY   # m.B
i.m.A <- solve(m.A)
i.m.B <- solve(m.B)

# Correlações entre variáveis canônicas e originais

m.CC1.Z1 <- m.A%*%m.corX
m.CC2.Z2 <- m.B%*%m.corY

# Percentual das somas das variâncias explicada
p.Z1.U <- sum(m.CC1.Z1^2)/4
p.Z2.V <- sum(m.CC2.Z2^2)/4
m.Z.1 <- cbind(scale(m.X))
m.Z.2 <- cbind(scale(m.Y))
m.U <- t(m.A[1:2,]%*%t(m.Z.1))
m.V <- t(m.B[1:2,]%*%t(m.Z.2))

result.ACC.mult <- list(v.CC=v.CC,m.A=m.A,m.B=m.B,m.CC1.Z1=m.CC1.Z1,m.CC2.Z2=m.CC2.Z2,m.U=m.U,m.V=m.V)

return(result.ACC.mult)

}


