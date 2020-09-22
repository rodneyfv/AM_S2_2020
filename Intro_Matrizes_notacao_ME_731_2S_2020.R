# Criando uma matriz A
A = matrix(c(1,2,3,0,1,4,5,6,0),ncol=3,nrow=3,byrow=TRUE)
A
# transposta da matriz
t(A)
# determinante da matriz
det(A)
# inversa da matriz
solve(A)
# multiplicação entre as matrizes A e B
B = matrix(c(7,2,1,0,3,-1,-3,4,-2),ncol=3,nrow=3,byrow=TRUE)
B
A%*%B
# produto de Kronecker entre matrizes A e B
B = matrix(c(1,0,-1,2),ncol=2,nrow=2,byrow=TRUE)
kronecker(A,B)
# decomposição de A em autovalores e autovetores
eigen(A)
Lambda = eigen(A)$value  # autovalores
E = eigen(A)$vectors  # autovetores
E%*%diag(Lambda)%*%solve(E)
# decomposição de Cholesky
A = matrix(c(4,12,-16,12,37,-43,-16,-43,98),ncol=3,nrow=3,byrow=TRUE)
A
L = chol(A); L # note que aqui L é diagonal superior
t(L)%*%L
