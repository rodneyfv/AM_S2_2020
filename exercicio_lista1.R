
library(car)

# lendo os dados
car_body <- read.table("./car_body_data.dat")
head(car_body)
summary(car_body)

# medidas descritivas
apply(car_body,2,mean)
apply(car_body,2,sd)
apply(car_body,2,min)
apply(car_body,2,max)

# histrogramas das variáveis
par(mfrow=c(3,2))
for(i in 1:6)
{
  hist(car_body[,i],main=paste(names(car_body)[i]),cex=1.2,xlab="")
}

# gráfico de quantis-quantis
par(mfrow=c(3,2))
for(i in 1:6)
{
  qqPlot(scale(car_body[,i]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantis da N(0,1)",main=names(car_body)[i],
         ylab="quantis da distribuição da distância",cex=1.2)
}

# a variável V2 é a que mais aparenta não ser normalmente distribuída

# estimando o vetor de médias e a matriz de covariâncias
vmu <- c(apply(car_body,2,mean))
mS2 <- cov(car_body)

# Teste de hipóteses

n = dim(car_body)[1]  # tamanho amostral

# queremos testar inicialmente se a média das variáveis é zero
# H0: mu1 = ... = mu6 = 0
# H1: pelo menos uma igualdade não vale
vmu0 = rep(0,6)
p = 6

#  levando em conta que a matriz de covariâncias é desconhecida,
#  usaremos a estatística T2 de Hotelling
T2 = n*t(vmu - vmu0)%*%solve(mS2)%*%(vmu - vmu0)
# usando a relação entre T2 e a distribuição F temos que
Fstat = (n-p)*T2/( (n-1)*p ); Fstat
qf(0.95,df1 = p, df2 = n-p, lower.tail = TRUE)

# rejeitamos então H0 ao nível de significância de 5%

# agora desejamos testar uma restrição do tipo mR*vmu=vb, dada por
# H0: mu3 = mu4 = mu6 = 0 e mu5 = 3/4
# H1: pelo menos uma igualdade não vale

# nesse caso, podemos usar a seguinte matriz de contraste
mR = matrix(c(0,0,1,-1,0,0,
              0,0,0,-1,0,1,
              0,0,0,0,1,0),3,6,byrow = TRUE); mR
vb = c(0,0,3/4)  # vetor com as restrições

# podemos usar a estatística de teste dada em aula
vY = mR%*%vmu
Fstat = ((n-3)*n/( (n-1)*3 ))*
  t(vY - vb)%*%solve(mR%*%mS2%*%t(mR))%*%(vY - vb); Fstat
# o valor crítico do teste será
qf(0.95,df1 = 3, df2 = n-3, lower.tail = TRUE)
# não rejeitamos H0 ao nível de 5% de significância


