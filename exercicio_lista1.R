
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

# histrogramas das vari�veis
par(mfrow=c(3,2))
for(i in 1:6)
{
  hist(car_body[,i],main=paste(names(car_body)[i]),cex=1.2,xlab="")
}

# gr�fico de quantis-quantis
par(mfrow=c(3,2))
for(i in 1:6)
{
  qqPlot(scale(car_body[,i]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantis da N(0,1)",main=names(car_body)[i],
         ylab="quantis da distribui��o da dist�ncia",cex=1.2)
}

# a vari�vel V2 � a que mais aparenta n�o ser normalmente distribu�da

# estimando o vetor de m�dias e a matriz de covari�ncias
vmu <- c(apply(car_body,2,mean))
mS2 <- cov(car_body)

# Teste de hip�teses

n = dim(car_body)[1]  # tamanho amostral

# queremos testar inicialmente se a m�dia das vari�veis � zero
# H0: mu1 = ... = mu6 = 0
# H1: pelo menos uma igualdade n�o vale
vmu0 = rep(0,6)
p = 6

#  levando em conta que a matriz de covari�ncias � desconhecida,
#  usaremos a estat�stica T2 de Hotelling
T2 = n*t(vmu - vmu0)%*%solve(mS2)%*%(vmu - vmu0)
# usando a rela��o entre T2 e a distribui��o F temos que
Fstat = (n-p)*T2/( (n-1)*p ); Fstat
qf(0.95,df1 = p, df2 = n-p, lower.tail = TRUE)

# rejeitamos ent�o H0 ao n�vel de signific�ncia de 5%

# agora desejamos testar uma restri��o do tipo mR*vmu=vb, dada por
# H0: mu3 = mu4 = mu6 = 0 e mu5 = 3/4
# H1: pelo menos uma igualdade n�o vale

# nesse caso, podemos usar a seguinte matriz de contraste
mR = matrix(c(0,0,1,-1,0,0,
              0,0,0,-1,0,1,
              0,0,0,0,1,0),3,6,byrow = TRUE); mR
vb = c(0,0,3/4)  # vetor com as restri��es

# podemos usar a estat�stica de teste dada em aula
vY = mR%*%vmu
Fstat = ((n-3)*n/( (n-1)*3 ))*
  t(vY - vb)%*%solve(mR%*%mS2%*%t(mR))%*%(vY - vb); Fstat
# o valor cr�tico do teste ser�
qf(0.95,df1 = 3, df2 = n-3, lower.tail = TRUE)
# n�o rejeitamos H0 ao n�vel de 5% de signific�ncia


