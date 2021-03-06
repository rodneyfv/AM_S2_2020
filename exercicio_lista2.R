
library(car)
library(tidyverse)

# Dados dispon�veis no livro Timm (2002, p. 213)
# Os dados foram obtidos de um estudo que envolveu 32 crian�as
# do jardim de inf�ncia. O objetivo era avaliar a influ�ncia de 
# cinco testes (representados por N, S, NAc, NS e SS)
# nas notas das crian�as em tr�s provas tradicionais, donotadas aqui
# por PPVT, RPMT e SAT.


# lendo os dados
criancas <- read.table("./criancas.dat", header = TRUE)
head(criancas)
summary(criancas)
# tamanho amostral
n <- nrow(criancas)

dados <- data.frame(criancas)

dados %>% select(PPVT, RPMT, SAT) %>% cor()
dados %>% select(PPVT, RPMT, SAT) %>% plot()

mY <- dados %>% select(PPVT, RPMT, SAT) %>% as.matrix()
mX <- dados %>% select(N, S, NS, NAc, SS) %>% as.matrix()
# n�mero de vari�veis explicativas
p <- ncol(mX)

# ajustando o modelo linear multivariado
mlrm_dados <- lm( mY ~ mX)
summary(mlrm_dados)
mlrm_dados$coefficients
mSigma <- (1/(n-p))*t(mlrm_dados$residuals)%*%mlrm_dados$residuals
mBeta <- mlrm_dados$coefficients
m1X <- cbind(rep(1,n),mX)
mY.hat <- m1X%*%mBeta

#######
# an�lise de res�duos

# res�duos studentizados
mRstud <- mlrm_dados$residuals%*%solve(chol(mSigma))

# res�duos vs. �ndice
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  plot(1:n,mRstud[,i],ylim = c(-3,3),ylab = "res�duo studentizado",
       xlab="�ndice",main=colnames(mRstud)[i])
  abline(h=c(-2,2),lty=2)
}

# res�duos vs. valores preditos
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  plot(mY.hat[,i],mRstud[,i],ylim = c(-3,3),ylab = "res�duo studentizado",
       xlab="valor ajustado",main=colnames(mRstud)[i])
  abline(h=c(-2,2),lty=2)
}

# histograma dos res�duos
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  hist(mRstud[,i],xlab = "res�duo studentizado",
       ylab="densidade", prob=TRUE, main=colnames(mRstud)[i])
}

# gr�fico de quantis-quantis
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  qqPlot(mRstud[,i],dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantil da N(0,1)",main=colnames(mRstud)[i],
         ylab="quantil do res�duo studentizado",cex=1.2)
}

# vamos testar se a influ�ncia das provas N e S na nota do SAT � a mesma
# H0: \beta_{2,3} - \beta_{3,3} = 0
# H1: \beta_{2,3} - \beta_{3,3} != 0

# utilizando os termos do slide 42 da parte 1 sobre MNLM
mC = matrix(c(0,1,-1,0,0,0),ncol=6)
mU= matrix(c(0,0,1),nrow = 3)
mC%*%mBeta%*%mU
vecBeta = matrix(c(mBeta),ncol = 1)
dim(vecBeta)
mCstar = kronecker(mC,t(mU))
dim(mCstar)
mSigmastar = kronecker(solve(t(m1X)%*%m1X),mSigma)
dim(mSigmastar)

# estat�stica de teste
t(mCstar%*%vecBeta)%*%solve(mCstar%*%mSigmastar%*%t(mCstar))%*%(mCstar%*%vecBeta)
# n�vel cr�tico do teste ao n�vel de signific�ncia de 5%
qchisq(.95,dim(mC)[1]*dim(mU)[2])
# n�o rejeitamos a hip�tese nula
