
library(car)
library(tidyverse)

# Dados disponíveis no livro Timm (2002, p. 213)
# Os dados foram obtidos de um estudo que envolveu 32 crianças
# do jardim de infância. O objetivo era avaliar a influência de 
# cinco testes (representados por N, S, NAc, NS e SS)
# nas notas das crianças em três provas tradicionais, donotadas aqui
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
# número de variáveis explicativas
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
# análise de resíduos

# resíduos studentizados
mRstud <- mlrm_dados$residuals%*%solve(chol(mSigma))

# resíduos vs. índice
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  plot(1:n,mRstud[,i],ylim = c(-3,3),ylab = "resíduo studentizado",
       xlab="índice",main=colnames(mRstud)[i])
  abline(h=c(-2,2),lty=2)
}

# resíduos vs. valores preditos
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  plot(mY.hat[,i],mRstud[,i],ylim = c(-3,3),ylab = "resíduo studentizado",
       xlab="valor ajustado",main=colnames(mRstud)[i])
  abline(h=c(-2,2),lty=2)
}

# histograma dos resíduos
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  hist(mRstud[,i],xlab = "resíduo studentizado",
       ylab="densidade", prob=TRUE, main=colnames(mRstud)[i])
}

# gráfico de quantis-quantis
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(i in 1:3){
  qqPlot(mRstud[,i],dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantil da N(0,1)",main=colnames(mRstud)[i],
         ylab="quantil do resíduo studentizado",cex=1.2)
}

# vamos testar se a influência das provas N e S na nota do SAT é a mesma
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

# estatística de teste
t(mCstar%*%vecBeta)%*%solve(mCstar%*%mSigmastar%*%t(mCstar))%*%(mCstar%*%vecBeta)
# nível crítico do teste ao nível de significância de 5%
qchisq(.95,dim(mC)[1]*dim(mU)[2])
# não rejeitamos a hipótese nula
