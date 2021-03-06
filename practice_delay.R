
# exemplo com os dados da tabela 3.7.4 do livro Timm (2002, p. 132)
# No estudo � investigado os efeitos do atraso de pr�ticas orais
# no come�o de estudos de um idioma estrangeiro. Usando um
# grupo em que foi imposto um atraso de 4 semanas nas pr�ticas
# orais e um grupo em que n�o houve atraso, foram feitas avalia��es
# de quatro habilidades: auditiva (L), oral (S), leitura (R) e
# escrita (W). Os dados foram obtidos de uma prova aplicada 
# para alunos dos dois grupos ao final das seis primeiras semanas
# do estudo

# as vari�veis s�o as seguintes
# W: notas para avalia��o escrita
# S: notas para avalia��o oral
# L: notas para avalia��o auditiva
# R: notas para avalia��o de leitura
# group: grupo do aluno, 1 para alunos que tiveram atraso de 4
#   semanas em pr�ticas orais e 2 para controle (sem atraso)

# bibliotecas necess�rias
library(car)
library(tidyverse)

# lendo os dados
practice_delay <- read.table("./Dados/practice_delay.dat",header = TRUE)
View(practice_delay)
dados <- as.data.frame(practice_delay)

head(dados)
# n�mero de vari�veis
p <- 4
# tamanhos amostrais em cada grupo
n1 <- dados %>% filter(group==1) %>% count() %>% as.numeric()
n2 <- dados %>% filter(group==2) %>% count() %>% as.numeric()

# medidas descritivas

for(i in 1:2){
  cat("Grupo: ",i,"\n")
  for(j in c("L","R","S","W")){
    cat("vari�vel: ",j,"\n")
    dados %>% filter(group == i) %>% mutate_(col=j) %>%
      summarise(media=round(mean(col),2), sd=round(sd(col),2), 
                var=round(var(col),4), min = min(col), 
                mediana = median(col), max=max(col), n=n()) %>%
      print.table()
  }
  cat("---------\n")
}

# Boxplots
par(mfrow=c(2,2),mar=c(4,4,2,2))
boxplot(dados$L~dados$group,ylab = "nota", xlab = "grupo", main = "vari�vel: L")
boxplot(dados$R~dados$group,ylab = "nota", xlab = "grupo", main = "vari�vel: R")
boxplot(dados$S~dados$group,ylab = "nota", xlab = "grupo", main = "vari�vel: S")
boxplot(dados$W~dados$group,ylab = "nota", xlab = "grupo", main = "vari�vel: W")

# Histogramas

par(mfrow=c(2,2),mar=c(4,4,2,2))
for(j in c("L","R","S","W")){
  dados %>% filter(group == 1) %>% mutate_(col=j) %>%
      with(hist(col,main=paste("vari�vel",j," e  grupo 1"),
                xlab = "nota", ylab = "frequ�ncia"))
}

par(mfrow=c(2,2),mar=c(4,4,2,2))
for(j in c("L","R","S","W")){
  dados %>% filter(group == 2) %>% mutate_(col=j) %>%
    with(hist(col,main=paste("vari�vel",j," e  grupo 2"),
              xlab = "nota", ylab = "frequ�ncia"))
}

# gr�ficos de dispers�o

dados %>% filter(group == 1) %>% select(L,R,S,W) %>% plot(main="grupo 1")
dados %>% filter(group == 2) %>% select(L,R,S,W) %>% plot(main="grupo 2")

# gr�ficos de quantis-quantis com envelopes

par(mfrow=c(2,2),mar=c(4,4,2,2))
for(j in c("L","R","S","W")){
  dados %>% filter(group == 1) %>% mutate_(col=j) %>%
    with(qqPlot(scale(col),dist="norm",mean=0,
                sd=1,col.lines=1,grid="FALSE",
                xlab="quantil da N(0,1)",ylab="quantil da distribui��o da vari�vel",
                main=paste("vari�vel",j," e  grupo 1")))
}

par(mfrow=c(2,2),mar=c(4,4,2,2))
for(j in c("L","R","S","W")){
  dados %>% filter(group == 2) %>% mutate_(col=j) %>%
    with(qqPlot(scale(col),dist="norm",mean=0,
                sd=1,col.lines=1,grid="FALSE",
                xlab="quantil da N(0,1)",ylab="quantil da distribui��o da vari�vel",
                main=paste("vari�vel",j," e  grupo 2")))
}

# avaliando a dist�ncia de Mahalanobis

par(mfrow=c(2,1),mar=c(4,4,2,2))
s2g1 <- dados %>% filter(group == 1) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% cov()
round(s2g1,2)
dados %>% filter(group == 1) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% cor() %>% round(2)
mug1 <- dados %>% filter(group == 1) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% colMeans()
vQg1 <- dados %>% filter(group == 1) %>% select(c(L,R,S,W)) %>% 
  mahalanobis(center=mug1,cov=s2g1)
qqPlot(n1*vQg1,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribui��o qui-quadrado",
       ylab="quantil da dist. da forma quadr�tica",main="grupo 1")

s2g2 <- dados %>% filter(group == 2) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% cov()
round(s2g2,2)
dados %>% filter(group == 2) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% cor() %>% round(2)
mug2 <- dados %>% filter(group == 2) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% colMeans()
vQg2 <- dados %>% filter(group == 2) %>% select(c(L,R,S,W)) %>% 
  mahalanobis(center=mug2,cov=s2g2)
qqPlot(n2*vQg2,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribui��o qui-quadrado",
       ylab="quantil da dist. da forma quadr�tica",main="grupo 2")


# Teste de hip�teses

n = n1 + n2  # tamanho amostral

# H0: mug1 = mug2 (vetor de m�dias iguais para os dois grupos)
# H1: pelo menos uma das vari�veis tem m�dias diferentes para 
#   cada grupo

source("./MANOVA_Multivariada_ME_731_2S_2020.R")
source("./Diag Multivariate Linear Models ME 731 2S 2020.R", encoding = "windows-1252")

# vetor com o grupo referente a cada linha dos dados
v.grupos <- cbind(as.numeric(dados$group))
v.n<-rbind(n1,n2) # vetor de tamanhos amostrais
G<-2  # n�mero de grupo
p<-4  # n�mero de vari�veis
# Teste de iguldade das matrizes de covari�ncia
m.Sigma.P<-Box.teste.Igual.MCov(dados[,1:4],v.grupos,v.n,G)$Sigma.P
# a hip�tese nula de igualdade das matrizes de covari�ncias �
# rejeita ao n�vel 5% de signific�ncia

# Usando a MANOVA para comparar os vetores de m�dias nos dois grupos

# Compara��o dos vetores de m�dias
fit.model<-m.ajuste <- manova(as.matrix(dados[,1:4]) ~ dados$group)
summary.manova(m.ajuste,test="Wilks")
summary.manova(m.ajuste,test="Pillai")
summary.manova(m.ajuste,test="Hotelling-Lawley")
summary.manova(m.ajuste,test="Roy")

# n�o rejeitamos a hipot�se nula na MANOVA

# levando em considera��o que as matrizes de covari�ncias n�o
# devem ser iguais

# estat�stica de teste na equa��o 6.27
# do livro Johnson e Wichern (2007, 6 ed, p. 294)
T2 <- t(mug1 - mug2)%*%solve((1/n1)*s2g1 + (1/n2)*s2g2)%*%(mug1 - mug2)

# fun��o para calcular o tra�o de uma matriz quadrada
tr <- function(x){ sum(diag(x)) }
# calculando os graus de liberdade fornecidos na equa��o 6.28
# do livro Johnson e Wichern (2007, 6 ed, p. 294)
tmp1 <- (1/n1)*s2g1%*%solve((1/n1)*s2g1 + (1/n2)*s2g2)
tmp2 <- (1/n2)*s2g2%*%solve((1/n1)*s2g1 + (1/n2)*s2g2)
nu <- (p + p^2)/(
  (1/n1)*(tr(tmp1%*%tmp1) + (tr(tmp1))^2) + 
    (1/n2)*(tr(tmp2%*%tmp2) + (tr(tmp2))^2)
)
# a condi��o abaixo em nu deve ser verdadeira
(min(n1,n2)<nu)&(nu<(n1+n2))
# valor cr�tico para a estat�stica de teste ao n�vel de 5%
# de signific�ncia
(nu*p/(nu - p + 1))*qf(.05,p,nu-p+1,lower.tail = FALSE)
# n�o rejeitamos H0 ao n�vel de 5% de signific�ncia
T2 > (nu*p/(nu - p + 1))*qf(.05,p,nu-p+1,lower.tail = FALSE)
# n�vel descritivo
pf(T2/(nu*p/(nu - p + 1)),p,nu-p+1,lower.tail = FALSE)

