
# exemplo com os dados da tabela 3.7.4 do livro Timm (2002, p. 132)
# No estudo é investigado os efeitos do atraso de práticas orais
# no começo de estudos de um idioma estrangeiro. Usando um
# grupo em que foi imposto um atraso de 4 semanas nas práticas
# orais e um grupo em que não houve atraso, foram feitas avaliações
# de quatro habilidades: auditiva (L), oral (S), leitura (R) e
# escrita (W). Os dados foram obtidos de uma prova aplicada 
# para alunos dos dois grupos ao final das seis primeiras semanas
# do estudo

# as variáveis são as seguintes
# W: notas para avaliação escrita
# S: notas para avaliação oral
# L: notas para avaliação auditiva
# R: notas para avaliação de leitura
# group: grupo do aluno, 1 para alunos que tiveram atraso de 4
#   semanas em práticas orais e 2 para controle (sem atraso)

# bibliotecas necessárias
library(car)
library(tidyverse)

# lendo os dados
practice_delay <- read.table("./Dados/practice_delay.dat",header = TRUE)
View(practice_delay)
dados <- as.data.frame(practice_delay)

head(dados)
# número de variáveis
p <- 4
# tamanhos amostrais em cada grupo
n1 <- dados %>% filter(group==1) %>% count() %>% as.numeric()
n2 <- dados %>% filter(group==2) %>% count() %>% as.numeric()

# medidas descritivas

for(i in 1:2){
  cat("Grupo: ",i,"\n")
  for(j in c("L","R","S","W")){
    cat("variável: ",j,"\n")
    dados %>% filter(group == i) %>% mutate_(col=j) %>%
      summarise(min = min(col), max=max(col), media=mean(col), sd=sd(col)) %>%
      print.table()
  }
  cat("---------\n")
}

# Boxplots
boxplot(dados$L~dados$group)
boxplot(dados$R~dados$group)
boxplot(dados$S~dados$group)
boxplot(dados$W~dados$group)

# Histogramas

par(mfrow=c(4,2),mar=c(2,2,2,2))
for(j in c("L","R","S","W")){
    for(i in 1:2){
      dados %>% filter(group == i) %>% mutate_(col=j) %>%
      with(hist(col,main=paste("variavel:",j," e  grupo:",i)))
  }
}

# gráficos de quantis-quantis com envelopes

par(mfrow=c(4,2),mar=c(2,2,2,2))
for(j in c("L","R","S","W")){
  for(i in 1:2){
    dados %>% filter(group == i) %>% mutate_(col=j) %>%
      with(qqPlot(scale(col),dist="norm",mean=0,
                  sd=1,col.lines=1,grid="FALSE",
                  xlab="quantil da N(0,1)",ylab="",
                  main=paste("variavel:",j," e  grupo:",i)))
  }
}

# avaliando a distância de Mahalanobis

s2g1 <- dados %>% filter(group == 1) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% cov()
mug1 <- dados %>% filter(group == 1) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% colMeans()
vQg1 <- dados %>% filter(group == 1) %>% select(c(L,R,S,W)) %>% 
  mahalanobis(center=mug1,cov=s2g1)
qqPlot(n1*vQg1,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",
       ylab="quantil da forma quadrática",main="grupo 1")

s2g2 <- dados %>% filter(group == 2) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% cov()
mug2 <- dados %>% filter(group == 2) %>% mutate_(col=j) %>%
  select(c(L,R,S,W)) %>% colMeans()
vQg2 <- dados %>% filter(group == 2) %>% select(c(L,R,S,W)) %>% 
  mahalanobis(center=mug2,cov=s2g2)
qqPlot(n2*vQg2,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",
       ylab="quantil da forma quadrática",main="grupo 1")


# Teste de hipóteses

n = n1 + n2  # tamanho amostral

# H0: mug1 = mug2 (vetor de médias iguais para os dois grupos)
# H1: pelo menos uma das variáveis tem médias diferentes para 
#   cada grupo

source("./MANOVA_Multivariada_ME_731_2S_2020.R")
source("./Diag Multivariate Linear Models ME 731 2S 2020.R", encoding = "windows-1252")

# vetor com o grupo referente a cada linha dos dados
v.grupos <- cbind(as.numeric(dados$group))
v.n<-rbind(n1,n2) # vetor de tamanhos amostrais
G<-2  # número de grupo
p<-4  # número de variáveis
# Teste de iguldade das matrizes de covariância
m.Sigma.P<-Box.teste.Igual.MCov(dados[,1:4],v.grupos,v.n,G)$Sigma.P
# a hipótese nula de igualdade das matrizes de covariâncias é
# rejeita ao nível 5% de significância

# Usando a MANOVA para comparar os vetores de médias nos dois grupos

# Comparação dos vetores de médias
fit.model<-m.ajuste <- manova(as.matrix(dados[,1:4]) ~ dados$group)
summary.manova(m.ajuste,test="Wilks")
summary.manova(m.ajuste,test="Pillai")
summary.manova(m.ajuste,test="Hotelling-Lawley")
summary.manova(m.ajuste,test="Roy")

# a hipotése nula é rejeita na MANOVA, mas devemos lembrar
# que ela utiliza a suposição de que as matrizes de covariâncias
# são iguais

# levando em consideração que as matrizes de covariâncias não
# devem ser iguais

# estatística de teste na equação 6.27
# do livro Johnson e Wichern (2007, 6 ed, p. 294)
T2 <- t(mug1 - mug2)%*%solve((1/n1)*s2g1 + (1/n2)*s2g2)%*%(mug1 - mug2)

# função para calcular o traço de uma matriz quadrada
tr <- function(x){ sum(diag(x)) }
# calculando os graus de liberdade fornecidos na equação 6.28
# do livro Johnson e Wichern (2007, 6 ed, p. 294)
tmp1 <- (1/n1)*s2g1%*%solve((1/n1)*s2g1 + (1/n2)*s2g2)
tmp2 <- (1/n2)*s2g2%*%solve((1/n1)*s2g1 + (1/n2)*s2g2)
nu <- (p + p^2)/(
  (1/n1)*(tr(tmp1%*%tmp1) + (tr(tmp1))^2) + 
    (1/n2)*(tr(tmp2%*%tmp2) + (tr(tmp2))^2)
)
# a condição abaixo em nu deve ser verdadeira
(min(n1,n2)<nu)&(nu<(n1+n2))
# valor crítico para a estatística de teste ao nível de 5%
# de significância
(nu*p/(nu - p + 1))*qf(.05,p,nu-p+1,lower.tail = FALSE)
# não rejeitamos H0 ao nível de 5% de significância
T2 > (nu*p/(nu - p + 1))*qf(.05,p,nu-p+1,lower.tail = FALSE)

