
# exemplo com os dados do exemplo 6.1 do livro do Daniel Furtado
# Ferreira (1996, Análise Multivariada, p. 226). São analisadas
# quatro variedades de feijão quanto à produtividade em Kg/ha e
# número de grãos por vagem.
# as variáveis são as seguintes
# P: produtividade em Kg/ha
# NVG: número de grãos por vagem
# grupo: variedade de feijão analisada

# bibliotecas necessárias
library(car)
library(tidyverse)

# lendo os dados
feijao_dados <- read.table("./Dados/feijao_data.dat",header = TRUE)
View(feijao_dados)
dados <- as.data.frame(feijao_dados)

head(dados)
# número de variáveis
p <- 2
# tamanhos amostrais em cada grupo
v.n <- as.numeric(table(dados$grupo))

# medidas descritivas
for(i in 1:4){
  cat("Grupo: ",i,"\n")
  for(j in c("P","NGV")){
    cat("variável: ",j,"\n")
    dados %>% filter(grupo == i) %>% mutate_(col=j) %>%
      summarise(min = min(col), max=max(col), media=mean(col), sd=sd(col)) %>%
      print.table()
  }
  cat("---------\n")
}

# Boxplots
boxplot(dados$P~dados$grupo)
boxplot(dados$NGV~dados$grupo)

# Histogramas

par(mfrow=c(4,2),mar=c(2,2,2,2))
for(j in c("P","NGV")){
    for(i in 1:4){
      dados %>% filter(grupo == i) %>% mutate_(col=j) %>%
      with(hist(col,main=paste("variavel:",j," e  grupo:",i)))
  }
}

# gráficos de quantis-quantis com envelopes

par(mfrow=c(4,2),mar=c(2,2,2,2))
for(j in c("P","NGV")){
  for(i in 1:4){
    dados %>% filter(grupo == i) %>% mutate_(col=j) %>%
      with(qqPlot(scale(col),dist="norm",mean=0,
                  sd=1,col.lines=1,grid="FALSE",
                  xlab="quantil da N(0,1)",ylab="",
                  main=paste("variavel:",j," e  grupo:",i)))
  }
}

# avaliando a distância de Mahalanobis

s2g1 <- dados %>% filter(grupo == 1) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% cov()
mug1 <- dados %>% filter(grupo == 1) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% colMeans()
vQg1 <- dados %>% filter(grupo == 1) %>% select(c(P,NGV)) %>% 
  mahalanobis(center=mug1,cov=s2g1)
qqPlot(v.n[1]*vQg1,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",
       ylab="quantil da forma quadrática",main="grupo 1")

s2g2 <- dados %>% filter(grupo == 2) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% cov()
mug2 <- dados %>% filter(grupo == 2) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% colMeans()
vQg2 <- dados %>% filter(grupo == 2) %>% select(c(P,NGV)) %>% 
  mahalanobis(center=mug2,cov=s2g2)
qqPlot(v.n[2]*vQg2,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",
       ylab="quantil da forma quadrática",main="grupo 1")

s2g3 <- dados %>% filter(grupo == 3) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% cov()
mug3 <- dados %>% filter(grupo == 3) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% colMeans()
vQg3 <- dados %>% filter(grupo == 3) %>% select(c(P,NGV)) %>% 
  mahalanobis(center=mug3,cov=s2g3)
qqPlot(v.n[3]*vQg3,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",
       ylab="quantil da forma quadrática",main="grupo 1")

s2g4 <- dados %>% filter(grupo == 4) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% cov()
mug4 <- dados %>% filter(grupo == 4) %>% mutate_(col=j) %>%
  select(c(P,NGV)) %>% colMeans()
vQg4 <- dados %>% filter(grupo == 4) %>% select(c(P,NGV)) %>% 
  mahalanobis(center=mug4,cov=s2g4)
qqPlot(v.n[4]*vQg4,dist="chisq",df=p,col.lines=1,grid="FALSE",
       xlab="quantil da distribuição qui-quadrado",
       ylab="quantil da forma quadrática",main="grupo 1")


# Teste de hipóteses

n = sum(v.n)  # tamanho amostral

# H0: mug1 = mug2 (vetor de médias iguais para os dois grupos)
# H1: pelo menos uma das variáveis tem médias diferentes para 
#   cada grupo

source("./MANOVA_Multivariada_ME_731_2S_2020.R")
source("./Diag Multivariate Linear Models ME 731 2S 2020.R", 
       encoding = "windows-1252")

# vetor com o grupo referente a cada linha dos dados
v.grupos <- cbind(as.numeric(dados$grupo))
G <-4  # número de grupo
p <-2  # número de variáveis
# Teste de iguldade das matrizes de covariância
m.Sigma.P<-Box.teste.Igual.MCov(dados[,1:2],v.grupos,v.n,G)$Sigma.P
# a hipótese nula de igualdade das matrizes de covariâncias é
# rejeita ao nível 5% de significância

# Usando a MANOVA para comparar os vetores de médias nos dois grupos

# Comparação dos vetores de médias
fit.model<-m.ajuste <- manova(as.matrix(dados[,1:2]) ~ dados$grupo)
summary.manova(m.ajuste,test="Wilks")
summary.manova(m.ajuste,test="Pillai")
summary.manova(m.ajuste,test="Hotelling-Lawley")
summary.manova(m.ajuste,test="Roy")

# a hipotése nula é rejeita na MANOVA ao nível de significância de 5%


