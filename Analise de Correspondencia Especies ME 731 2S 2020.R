library(xtable)
library(MASS)
library(ca)

# Fornecer

  # m.X : tabela de contingência

# Saída

  # v.gamma : valores singulares > 0
  # inercia : inercia associada a cada componente
  
# Tabela de contingência
m.X <- rbind(cbind(0,1,	0	,0,	14,	5),cbind(2,	1	,0,	0	,4,	3),cbind(40,	9	,31	,9,	18,	30),cbind(81	,47	,1,	2,	51,	23),cbind(68,	3	,0,	12,	39,	5))
dimnames(m.X) <- list(c("Ásia","Europa","América","Oceania","África"),c("Moluscos","Insetos","Peixes","Répteis","Aves","Mamíferos"))
names(dimnames(m.X)) <- c("Continente","                  Espécie")

# teste de chi-quadrado
chisq.test(m.X)

# Inércia
resultCA <- ca(m.X)
inercia<-summary(resultCA)$scree
xtable(cbind(sqrt(inercia[,2]),inercia[,2],inercia[,3],inercia[,4]),digits=4)

# Componentes
resultFCA <- plot(resultCA,xlab="componente 1",ylab="componente 2")
xtable(resultFCA$rows,digits=4)
xtable(resultFCA$cols,digits=4)
biplot(resultFCA$rows,resultFCA$cols,var.axes=FALSE,xlab="componente 1", ylab="componente 2",cex=1.2)
abline(0,0,lty=2)
abline(v=0,lty=2)

# perfis das linhas e das colunas
source("./ACaux.r", encoding = "windows-1252")
#
resultaux<-ACaux(m.X)
m.R<-resultaux$m.R
m.C<-resultaux$m.C
xtable(100*m.R,digits=2)
xtable(100*m.C,digits=2)

