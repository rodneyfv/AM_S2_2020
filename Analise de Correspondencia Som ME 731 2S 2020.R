library(xtable)
library(MASS)
library(ca)

# Fornecer

  # m.X : tabela de contingência

# Saída

  # v.gamma : valores singulares > 0
  # inercia : inercia associada a cada componente
  
# Tabela de contingência
m.X <- rbind(cbind(135, 140, 95, 55, 40, 60),cbind(50, 115, 40, 60, 5, 15),cbind(90, 55, 20, 35, 40, 10),cbind(60, 25, 35, 10, 5, 30),cbind(30, 20, 5, 10, 10 ,20))
dimnames(m.X) <- list(c("Sony","Aiwa","Gradiente","Philips","Sharp"),c("Qualidade","Tecnologia","Potência","Recursos","Preço","Confiança"))
names(dimnames(m.X)) <- c("Marca","                  Atributo")

# teste de chi-quadrado
chisq.test(m.X)

# Inércia
resultCA <- ca(m.X) # names(resultCA)
inercia<-summary(resultCA)$scree # names(summary(resultCA))
xtable(cbind(inercia[,2], inercia[,2]^2,inercia[,3],inercia[,4]),digits=4)

# Componentes
resultFCA <- plot(resultCA,xlab="componente 1",ylab="componente 2",cex=1.2)
xtable(resultFCA$rows,digits=4)
xtable(resultFCA$cols,digits=4)
biplot(resultFCA$rows,resultFCA$cols,var.axes=FALSE,xlab="componente 1", ylab="componente 2",cex=1.2,xlim=c(-0.9,0.5))
abline(0,0,lty=2)
abline(v=0,lty=2)

source("./ACaux.r", encoding = "windows-1252")

# Perfis das linhas e perfis das colunas
resultaux<-ACaux(m.X)
m.R<-resultaux$m.R
m.C<-resultaux$m.C
xtable(100*m.R,digits=2)
xtable(100*m.C,digits=2)

 


