# Bibliotecas necessarias
library(plotrix)
library(plyr)
library(car)
library(mice)
library(xtable)

# Os dados consistem em distâncias medidas em jovens de ambos os sexos 
# e com idades entre 8, 10, 12 e 14 anos. O objetivo do estudo era
# descrever tais distâncias em função das idades e comparar as diferenças
# dessas medidas em garotos e garotas
data(potthoffroy)
dados <- potthoffroy
summary(dados)
# mudando a nomenclatura da variável sex
levels(dados$sex)<-list(Feminino=c("F"),Masculino=c("M"))
# mudando o nome da variável sex para genero
colnames(dados)[2]<-c("genero")
inames<-c("idade 8","idade 10","idade 12","idade 14") 
nvar<-4
n<-nrow(dados)
#
# Preparação do banco de dados para o cálculo de medidas descritivas e 
# para o gráfico de perfis médios
# vetor com as idades que correspondem às distâncias d8, d10. d12 e d14
vanos <-c(8,10,12,14)
dadosm <-cbind(dados[,3:ncol(dados)])
vgenero <- dados[,2]
# criando uma versão empilhada dos dados, em que os anos ficam como
# uma variável ao invés de 4 colunas
dadosl <- cbind(as.numeric(dadosm[1,]),vgenero[1],vanos,1)
for (i in 2: nrow(dadosm))
{
  dadosl <- rbind(dadosl,cbind(as.numeric(dadosm[i,]),vgenero[i],vanos,i))
}
dadosl<-data.frame(dadosl)
# auxgenero terá os gêneros em dadosl no formato string
auxgenero <- dadosl[,2]
auxgenero[auxgenero==1] <- "Feminino"
auxgenero[auxgenero==2] <- "Masculino"
#
colnames(dadosl)<-c("Distancia","Genero","Ano","Individuo")
dadosl$Genero <- as.factor(dadosl$Genero)
levels(dadosl$Genero)<-list(Feminino=c("1"),Masculino=c("2"))
genero <- dadosl[,2]
#
distancia <- dadosl[,1]
ano <- dadosl[,3]
indiv <- dadosl[,4]
anof <-as.factor(ano)
nano <- nrow(dadosm)  # número de observações por ano
######################
# Medidas descritivas por ano e gênero
datadados <- data.frame(distancia,genero,anof)
medados <- ddply(datadados,.(genero,anof),summarise,media=mean(distancia),
                 dp=sqrt(var(distancia)),vari=var(distancia),
                 cv=100*((sqrt(var(distancia))/mean(distancia))),
                 minimo=min(distancia),mediana=quantile(distancia,0.5),
                 maximo=max(distancia),n=length(distancia))
medados
xtable(medados)
##########################
# Gráfico de perfis médios em cada ano e gênero
mmeanIC <- rbind(medados$media+qnorm(0.025)*medados$dp/sqrt(medados$n),
                 medados$media-qnorm(0.025)*medados$dp/sqrt(medados$n))
#
plotCI(vanos,medados$media[medados$genero == "Feminino"],
       li=mmeanIC[1,medados$genero == "Feminino"],
       ui=mmeanIC[2,medados$genero == "Feminino"],pch=19,slty=1,lwd=2,
       xlab="ano",ylab="distância",cex=1.2,cex.axis=1.2,cex.lab=1.2,
       ylim=c(min(mmeanIC),max(mmeanIC)))
lines(vanos,medados$media[medados$genero == "Feminino"],lwd=2)
#
plotCI(vanos,medados$media[medados$genero == "Masculino"],
       li=mmeanIC[1,medados$genero == "Masculino"],
       ui=mmeanIC[2,medados$genero == "Masculino"],pch=23,slty=1,lwd=2,
       xlab="anos",ylab="distância",cex=1.2,cex.axis=1.2,cex.lab=1.2,add=TRUE,
       scol=2,pt.bg=2,col=2)
lines(vanos,medados$media[medados$genero == "Masculino"],lwd=2,col=2)
#
legend(8,27,col=c(1,2),lwd=c(2,2),pch=c(19,23),pt.bg=c(1,2),
       legend=c("Feminino","Masculino"),bty="n",cex=1.5)

######################
# Medidas descritivas gerais
datadados<-data.frame(distancia,anof)
medados<-ddply(datadados,.(anof),summarise,media=mean(distancia),
               dp=sqrt(var(distancia)),vari=var(distancia),
               cv=100*((sqrt(var(distancia))/mean(distancia))),
               minimo=min(distancia),mediana=quantile(distancia,0.5),
               maximo=max(distancia),n=length(distancia))
medados
xtable(medados)
##########################
# Gráfico de perfis médios para cada ano
mmeanIC <- rbind(medados$media+qnorm(0.025)*medados$dp/sqrt(medados$n),
                 medados$media-qnorm(0.025)*medados$dp/sqrt(medados$n))
#
plotCI(vanos,medados$media,li=mmeanIC[1,],ui=mmeanIC[2,],pch=19,slty=1,lwd=2,
       xlab="ano",ylab="distância",cex=1.2,cex.axis=1.2,cex.lab=1.2,
       ylim=c(min(mmeanIC),max(mmeanIC)))
lines(vanos,medados$media,lwd=2)

# Vetor de médias e matriz de covariâncias
mx<- as.matrix(dados[,3:6])# só os dados
plot(dados[,3:6])
vmu<-apply(mx,2,mean)
s2 <- cov(mx)
#
# Envelopes para cada variável e testes ks
par(mfrow=c(2,2)) 
for (j in 1:nvar)
{
  qqPlot(scale(dados[j+2]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",
         xlab="quantis da N(0,1)",main=inames[j],
         ylab="quantis da distribuição da distância",cex=1.2)
}
ks.test(scale(dados[3]),"pnorm",0,1)
ks.test(scale(dados[4]),"pnorm",0,1)
ks.test(scale(dados[5]),"pnorm",0,1)
ks.test(scale(dados[6]),"pnorm",0,1)
#
# Envelopes para a forma quadrática
par(mfrow=c(1,1))
mmu <- t(matrix(t(vmu),nvar,n))
#vF <- apply(((mx-vmu)*(mx-vmu)%*%solve(s2)),1,sum)
#vF<- (n-nvar)*vF/((n-1)*nvar)
#qqPlot(vF,dist="f",df1=nvar,df2=n-nvar,col.lines=1,grid="FALSE",xlab="quantis da distribui??o F",ylab="quantis da forma quadr?tica",cex=1.2,id.cex=1.2)
#vQ <- n*apply(((mx-vmu)*(mx-vmu)%*%solve(s2)),1,sum)
vQ <- n* mahalanobis(mx,center=vmu,cov=s2)
qqPlot(vQ,dist="chisq",df=nvar,col.lines=1,grid="FALSE",
       xlab="quantis da distribuição qui-quadrado",
       ylab="quantis da forma quadrática",cex=1.2)

######################
# Testes de Hipóteses
######################
#
# Teste para um vetor de médias

# os arquivos abaixo devem estar na mesma pasta do código 
# Inf_DNM_dados_Potthof_Roy_ME_731_2S_2020.R
source("./TH_DNM_ME_731_2S_2020.R")
source("./MANOVA_Multivariada_ME_731_2S_2020.R")
# vetor de médias de interesse
vmu0<- rbind(22,23,25,26)
#
# Sigma conhecido
teste.mu.SigmaC(mx,vmu0,s2,0.05)
# Sigma desconhecido
teste.mu.SigmaD(mx,vmu0,0.05)
#
# Teste para combinações lineares de vetores de médias
# Hipótese (1): igualdade das distâncias de crianças com 8 e 10
R <- cbind(1,-1,0,0)
b<-0
teste.Rmub.SigmaC(mx,R,b,s2,0.05)
teste.Rmub.SigmaD(mx,R,b,0.05)
# Hipótese (2): igualdade das distâncias de crianças com 10 e 12
R <- cbind(0,1,-1,0)
b<-0
teste.Rmub.SigmaC(mx,R,b,s2,0.05)
teste.Rmub.SigmaD(mx,R,b,0.05)
# Hipótese (3): igualdade das distâncias de crianças com 12 e 14
R <- cbind(0,0,1,-1)
b<-0
teste.Rmub.SigmaC(mx,R,b,s2,0.05)
teste.Rmub.SigmaD(mx,R,b,0.05)
#
# Teste para igualdade de vetores de médias
# Considerando os gêneros
vgrupos<-cbind(as.numeric(dados[,2]))
Delta <- rbind(0,0,0,0)
alpha<-0.05
teste.mu1mu2.Homocedast(mx,vgrupos,Delta,alpha)
#
# Teste para verificação da homocedasticidade
G<-2
v.n <- rbind(11,16)
#bartlet.teste.Igual.MCov(mx,vgrupos,G,v.n)
Box.teste.Igual.MCov(mx,vgrupos,v.n,G)






