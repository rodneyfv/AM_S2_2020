library(car)
library(xtable)
source("./Diag Multivariate Linear Models ME 731 2S 2020.R", encoding = "windows-1252")
#
dados<-read.table(file="./Dados/Amitripilina.txt", encoding = "windows-1252")
colnames(dados) <- c("Tot","Ami","Gen","Amt","Pr","Diap","Qrs")
dadosdf <- data.frame(dados)
#xtable(dados)
#
par(mfrow=c(1,1))
plot(dadosdf$Ami,dadosdf$Tot,pch=19,cex.axis=1.2,cex.lab=1.2,cex=1.2,xlab="Ami",ylab="Tot")
cor(dadosdf$Ami,dadosdf$Tot)
#
par(mfrow=c(1,2))
plot(dadosdf$Amt,dadosdf$Tot,pch=19,cex.axis=1.2,cex.lab=1.2,cex=1.2,xlab="Amt",ylab="Tot")
plot(dadosdf$Amt,dadosdf$Ami,pch=19,cex.axis=1.2,cex.lab=1.2,cex=1.2,xlab="Amt",ylab="Ami")
#
#plot(dadosdf$Pr,dadosdf$Tot)
#plot(dadosdf$Pr,dadosdf$Ami)

# A função lm, no caso multivariado, trabalha com o vec(B) e não com o Vec(B')
fit.model<-lm(cbind(dadosdf$Tot,dadosdf$Ami)~Amt,data=dadosdf)
summary(fit.model)
m.ajuste<-fit.manova<-manova(cbind(dadosdf$Tot,dadosdf$Ami)~Amt,data=dadosdf)
summary.manova(m.ajuste,test="Wilks")
summary.manova(m.ajuste,test="Pillai")
summary.manova(m.ajuste,test="Hotelling-Lawley")
summary.manova(m.ajuste,test="Roy")
aux<-summary.aov(m.ajuste)
#
# Estimando a matriz Sigma
mX <- as.matrix(model.matrix(fit.manova))
n<-nrow(mX)
eq<-ncol(mX)
mY <- cbind(dadosdf$Tot,dadosdf$Ami)
mB <- solve(t(mX)%*%mX)%*%t(mX)%*%mY
mSigma <- (t(mY-mX%*%mB)%*%(mY-mX%*%mB))/(n-eq)
#

# Estimativa dos parâmetros
vbeta<- c(coef(m.ajuste))
mcovbeta <- vcov(m.ajuste)
epbeta <- as.vector(sqrt(diag(mcovbeta)))
#
quantt <- qt(0.975,df=17-2)

mresult <- cbind(vbeta,epbeta,vbeta-quantt*epbeta,vbeta+quantt*epbeta,vbeta/epbeta,2*(1-pt(vbeta/epbeta,df=17-2)))
xtable(mresult)


# Testes de hipótese e intervalos de confiança

# Valores preditos (usando a aproximação pela normal)
# Variável Tot
par(mfrow=c(1,2))
m.fitted <- fitted(fit.model)
plot(dadosdf$Amt,dadosdf$Tot,pch=19,cex.axis=1.2,cex.lab=1.2,cex=1.2,xlab="amt",ylab="Tot",ylim=c(000,4000))
lines(dadosdf$Amt,m.fitted[,1],col=1,type="l",lwd=2,pch=19,cex.axis=1.2)
# média
epmupred <- sqrt(mcovbeta[1,1] + mcovbeta[2,2]*(sort(dadosdf$Amt^2)) + mcovbeta[1,2])
liicmupred <- sort(m.fitted[,1]) - 1.96*epmupred
liscmupred <- sort(m.fitted[,1]) + 1.96*epmupred
lines(sort(dadosdf$Amt),liicmupred,col=2,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
lines(sort(dadosdf$Amt),liscmupred,col=2,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
# uma única observação
epY <- sqrt(mcovbeta[1,1] + mcovbeta[2,2]*(sort(dadosdf$Amt^2)) + mcovbeta[1,2]+diag(mSigma)[1])
liicY <- sort(m.fitted[,1]) - 1.96*epY
liscY <- sort(m.fitted[,1]) + 1.96*epY
lines(sort(dadosdf$Amt),liicY,col=3,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
lines(sort(dadosdf$Amt),liscY,col=3,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
legend(10,4200,col=c(1,2,3),lwd=c(2,2,2),legend=c("Reta predita","IC para média","IP para uma única observação"),bty="n",cex=1.2)
#
plot(dadosdf$Amt,dadosdf$Ami,pch=19,cex.axis=1.2,cex.lab=1.2,cex=1.2,xlab="amt",ylab="Ami",ylim=c(000,4000))
lines(dadosdf$Amt,m.fitted[,2],col=1,type="l",lwd=2,pch=19,cex.axis=1.2)
# Variável Ami
# média
epmupred <- sqrt(mcovbeta[1,1] + mcovbeta[2,2]*(sort(dadosdf$Ami^2)) + mcovbeta[1,2])
#epmupred <- sqrt(mcovbeta[3,3] + mcovbeta[4,4]*(sort(dadosdf$Amt^2)) + mcovbeta[3,4])
liicmupred <- sort(m.fitted[,2]) - 1.96*epmupred
liscmupred <- sort(m.fitted[,2]) + 1.96*epmupred
lines(sort(dadosdf$Amt),liicmupred,col=2,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
lines(sort(dadosdf$Amt),liscmupred,col=2,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
# uma única observação
epY <- sqrt(mcovbeta[1,1] + mcovbeta[2,2]*(sort(dadosdf$Ami^2)) + mcovbeta[1,2]+diag(mSigma)[2])
#epY <- sqrt(mcovbeta[3,3] + mcovbeta[4,4]*(sort(dadosdf$Amt^2)) + mcovbeta[3,4]+diag(mSigma)[2])
liicY <- sort(m.fitted[,2]) - 1.96*epY
liscY <- sort(m.fitted[,2]) + 1.96*epY
lines(sort(dadosdf$Amt),liicY,col=3,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
lines(sort(dadosdf$Amt),liscY,col=3,type="l",lwd=2,lty=2,pch=19,cex.axis=1.2)
legend(10,4200,col=c(1,2,3),lwd=c(2,2,2),legend=c("Reta predita","IC para média","IP para uma única observação"),bty="n",cex=1.2)

#

# Resíduos univariados
mY <- as.matrix(cbind(dadosdf$Tot,dadosdf$Ami))
mresult<-m.ajuste
typeresid <- "univariate"
wplot <- "diagnostics"
#
# Tot
var<-1
gen.graf.resid(mY,mresult,var,typeresid,wplot)
# Ami
var<-2
gen.graf.resid(mY,mresult,var,typeresid,wplot)

# Zoom nos envelopes
wplot <- "envelope"
#
# Tot
var<-1
gen.graf.resid(mY,mresult,var,typeresid,wplot)
# Ami
var<-2
gen.graf.resid(mY,mresult,var,typeresid,wplot)

# Resíduos multivariados

typeresid <- "multivariate"
wplot <- "diagnostics"
#
# Tot
var<-1
gen.graf.resid(mY,mresult,var,typeresid,wplot)
# Ami
var<-2
gen.graf.resid(mY,mresult,var,typeresid,wplot)

# Zoom nos envelopes
wplot <- "envelope"
#
# Tot
var<-1
gen.graf.resid(mY,mresult,var,typeresid,wplot)
# Ami
var<-2
gen.graf.resid(mY,mresult,var,typeresid,wplot)

# Resíduo baseado na distância Mahalanobis

gen.graf.resid.quad.form(mY,mresult)


# Testanto se os coeficientes angulares são iguais
source("./MANOVA_Multivariada_ME_731_2S_2020.R")
#
# Lembrando que para esta função trabalhamos
m.C <-cbind(0,1)
m.U <- rbind(1,-1)
m.M <-0
TesteF.CBU.M(fit.manova,mSigma,2,2,m.C,m.U,m.M)



# Verificando os resultado
mX <- as.matrix(model.matrix(fit.manova))
n<-nrow(mX)
eq<-ncol(mX)
mY <- cbind(dadosdf$Tot,dadosdf$Ami)
mB <- solve(t(mX)%*%mX)%*%t(mX)%*%mY
vbeta <- c(t(mB))
mSigma <- (t(mY-mX%*%mB)%*%(mY-mX%*%mB))/(n-eq)
mcovbeta <- kronecker(solve(t(mX)%*%mX),mSigma)


#### desativo daqui para a frente

# Análise residual
# Resíduos ordinários
m.residuo <- m.ajuste$residuals
m.X <-as.matrix(model.matrix(m.ajuste))
auxres <- diag((diag(n) - m.X%*%solve(t(m.X)%*%m.X)%*%t(m.X)))
m.residuo <- m.residuo/(sqrt((matrix(auxres,17,2))%*%diag(diag(mSigma))))
m.ajustado <- fitted.values(m.ajuste)
#
# Tot
par(mfrow =c(2,2))
plot(m.residuo[,1],ylim=c(min(-3,min(m.residuo[,1])),max(3,max(m.residuo[,1]))),xlab="índice",ylab="resíduo studentizado")
abline(-2,0,lty=2)
abline(2,0,lty=2)
abline(0,0,lty=2)
#
plot(m.ajustado[,1],m.residuo[,1],ylim=c(min(-3,min(m.residuo[,1])),max(3,max(m.residuo[,1]))),xlab="valor ajustado",ylab="resíduo studentizado")
abline(-2,0,lty=2)
abline(2,0,lty=2)
abline(0,0,lty=2)
#
hist(m.residuo[,1],probability=TRUE,xlab="resíduo studentizado",main="",ylab="densidade")
#
qqPlot((m.residuo[,1]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab=paste("quantil da distribuição do resíduo studentizado"),cex=1.2)
#
# Ami
par(mfrow =c(2,2))
plot(m.residuo[,2],ylim=c(min(-3,min(m.residuo[,2])),max(3,max(m.residuo[,2]))),xlab="índice",ylab="resíduo studentizado")
abline(-2,0,lty=2)
abline(2,0,lty=2)
abline(0,0,lty=2)
#
plot(m.ajustado[,2],m.residuo[,2],ylim=c(min(-3,min(m.residuo[,2])),max(3,max(m.residuo[,2]))),xlab="valor ajustado",ylab="resíduo studentizado")
abline(-2,0,lty=2)
abline(2,0,lty=2)
abline(0,0,lty=2)
#
hist(m.residuo[,2],probability=TRUE,xlab="resíduo studentizado",main="",ylab="densidade")
#
qqPlot((m.residuo[,2]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab=paste("quantil da distribuição do resíduo studentizado"),cex=1.2)
#
