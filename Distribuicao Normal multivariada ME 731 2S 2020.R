
library(MASS)

# Superfície das densidades

# vetor de correlações que usaremos nas densidades
corre<-c(0,0,-0.9,0.9)

# Vamos primeiro simular uma amostra da normal bivariada
bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, corre[1], corre[1], 1), 2))

# agora vamos estimar a densidade usando um núcleo bivariado
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)


#dev.new(width=20, height=5)
#par(mar=c(0.01,0.01,0.01,0.01))
par(mfrow=c(2,2))
par(mar = c(0, 0, 0, 0))

#par(mar=c(1.0, 1.0, 0.1, 0.1)) 
#par(mai=c(0.1, 0.1, 0.1, 0.1)) 

# esses são o resultados dos gráficos
persp(bivn.kde, phi = 45, theta = 30, shade = .1,main=paste("correlação = ",0),
      col="lightblue",ticktype="detailed",cex.axis=1.3,cex.lab=1.3,cex.main=1.3,
      xlab="",ylab="",zlab="")



bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(9, corre[2], corre[2], 9), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# gráfico dos resultados
persp(bivn.kde, phi = 45, theta = 30, shade = .1,
      main=paste("correlação = ",0, ", variâncias = ",9),col="lightblue",
      ticktype="detailed",cex.axis=1.3,cex.lab=1.3,cex.main=1.3,xlab="",
      ylab="",zlab="")



bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, corre[3], corre[3], 1), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# gráfico dos resultados
persp(bivn.kde, phi = 45, theta = 30, shade = .1,
      main=paste("correlação = ",corre[3]),col="lightblue",ticktype="detailed",
      cex.axis=1.3,cex.lab=1.3,cex.main=1.3,xlab="",ylab="",zlab="")


bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, corre[4], corre[4], 1), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# gráfico dos resultados
persp(bivn.kde, phi = 45, theta = 30, shade = .1,
      main=paste("correlaçao = ",corre[4]),col="lightblue",ticktype="detailed",
      cex.axis=1.3,cex.lab=1.3,cex.main=1.3,xlab="",ylab="",zlab="")

#dev.off()


#####################
# Gráficos de contorno

# função para sair do modo que divide a tela nos gráficos
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

# vetor de correlações que usaremos nas densidades
corre<-c(0,0,-0.9,0.9)

#par(mfrow=c(2,2))
# simulando uma amostra da normal bivariada
bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, corre[1], corre[1], 1), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)

#dev.new(width=20, height=5)
#par(mar=c(0.01,0.01,0.01,0.01))
par(resetPar()) 
par(mfrow=c(2,2))

#par(mar = c(0, 0, 0, 0))
# gráfico dos resultados
contour(bivn.kde,main=paste("correlação = ",0),cex.axis=1.3,cex.lab=1.3,cex.main=1.3,xlab="",ylab="")


bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(9, corre[2], corre[2], 9), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# gráfico dos resultados
contour(bivn.kde,main=paste("correlação = ",0),cex.axis=1.3,cex.lab=1.3,
        cex.main=1.3,xlab="",ylab="")


bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, corre[3], corre[3], 1), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# gráfico dos resultados
contour(bivn.kde,main=paste("correlação = ",corre[3]),cex.axis=1.3,cex.lab=1.3,cex.main=1.3,xlab="",ylab="")


bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, corre[4], corre[4], 1), 2))
# estimação da densidade por núcleo
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# gráfico dos resultados
contour(bivn.kde, main=paste("correlação = ",corre[4]),cex.axis=1.3,cex.lab=1.3,cex.main=1.3,xlab="",ylab="")

#dev.off()


