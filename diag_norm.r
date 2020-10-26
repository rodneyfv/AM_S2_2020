# Programa extraído do site: https://www.ime.usp.br/~giapaula/textoregressao.htm
# Créditos: Prof. Dr. Gilberto Alvarenga Paula
# Adaptado por Caio L. N. Azevedo
# source("C:\\Users\\cnaber\\Trabalho\\windows\\Unicamp\\Disciplinas\\2_semestre_2016\\ME 613\\Programas\\diag_norm.r")


diagnorm<-function(fit.model){
        # fit.model: objeto com o ajuste do modelo normal linear homocedástico 
        # obtido através da função "lm"
        
        par(mfrow=c(2,2))
        X <- model.matrix(fit.model)
        n <- nrow(X)
        p <- ncol(X)
        H <- X%*%solve(t(X)%*%X)%*%t(X)
        h <- diag(H)
        lms <- summary(fit.model)
        s <- lms$sigma
        r <- resid(lms)
        ts <- r/(s*sqrt(1-h))
        di <- (1/p)*(h/(1-h))*(ts^2)
        si <- lm.influence(fit.model)$sigma
        tsi <- r/(si*sqrt(1-h))
        a <- max(tsi)
        b <- min(tsi)
        par(mfrow=c(2,2))
        #
        plot(tsi,xlab="Índice", ylab="Resíduo Studentizado",
             ylim=c(b-1,a+1), pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)
        abline(2,0,lty=2)
        abline(-2,0,lty=2)
        abline(0,0,lty=2)
        #identify(tsi, n=1)
        #title(sub="(c)")
        #
        plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
             ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)
        #
        abline(2,0,lty=2)
        abline(-2,0,lty=2)
        abline(0,0,lty=2)
        #boxplot(tsi,ylab="Resíduo Studentizado",cex=1.1,cex.axis=1.1,cex.lab=1.1)
        hist(tsi,xlab="Resíduo Studentizado",ylab="densidade",probability=TRUE,main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
        #title(sub="(d)")
        #identify(fitted(fit.model),tsi, n=1)
        #
        ident <- diag(n)
        epsilon <- matrix(0,n,100)
        e <- matrix(0,n,100)
        e1 <- numeric(n)
        e2 <- numeric(n)
        #
        for(i in 1:100){
                epsilon[,i] <- rnorm(n,0,1)
                e[,i] <- (ident - H)%*%epsilon[,i]
                u <- diag(ident - H)
                e[,i] <- e[,i]/sqrt(u)
                e[,i] <- sort(e[,i]) }
        #
        for(i in 1:n){
                eo <- sort(e[i,])
                e1[i] <- (eo[2]+eo[3])/2
                e2[i] <- (eo[97]+eo[98])/2 }
        #
        med <- apply(e,1,mean)
        faixa <- range(tsi,e1,e2)
        #
        #par(pty="s")
        qqnorm(tsi,xlab="Percentil da N(0,1)",
               ylab="Resíduo Studentizado", ylim=faixa, pch=16, main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
        par(new=T)
        qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
        par(new=T)
        qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
        par(new=T)
        qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")
        #---------------------------------------------------------------#
        return(tsi)
}
