ACaux <-function(m.X)     
{
  n.I <- nrow(m.X)
  n.J <- ncol(m.X)
  m.X.completa <- cbind(m.X,apply(m.X,1,sum))
  m.X.completa <- rbind(m.X.completa,apply(m.X.completa,2,sum))
  
  # Matriz de proporções
  m.P <-  m.X.completa/sum(m.X)
  
  # Vetor Pr e Pc
  P.r <- cbind(m.P[,n.J+1])
  P.c <- cbind(m.P[n.I+1,])
  
  # Matrizes Dr e Dc
  D.r <- diag(c(P.r),n.I+1,n.I+1)
  D.c <- diag(c(P.c),n.J+1,n.J+1)
  
  # Perfis das linhas e colunas
  m.R <- solve(D.r)%*%m.P
  m.C <- t(solve(D.c)%*%t(m.P))
  round(m.R*100,2)
  
  #t(round(m.C*100,2))
  
  m.P.aux <- m.P[1:n.I,1:n.J]
  P.c.aux <- cbind(P.c[1:n.J,])
  P.r.aux <- cbind(P.r[1:n.I,])
  D.r.aux <- diag(sqrt(c(P.r.aux)),n.I,n.I)
  D.c.aux <- diag(sqrt(c(P.c.aux)),n.J,n.J)
  m.P.rc <- (solve(D.r.aux))%*%(m.P.aux - P.r.aux%*%t(P.c.aux))%*%(solve(D.c.aux))
  result.svd <- svd(m.P.rc)
  v.gamma <- cbind(result.svd$d)
  inercia <- (v.gamma^2)
  #round(cbind(v.gamma,inercia),4)
  
  # Valor singular é raiz quadrada do autovalor (positivo)
  eigen1 <- eigen(m.P.rc%*%t(m.P.rc))
  eigen2 <- eigen(t(m.P.rc)%*%(m.P.rc))
  m.Gamma <- diag(result.svd$d,min(n.I,n.J),min(n.I,n.J))
  m.U <- (result.svd$u)
  m.V <- (result.svd$v)
  
  # componentes
  m.PL <- (solve(D.r.aux)%*%m.U%*%(m.Gamma))
  m.PC <- (solve(D.c.aux)%*%m.V%*%(m.Gamma))
  m.FullLC <- rbind(m.PL,m.PC)
  #result.AC.inercia <- list(v.gamma=v.gamma,inercia=inercia)
  return(list(inercia=inercia,m.R=m.R,m.C=m.C))
} # end of the function


