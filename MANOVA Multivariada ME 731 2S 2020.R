Box.teste.Igual.MCov<-function(m.X.completa,v.grupos,v.n,G)
{
  # v.grupos (1,2,3...)
  # m.X.completa : matriz de dados com todos os grupos
  grupo <- 1
  p<- ncol(m.X.completa)
  m.X.k <- m.X.completa[v.grupos==grupo,]
  Sigma.k <- cov(m.X.k)
  m.Sigma.completa <- cbind(grupo,Sigma.k)
  Sigma.P <- (v.n[grupo]-1)*Sigma.k # estimativa ponderada
  aux.k.1 <- (v.n[grupo] - 1)*log(det(Sigma.k))
  grupo <- grupo + 1
  for (i in 2:G)
  {
    m.X.k <- m.X.completa[v.grupos==grupo,] # pegar os dados referentes ao grupo i
    Sigma.k <- cov(m.X.k)
    m.Sigma.completa <- rbind(m.Sigma.completa,cbind(grupo,Sigma.k))
    Sigma.P <- Sigma.P + (v.n[grupo]-1)*Sigma.k # estimativa ponderada
    aux.k.1 <- aux.k.1 + (v.n[grupo] - 1)*log(det(Sigma.k))
    grupo <- grupo + 1
  }
  Sigma.P <- Sigma.P/(sum(v.n)-G)
  
  # Estatística de ajuste
  aux.u <- (sum(1/(v.n - 1)) - (1/(sum(v.n - 1))))*(2*p^2 + 3*p - 1)/(6*(p+1)*(G-1))
  Q.B <-  (1 - aux.u)*(sum(v.n-1)*log(det(Sigma.P)) - aux.k.1)
  aux.v <- 0.5*p*(p+1)*(G-1)
  e.nd.QB <- 1 - pchisq(Q.B,aux.v)
  cat("Estatística do Teste: ", Q.B, "\n")
  cat("nível descritivo: ",e.nd.QB,"\n")
  cat("Matrizes de Covariâncias por grupo: \n")
  print(m.Sigma.completa)
  Sigma.P <-as.matrix(data.frame(Sigma.P))
  list(Sigma.P=Sigma.P)
} # fim fa função


TesteF.CBU.M<-function(fit.model,m.Sigma.P,p,G,m.C,m.U,m.M)
{
  m.B <- matrix(coef(fit.model),G,p)
  v.beta <- matrix(t(m.B))
  m.X <- model.matrix(fit.model)
  m.Ca <- kronecker(m.C,t(m.U))
  m.Ma <- matrix(t(m.M))
  v.theta <- m.Ca%*%v.beta - m.Ma
  m.Sigmabeta <- kronecker(solve(t(m.X)%*%m.X),m.Sigma.P)
  estat <- t(v.theta)%*%solve(m.Ca%*%(m.Sigmabeta)%*%t(m.Ca))%*%v.theta
  p.valor <- 1-pchisq(estat,df=nrow(m.C)*ncol(m.U))
  cat("Estatistica Qui-quadrado = ",round(estat,2),"\n")
  cat("pvalor = ",round(p.valor,4),"\n")
  cat("Matriz C :","\n")
  print(m.C)
  cat("Matriz U :","\n")
  print(m.U)
  cat("Matriz M :","\n")
  print(m.M)
}