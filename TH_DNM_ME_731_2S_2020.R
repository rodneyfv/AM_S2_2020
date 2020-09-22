# Um único vetor de Médias
# Teste para um vetor de médias com Sigma conhecido
teste.mu.SigmaC<-function(m.X,v.mu.0,m.Sigma,alpha)
{
  n = nrow(m.X)
  p = ncol(m.X)
  v.mu = cbind(apply(m.X,2,mean))
  # T2 de Hotelling para Sigma conhecido
  Q = n*(t(v.mu - v.mu.0))%*%(solve(m.Sigma))%*%(v.mu - v.mu.0)
  p.valor = 1 - pchisq(Q,p)
  e.pnc = Q
  qc = qchisq(1 - alpha,p)
  e.pt = 1 - pchisq(qc,p,ncp=e.pnc)
  cat("Estatística do Teste: ",Q,"\n")
  cat("Nível descritivo: ",p.valor,"\n")
  cat("Poder do teste estimado: ",e.pt, "para um nível de significância de ",alpha,"\n")
  m.result<-list(e.Q = Q, p.valor=p.valor,qc=qc, e.pnc=e.pnc,e.pt=e.pt)
  return(m.result)
}
#
# Teste para um vetor de médias com Sigma desconhecido
teste.mu.SigmaD<-function(m.X,v.mu.0,alpha)
{
  n = nrow(m.X)
  p = ncol(m.X)
  m.Sigma<-cov(m.X)
  v.mu = cbind(apply(m.X,2,mean))
  # T2 de Hotelling
  Tstat = n*(t(v.mu - v.mu.0))%*%(solve(m.Sigma))%*%(v.mu - v.mu.0)
  Fstat = (n-p)*Tstat/((n-1)*p)
  p.valor = 1 - pf(Fstat,p,n-p)
  e.pnc = Tstat
  qdf = qf(1 - alpha,p,n-p)
  e.pt = 1 - pf(qdf,p,n-p,ncp=e.pnc)
  cat("Estatística do Teste: ",Fstat,"\n")
  cat("Nível descritivo: ",p.valor,"\n")
  cat("Poder do teste estimado: ",e.pt, "para um nível de significância de ",alpha,"\n")
  m.result<-list(e.F = Fstat, p.valor=p.valor,qdf=qdf, e.pnc=e.pnc,e.pt=e.pt)
  return(m.result)
}

# Combinações lineares para a média
# Sigma conhecido
teste.Rmub.SigmaC<-function(m.X,R,b,m.Sigma,alpha)
{
  n = nrow(m.X)
  p = ncol(m.X)
  c = nrow(R)
  v.mu = cbind(apply(m.X,2,mean))
  Q = n*(t(R%*%v.mu - b))%*%(solve(R%*%m.Sigma%*%t(R)))%*%(R%*%v.mu - b)
  p.valor = 1 - pchisq(Q,c)
  e.pnc = Q
  qc = qchisq(1 - alpha,c)
  e.pt = 1 - pchisq(qc,c,ncp=e.pnc)
  cat("Estatística do Teste: ",Q,"\n")
  cat("Nível descritivo: ",p.valor,"\n")
  cat("Poder do teste estimado: ",e.pt, "para um nível de significância de ",alpha,"\n")
  m.result<-list(e.Q = Q, p.valor=p.valor,qc=qc, e.pnc=e.pnc,e.pt=e.pt,R=R,b=b)
  return(m.result)
}
# Sigma desconhecido
teste.Rmub.SigmaD<-function(m.X,R,b,alpha)
{
  n = nrow(m.X)
  p = ncol(m.X)
  c = nrow(R)
  m.Sigma<-cov(m.X)
  v.mu = cbind(apply(m.X,2,mean))
  Tstat = n*(t(R%*%v.mu - b))%*%(solve(R%*%m.Sigma%*%t(R)))%*%(R%*%v.mu - b)
  Fstat = (n-c)*T/((n-1)*c)
  p.valor = 1 - pf(F,c,n-c)
  e.pnc = Tstat
  qdf = qf(1 - alpha,c,n-c)
  e.pt = 1 - pf(qdf,c,n-c,ncp=e.pnc)
  cat("Estatística do Teste: ",Fstat,"\n")
  cat("Nível descritivo: ",p.valor,"\n")
  cat("Poder do teste estimado: ",e.pt, "para um nível de significância de ",alpha,"\n")
  m.result<-list(e.F = Fstat, p.valor=p.valor,qdf=qdf, e.pnc=e.pnc,e.pt=e.pt,R=R,b=b)
  return(m.result)
}

# Dois ou mais vetores de médias
teste.mu1mu2.Homocedast<-function(m.X.completa,v.grupos,Delta,alpha)
{
  m.X.1 <-  m.X.completa[v.grupos==1,]
  m.X.2 <-  m.X.completa[v.grupos==2,]
  p<- ncol(m.X.1)
  v.n <- rbind(nrow(m.X.1),nrow(m.X.2))
  v.mu1 <-  cbind(apply(m.X.1,2,mean))
  v.mu2 <-  cbind(apply(m.X.2,2,mean))
  m.Sigma1 <- cov(m.X.1)
  m.Sigma2 <- cov(m.X.2)
  m.SigmaP <- ((v.n[1]-1)*m.Sigma1 + (v.n[2]-1)*m.Sigma2)/(v.n[1] + v.n[2] - 2)
  e.F <- (1/(1/v.n[1] + 1/v.n[2]))*(t(((v.mu1 - v.mu2) - Delta)))%*%solve(m.SigmaP)%*%(((v.mu1 - v.mu2) - Delta))
  df1 <- p
  df2 <- v.n[1] + v.n[2] - p - 1
  e.F <- e.F *df2/(df1*(v.n[1] + v.n[2] - 2))
  p.valor <- 1 - pf(e.F,df1,df2)
  fc = qf(1 - alpha,df1,df2)
  e.pnc = e.F
  e.pt = 1 - pf(fc,df1,df2,ncp=e.pnc) # poder aproximado
  cat("Estatística do Teste: ",e.F,"\n")
  cat("nível descritivo: ",p.valor,"\n")
  cat("Estimativa do poder do teste: ",e.pt,"\n")
} # fim da função

# Teste da Razão de Verossimilhanças para igualdade entre duas matrizes de Covariâncias
bartlet.teste.Igual.MCov<-function(m.X.completa,v.grupos,G,v.n)
{
  #m.X.completa : matriz de dados com todos os grupos
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
  Sigma.P <- Sigma.P/(sum(v.n-1))
  # Estatística ajustada
  aux.u <- (sum(1/(v.n - 1)) - (1/(sum(v.n - 1))))*(2*p^2 + 3*p - 1)/(6*(p+1)*(G-1))
  Q.B <-  (1 - aux.u)*(sum(v.n-1)*log(det(Sigma.P)) - aux.k.1)
  aux.v <- 0.5*p*(p+1)*(G-1)
  e.nd.QB <- 1 - pchisq(Q.B,aux.v)
  cat("Estatística do Teste: ", Q.B, "\n")
  cat("nível descritivo: ",e.nd.QB,"\n")
  cat("Matrizes de Covariâncias por grupo: \n")
  print(m.Sigma.completa)
} # fim da função


