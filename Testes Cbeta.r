# fit.model: saída do modelo ajustado
# m.C: matriz C relativa às hipóteses de interesse

testeF.CB <- function(fit.model,m.C)
{
  v.beta <-  cbind(fit.model$coef) # vetor com a estimativa dos parâmetros
  n <- nrow(model.matrix(fit.model)) # número de observações
  e.p <- nrow(v.beta) # número de parâmetros
  e.q <- nrow(m.C)  # número de linhas da matriz C
  m.cov.beta <- (vcov(fit.model)) # matriz de covariâncias dos parâmetros do modelo
  # Estatística do Teste
  e.F <- t(m.C%*%v.beta)%*%solve(m.C%*%m.cov.beta%*%t(m.C))%*%(m.C%*%v.beta)/e.q
  e.pvalor <- 1-pf(e.F,e.q,n-e.p) # p-0valor
  cat("Estatistica F = ",round(e.F,2),"\n")
  cat("pvalor = ",round(e.pvalor,4),"\n")
  cat("Matriz C :","\n")
  print(m.C)
}

# alpha: nível de significância do teste

testeF.CB.poder <- function(fit.model,m.C,alpha)
{
  v.beta <-  cbind(fit.model$coef) # vetor com a estimativa dos parâmetros
  n <- nrow(model.matrix(fit.model)) # número de observações
  e.p <- nrow(v.beta)# número de parâmetros
  e.q <- nrow(m.C)# número de linhas da matriz C
  m.cov.beta <- (vcov(fit.model)) # matriz de covariâncias dos parâmetros do modelo
  m.X <- model.matrix(fit.model) # matriz de planejamento do modelo
  # Estatística do teste
  e.F <- t(m.C%*%v.beta)%*%solve(m.C%*%m.cov.beta%*%t(m.C))%*%(m.C%*%v.beta)/e.q
  e.pvalor <- 1-pf(e.F,e.q,n-e.p) # p-valor
  e.sigma2 <- deviance(fit.model)/df.residual(fit.model) # estimativa de sigma2 (variância das observações)
  # Parâmetro de não-centralidade
  e.delta <- (t(m.C%*%v.beta)%*%solve(m.C%*%solve(t(m.X)%*%m.X)%*%t(m.C))%*%(m.C%*%v.beta))/e.sigma2
  F.critico <- qf(1-alpha,e.q,n-e.p) # ponto crítico do testes
  e.poder <- 1-pf(F.critico,e.q,n-e.p,ncp=e.delta) # poder estimado
  cat("Estatistica F = ",round(e.F,2),"\n")
  cat("pvalor = ",round(e.pvalor,4),"\n")
  cat("poder observado = ",round(e.poder,4),"\n")
  cat("Matriz C :","\n")
  print(m.C)
}


#m.M : matriz M relativa às hipóteses de interesse
testeF.CBM <- function(fit.model,m.C,m.M)
  
{
  v.beta <-  cbind(fit.model$coef) # vetor com a estimativa dos parâmetros
  n <- nrow(model.matrix(fit.model)) # número de observações
  e.p <- nrow(v.beta) # número de parâmetros
  e.q <- nrow(m.C)  # número de linhas da matriz C
  m.cov.beta <- (vcov(fit.model)) # matriz de covariâncias dos parâmetros do modelo
  # Estatística do teste
  e.F <- t(m.C%*%v.beta-m.M)%*%solve(m.C%*%m.cov.beta%*%t(m.C))%*%(m.C%*%v.beta-m.M)/e.q
  e.pvalor <- 1-pf(e.F,e.q,n-e.p) # p-valor
  cat("Estatistica F = ",round(e.F,2),"\n")
  cat("pvalor = ",round(e.pvalor,4),"\n")
  cat("Matriz C :","\n")
  print(m.C)
  cat("Matriz M :","\n")
  print(m.M)
  
}


# estimação por mínimos quadrados generalizados
estim.ML.MQG <- function(resp,m.X,nc=0.95,sigma2=1,Sigma)
{
  vbeta <-  solve(t(m.X)%*%solve(Sigma)%*%m.X)%*%t(m.X)%*%solve(Sigma)%*%resp
  mcovbeta <-sigma2*solve(t(m.X)%*%solve(Sigma)%*%m.X) 
  epbeta <- sqrt(diag(mcovbeta))
  m.test <- vbeta/epbeta
  m.pvalor <- 2*(1-pnorm(abs(m.test)))
  qnormal <- qnorm(0.5*(1+nc))
  m.IC <- cbind(vbeta-qnormal*epbeta,vbeta+qnormal*epbeta)
  result <- cbind(vbeta,epbeta,m.IC,m.test,m.pvalor)
  colnames(result)<-c("Est.","EP","LIIC","LSIC","Estat. Z","p-valor")
  result <- list(result=result,mcovbeta=mcovbeta)
  return(result)
}

#m.M : matriz M relativa às hipóteses de interesse
testeF.CBM.MQG <- function(result,m.C,m.M)
{
  aux<- result$result
  v.beta <-  aux[,1] # vetor com a estimativa dos parâmetros
  e.q <- nrow(m.C)  # número de linhas da matriz C
  m.cov.beta <- result$mcovbeta # matriz de covariâncias dos parâmetros do modelo
  # Estatística do teste
  e.Q <- t(m.C%*%v.beta-m.M)%*%solve(m.C%*%m.cov.beta%*%t(m.C))%*%(m.C%*%v.beta-m.M)
  e.pvalor <- 1-pchisq(e.Q,df=e.q) # p-valor
  cat("Estatistica Q = ",round(e.Q,2),"\n")
  cat("pvalor = ",round(e.pvalor,4),"\n")
  cat("Matriz C :","\n")
  print(m.C)
  cat("Matriz M :","\n")
  print(m.M)
}

