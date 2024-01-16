# ggplot theme
theme_ravshan <- function(){
  theme(legend.position = "bottom",
        plot.margin = margin(t=1,r=1,b=0,l=0,"pt"),
        plot.background = element_rect(size=0),
        panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, face = "plain"),
        legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 15)
  )
}

# Bodie et al.
bodie <- function(Lx, Fx, mu, rf, gamma, sig){
  tmp <- ((mu-rf) / (gamma*sig^2) ) * (1+Lx/Fx)
  output <- ifelse(tmp<=100,tmp,100)
  return(output)
}

# Constrained Munk (2020) Ravshan
munk <- function(Lx,Fx,gamma,mu,sig,covL){#Munk with only risky assets
  l = Lx/Fx
  N <- length(mu)
  sig1 <- solve(sig)
  eye <- rep(1,N)
  alloc <- rep(NA,N)
  lambda <- (gamma + (gamma*l * covL - (1+l) * mu)%*%sig1%*%eye)/((1+l)*(eye %*% sig1 %*% eye))
  alloc = (  (1/gamma) * ((1+l)*(mu+lambda%*%eye) - gamma*l*covL)  ) %*% sig1
  output = alloc
  if(any(alloc<0)){
    idx <- which(alloc>=0)
    output <- alloc * (alloc>=0)
    output[idx] = munk(Lx,Fx,gamma,mu[idx],sig[idx,idx],covL[idx])
  }
  return(output)
}

munk.rf <- function(Lx,Fx,gamma,rf,mu,sig,covL){#Munk with risk-free asset
  l = Lx/Fx
  N <- length(mu)
  sig1 <- solve(sig)
  eye <- rep(1,N)
  alloc <- rep(NA,N)
  alloc <- (  (1/gamma) * ((1+l)*(mu-rf%*%eye) - gamma*l*covL)  ) %*% sig1
  output <- c(1-sum(alloc),alloc)
  if(sum(alloc)>1){
    output <- rep(0,(N+1))
    output[-1] <- munk(Lx,Fx,gamma,mu,sig,covL)
  }else if(all(alloc<0)){
    output <- rep(0,(N+1))
    output[1] <- 1
  }else if(any(alloc<0)){
    output <- rep(0,(N+1))
    idx <- which(alloc>=0)
    output[c(1,idx+1)] <- munk.rf(Lx,Fx,gamma,rf,mu[idx],sig[idx,idx],covL[idx])
  }
  return(output)
}



human.capital <- function(x,rf){
  Tx <- length(x)
  tmp <- rep(NA,Tx)
  for(t in 1:Tx){
    tmp[t] <- sum((1/(1+rf))^c(0:(Tx-t)) * x[t:Tx])
  }
  return(tmp)
}

portfolio.return <- function(invested, weights, rates){
  output <- invested * (1 + as.double(weights %*% rates))
  return(output)
}


# Cocco et al.
cocco <- function(x){
  tmp <- rep(NA, length(x))
  idx1 <- which(x >= 0 & x < 40)
  idx2 <- which(x >= 40 & x <= 60)
  idx3 <- which(x > 60 & x <= 100)
  tmp[idx1] <- 100
  tmp[idx2] <- 200 - 2.5 * x[idx2]
  tmp[idx3] <- 50
  output <- tmp/100
  return(output)
}


# Sharpe Ratio
sharpe <- function(mu,sig,rf){
  (mu-rf)/sig
}


# Ascheberg et al.
ascheberg <- function(mus,sds,corr,Tp=1){
  N <- length(mus)
  cholesky <- t(chol(corr))
  eps <- matrix(rnorm(N*Tp),N,Tp)
  brownian <- cholesky %*% eps
  errors <- sds * brownian
  output <- mus + errors
  output <- t(output)
  return(output)
}

fin.capital <- function(inputs, weights, rates){
  tmp <- 1 + rowSums(weights * rates)
  output <- cumsum(tmp * inputs)
  return(output)
}

fin.capital.bodie <- function(Lx,inputs,mu,rf,gamma,sigs,rates){
  t <- length(Lx)
  bodie.x <- rep(NA,t)
  bodie.fc <- rep(0,t)
  bodie.fc[1] <- inputs[1]
  for(i in 2:t){
    bodie.x[i-1] <- bodie(Lx[i-1], bodie.fc[i-1], mu, rf, gamma, sigs) / 100
    bodie.fc[i] <- fin.capital(inputs = inputs[1:(i-1)], weights = cbind((1-bodie.x[1:(i-1)]), bodie.x[1:(i-1)]), rates = cbind(rf, rates[1:(i-1)]))[i-1]
  }
  bodie.x[t] <- bodie(Lx[t], bodie.fc[t], mu, rf, gamma, sigs) / 100
  output <- list(x = bodie.x, fc = bodie.fc)
  return(output)
}

fin.capital.munk <- function(Lx,inputs,gamma,rf,mu,sig,cov_L,rates){
  t <- length(Lx)
  q <- length(mu) + 1 # q refers to assets (stock, house)
  munk.x <- matrix(NA,t,q)
  munk.fc <- rep(0,t)
  munk.fc[1] <- inputs[1]
  for(i in 2:t){
    munk.x[i-1,] <- munk.rf(Lx[i-1], munk.fc[i-1], gamma, rf, mu, sig, cov_L)
    munk.fc[i] <- fin.capital(inputs = inputs[1:(i-1)], weights = munk.x[1:(i-1),,drop=FALSE], rates = rates[1:(i-1),])[i-1]
  }
  munk.x[t,] <- munk.rf(Lx[t], munk.fc[t], gamma, rf, mu, sig, cov_L)
  output <- list(x = munk.x, fc = munk.fc)
  return(output)
}


make.corr <- function(xy,xz,yz){
  matrix(c(1,xy,xz,
           xy,1,yz,
           xz,yz,1),3,3)
}

# Annuity calculation
annuity <- function(wT,surv,r_f){
  nT <- length(surv)
  rfs <- cumprod(rep( (1+r_f),nT))
  denom <- 1 + sum(surv/rfs)
  paid <- wT / denom
  output <- list(paid=paid,denom=denom)
  return(output)
}

# Constant Relative Risk Aversion Utility function
crrautil <- function(x, gamma, surv, delta=0.89){
  nT <- length(surv)
  deltas <- cumprod(rep(delta,nT))
  tmp <- (deltas * surv) %*% t(x^(1-gamma) / (1-gamma))
  output <- colSums(tmp)
  return(output)
}

meanvar <- function(x,gamma){
  output = mean(x) - (gamma/2)*var(x)
  return(output)
}

# Stats table
desc.tab <- function(asset){
  print("Mean,Median,Standard Deviation,Variance,Skewness,Kurtosis,Minimum,Maximum")
  paste0(round(mean(asset),5),",",
         round(median(asset),5),",",
         round(sd(asset),5),",",
         round(var(asset),5),",",
         round(skewness(asset),5),",",
         round(kurtosis(asset),5),",",
         round(min(asset),5),",",
         round(max(asset),5)
         )
}

remove.outliers <- function(x,lower=NA,upper=NA){
  tmp <- x
  lower.bound <- ifelse(is.na(lower),quantile(tmp,0.001),lower)
  upper.bound <- ifelse(is.na(upper),quantile(tmp,0.999),upper)
  output <- tmp[tmp>=lower.bound & tmp<=upper.bound]
  return(output)
}
