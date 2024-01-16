rm(list=ls())
library('tidyverse')
library('lubridate')
library('urca')
library('forecast')
options(scipen=999)
source("_import_data.R")
source("_define_functions.R")

# Define parameters
param <- list()
param$rf <- 0.03 # risk-free rate
param$c <- 0.03 # % of wage invested
param$MC <- 10000
param$gamma <- c(1.5, 3, 5, 10)
param$t <- 40
param$delta <- 0.89

# Params taken from monthly data:
param$muS <- (1 + mean(diff(log(bist100$Open))))^12-1
param$sdS <- sd(diff(log(bist100$Open))) * sqrt(12)
param$muH <- (1 + mean(diff(log(realty$house))))^12-1
param$sdH <- sd(diff(log(realty$house))) * sqrt(12)
both.assets <- inner_join(bist100,realty,by="Date")
param$covSH <- cov( diff(log(both.assets$Open)), diff(log(both.assets$house))) * 12
param$rhoSH <- cor( diff(log(both.assets$Open)), diff(log(both.assets$house)))
param$varcov <- matrix(c(param$sdS^2,param$covSH,param$covSH,param$sdH^2),2,2)

# Params related to wage data
hhba.annual <- hhba %>% 
  group_by(year) %>% 
  summarize_at(vars(salary_real),median)

param$muL <- mean(diff(log(hhba.annual$salary_real)))
param$sdL <- sd(diff(log(hhba.annual$salary_real)))
param$rhoHL <- cor( diff(log(hhba.annual$salary_real))[-1] , diff(log(realty.annual$house))) 
param$covHL <- cov( diff(log(hhba.annual$salary_real))[-1] , diff(log(realty.annual$house))) 
param$rhoSL <- c(0, 0.2, 0.4)
param$covSL <- param$rhoSL * param$sdS * param$sdL

# Wage
state.main <- hhba %>%
  group_by(educ,age) %>%
  summarize_at(vars(salary_real),median) %>%
  filter(educ %in% c("elementary","high","undergrad")) %>% 
  filter(age >= 25 & age < 25 + param$t) %>%
  mutate(age2 = age^2, lwage = log(salary_real)) %>% 
  mutate(lwage_fit = predict(lm(lwage ~ age + age2))) %>% 
  mutate(wage = 0, humcap = 0,
         thumb = (100-age)/100,
         cocco = cocco(age),
         markowitz = 0, bodie = 0, munk = 0, h.munk = 0,
         fincap_bodie = 0,fincap_munk = 0,fincap_h.munk = 0) %>% 
  select(-age2, -lwage, -salary_real)

fincaps.all <- list()
for(X in 1:3){# rhoSL 0 0.2 0.4
  fincaps.all[[X]] <- list()
  for(Y in 1:4){# gamma 1.5 3 5 10
    fincaps.all[[X]][[Y]] <- list()
    for(mc in 1:param$MC){
      # Set current params
      RHO <- make.corr(xy = param$rhoSH, xz = param$rhoSL[X], yz = param$rhoHL)
      covL <- c(param$covSL[X], param$covHL)
      # generate correlated asset series (Stock returns, Housing returns, Wage shocks)
      assets.mc <- ascheberg(mus = c(param$muS,param$muH,param$muL), sds = c(param$sdS,param$sdH,param$sdL), corr = RHO, Tp =  param$t)
      
      # actual calculation
      state.mc <- state.main %>% 
        mutate(wage = exp(lwage_fit + assets.mc[,3])) %>% 
        mutate(humcap = human.capital(wage,param$rf)) %>% 
        mutate(fincap_thumb = fin.capital(inputs = param$c*wage, weights = cbind(1-thumb,thumb), rates = cbind(param$rf, assets.mc[,1])),
               fincap_cocco = fin.capital(inputs = param$c*wage, weights = cbind(1-cocco,cocco), rates = cbind(param$rf, assets.mc[,1])),
               markowitz = (param$muS - param$rf)/(param$gamma[Y] * param$sdS^2),
               fincap_markowitz = fin.capital(inputs = param$c*wage, weights = cbind((1-markowitz),markowitz), rates = cbind(param$rf, assets.mc[,1])),
               bodie = fin.capital.bodie(humcap, param$c*wage, param$muS, param$rf, param$gamma[Y], param$sdS, assets.mc[,1])$x,
               fincap_bodie = fin.capital.bodie(humcap, param$c*wage, param$muS, param$rf, param$gamma[Y], param$sdS, assets.mc[,1])$fc,
               munk = fin.capital.munk(humcap, param$c*wage, param$gamma[Y], param$rf, param$muS, param$sdS^2, covL[1], cbind(param$rf,assets.mc[,1]))$x[,2],
               fincap_munk = fin.capital.munk(humcap, param$c*wage, param$gamma[Y], param$rf, param$muS, param$sdS^2, covL[1], cbind(param$rf,assets.mc[,1]))$fc,
               h.munk = fin.capital.munk(humcap, param$c*wage, param$gamma[Y], param$rf, c(param$muS,param$muH), param$varcov, covL, cbind(param$rf,assets.mc[,1:2]))$x,
               fincap_h.munk = fin.capital.munk(humcap, param$c*wage, param$gamma[Y], param$rf, c(param$muS,param$muH), param$varcov, covL, cbind(param$rf,assets.mc[,1:2]))$fc
        )
      
      fincaps.all[[X]][[Y]][[mc]] <- state.mc %>% filter(age==64) %>% ungroup() %>% select(fincap_markowitz,fincap_thumb,fincap_cocco,fincap_bodie,fincap_munk,fincap_h.munk) %>% as.matrix()
    }
  }
}


fincaps.all[[1]][[1]][[1]][,3]

save(fincaps.all, file = "_latest_simulation_results.RData")
