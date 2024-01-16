# bist100 <- bist100 %>% 
#   mutate_at(vars(Date),function(x){as_date(x,format="%B %d,%Y")}) %>% 
#   select(Date,Open) %>% 
#   group_by(year(date))
#   #mutate_at(vars(Open),as.double) %>% 
#   #filter(Date<="2020-02-01") %>% 
#   na.omit() #%>% 
#   #mutate_at(vars(Date), function(x){floor_date(x,unit="month")}) %>% 
#   #group_by(Date) %>% 
#   #summarize_at(vars(Open),mean)


# aschebergOLD <- function(mus,sds,corr){
#   N <- length(mus)
#   cholesky <- t(chol(corr))
#   eps <- rnorm(N)
#   brownian <- c(cholesky %*% eps)
#   errors <- sds * brownian
#   output <- mus + errors
#   return(output)
# }
# cocco <- function(t){
#   tmp <- NA
#   if(t>=0 & t<40){
#     tmp <- 100
#   }else if(t>=40 & t<=60){
#     tmp <- 200 - 2.5*t
#   }else if(t>60 & t<=100){
#     tmp <- 50
#   }
#   output <- tmp
#   return(output)
# }


# h.munk2$fin_capital <- 0
# h.munk2$fin_capital[1] <- 0.03*h.munk2$salary_fit[1]
# h.munk2$alpha0 <- 0
# h.munk2$alpha1 <- 0
# h.munk2$alpha2 <- 0
# for(i in 2:nrow(h.munk2)){
#   alpha.i <- munk.rf(Lx = h.munk2$human_capital[i-1],
#                   Fx = h.munk2$fin_capital[i-1],
#                   gamma = 5,
#                   rf = param$RF,
#                   mu = c(param$MUS,param$MUH),
#                   sig = matrix(c(param$SIGS^2, 0,
#                                  0, param$SIGH^2),2),
#                   covL = c(0,0))
#   h.munk2$alpha0[i-1] <- alpha.i[1]*100
#   h.munk2$alpha1[i-1] <- alpha.i[2]*100
#   h.munk2$alpha2[i-1] <- alpha.i[3]*100
#   h.munk2$fin_capital[i] <- fin.capital.fake(0.03*h.munk2$salary_fit[1:(i-1)], alpha.i, c(param$RF, param$MUS, param$MUH))[i-1]
# }


fin.capital.fake <- function(x,weights,means){
  Tx <- length(x)
  tmp <- rep(NA,Tx)
  nseries <- length(means)
  rates <- matrix(NA,nseries,Tx)
  for(i in 1:nseries){
    rates[i,] <- rep(means[i],Tx)
  }
  for(t in 1:Tx){
    tmp[t] <- sum((1+ as.double(weights %*% rates[,t]) ) * x[1:t])
  }
  return(tmp)
}





















# 
# # Stock
# bist100 <- bist100 %>% mutate(Rate=c(0,diff(log(Open))))
# # get parameters:
# mu_mon <- mean(bist100$Rate) # daily mean
# sig_mon <- sd(bist100$Rate) # daily sd
# mu_ann <- (1+mu_mon)^12-1 # annual mean
# sig_ann <- sig_mon*sqrt(12) # annual sd
# 
# param$MUS <- mu_ann
# param$SIGS <- sig_ann
# 
# 
# # Housing
# realty <- realty %>% mutate(Rate = c(0,diff(log(house))))
# mu_mon <- mean(realty$Rate) # daily mean
# sig_mon <- sd(realty$Rate) # daily sd
# mu_ann <- (1+mu_mon)^12-1 # annual mean
# sig_ann <- sig_mon*sqrt(12) # annual sd
# 
# param$MUH <- mu_ann
# param$SIGH <- sig_ann
# 
# # Collect all assets
# assets <- inner_join(bist100,realty,"Date") %>% rename(stock=Open,Rate.stock=Rate.x,house=house,Rate.house=Rate.y)# %>% select(Date,stock,house)
# param$RHOSH <- cor(assets[,c(3,5)])
# param$COVSH <- cov(assets$Rate.stock, assets$Rate.house) * 12
# param$VARCOVSH <- cov(assets[,c(3,5)]) * 12


















# Params with annual sectoral data 
# hhba.by_sector <- hhba %>%
#   group_by(sector,year) %>%
#   summarize_at(vars(salary_real),median) %>%
#   filter(sector %in% c(1,7,8)) %>% # agriculture, hotels, transportation
#   full_join(bist100.annual,by="year") %>% 
#   full_join(realty.annual,by='year') %>% 
#   slice(-1) %>% 
#   mutate_at(vars(salary_real,Open,house),function(x){c(0,diff(log(x)))}) %>% 
#   rename(stock=Open)


# kovs <- hhba.by_sector %>% 
#   relocate(salary_real,.after = house) %>% 
#   nest(data = -sector) %>% 
#   mutate(varkov = map(data,function(x){cov(x[,-1])})) %>% 
#   mutate(korr = map(data,function(x){cor(x[,-1])})) %>% 
#   select(sector,varkov,korr)

# param$RHO <- kovs$korr
# param$VAR <- kovs$varkov





# Wage
state.main <- hhba %>%
  group_by(educ,age) %>%
  summarize_at(vars(salary_real),median) %>%
  filter(educ %in% c("elementary","high","undergrad")) %>% 
  filter(age >= 25 & age < 25 + param$t) %>%
  mutate(age2 = age^2, lwage = log(salary_real)) %>% 
  #mutate(salary_fit=predict(lm(salary_real~age+age2))) %>%
  mutate(lwage_fit = predict(lm(lwage ~ age + age2)))
mutate(humcap = human.capital(salary_fit,param$rf),
       rL = c(0,diff(log(salary_fit))),
       thumb = (100-age)/100,
       cocco = cocco(age),
       markowitz = (param$muS - param$rf)/(1.5 * param$sdS^2),
       bodie = 0, munk = 0, h.munk = 0, s.munk = 0,
       fincap_bodie = 0,fincap_munk = 0,fincap_h.munk = 0) %>% 
  select(-salary_real,-age2,-rL)




#fc.means <- apply(simplify2array(fincaps.all[[2]][[1]]), c(1,2), mean)
#fc.sds <- apply(simplify2array(fincaps.all[[2]][[1]]), c(1,2), sd)
# for(X in 1:3){
#   for(Y in 1:4){
#     tmp <- sapply(fincaps.all[[X]][[Y]], function(x){x[1,6]})
#     tmp <- c(mean(tmp),sd(tmp))
#     print(tmp)
#   }
# }
# 
# ggplot(state.mc) +
#   geom_line(aes(age,markowitz)) +
#   geom_line(aes(age,thumb)) +
#   geom_line(aes(age,cocco)) +
#   geom_line(aes(age,bodie,col=educ)) +
#   geom_line(aes(age,munk,col=educ)) +
#   geom_line(aes(age,h.munk[,3],col=educ))
# 
# ggplot(state.mc) + 
#   geom_line(aes(age,fincap_thumb,col=educ,linetype="100-age")) + 
#   geom_line(aes(age,fincap_cocco,col=educ,linetype="Cocco et al.")) +
#   geom_line(aes(age,fincap_markowitz,col=educ,linetype="Markowitz")) +
#   geom_line(aes(age,fincap_bodie,col=educ,linetype="Bodie et al.")) +
#   geom_line(aes(age,fincap_munk,col=educ,linetype="Munk without housing")) +
#   geom_line(aes(age,fincap_h.munk,col=educ,linetype="Munk with housing"))
# 
# 
# ggplot(state.mc) + 
#   geom_area(aes(age,h.munk[,1] + h.munk[,2] + h.munk[,3],fill="Bond percentage")) +
#   geom_area(aes(age,h.munk[,2] + h.munk[,3],fill="House percentage")) + 
#   geom_area(aes(age,h.munk[,2], fill="Stock percentage")) +
#   facet_grid(~educ)


# Constant Relative Risk Aversion Utility function
crrautilOLD <- function(x, gamma, surv, delta=0.89){
  nT <- length(surv)
  deltas <- cumprod(rep(delta,nT))
  tmp <- deltas * surv * x^(1-gamma) / gamma
  output <- sum(tmp)
  return(output)
}
