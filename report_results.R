# rm(list=ls())
# library('tidyverse')
# library('lubridate')
# library('urca')
# library('forecast')
# options(scipen=999)
# 
# # IMPORT THE _SIMULATION_RESULTS.RDATA FILE FIRST!!!
# setwd("~/Dropbox/ravshan_tez/codes/")
# source("_import_data.R")
# source("_define_functions.R")

load("_simulation_results.RData")




# MAIN RESULTS

fc.elem <- list()
fc.high <- list()
fc.ugrad <- list()

# X is sector rhos; # rhoSL 0 0.2 0.4
# Y is risk aversions # gamma 1.5 3 5 10
for(X in 2){
  fc.elem[[X]] <- list()
  fc.high[[X]] <- list()
  fc.ugrad[[X]] <- list()
  for(Y in 1){
    fcelem <- apply(t(sapply(fincaps.all[[X]][[Y]],function(x){x[1,]})),2,remove.outliers)
    fchigh <- apply(t(sapply(fincaps.all[[X]][[Y]],function(x){x[2,]})),2,remove.outliers)
    fcugrad <- apply(t(sapply(fincaps.all[[X]][[Y]],function(x){x[3,]})),2,remove.outliers)
    fc.elem[[X]][[Y]] <- stack(as.data.frame(fcelem))
    fc.high[[X]][[Y]] <- stack(as.data.frame(fchigh))
    fc.ugrad[[X]][[Y]] <- stack(as.data.frame(fcugrad))
    
    pdf(paste0("../tex/fig/sim/elem",X,"-",Y,".pdf"),height=7,width=7)
    print(ggplot(fc.elem[[X]][[Y]]) +
      geom_boxplot(aes(y = values,fill=ind),alpha=0.7) +
      #ylim(quantile(fc.elem[[X]][[Y]][,1],c(0.001,0.999))) +
      labs(fill="") + 
      xlab("Portfolios") +
      ylab("Financial capital") +
      scale_fill_discrete(labels=c("Markowitz","100-age","Cocco et al.", "Bodie et al.", "Munk without housing", "Munk with housing")) +
      ggtitle("Elementary school") + 
      theme_ravshan() + 
      theme(axis.text.x = element_blank())
    )
    dev.off()
    
    pdf(paste0("../tex/fig/sim/high",X,"-",Y,".pdf"),height=7,width=7)
    print(ggplot(fc.high[[X]][[Y]]) +
      geom_boxplot(aes(y = values,fill=ind),alpha=0.7) +
      #ylim(quantile(fc.high[[X]][[Y]][,1],c(0.001,0.999))) +
      labs(fill="") + 
      xlab("Portfolios") +
      ylab("Financial capital") +
      scale_fill_discrete(labels=c("Markowitz","100-age","Cocco et al.", "Bodie et al.", "Munk without housing", "Munk with housing")) +
      #ggtitle("High school") + 
      theme_ravshan() + 
      theme(axis.text.x = element_blank())
    )
    dev.off()
    
    pdf(paste0("../tex/fig/sim/ugrad",X,"-",Y,".pdf"),height=7,width=7)
    print(ggplot(fc.ugrad[[X]][[Y]]) +
      geom_boxplot(aes(y = values,fill=ind),alpha=0.7) +
      #ylim(quantile(fc.ugrad[[X]][[Y]][,1],c(0.001,0.999))) +
      labs(fill="") + 
      xlab("Portfolios") +
      ylab("Financial capital") +
      scale_fill_discrete(labels=c("Markowitz","100-age","Cocco et al.", "Bodie et al.", "Munk without housing", "Munk with housing")) +
      ggtitle("Undergraduate") + 
      theme_ravshan() + 
      theme(axis.text.x = element_blank())
    )
    dev.off()
    
  }
}

apply(simplify2array(fincaps.all[[2]][[4]]),1:2,function(x){round(mean(remove.outliers(x)),3)})
apply(simplify2array(fincaps.all[[2]][[4]]),1:2,function(x){round(sd(remove.outliers(x)),3)})












# MEAN VARIANCE UTILITY
vals <- data.frame(name=NA,util=NA,gamma=NA,rho=NA)
for(X in 1:3){
  for(Y in 1:4){
    print(paste0("X",X,"Y",Y))
    kj <- apply(simplify2array(fincaps.all[[X]][[Y]]),1:2,function(x){meanvar(remove.outliers(x),param$gamma[Y])})[2,]
    kjdf <- data.frame(name=names(kj),util=unname(kj),gamma=param$gamma[Y],rho=param$rhoSL[X])
    vals <- rbind(vals,kjdf)
  }
}
vals <- vals[-1,]

#pdf("../tex/fig/sim/utilgamma.pdf",height=7,width=7.2)
pdf("../tex/fig/sim/utilgamma.pdf",height=5.83,width=8.27)
ggplot(vals) + 
  geom_line(aes(gamma,util,col=name),size=2) +
  geom_point(aes(gamma,util,col=name),size=5) +
  facet_wrap(~rho) +
  labs(col="") + 
  xlab("Risk aversion") +
  ylab("Utility") +
  scale_x_continuous(breaks = c(1.5,3,5,10)) +
  scale_color_hue(labels=c("Bodie et al.","Cocco et al.","Munk with housing","Markowitz","Munk without housing","100-age")) +
  theme_ravshan()
dev.off()









# MEAN VARIANCE UTILITY SENSITIVITY IN RHO
vals <- data.frame(name=NA,util=NA,gamma=NA,rho=NA)
for(X in 1:3){
  for(Y in 1:4){
    print(paste0("X",X,"Y",Y))
    kj <- apply(simplify2array(fincaps.all[[X]][[Y]]),1:2,function(x){meanvar(remove.outliers(x),param$gamma[Y])})[2,]
    kjdf <- data.frame(name=names(kj),util=unname(kj),gamma=param$gamma[Y],rho=param$rhoSL[X])
    vals <- rbind(vals,kjdf)
  }
}
vals <- vals[-1,]

#pdf("../tex/fig/sim/utilrho.pdf",height=7,width=7.2)
pdf("../tex/fig/sim/utilrhosens.pdf",height=7,width=8)
ggplot(vals) + 
  geom_line(aes(rho,util,col=name),size=2) +
  geom_point(aes(rho,util,col=name),size=5) +
  facet_wrap(~gamma) +
  labs(col="") + 
  xlab("Stock-income correlation") +
  ylab("Utility") +
  scale_x_continuous(breaks = c(0,0.2,0.4)) +
  scale_color_hue(labels=c("Bodie et al.","Cocco et al.","Munk with housing","Markowitz","Munk without housing","100-age")) +
  theme_ravshan()
dev.off()

















# 
# 
# # Sensitivity in RHOSL
# 
# sens.rho.elem <- list()
# sens.rho.high <- list()
# sens.rho.ugrad <- list()
# 
# for(X in 1:3){
#   tmp <- stack(as.data.frame(t(sapply(fincaps.all[[X]][[4]],function(x){x[1,-c(1,2,3)]}))))
#   tmp$rhoSL = param$rhoSL[X]
#   sens.rho.elem[[X]] <- tmp
#   
#   tmp <- stack(as.data.frame(t(sapply(fincaps.all[[X]][[4]],function(x){x[2,-c(1,2,3)]}))))
#   tmp$rhoSL = param$rhoSL[X]
#   sens.rho.high[[X]] <- tmp
#   
#   tmp <- stack(as.data.frame(t(sapply(fincaps.all[[X]][[4]],function(x){x[3,-c(1,2,3)]}))))
#   tmp$rhoSL = param$rhoSL[X]
#   sens.rho.ugrad[[X]] <- tmp
# }
# 
# sens.rho.elem <- bind_rows(sens.rho.elem)
# sens.rho.high <- bind_rows(sens.rho.high)
# sens.rho.ugrad<- bind_rows(sens.rho.ugrad)
# 
# pdf("../tex/fig/sim/sens_rho_high.pdf",height=5.83,width=8.27)
# ggplot(sens.rho.high) +
#   geom_boxplot(mapping = aes(y = values,fill=ind),alpha=0.7) +
#   facet_wrap(~rhoSL) +
#   ylim(quantile(sens.rho.high[,1],c(0.001,0.999))) +
#   labs(fill="") + 
#   xlab("Portfolios") +
#   ylab("Financial capital") +
#   scale_fill_discrete(labels=c("Bodie et al.", "Munk without housing", "Munk with housing")) +
#   #ggtitle(paste0("Undergraduate; rho sensitivity; gamma=",param$gamma[Y])) + 
#   theme_ravshan() + 
#   theme(axis.text.x = element_blank()) 
# dev.off()
# 
# 
# 
# # Sensitivity in GAMMA
# 
# 
# sens.gamma.elem <- list()
# sens.gamma.high <- list()
# sens.gamma.ugrad <- list()
# 
# for(Y in 1:4){
#   tmp <- stack(as.data.frame(t(sapply(fincaps.all[[3]][[Y]],function(x){x[1,-c(2,3)]}))))
#   tmp$gamma = param$gamma[Y]
#   sens.gamma.elem[[Y]] <- tmp
#   
#   tmp <- stack(as.data.frame(t(sapply(fincaps.all[[3]][[Y]],function(x){x[2,-c(2,3)]}))))
#   tmp$gamma = param$gamma[Y]
#   sens.gamma.high[[Y]] <- tmp
#   
#   tmp <- stack(as.data.frame(t(sapply(fincaps.all[[3]][[Y]],function(x){x[3,-c(2,3)]}))))
#   tmp$gamma = param$gamma[Y]
#   sens.gamma.ugrad[[Y]] <- tmp
# }
# 
# sens.gamma.elem <- bind_rows(sens.gamma.elem)
# sens.gamma.high <- bind_rows(sens.gamma.high)
# sens.gamma.ugrad<- bind_rows(sens.gamma.ugrad)
# 
# pdf("../tex/fig/sim/sens_gamma_elem.pdf",height=5.83,width=8.27)
# ggplot(sens.gamma.elem) +
#   geom_boxplot(mapping = aes(y = values,fill=ind),alpha=0.7) +
#   facet_wrap(~gamma,nrow=1) +
#   ylim(quantile(sens.gamma.elem[,1],c(0.001,0.999))) +
#   labs(fill="") + 
#   xlab("Portfolios") +
#   ylab("Financial capital") +
#   scale_fill_discrete(labels=c("Markowitz", "Bodie et al.", "Munk without housing", "Munk with housing")) +
#   theme_ravshan() + 
#   theme(axis.text.x = element_blank()) 
# dev.off()
# 
# pdf("../tex/fig/sim/sens_gamma_high.pdf",height=5.83,width=8.27)
# ggplot(sens.gamma.high) +
#   geom_boxplot(mapping = aes(y = values,fill=ind),alpha=0.7) +
#   facet_wrap(~gamma,nrow=1) +
#   ylim(quantile(sens.gamma.high[,1],c(0.001,0.999))) +
#   labs(fill="") + 
#   xlab("Portfolios") +
#   ylab("Financial capital") +
#   scale_fill_discrete(labels=c("Markowitz", "Bodie et al.", "Munk without housing", "Munk with housing")) +
#   theme_ravshan() + 
#   theme(axis.text.x = element_blank()) 
# dev.off()
# 
# 
# pdf("../tex/fig/sim/sens_gamma_ugrad.pdf",height=5.83,width=8.27)
# ggplot(sens.gamma.ugrad) +
#   geom_boxplot(mapping = aes(y = values,fill=ind),alpha=0.7) +
#   facet_wrap(~gamma,nrow=1) +
#   ylim(quantile(sens.gamma.ugrad[,1],c(0.001,0.999))) +
#   labs(fill="") + 
#   xlab("Portfolios") +
#   ylab("Financial capital") +
#   scale_fill_discrete(labels=c("Markowitz", "Bodie et al.", "Munk without housing", "Munk with housing")) +
#   theme_ravshan() + 
#   theme(axis.text.x = element_blank()) 
# dev.off()
# 
# 
# 
# 
# 
# # ANNUITITES / UTILITY / WELFARE 
# 
# 
# # X is sector rhos
# # Y is risk aversions
# u.elem <- list()
# u.high <- list()
# u.ugrad <- list()
# denom <- annuity(1, survival.65, 0.03)$denom
# param$denom <- denom
# for(X in 2){
#   u.elem[[X]] <- list()
#   u.high[[X]] <- list()
#   u.ugrad[[X]] <- list()
#   for(Y in 1:4){
#     u.elem[[X]][[Y]] <- stack(as.data.frame(t(sapply(fincaps.all[[X]][[Y]],function(x){ crrautil(x[1,]/denom,param$gamma[Y],survival.65,param$delta)}))))
#     u.high[[X]][[Y]] <- stack(as.data.frame(t(sapply(fincaps.all[[X]][[Y]],function(x){ crrautil(x[2,]/denom,param$gamma[Y],survival.65,param$delta)}))))
#     u.ugrad[[X]][[Y]]<- stack(as.data.frame(t(sapply(fincaps.all[[X]][[Y]],function(x){ crrautil(x[3,]/denom,param$gamma[Y],survival.65,param$delta)}))))
#     
#     pdf(paste0("../tex/fig/sim/uelem",X,"-",Y,".pdf"),height=7,width=7)
#     print(ggplot(u.elem[[X]][[Y]]) +
#             geom_boxplot(aes(y = values,fill=ind),alpha=0.7) +
#             ylim(quantile(u.elem[[X]][[Y]][,1],c(0.001,0.999),na.rm=T)) +
#             labs(fill="") + 
#             xlab("Portfolios") +
#             ylab("Financial capital") +
#             scale_fill_discrete(labels=c("Markowitz","100-age","Cocco et al.", "Bodie et al.", "Munk without housing", "Munk with housing")) +
#             ggtitle(paste0("Elementary; rho=",param$rhoSL[X],"; gamma=",param$gamma[Y])) + 
#             theme_ravshan() + 
#             theme(axis.text.x = element_blank())
#     )
#     dev.off()
#     
#     pdf(paste0("../tex/fig/sim/uhigh",X,"-",Y,".pdf"),height=7,width=7)
#     print(ggplot(u.high[[X]][[Y]]) +
#             geom_boxplot(aes(y = values,fill=ind),alpha=0.7) +
#             ylim(quantile(u.high[[X]][[Y]][,1],c(0.001,0.999),na.rm=T)) +
#             labs(fill="") + 
#             xlab("Portfolios") +
#             ylab("Financial capital") +
#             scale_fill_discrete(labels=c("Markowitz","100-age","Cocco et al.", "Bodie et al.", "Munk without housing", "Munk with housing")) +
#             #ggtitle(paste0("High; rho=",param$rhoSL[X],"; gamma=",param$gamma[Y])) + 
#             theme_ravshan() + 
#             theme(axis.text.x = element_blank())
#     )
#     dev.off()
#     
#     pdf(paste0("../tex/fig/sim/uugrad",X,"-",Y,".pdf"),height=7,width=7)
#     print(ggplot(u.ugrad[[X]][[Y]]) +
#             geom_boxplot(aes(y = values,fill=ind),alpha=0.7) +
#             ylim(quantile(u.ugrad[[X]][[Y]][,1],c(0.001,0.999),na.rm=T)) +
#             labs(fill="") + 
#             xlab("Portfolios") +
#             ylab("Financial capital") +
#             scale_fill_discrete(labels=c("Markowitz","100-age","Cocco et al.", "Bodie et al.", "Munk without housing", "Munk with housing")) +
#             ggtitle(paste0("Undergraduate; rho=",param$rhoSL[X],"; gamma=",param$gamma[Y])) + 
#             theme_ravshan() + 
#             theme(axis.text.x = element_blank())
#     )
#     dev.off()
    
  }
}



# X is sector rhos
# Y is risk aversions
# RATIOS
ratios <- list()
for(X in 1:3){
  ratios[[X]] <- list()
  for(Y in 1:4){
      # Set current params
      RHO <- make.corr(xy = param$rhoSH, xz = param$rhoSL[X], yz = param$rhoHL)
      covL <- c(param$covSL[X], param$covHL)
      # generate correlated asset series (Stock returns, Housing returns, Wage shocks)
      assets.mc <- ascheberg(mus = c(param$muS,param$muH,param$muL), sds = c(param$sdS,param$sdH,param$sdL), corr = RHO, Tp =  param$t)
      # actual calculation
      ratios[[X]][[Y]] <- state.main %>% 
        mutate(wage = exp(lwage_fit)) %>% 
        mutate(humcap = human.capital(wage,param$rf)) %>% 
        mutate(bodie = fin.capital.bodie(humcap, param$c*wage, param$muS, param$rf, param$gamma[Y], param$sdS, assets.mc[,1])$x,
               munk = fin.capital.munk(humcap, param$c*wage, param$gamma[Y], param$rf, param$muS, param$sdS^2, covL[1], cbind(param$rf,assets.mc[,1]))$x[,2],
               h.munk = fin.capital.munk(humcap, param$c*wage, param$gamma[Y], param$rf, c(param$muS,param$muH), param$varcov, covL, cbind(param$rf,assets.mc[,1:2]))$x
        ) %>%
        select(educ,age,bodie,munk,h.munk)
  }
}

rho0 <- sapply(ratios[[1]], function(x){x$bodie})
rho2 <- sapply(ratios[[2]], function(x){x$bodie})
rho4 <- sapply(ratios[[3]], function(x){x$bodie})
rho.all <- as.data.frame(cbind(rho0,rho2,rho4))
rho.all$Age <- c(25:64)
rho.all$Education <- rep(c("Elementary","High","Undergraduate"),each=40)

pdf("../tex/fig/sim/bodies.pdf",height=6,width=12.00)
ggplot(rho.all) +
  geom_line(aes(Age, V1, col="rho=0", linetype="gamma=1.5")) +
  geom_line(aes(Age, V2, col="rho=0", linetype="gamma=3")) +
  geom_line(aes(Age, V3, col="rho=0", linetype="gamma=5")) +
  geom_line(aes(Age, V4, col="rho=0", linetype="gamma=10")) +
  geom_line(aes(Age, V5, col="rho=0.2", linetype="gamma=1.5")) +
  geom_line(aes(Age, V6, col="rho=0.2", linetype="gamma=3")) +
  geom_line(aes(Age, V7, col="rho=0.2", linetype="gamma=5")) +
  geom_line(aes(Age, V8, col="rho=0.2", linetype="gamma=10")) +
  geom_line(aes(Age, V9, col="rho=0.4", linetype="gamma=1.5")) +
  geom_line(aes(Age, V10, col="rho=0.4", linetype="gamma=3")) +
  geom_line(aes(Age, V11, col="rho=0.4", linetype="gamma=5")) +
  geom_line(aes(Age, V12, col="rho=0.4", linetype="gamma=10")) +
  facet_wrap(~Education) +
  ylab("Stock share") +
  labs(col="",linetype="") +
  theme_ravshan()
dev.off()




# MUNK
rho0 <- sapply(ratios[[1]], function(x){x$munk})
rho2 <- sapply(ratios[[2]], function(x){x$munk})
rho4 <- sapply(ratios[[3]], function(x){x$munk})
rho.all <- as.data.frame(cbind(rho0,rho2,rho4))
rho.all$Age <- c(25:64)
rho.all$Education <- rep(c("Elementary","High","Undergraduate"),each=40)

pdf("../tex/fig/sim/munks.pdf",height=6,width=12.00)
ggplot(rho.all) +
  geom_line(aes(Age, V1, col="rho=0", linetype="gamma=1.5")) +
  geom_line(aes(Age, V2, col="rho=0", linetype="gamma=3")) +
  geom_line(aes(Age, V3, col="rho=0", linetype="gamma=5")) +
  geom_line(aes(Age, V4, col="rho=0", linetype="gamma=10")) +
  geom_line(aes(Age, V5, col="rho=0.2", linetype="gamma=1.5")) +
  geom_line(aes(Age, V6, col="rho=0.2", linetype="gamma=3")) +
  geom_line(aes(Age, V7, col="rho=0.2", linetype="gamma=5")) +
  geom_line(aes(Age, V8, col="rho=0.2", linetype="gamma=10")) +
  geom_line(aes(Age, V9, col="rho=0.4", linetype="gamma=1.5")) +
  geom_line(aes(Age, V10, col="rho=0.4", linetype="gamma=3")) +
  geom_line(aes(Age, V11, col="rho=0.4", linetype="gamma=5")) +
  geom_line(aes(Age, V12, col="rho=0.4", linetype="gamma=10")) +
  facet_wrap(~Education) +
  ylab("Stock share") +
  labs(col="",linetype="") +
  theme_ravshan()
dev.off()



# MUNK STOCK
rho0 <- sapply(ratios[[1]], function(x){x$h.munk[,2]})
rho2 <- sapply(ratios[[2]], function(x){x$h.munk[,2]})
rho4 <- sapply(ratios[[3]], function(x){x$h.munk[,2]})
rho.all <- as.data.frame(cbind(rho0,rho2,rho4))
rho.all$Age <- c(25:64)
rho.all$Education <- rep(c("Elementary","High","Undergraduate"),each=40)

pdf("../tex/fig/sim/h.munks1.pdf",height=6,width=12.00)
ggplot(rho.all) +
  geom_line(aes(Age, V1, col="rho=0", linetype="gamma=1.5")) +
  geom_line(aes(Age, V2, col="rho=0", linetype="gamma=3")) +
  geom_line(aes(Age, V3, col="rho=0", linetype="gamma=5")) +
  geom_line(aes(Age, V4, col="rho=0", linetype="gamma=10")) +
  geom_line(aes(Age, V5, col="rho=0.2", linetype="gamma=1.5")) +
  geom_line(aes(Age, V6, col="rho=0.2", linetype="gamma=3")) +
  geom_line(aes(Age, V7, col="rho=0.2", linetype="gamma=5")) +
  geom_line(aes(Age, V8, col="rho=0.2", linetype="gamma=10")) +
  geom_line(aes(Age, V9, col="rho=0.4", linetype="gamma=1.5")) +
  geom_line(aes(Age, V10, col="rho=0.4", linetype="gamma=3")) +
  geom_line(aes(Age, V11, col="rho=0.4", linetype="gamma=5")) +
  geom_line(aes(Age, V12, col="rho=0.4", linetype="gamma=10")) +
  facet_wrap(~Education) +
  ylab("Stock share") +
  labs(col="",linetype="") +
  theme_ravshan()
dev.off()



# MUNK STOCK
rho0 <- sapply(ratios[[1]], function(x){x$h.munk[,3]})
rho2 <- sapply(ratios[[2]], function(x){x$h.munk[,3]})
rho4 <- sapply(ratios[[3]], function(x){x$h.munk[,3]})
rho.all <- as.data.frame(cbind(rho0,rho2,rho4))
rho.all$Age <- c(25:64)
rho.all$Education <- rep(c("Elementary","High","Undergraduate"),each=40)

pdf("../tex/fig/sim/h.munks2.pdf",height=6,width=12.00)
ggplot(rho.all) +
  geom_line(aes(Age, V1, col="rho=0", linetype="gamma=1.5")) +
  geom_line(aes(Age, V2, col="rho=0", linetype="gamma=3")) +
  geom_line(aes(Age, V3, col="rho=0", linetype="gamma=5")) +
  geom_line(aes(Age, V4, col="rho=0", linetype="gamma=10")) +
  geom_line(aes(Age, V5, col="rho=0.2", linetype="gamma=1.5")) +
  geom_line(aes(Age, V6, col="rho=0.2", linetype="gamma=3")) +
  geom_line(aes(Age, V7, col="rho=0.2", linetype="gamma=5")) +
  geom_line(aes(Age, V8, col="rho=0.2", linetype="gamma=10")) +
  geom_line(aes(Age, V9, col="rho=0.4", linetype="gamma=1.5")) +
  geom_line(aes(Age, V10, col="rho=0.4", linetype="gamma=3")) +
  geom_line(aes(Age, V11, col="rho=0.4", linetype="gamma=5")) +
  geom_line(aes(Age, V12, col="rho=0.4", linetype="gamma=10")) +
  facet_wrap(~Education) +
  ylab("House share") +
  labs(col="",linetype="") +
  theme_ravshan()
dev.off()

