rm(list=ls())
library('tidyverse')
library('lubridate')
library('urca')
library('forecast')
library('moments')
options(scipen=999)
source("_import_data.R")
source("_define_functions.R")


# Define parameters
param <- list()
param$RF <- 0.03 # risk-free rate
param$C <- 0.03 # % of wage invested
param$GAMMA <- 5 
param$RHOSL <- 0.0







# Figure 1. Stock price index (XU100)
bist100 <- bist100 %>% mutate(Rate=c(0,diff(log(Open))))

pdf("../tex/fig/bist100.pdf",height=5.83,width=8.27)
ggplot(bist100 %>% filter(Date>="2002-01-01")) + 
  geom_line(aes(Date,Open),col="red") +
  ylab("Stock price index") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# Figure C.12. Stock rate of return
pdf("../tex/fig/bist100_rate.pdf",height=5.83,width=8.27)
ggplot(bist100 %>% filter(Date>="2002-01-01")) + 
  geom_line(aes(Date,Rate),col="red") +
  ylab("Stock rate of return") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# Estimate stocks:
mu_mon <- mean(bist100$Rate) # daily mean
sig_mon <- sd(bist100$Rate) # daily sd
mu_ann <- (1+mu_mon)^12-1 # annual mean
sig_ann <- sig_mon*sqrt(12) # annual sd
param$MUS <- mu_ann
param$SIGS <- sig_ann



# Figure 2. House price index 
pdf("../tex/fig/realty.pdf",height=5.83,width=8.27)
ggplot(realty) +
  geom_line(aes(Date,house),col="blue") +
  ylab("House price index") +
  #scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

realty <- realty %>% mutate(Rate = c(0,diff(log(house))))
mu_mon <- mean(realty$Rate) # daily mean
sig_mon <- sd(realty$Rate) # daily sd
mu_ann <- (1+mu_mon)^12-1 # annual mean
sig_ann <- sig_mon*sqrt(12) # annual sd
param$MUH <- mu_ann
param$SIGH <- sig_ann





# Collect assets
assets <- inner_join(bist100,realty,"Date") %>% rename(stock=Open,Rate.stock=Rate.x,house=house,Rate.house=Rate.y)
param$RHOSH <- cor(assets[,c(3,5)])
param$COVSH <- cov(assets$Rate.stock, assets$Rate.house) * 12









# Figure 4. Life table
pdf("../tex/fig/survival.pdf",height=5.83,width=8.27)
ggplot(survival) +
  geom_point(aes(age,probability),size=3) +
  ylab("Survival probability") +
  xlab("Age") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()









# Figure 3. Hump-shaped lifetime income
hhba.by_age <- hhba %>% group_by(age) %>% summarize_at(vars(salary_real),median)
pdf("../tex/fig/humpwage.pdf",height=5.83,width=8.27)
ggplot(hhba.by_age) +
  geom_point(aes(age,salary_real),size=3) +
  ylab("Median real wage") +
  xlab("Age") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# Figure 5. Wage by education
hhba.by_educ <- hhba %>%
  group_by(educ,age) %>%
  summarize_at(vars(salary_real),median) %>%
  filter(age < 70)
pdf("../tex/fig/educwage.pdf",height=5.83,width=8.27)
ggplot(hhba.by_educ)+
  geom_line(aes(age,salary_real,col=educ),size=1) +
  xlab("Age") +
  ylab("Median real wage") +
  labs(col="") +
  scale_color_discrete(labels=c("Elementary","High school","Undergraduate"))+
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# Figure C.15. Wage curves
hhba.by_educ <- hhba.by_educ %>%
  mutate(age2=age^2) %>% 
  nest(data=-educ) %>%
  mutate(salary_fit=map(data, ~predict(lm(salary_real~age+age2, .)))) %>%
  unnest(c(data,salary_fit)) %>% 
  mutate(human_capital = human.capital(salary_fit,param$RF)) %>% 
  mutate(rL = c(0,diff(log(human_capital))))
pdf("../tex/fig/wagepred.pdf",height=5.83,width=8.27)
ggplot(hhba.by_educ) + 
  geom_point(aes(age,salary_real,col=educ)) + 
  geom_line(aes(age,salary_fit,group=educ)) +
  scale_color_discrete(labels=c("Elementary","High school","Undergraduate")) +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  labs(col="") +
  xlab("Age") +
  ylab("Median real wage") +
  theme_ravshan()
dev.off()

# Figure 6. Wage by sector
hhba.by_sector <- hhba %>%
  group_by(sector,year) %>%
  summarize_at(vars(salary_real),mean) %>%
  filter(sector %in% c(1,7,8))
hhba.by_sector <- right_join(hhba.by_sector,bist100.annual,by="year")
pdf("../tex/fig/sectorwage.pdf",height=5.83,width=8.27)
ggplot(hhba.by_sector) +
  geom_line(aes(year,salary_real,col=sector),size=1) +
  geom_line(aes(year,Open/450), col="black", linetype="dashed",size=1) +
  xlab("Year") +
  ylab("Median real wage") +
  labs(col="") +
  scale_color_discrete(labels=c("Agriculture","Hotels and restaurants","Transportation and Logistics","Normalized stock prices")) +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

## Figure 7. Human capital
pdf("../tex/fig/humcapital.pdf",height=5.83,width=8.27)
ggplot(hhba.by_educ) + 
  geom_line(aes(age,human_capital,col=educ),size=1) +
  scale_color_discrete(labels=c("Elementary","High school","Undergraduate")) +
  labs(col="") +
  xlab("Age") +
  ylab("Human capital") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# Figure 8. Financial capital
hhba.by_educ.fincap <- hhba.by_educ %>%
  mutate(fincap.hi =  fin.capital(0.03*salary_real, c(0.5,0.5), matrix( c(param$RF, param$MUS+param$SIGS), n(), 2, byrow = TRUE )),
         fincapital =  fin.capital(0.03*salary_real, c(0.5,0.5), matrix( c(param$RF, param$MUS), n(), 2, byrow = TRUE )),
         fincap.lo =  fin.capital(0.03*salary_real, c(0.5,0.5), matrix( c(param$RF, param$MUS-param$SIGS), n(), 2, byrow = TRUE ))
  )

pdf("../tex/fig/fincapital.pdf",height=5.83,width=8.27)
ggplot(hhba.by_educ.fincap) +
  geom_line(aes(age,fincapital,col=educ)) +
  geom_ribbon(aes(age,fincapital,ymin=fincap.lo, ymax=fincap.hi,fill=educ),alpha=0.1) +
  scale_color_discrete(labels=c("Elementary","High school","Undergraduate")) +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  guides(fill=FALSE) +
  labs(col="",fill="") +
  xlab("Age") +
  ylab("Financial capital") +
  theme_ravshan()
dev.off()







# Figure 9. Portfolio allocations (homogenous)
ages <- c(25:65)
thumb.pct <- 100 - ages

# 9.a. 100-age
pdf("../tex/fig/allocation1.pdf",height=5.83,width=8.27)
ggplot() +
  geom_area(aes(ages,100,fill="Bond percentage")) +
  geom_area(aes(ages,thumb.pct,fill="Stock percentage")) + 
  labs(fill="") + 
  xlab("Age") +
  ylab("Portfolio") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# 9.b. Cocco et al.
cocco.pct <- cocco(ages)*100
pdf("../tex/fig/allocation2.pdf",height=5.83,width=8.27)
ggplot() +
  geom_area(aes(ages,100,fill="Bond percentage")) +
  geom_area(aes(ages,cocco.pct,fill="Stock percentage")) + 
  labs(fill="") + 
  xlab("Age") +
  ylab("Portfolio") +
  scale_x_continuous(expand = c(0.015,0.015)) +
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# 9.c. Markowitz
markowitz.share <- 100 * (param$MUS - param$RF)/(1.5 * param$SIGS^2)
pdf("../tex/fig/allocation3.pdf",height=5.83,width=8.27)
ggplot() +
  geom_area(aes(ages,100,fill="Bond percentage")) +
  geom_area(aes(ages,markowitz.share,fill="Stock percentage")) + 
  labs(fill="") + 
  xlab("Age") +
  ylab("Portfolio") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

# 10.a. Bodie et al.
h.bodie <- hhba.by_educ.fincap %>% 
  filter(educ=="undergrad") %>% 
  ungroup() %>% 
  select(age,salary_fit,human_capital) %>% 
  mutate(
  alpha = fin.capital.bodie( human_capital, param$C*salary_fit, param$MUS, param$RF, param$GAMMA, param$SIGS, matrix( c(param$RF,param$MUS), n(), 2, TRUE ))$x * 100,
  fin_capital = fin.capital.bodie( human_capital, param$C*salary_fit, param$MUS, param$RF, param$GAMMA, param$SIGS, matrix( c(param$RF,param$MUS), n(), 2, TRUE ))$fc
)

pdf("../tex/fig/allocation4.pdf",height=5.83,width=8.27)
ggplot(h.bodie) + 
  geom_area(aes(age,100,fill="Bond percentage")) +
  geom_area(aes(age,alpha,fill="Stock percentage")) + 
  labs(fill="") + 
  xlab("Age") +
  ylab("Portfolio") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()


# 10.b. Constrained Munk no housing
h.munk <- h.bodie %>% 
  mutate(alpha = fin.capital.munk(human_capital, param$C*salary_fit, 30, param$RF, param$MUS, param$SIGS^2, 0.001215349, matrix( c(param$RF,param$MUS), n(), 2, TRUE ))$x[,2] * 100,
         fin_capital = fin.capital.munk(human_capital, param$C*salary_fit, 30, param$RF, param$MUS, param$SIGS^2, 0.001215349, matrix( c(param$RF,param$MUS), n(), 2, TRUE ))$fc)

pdf("../tex/fig/allocation5.pdf",height=5.83,width=8.27)
ggplot(h.munk) + 
  geom_area(aes(age,100,fill="Bond percentage")) +
  geom_area(aes(age,alpha,fill="Stock percentage")) + 
  labs(fill="") + 
  xlab("Age") +
  ylab("Portfolio") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
dev.off()

param$GAMMA <- 5
# 10.b. Constrained Munk with housing
h.munk2 <- h.bodie %>% 
  mutate(
    alpha1 = fin.capital.munk(Lx = human_capital,
                              inputs = param$C*salary_fit, 
                              gamma = param$GAMMA,
                              rf =  param$RF,
                              mu =  c(param$MUS,param$MUH),
                              sig =  matrix(c(param$SIGS^2,param$COVSH,param$COVSH,param$SIGH^2),2),
                              cov_L = c(0.0000319,0.0000834),
                              rates = matrix( c(param$RF,param$MUS,param$MUH), n(), 3, TRUE ))$x[,2]*100,
    alpha2 = fin.capital.munk(Lx = human_capital,
                              inputs = param$C*salary_fit,
                              gamma = param$GAMMA,
                              rf =  param$RF,
                              mu = c(param$MUS,param$MUH),
                              sig = matrix(c(param$SIGS^2,param$COVSH,param$COVSH,param$SIGH^2),2),
                              cov_L =  c(0.0000319,0.0000834), matrix( c(param$RF,param$MUS,param$MUH), n(), 3, TRUE ))$x[,3]*100,
    fin_capital = fin.capital.munk(Lx = human_capital,
                                   inputs = param$C*salary_fit,
                                   gamma =  param$GAMMA,
                                   rf = param$RF, 
                                   mu = c(param$MUS,param$MUH),
                                   sig = matrix(c(param$SIGS^2,param$COVSH,param$COVSH,param$SIGH^2),2),
                                   cov_L = c(0.0000319,0.0000834), matrix( c(param$RF,param$MUS,param$MUH), n(), 3, TRUE ))$fc
    )


#pdf("../tex/fig/allocation6.pdf",height=5.83,width=8.27)
ggplot(h.munk2) + 
  geom_area(aes(age,100,fill="Bond percentage")) +
  geom_area(aes(age,alpha1+alpha2,fill="House percentage")) + 
  geom_area(aes(age,alpha1,fill="Stock percentage")) + 
  labs(fill="") + 
  xlab("Age") +
  ylab("Portfolio") +
  scale_x_continuous(expand = c(0.015,0.015)) + 
  scale_y_continuous(expand = c(0.015,0.015)) +
  theme_ravshan()
#dev.off()




# Table 1: Descriptive statistics
trbond <- trbond %>% mutate(Rate=Rate/100)

desc.tab(bist100$Rate)
desc.tab(realty$Rate)
desc.tab(trbond$Rate)
