# CPI
cpi <- read_csv("../data/pricelevel.csv")
cpi <- cpi %>% rename(Date=DATE,value=TURCPIALLMINMEI)
cpi.annual <- cpi %>% group_by(year(Date)) %>% summarize(cpi=mean(value)) %>% rename(year=`year(Date)`)#filter(year<=2017 & year>=2002)

# Household Budget Survey Data
hhba <- read_csv("../data/hhba17.csv")
hhba <- hhba %>% 
  rename(age=YAS,male=CINSIYET,educ=EGITIM,sector=SEKKOD,marital=MEDENIDR,
         union=SENDIKA,salary=UCRN_YIL,side=EKGLN_YL,total=TOPGN_YL) %>% 
  select(year,age,male,educ,marital,sector,union,salary,side,total) %>% 
  mutate_at(vars(male,union),function(x){ifelse(x==2,0,1)}) %>%
  mutate_at(vars(male,union),as.logical) %>% 
  mutate_at(vars(educ,marital,sector),as_factor) %>% 
  filter(!is.na(salary)) %>% 
  filter(age>=18 & age<=80) %>% 
  mutate(salary=ifelse(year<2005,salary/1000000,salary)) %>% 
  mutate(educ=fct_collapse(educ,elementary=c("1","2","3","4","5","6"), high=c("7","8","9"), undergrad=c("10","11"))) %>% 
  filter(educ %in% c("elementary","high","undergrad"))
hhba <- right_join(hhba,cpi.annual,by="year")
hhba$salary_real <- hhba$salary/hhba$cpi
hhba <- hhba %>%
  filter(salary_real < quantile(salary_real,0.99,na.rm=T) & salary_real > quantile(salary_real,0.01,na.rm=T)) %>% 
  filter(salary_real>0)

# BIST30
bist30 <- read_csv("../data/xu030.csv")
bist30 <- bist30 %>% 
  mutate_at(vars(Date),function(x){as_date(x,format="%B %d,%Y")}) %>% 
  rename(Change=`Change %`) %>% 
  select(Date,Price,Change)

# BIST100
bist100 <- read_csv("../data/xu100.csv")
bist100 <- bist100 %>% 
  filter(Date<="2020-02-01") %>% # exclude global pandemic
  mutate_at(vars(Date), function(x){floor_date(x,unit="month")}) %>% 
  group_by(Date) %>% 
  summarize_at(vars(Open),first) %>% 
  na.omit()


bist100.annual <- bist100 %>% 
  group_by(year(Date)) %>% 
  summarize_at(vars(Open),mean) %>% 
  rename(year=`year(Date)`) %>% 
  filter(year<=2017 & year>=2002)


# Istanbul Housing Price Data
realty <- read_csv("../data/housing.csv")

realty.annual <- realty %>% 
  group_by(year(Date)) %>% 
  summarize_at(vars(house),mean) %>% 
  rename(year=`year(Date)`) %>% 
  filter(year<=2017 & year>=2002)
                   
# Survival
survival <- read_csv("../data/survival.csv")
survival.65 <- filter(survival,age>65)$probability/survival$probability[66]

# 10y bond maturity rates
trbond <- read_csv("../data/trbond.csv")