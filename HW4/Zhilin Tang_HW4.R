# ECON613_HW4 Zhilin Tang 
options(warn = -1)
options(scipen=20)

setwd("~/Desktop/ECON613/A4/Data")

library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(tinytex)
library(tidyr)
library(panelr)
library(stats)
library(stargazer)


########### Exercise 1 ###########
dat_raw = read.csv('dat_A4.csv', header=TRUE)
dat = dat_raw[,-1]

# 1.1 Create additional variable for the age of the agent `age`, total work experience measured in years `work_exp`.
# age
dat['age'] = 2019 - dat$KEY_BDATE_Y_1997
# work_exp
worked_week = c(paste0('CV_WKSWK_JOB_DLI.0',1:9,'_2019'),
                'CV_WKSWK_JOB_DLI.10_2019',
                'CV_WKSWK_JOB_DLI.11_2019')
dat = dat %>%
  mutate_at(worked_week,funs(ifelse(is.na(.),0,.))) %>%
  mutate(work_exp=rowSums(select(.,CV_WKSWK_JOB_DLI.01_2019:CV_WKSWK_JOB_DLI.11_2019))/52)

# 1.2 Create additional education variables indicating total years of schooling from all variables related to education 
dat['edu'] = case_when(
  dat$YSCH.3113_2019==1 ~ 0,  # None
  dat$YSCH.3113_2019==2 ~ 4,  # GED
  dat$YSCH.3113_2019==3 ~ 12, # High school diploma
  dat$YSCH.3113_2019==4 ~ 15, # AA
  dat$YSCH.3113_2019==5 ~ 16, # BA, BS
  dat$YSCH.3113_2019==6 ~ 18, # MA, MS
  dat$YSCH.3113_2019==7 ~ 22, # PhD
  dat$YSCH.3113_2019==8 ~ 18  # DDS, JD, MD
)

# 1.3 Provide the following visualizations
# 1.3.1 Plot the income data (where income is positive)
dat131 = dat %>%
  filter(YINC_1700_2019>0)
# i) by age groups
ggplot(dat131,aes(as.character(age),YINC_1700_2019)) + 
  geom_boxplot() + 
  xlab('age') +
  ylab('income') +
  ggtitle('Boxplot of income by age groups')
  
# ii) by gender groups 
ggplot(dat131,aes(as.character(KEY_SEX_1997),YINC_1700_2019)) + 
  geom_boxplot() +
  xlab('gender') + 
  ylab('income') + 
  ggtitle('Boxplot of income by gender groups')

# iii) by number of children
ggplot(dat131%>%filter(!is.na(CV_BIO_CHILD_HH_U18_2019)),
       aes(as.character(CV_BIO_CHILD_HH_U18_2019),YINC_1700_2019)) + 
  geom_boxplot() +
  xlab('number of children') + 
  ylab('income') +
  ggtitle('Boxplot of income by number of children')

# 1.3.2 Table the share of "0" in the income data 
dat132 = dat %>%
  filter(YINC_1700_2019>=0)
dat132_0 = dat %>%
  filter(YINC_1700_2019==0)
# i) by age groups
table_income = dat132 %>%
  group_by(age) %>%
  summarise(n = n()) 
table_income0 = dat132_0 %>%
  group_by(age) %>%
  summarise(n0 = n()) 
cbind(age=table_income$age,
      share_of_0=table_income0$n0/table_income$n)

# ii) by gender groups
table_gender = dat132 %>%
  filter(KEY_SEX_1997>0) %>%
  group_by(KEY_SEX_1997) %>%
  summarise(n = n()) 
table_gender0 = dat132_0 %>%
  filter(KEY_SEX_1997>0) %>%
  group_by(KEY_SEX_1997) %>%
  summarise(n0 = n()) 
cbind(gender=table_gender$KEY_SEX_1997,
      share_of_0=table_gender0$n0/table_gender$n)

# iii) by number of children and marital status
# children
table_child = dat132 %>%
  filter(!is.na(CV_BIO_CHILD_HH_U18_2019)) %>%
  group_by(CV_BIO_CHILD_HH_U18_2019) %>%
  summarise(n = n()) 
table_child0 = dat132_0 %>%
  filter(!is.na(CV_BIO_CHILD_HH_U18_2019)) %>%
  group_by(CV_BIO_CHILD_HH_U18_2019) %>%
  summarise(n0 = n()) 
table_child0 = rbind(table_child0,data.frame(CV_BIO_CHILD_HH_U18_2019=4:9,n0=c(0)*6))
cbind(children=table_child$CV_BIO_CHILD_HH_U18_2019,
      share_of_0=table_child0$n0/table_child$n)

# marital status
table_marital = dat132 %>%
  filter(!is.na(CV_MARSTAT_COLLAPSED_2019)) %>%
  group_by(CV_MARSTAT_COLLAPSED_2019) %>%
  summarise(n = n()) 
table_marital0 = dat132_0 %>%
  filter(!is.na(CV_MARSTAT_COLLAPSED_2019)) %>%
  group_by(CV_MARSTAT_COLLAPSED_2019) %>%
  summarise(n0 = n()) 
table_marital0 = rbind(table_marital0,data.frame(CV_MARSTAT_COLLAPSED_2019=4,n0=0))
cbind(marital_status=table_marital$CV_MARSTAT_COLLAPSED_2019,
      share_of_0=table_marital0$n0/table_marital$n)



########### Exercise 2 ###########
# 2.1 Specify and estimate an OLS model to explain the income variable (where income is positive)
dat['positive_income'] = as.numeric(dat$YINC_1700_2019>0)
dat1 = dat %>%
  filter(!is.na(positive_income),
         !is.na(age),
         !is.na(work_exp),
         !is.na(edu),
         !is.na(KEY_SEX_1997),
         !is.na(CV_BIO_CHILD_HH_U18_2019)) %>%
  mutate(KEY_SEX_1997=KEY_SEX_1997-1,
         work_exp2=work_exp^2)
dat1_positive = dat1[which(dat1$positive_income==1),]
linear_model = lm(YINC_1700_2019~work_exp+work_exp2+edu+KEY_SEX_1997+CV_BIO_CHILD_HH_U18_2019,
                  data=dat1_positive)
summary(linear_model)


# 2.3 Estimate a Heckman selection model
# Stage 1: Probit model
probit_stage1 = glm(positive_income~work_exp+work_exp2+edu+KEY_SEX_1997+CV_BIO_CHILD_HH_U18_2019,
                    data=dat1,family=binomial(link='probit'))
summary(probit_stage1)
mill0 = dnorm(predict(probit_stage1))/pnorm(predict(probit_stage1))
imr = mill0[dat1$positive_income==1]

# Stage 2: OLS model
linear_stage2 = lm(YINC_1700_2019~work_exp+work_exp2+edu+KEY_SEX_1997+CV_BIO_CHILD_HH_U18_2019+imr,
                   data=dat1_positive)
summary(linear_stage2)



########### Exercise 3 ###########
# 3.1 Plot a histogram to check whether the distribution of the income variable. 
# What might be the censored value here?
ggplot(dat1,aes(x=YINC_1700_2019)) + 
  geom_histogram(color='#999999', fill='#999999',alpha=0.8) + 
  ggtitle('Histogram of income') + 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(family='Times'))
# The censored value here is 100000. 

# 3.2 Propose a model to deal with the censoring problem
# Use the Tobit model 

# 3.3 Estimate the appropriate model with the censored data 
# the likelihood function
Flike = function(bata,x1,x2,x3,x4,x5,y){
  a = -Inf
  b = 100000 # censored value
  Xbeta = beta[1]+beta[2]*x1+beta[3]*x2+beta[4]*x3+beta[5]*x4+beta[6]*x5
  s = beta[7] # sigma
  cdf1 = pnorm((a-Xbeta)/s)
  cdf2 = pnorm((Xbeta-b)/s)
  pdf_z = dnorm((y-Xbeta)/s)
  cdf1[cdf1>0.999999] = 0.999999
  cdf1[cdf1<0.000001] = 0.000001
  cdf2[cdf2>0.999999] = 0.999999
  cdf2[cdf2<0.000001] = 0.000001
  pdf_z[pdf_z<0.000001] = 0.000001
  
  I_a = as.numeric(y==a)
  I_b = as.numeric(y==b)
  log_likelihood = sum(I_a*log(cdf1) + I_b*log(cdf2) + (1-I_a-I_b)*(log(pdf_z)-log(s)))
  return(-log_likelihood)
}

num_try = 10
out = mat.or.vec(num_try,8)
for (i in 1:num_try) {
  set.seed(i)
  start = c(runif(1,17000,17050),
            runif(1,3200,3250),
            runif(1,-110,-105),
            runif(1,2200,2250),
            runif(1,-22000,-21500),
            runif(1,1300,1400),
            runif(1,26500,27000))
  res = optim(start,fn=Flike,method='BFGS',control=list(trace=6,maxit=1000),
              x1=dat1$work_exp,x2=dat1$work_exp2,x3=dat1$edu,x4=dat1$KEY_SEX_1997,x5=dat1$CV_BIO_CHILD_HH_U18_2019,
              y=dat1$YINC_1700_2019)
  out[i,] = c(res$par,res$value)
}
out[which(out[,8]==min(out[,8]))[1],]


# 3.4 Interpret the results above and compare to those when not correcting for the censored dat
cbind('2.1 OLS'=linear_model$coefficients,
      '2.2 Heckman'=linear_stage2$coefficients[-7],
      '2.3 Tobit'=out[which(out[,8]==min(out[,8]))[1],c(-7,-8)])

ggplot(dat1,aes(x=edu,y=YINC_1700_2019)) + 
  geom_point(alpha=0.2) +
  geom_abline(intercept=c(linear_model$coefficients[1],
                          linear_stage2$coefficients[1],
                          out[which(out[,8]==min(out[,8]))[1],1]),
              slope=c(linear_model$coefficients[4],
                      linear_stage2$coefficients[4],
                      out[which(out[,8]==min(out[,8]))[1],4]),
              color=c('#FFDB6D','#D16103','#C3D7A4'),
              size=1) + 
  ggtitle('The relationship between income and education') 



########### Exercise 4 ###########
dat_panel_raw = read.csv('dat_A4_panel.csv', header=TRUE)
dat4 = dat_panel_raw

# Data pre-processing
# work experience
years = c(1997:2010,seq(2011,2019,2))
for (t in years) {
  wkswk_t = names(dat4)[grepl('WKSWK',names(dat4)) & grepl(as.character(t),names(dat4))] 
  dat4 = dat4 %>%
    mutate_at(wkswk_t,funs(ifelse(is.na(.),0,.))) 
  dat4[paste0('work_exp_',t)] = rowSums(dat4[,wkswk_t])/52
}

# education
# if edu is NA,fill it with previous year's education
edu_vnames = names(dat4)[grepl('DEGREE',names(dat4))] 
edu_vnames = edu_vnames[c(-16,-18)]
dat4 = dat4 %>%
  mutate_at('CV_HIGHEST_DEGREE_9899_1998',funs(ifelse(is.na(.),0,.))) 
for (i in 1:dim(dat4)[1]) {
  for (t in 2:19) {
    edu_t_minus1 = edu_vnames[t-1]
    edu_t = edu_vnames[t]
    if (is.na(dat4[i,edu_t])){
      dat4[i,edu_t] = dat4[i,edu_t_minus1]
    }
  }
}
# marital status
marital_vnames = names(dat4)[grepl('MARSTAT',names(dat4))] 
# wages
income_vnames = names(dat4)[grepl('YINC',names(dat4))] 

# convert data from Wide to Long
dat_wide = dat4[,c(edu_vnames,marital_vnames,paste0('work_exp_',years),income_vnames)]
names(dat_wide) = c(paste0('edu_',years),paste0('marital_',years),paste0('work_exp_',years),paste0('income_',years))
dat_long = long_panel(dat_wide,prefix='_',begin=1997,end=2019,label_location='end')
dat_long = dat_long %>%
  filter(wave!=2012,wave!=2014,wave!=2016,wave!=2018)
dat_long4 = dat_long %>%
  filter(!is.na(work_exp),!is.na(marital),!is.na(income))
dat_long4 = dat_long4 %>% 
  group_by(id) %>% 
  mutate(panel_length=n())
dat_long4['edu_year'] = case_when(
  dat_long4$edu==0 ~ 0,  # None
  dat_long4$edu==1 ~ 4,  # GED
  dat_long4$edu==2 ~ 12, # High school diploma
  dat_long4$edu==3 ~ 15, # AA
  dat_long4$edu==4 ~ 16, # BA
  dat_long4$edu==5 ~ 18, # MA, MS
  dat_long4$edu==6 ~ 22, # PhD
  dat_long4$edu==7 ~ 18  # DDS, JD, MD
  
)
dat_long4['marital_NM'] = as.numeric(dat_long4$marital==0) # Never married
dat_long4['marital_M'] = as.numeric(dat_long4$marital==1) # Married
dat_long4['marital_S'] = as.numeric(dat_long4$marital==2) # Separated
dat_long4['marital_D'] = as.numeric(dat_long4$marital==3) # Divorced
dat_long4['marital_W'] = as.numeric(dat_long4$marital==4) # Widowed

# average x and y over time
dat_long4 = dat_long4 %>%
  group_by(id) %>%
  mutate(avg_income=mean(income),
         avg_edu_year=mean(edu_year),
         avg_work_exp=mean(work_exp),
         avg_marital_NM=mean(marital_NM),
         avg_marital_M=mean(marital_M),
         avg_marital_S=mean(marital_S),
         avg_marital_D=mean(marital_D),
         avg_marital_W=mean(marital_W),
         )

# xi-x_avg, yi-y_avg
dat_long4 = dat_long4 %>%
  mutate(income_minus_avg=income-avg_income,
         edu_year_minus_avg=edu_year-avg_edu_year,
         work_exp_minus_avg=work_exp-avg_work_exp,
         marital_NM_minus_avg=marital_NM-avg_marital_NM,
         marital_M_minus_avg=marital_M-avg_marital_M,
         marital_S_minus_avg=marital_S-avg_marital_S,
         marital_D_minus_avg=marital_D-avg_marital_D,
         marital_W_minus_avg=marital_W-avg_marital_W)

# x_(t)-x_(t-1), y_(t)-y_(t-1)
dat_long4 = dat_long4 %>%
  group_by(id) %>%
  mutate(diff_income=income-dplyr::lag(income,n=1,default=NA),
         diff_edu_year=edu_year-dplyr::lag(edu_year,n=1,default=NA),
         diff_work_exp=work_exp-dplyr::lag(work_exp,n=1,default=NA),
         diff_marital_NM=marital_NM-dplyr::lag(marital_NM,n=1,default=NA),
         diff_marital_M=marital_M-dplyr::lag(marital_M,n=1,default=NA),
         diff_marital_S=edu_year-dplyr::lag(marital_S,n=1,default=NA),
         diff_marital_D=edu_year-dplyr::lag(marital_D,n=1,default=NA),
         diff_marital_W=edu_year-dplyr::lag(marital_W,n=1,default=NA))
d = data.frame(dat_long4)

# 4.2.1 Within estimator
formula_within = income_minus_avg~edu_year_minus_avg+work_exp_minus_avg+marital_M_minus_avg+marital_S_minus_avg+marital_D_minus_avg+marital_W_minus_avg-1 
within_model = lm(formula_within,data=dat_long4)
summary(within_model)

# 4.2.2 Between estimator
btw_data = d[,c('id','avg_income','avg_edu_year','avg_work_exp','avg_marital_M','avg_marital_S','avg_marital_D','avg_marital_W')]
btw_data = distinct(btw_data,id,.keep_all = TRUE)

formula_btw = avg_income~avg_edu_year+avg_work_exp+avg_marital_M+avg_marital_S+avg_marital_D+avg_marital_W
btw_model = lm(formula_btw,data=btw_data)
summary(btw_model)

# 4.2.3 First difference estimator
formula_fd = diff_income~diff_edu_year+diff_work_exp+diff_marital_M+diff_marital_M+diff_marital_S+diff_marital_D+diff_marital_W-1
fd_model = lm(formula_fd,data=dat_long4[!is.na(d$diff_income),])
summary(fd_model)

















