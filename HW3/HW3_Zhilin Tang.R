# ECON613_HW3 Zhilin Tang 
options(warn = -1)
options(scipen=20)
setwd("~/Desktop/ECON613/A3/Data")
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(tinytex)
library(tidyr)

datstu_raw = read.csv('datstu_v2.csv', header=TRUE)
datjss_raw = read.csv('datjss.csv', header=TRUE)
datsss_raw = read.csv('datsss.csv',header=TRUE)

datstu = datstu_raw
datjss = datjss_raw
datsss = datsss_raw



########### Exercise 1 ###########
# 1.1 Number of students, schools, programs
# Number of students
length(datstu$V1)

# Number of schools
# Unify the name of each school
find_mode = function(x){
  x = x[!is.na(x)]
  x = x[which(x!='')]
  u = unique(x)
  u[which.max(tabulate(match(x,u)))]
}

datsss_schoolname = datsss %>%
  group_by(schoolcode) %>%
  summarise(mode_school_name=find_mode(schoolname)) %>%
  filter(!is.na(mode_school_name)) # delete school only have code but no name
dim(datsss_schoolname)[1]

# Number of programs
pgm_all = c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)
pgm_all = pgm_all[-which(pgm_all=='')]
length(unique(pgm_all))


# 1.2 Number of choices (school, program)
m = datstu[,5:16]
choicepgm_all = c()
for (i in 1:6){
  choicepgm_all = c(choicepgm_all,paste(m[,i],m[,(i+6)],sep='_'))
}
length(unique(choicepgm_all))


# 1.3 Number of students applying to at least one senior high schools in the same district to home
schoolcode_sssdistrict = datsss %>%
  group_by(schoolcode) %>%
  summarise(mode_sssdistrict=find_mode(sssdistrict)) %>%
  filter(!is.na(mode_sssdistrict))
names(schoolcode_sssdistrict)[2] = 'sssdistrict'

datstu13 = datstu %>% 
  Reduce(function(x,col_to_join) left_join(x,schoolcode_sssdistrict,by=setNames('schoolcode',col_to_join)),
         c(paste0('schoolcode',1:6)),init=.)
names(datstu13)[(dim(datstu13)[2]-5):dim(datstu13)[2]] = paste0('sssdistrict',1:6)
col_end = dim(datstu13)[2]
for (i in 1:6) {
  datstu13[paste0('same',i)] = as.integer(datstu13$jssdistrict==datstu13[,col_end-6+i])
}
datstu13['num_same'] = apply(datstu13[,25:30], 1, sum,na.rm=TRUE)
sum(datstu13$num_same>=1)


# 1.4 Number of students each senior high school admitted
datstu_admit = datstu %>%
  filter(!is.na(rankplace))
datstu_admit['place'] = NA
for (stu in 1:dim(datstu_admit)[1]) {
  rankplace_stu = datstu_admit$rankplace[stu]
  if (rankplace_stu>=1 & rankplace_stu<=6){
    datstu_admit$place[stu] = datstu_admit[stu,4+rankplace_stu]
  }
}
datstu_admit = datstu_admit %>%
  filter(!is.na(place))

stu_admit = data.frame(table(datstu_admit$place))
View(stu_admit)


# 1.5 The cutoff of senior high schools (the lowest score to be admitted)
school_cutoff = datstu_admit %>%
  group_by(place) %>%
  summarise(cutoff=min(score))
View(school_cutoff)  


# 1.6 The quality of senior high schools (the average score of students admitted)
school_quality = datstu_admit %>%
  group_by(place) %>%
  summarise(avg=mean(score))
View(school_quality)



########### Exercise 2 ###########
# 2.1 the district where the school is located
sss_pgm = datstu13[,c(1,5:17,19:24)]


# 2.2 the latitude and longitude of the district
# location of the junior high school
sss_pgm = sss_pgm %>%
  left_join(datjss[,-1],by=c('jssdistrict'))

# location of the school students apply to
datsss_xy = datsss %>%
  group_by(schoolcode) %>%
  summarise(mode_x=find_mode(ssslong),mode_y=find_mode(ssslat)) %>%
  filter(!is.na(mode_x)) %>%
  filter(!is.na(mode_y))

sss_pgm = sss_pgm %>% 
  Reduce(function(x,col_to_join) left_join(x,datsss_xy,by=setNames('schoolcode',col_to_join)),
         c(paste0('schoolcode',1:6)),init=.)
names(sss_pgm)[23:34] = paste0(rep(c('x','y'),6),rep(1:6,each=2))


# 2.3 cutoff (the lowest score to be admitted)
sss_pgm = sss_pgm %>% 
  Reduce(function(x,col_to_join) left_join(x,school_cutoff,by=setNames('place',col_to_join)),
         c(paste0('schoolcode',1:6)),init=.)
names(sss_pgm)[35:40] = paste0('cutoff',1:6)


# 2.4 quality (the average score of the students admitted)
sss_pgm = sss_pgm %>% 
  Reduce(function(x,col_to_join) left_join(x,school_quality,by=setNames('place',col_to_join)),
         c(paste0('schoolcode',1:6)),init=.)
names(sss_pgm)[41:46] = paste0('quality',1:6)


# 2.5 size (number of students admitted)
stu_admit = stu_admit %>%
  mutate(Var1=as.integer(as.character(Var1)))
sss_pgm = sss_pgm %>% 
  Reduce(function(x,col_to_join) left_join(x,stu_admit,by=setNames('Var1',col_to_join)),
         c(paste0('schoolcode',1:6)),init=.)
names(sss_pgm)[47:52] = paste0('size',1:6)



########### Exercise 3 ###########
for (i in 1:6) {
  term1 = (69.172*(sss_pgm[,21+2*i]-sss_pgm[,21])*cos(sss_pgm[,22]/57.3))^2
  term2 = (69.172*(sss_pgm[,22+2*i]-sss_pgm[,22]))^2
  sss_pgm[paste0('dist',i)] = sqrt(term1+term2)
}



########### Exercise 4 ###########
# 4.1 Recode the schoolcode into its first three digits (substr). Call this new variable `scode_rev`
sss_dt = sss_pgm
for (i in 1:6){
  sss_dt[paste0('scode_rev',i)] = substr(sss_dt[,c(paste0('schoolcode',i))],1,3)
}


# 4.2 Recode the program variable into 4 categories
#Arts: 'General Arts','Visual Arts'
#Economics: 'Home Economics','Business'
#Science: 'General Science'
#Others
for (i in 1:6) {
  select_col = sss_dt[,c(paste0('choicepgm',i))]
  sss_dt[paste0('pgm_rev',i)] = case_when(
    (select_col=='General Arts' | select_col=='Visual Arts') ~ 'Arts',
    (select_col=='Home Economics' | select_col=='Business') ~ 'Economics',
    (select_col=='General Science') ~ 'Science',
    TRUE ~ 'Others' # TRUE here force case_when to output else value if none of the previous conditions  were TRUE
  )
}


# 4.3 Create a new choice variable `choice_rev`
for (i in 1:6){
  scode_rev_s = paste0('scode_rev',1:6)
  pgm_rev_s = paste0('pgm_rev',1:6)
  sss_dt[paste0('choice_rev',i)] = paste(sss_dt[,c(scode_rev_s[i])],
                                         sss_dt[,c(pgm_rev_s[i])],sep='_')
}


# 4.4 Recalculate the cutoff and quality for each recoded choice
sss_dt = cbind(sss_dt,datstu[,c('score','agey','male','rankplace')])

sss_dt_admit = sss_dt %>%
  filter(!is.na(rankplace))
sss_dt_admit['choice_place'] = NA
for (stu in 1:dim(sss_dt_admit)[1]){
  rankplace_stu = sss_dt_admit$rankplace[stu]
  if (rankplace_stu>=1 & rankplace_stu<=6){
    sss_dt_admit$choice_place[stu] = sss_dt_admit[stu,70+rankplace_stu]
  }
}
sss_dt_admit = sss_dt_admit %>%
  filter(!is.na(choice_place))

# cutoff
choice_cutoff = sss_dt_admit %>%
  group_by(choice_place) %>%
  summarise(cutoff=min(score))
View(choice_cutoff)

# quality 
choice_quality = sss_dt_admit %>%
  group_by(choice_place) %>%
  summarise(avg=mean(score))
View(choice_quality)


# 4.5 Consider the 20,000 highest score students
dat = sss_dt_admit %>%
  select(V1,choice_rev1,score,agey,male,point_x,point_y) %>%
  left_join(choice_cutoff,by=c('choice_rev1'='choice_place')) %>%
  left_join(choice_quality,by=c('choice_rev1'='choice_place')) %>%
  filter(!is.na(cutoff) & !is.na(avg) & !is.na(agey)) %>%
  arrange(desc(score)) %>%
  head(20000)


# 4.6 The rest of the assignment uses the recoded choices and the 20,000 highest score students
# Remove choices with frequency=1
dat = dat %>%
  group_by(choice_rev1) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  select(-count)

first_choice = sort(unique(dat$choice_rev1))
for (ch in first_choice) {
  dat[paste0('cutoff.',ch)] = choice_cutoff$cutoff[which(choice_cutoff$choice_place==ch)]
}
for (ch in first_choice) {
  dat[paste0('quality.',ch)] = choice_quality$avg[which(choice_quality$choice_place==ch)]
}

choice_xy = sss_dt_admit %>%
  group_by(choice_place) %>%
  filter(row_number()==1) %>%
  select(choice_place,x1,y1)

for (ch in first_choice) {
  x = choice_xy$x1[which(choice_xy$choice_place==ch)]
  y = choice_xy$y1[which(choice_xy$choice_place==ch)]
  term1 = (69.172*(x-dat$point_x)*cos(dat$point_y/57.3))^2
  term2 = (69.172*(y-dat$point_y))^2
  dat[paste0('dist.',ch)] = sqrt(term1+term2)
}


  
########### Exercise 5 ###########
dat_sort = dat[order(dat$choice_rev1),]
score = dat_sort$score
age = dat_sort$agey
male = dat_sort$male
choice = dat_sort$choice_rev1
choice_unique = unique(choice)
length(choice_unique)

# 5.1 Propose a model specification. Write the likelihood function
# Multinomial logit model
like_fun5 = function(param,score,male,choice){
  ni = length(score)
  nj = length(unique(choice))
  ut = mat.or.vec(ni,nj)
  
  pn1    = param[1:nj-1]
  pn2    = param[(nj):(2*nj-2)]
  
  ut[,1] = 0
  for (j in seq(1,nj-1)) {
    ut[,j+1] = pn1[j] + pn2[j]*score
  }
  prob = exp(ut)
  prob = sweep(prob,MARGIN=1,STATS=rowSums(prob),FUN='/')
  
  probc = NULL
  for (i in 1:ni) {
    probc[i] = prob[i,which(choice_unique==choice[i])]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}


# 5.2 Estimate parameters and compute the marginal effect of the proposed model
# Estimate parameters
num_try = 20
out_mlogit = mat.or.vec(num_try,393)
for (n in 1:num_try) {
  start = c(runif(196,-40,90),runif(196,-0.1,0.1))
  res = optim(start,like_fun5,method='BFGS',control=list(trace=6,maxit=1000),
              score=score,choice=choice)
  out_mlogit[n,] = c(res$par,res$value)
}

out_mlogit_para = out_mlogit[which(out_mlogit[,393]==min(out_mlogit[,393]))[1],-393]

# Compute the marginal effect
prob_fun5 = function(param,score,male,choice){
  ni = length(score)
  nj = length(unique(choice))
  ut = mat.or.vec(ni,nj)
  
  pn1    = param[1:nj-1]
  pn2    = param[(nj):(2*nj-2)]
  
  ut[,1] = 0
  for (j in seq(1,nj-1)) {
    ut[,j+1] = pn1[j] + pn2[j]*score
  }
  prob = exp(ut)
  prob = sweep(prob,MARGIN=1,STATS=rowSums(prob),FUN='/')
 
  return(prob)
}

pij = prob_fun5(out_mlogit_para,score,male,choice)[,2:197]
beta_j = out_mlogit_para[197:392]
beta_i_bar = apply(pij,1,function(x) return(sum(x * beta_j)))
ME_ex5 = data.frame(pij * beta_j - pij * beta_i_bar)
apply(ME_ex5,MARGIN=2,mean)



########### Exercise 6 ###########
# Conditional logit model
cutoff = dat_sort[,c(10:206)]
quality = dat_sort[,c(207:403)]
dist = dat_sort[,c(404:600)]
choice = dat_sort$choice_rev1

# 6.1 Propose a model specification. Write the likelihood function
like_fun6 = function(param,quality,dist,choice){
  ni = dim(quality)[1]
  nj = length(unique(choice))
  ut = param[1] + param[2]*quality[,1] 
  
  for (j in 2:nj){
    ut = cbind(ut,param[1] + param[2]*quality[,j])
  }
  prob   = exp(ut) 
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))

  probc = NULL
  for (i in 1:ni) {
    probc[i] = prob[i,which(choice_unique==choice[i])]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  
  return(-like)
}


# 6.2 Estimate parameters and compute marginal effect of the proposed model
# Estimate parameters
num_try = 20
out_clogit = mat.or.vec(num_try,4)
for (n in 1:num_try) {
  start = c(runif(3,-1,1))
  res = optim(start,like_fun6,method='BFGS',control=list(trace=6,maxit=1000),
              quality=quality,dist=dist,choice=choice)
  out_clogit[n,] = c(res$par,res$value)
}

out_clogit_para = out_clogit[which(out_clogit[,4]==min(out_clogit[,4]))[1],-4]

# Compute the marginal effect
prob_fun6 = function(param,quality,dist,choice){
  ni = dim(quality)[1]
  nj = length(unique(choice))
  ut = 0 + pn2*quality[,1]
  
  pn1 = param[1:nj-1]
  pn2 = param[nj]
  
  for (j in seq(1,nj-1)){
    ut = cbind(ut,pn1[j] + pn2*quality[,j+1])
  }
  prob   = exp(ut) 
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  
  return(prob)
}

pij6 = prob_fun6(out_clogit_para,quality,dist,choice)
names(pij6) = choice_unique
beta = as.numeric(model62$coefficients)
ni = dim(quality)[1]
nj = length(unique(choice))
sigma_ijk = mat.or.vec(ni,nj)
for (i in 1:ni) {
  k = which(choice_unique==choice[i])
  sigma_ijk[i,k] = 1
}
pik6 = sigma_ijk * pij6
ME_ex6 = pij6 * (sigma_ijk-pik6) * beta
apply(ME_ex6,MARGIN=2,mean)



########### Exercise 7 ###########
# In this exercise, we are interested in the effect of excluding choices where the program is "Other"
others_col = sapply(names(dat_sort[,10:600]), function(x) strsplit(x,'_')[[1]][2]=='Others')
others_col_unique = sapply(choice_unique, function(x) strsplit(x,'_')[[1]][2]=='Others')
dat_no_others = dat_sort[,-(as.numeric(which(others_col==TRUE))+9)]
dat_no_others = dat_no_others %>%
  filter(strsplit(choice_rev1,'_')[[1]][2]!='Others')


# 7.2 Calculate choice probabilities under the appropriate model
quality_no_others = dat_no_others[,174:337]
dist_no_others = dat_no_others[,338:501]
choice_no_others = dat_no_others$choice_rev1

num_try = 20
out_clogit7 = mat.or.vec(num_try,4) 
for (n in 1:num_try) {
  start = c(runif(3,-1,1))
  res = optim(start,like_fun6,method='BFGS',control=list(trace=6,maxit=1000),
              quality=quality_no_others,
              dist=dist_no_others,
              choice=choice_no_others)
  out_clogit7[n,] = c(res$par,res$value)
}
out_clogit_para7 = out_clogit7[which(out_clogit7[,4]==min(out_clogit7[,4]))[1],-4]

pij7 = prob_fun6(out_clogit_para7,
                 quality_no_others,
                 dist_no_others,
                 choice_no_others)
names(pij7) = choice_unique[-as.numeric(which(others_col==TRUE))]
pij7[1,]


# 7.3 Simulate how these choice probabilities change when these choices are excluded
pij6_no_others = pij6[,-as.numeric(which(others_col==TRUE))]
others_row = unlist(strsplit(choice,'_'))
others_row = which(others_row[seq(2,length(others_row),2)]=='Others')
pij6_no_others = pij6_no_others[-others_row,]
pij_changes = pij7[1,] - pij6_no_others[1,]
pij_changes 