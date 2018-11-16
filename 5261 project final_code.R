require(quantmod)
library(MASS)
library(lpSolve)
library(quadprog)
library(ggplot2)
library(e1071)
library(sn)
library(fGarch)
library(ggplot2)
library(reshape2)

#Getting data from yahoo finance by quantmod library
my.tickers <- c('JPM','BAC','RY','JNJ','NVS','PFE',
                'GE','BA','MMM','DUK','NGG','AEP',
                'XOM','RDS-B','TOT','KO','UL','NKE',
                'GOOGL','INTC','T','AMZN','PCLN','UPS')
my.tickers=sort(my.tickers)
my.tickers
my.env <- new.env()
# download data and save to my.env
getSymbols(Symbols = my.tickers, env = my.env)
#get sp500 data
sp500=new.env()
getSymbols(Symbols = '^GSPC', env = sp500)
sp_500=sp500$GSPC[,6]
sp_500_month=monthlyReturn(sp_500)
#generate adjusted close price df

final_df=data.frame()
for(i in ls(my.env)){
  temp_df=get(i,my.env)
  print(head(temp_df)) 
  final_df=cbind(final_df,temp_df[,6])
}
colnames(final_df)
#monthly return
month_df=data.frame()
for(i in 1:ncol(final_df)){
  temp_month_return=monthlyReturn(final_df[,i])
  month_df=cbind(month_df,temp_month_return)
}
dim(month_df)
colnames(month_df)=my.tickers
#portfolio theory
final_data=cbind(month_df,sp_500_month)
write.csv(final_data,'5261_final.csv')
data=month_df
rownames(data)=1:nrow(data)
data=as.matrix(data)
data=data.frame(data)
colnames(data)=colnames(month_df)
#box plot and print outlier counts
for (j in 1:6){
  par(mfrow=c(2,2))
  for(i in (4*(j-1)+1):(4*j)){
    boxplot(data[,i])
    title(colnames(data)[i])
    print(colnames(data)[i])
    print(length(boxplot(data[,i],plot=FALSE)$out))
  }
}
getwd()
#qqnorm plot
for (j in 1:6){
  par(mfrow=c(2,2))
  for(i in (4*(j-1)+1):(4*j)){
    qqnorm(data[,i],main =colnames(data)[i] )
  }
}
for (j in 1:6){
  par(mfrow=c(2,2))
  for(i in (4*(j-1)+1):(4*j)){
    hist(data[,i],main =colnames(data)[i] )
  }
}

#generate basic statitics
beta_fun=function(x){
  beta=cov(x,sp_500_month)/var(sp_500_month)
  return(beta)
}

# mean,sd,skewness,kurtosis,beta
mean=apply(data,2,mean)
summary(mean)
sd=apply(data,2,sd)
summary(sd)
skewness=apply(data,2,skewness)
summary(skewness)
kurtosis=apply(data,2,kurtosis)
beta=apply(data,2,beta_fun)
summary(beta)
statistics_summary=rbind(mean,sd,skewness,kurtosis,beta)
head(statistics_summary)
write.csv(t(statistics_summary),'statitic_summary.csv')
getwd()
hist(statistics_summary[1,])
hist(statistics_summary[2,])
hist(statistics_summary[3,])
hist(statistics_summary[4,])



#fit differetn distributions to each stocks
AIC_t=function(distribution){
  aic=2*distribution$objective+2*3
  return(aic)
}
AIC_skew_t=function(distribution,n){
  aic=2*distribution$minimum+2*4
  return(aic)
}
AIC_ged=function(distribution){
  aic=2*distribution$objective+2*3
  return(aic)
}
AIC_skewed_ged=function(distribution){
  aic=2*distribution$objective+2*4
  return(aic)
}
options(warn=-1)


#var and expected shortfall for t distribution






#for correlation
correlation=cor(data)
write.csv(correlation,'correlation.csv')
cor1=rep(NA,3)
for (i in 1:24){
  for (j in 1:24){
    if(cor(data)[i,j]<0.1&& cor(data)[i,j]!=1 ){
      print(c(rownames(correlation)[i],colnames(correlation)[j],cor(data)[i,j]))
      cor1=rbind(cor1,c(rownames(correlation)[i],colnames(correlation)[j],cor(data)[i,j]))
    }
  }
}
write.csv(cor1,'cor1.csv')

# plot equity curve

net_asset=1+data
net_asset=apply(net_asset,2,cumprod)
net_asset=data.frame(net_asset)

sp_500_month_return=sp_500_month+1
sp_500_month_return=as.numeric(sp_500_month_return)
sp_500_month_return=cumprod(sp_500_month_return)
net_asset=cbind(net_asset,sp_500_month_return)
head(net_asset)
colnames(net_asset)[25]='S&P 500'
date=seq(as.Date("2007-01-01"), by="1 month", length.out=132)
test_net_asset=net_asset[,c(-16,-2)]
test_net_asset=cbind(test_net_asset,date)
test_net_asset1<- melt(test_net_asset, id="date")

head(test_net_asset1)
ggplot(data=test_net_asset1[test_net_asset1$variable!='S&P 500',])+
  geom_line(aes(x=date,y=value,colour=variable))+
  ggtitle('Equity curve')+
  geom_line(data=test_net_asset1[test_net_asset1$variable=='S&P 500',],
            aes(x=date,y=value,colour='s&p 500',size='s&p500'))

colnames(net_asset)[25]='S&P 500'
growth_net_asset=net_asset[,c(16,2)]
growth_net_asset=cbind(growth_net_asset,date)
growth_net_asset1<- melt(growth_net_asset, id="date") 

ggplot(data=growth_net_asset1,
       aes(x=date,y=value,colour=variable))+
  geom_line()+
  ggtitle("two outperformance stocks'equity curve")


#no short selling 
mean=apply(month_df,2,mean)
len=length(mean)
covariance=cov(month_df)
sd=sqrt(diag(covariance))
Amat = cbind(rep(1, len), mean,diag(1,nrow=len))
muP = seq(min(mean)+0.0001, max(mean)-0.0001, length = 300)
weights = matrix(0, nrow = 300,ncol=len)
sdP=rep(NA,300)
for (i in 1:length(muP)) 
{
  bvec = c(1, muP[i],rep(0,len)) 
  result =solve.QP(Dmat = 2 * covariance, dvec = rep(0, len),
                   Amat = Amat, bvec = bvec, meq = 2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}
plot(sdP, muP,type='l',main='Efficient frontier no short selling') # plot efficient frontier (and
points(sdP[sdP==min(sdP)],muP[sdP==min(sdP)],col="red",cex=3,pch="*")
legend("topleft","min var portfolio",lwd=c(3,3,1,1),pch="*",col="red",pt.cex=3)
mvp_weights=weights[sdP==min(sdP)]

mvp_weight1=data.frame(mvp_weights)
mvp_weight1=cbind(mvp_weight1,colnames(month_df))
write.csv(mvp_weight1,'mvp_weight.csv')
mvp_return=mvp_weights%*%t(month_df)
mvp_sd=sdP[sdP==min(sdP)]
mean(mvp_sd)
getwd()
#no constrain

# use linear programming detect range of maximum mean and 
#minimum mean for quadratic programming
len=length(mean)
f.obj=mean
f.constrain_1=rep(1,len)
f.constrain_2=diag(1,len)
f.constrain_3=diag(-1,len)
f.constrain_final=rbind(f.constrain_1,f.constrain_2,f.constrain_3)
f.dir=c(c('='),rep('<=',len),rep('<=',len))
f.rhs=c(1,rep(1,len),rep(1,len))
upperbound=lp('max',f.obj,f.constrain_final,f.dir,f.rhs)
lowerbound=lp('min',f.obj,f.constrain_final,f.dir,f.rhs)
upperbound$objval
lowerbound$objval

covariance=cov(month_df)
sd=sqrt(diag(covariance))
Amat_no_constrain = cbind(rep(1, len), mean)
muP_no_constrain = seq(lowerbound$objval+0.001, upperbound$objval-0.0001, length = 300)
sdP_no_constrain=muP_no_constrain
weights_no_constrain = matrix(0, nrow = 300,ncol=len)
for (i in 1:length(muP_no_constrain)) 
{
  bvec = c(1, muP_no_constrain[i]) 
  result =solve.QP(Dmat = 2 * covariance, dvec = rep(0, len),
                   Amat = Amat_no_constrain, bvec = bvec, meq = 2)
  sdP_no_constrain[i] = sqrt(result$value)
  weights_no_constrain[i,] = result$solution
}
plot(sdP_no_constrain, muP_no_constrain,type='l',main='Efficient frontier no constrain')
points(sdP_no_constrain[sdP_no_constrain==min(sdP_no_constrain)],
       muP_no_constrain[sdP_no_constrain==min(sdP_no_constrain)],col="red",cex=3,pch="*")
legend("topleft","min var portfolio",lwd=c(3,3,1,1),pch="*",col="red",pt.cex=3)

mvp_weights_nc=weights_no_constrain[sdP_no_constrain==min(sdP_no_constrain)]



# adding constrain
#20% long position  10% shorting selling
mvp_constrain=function(short_constrain,long_constrain){
  f.rhs_cs=c(1,rep(long_constrain,len),rep(short_constrain,len))
  upperbound_cs=lp('max',f.obj,f.constrain_final,f.dir,f.rhs_cs)
  lowerbound_cs=lp('min',f.obj,f.constrain_final,f.dir,f.rhs_cs)
  print(lowerbound$objval)
  print(upperbound$objval)
  weights_constrain= matrix(0, nrow = 300, ncol = len)
  Amat_constrain = cbind(rep(1,len),mean,diag(1,nrow=len),-diag(1,nrow=len))
  muP_constrain = seq(lowerbound_cs$objval+0.0001,upperbound_cs$objval-0.001,length=300)
  sd_constrain=muP_constrain
  for (i in 1:length(muP_constrain))
  {
    result =solve.QP(Dmat=covariance,dvec=rep(0,len), Amat=Amat_constrain,
                   c(1,muP_constrain[i],rep(-short_constrain,len),rep(-long_constrain,len)), meq=2)
    sd_constrain[i] = sqrt(2*result$value)
    weights_constrain[i,] = result$solution
  }
  plot(sd_constrain,muP_constrain,type='l')
  options(scipen = 999)
  weights_constrain=weights_constrain[sd_constrain==min(sd_constrain)]
  result_list=list(weights_constrain)
  return(result_list)
}
mvp_constrain(0.05,0.2)
weights_5_20=mvp_constrain(0.05,0.2)[[1]]
weights_10_20=mvp_constrain(0.1,0.2)[[1]]

AUM=100000
var_and_es=function(weight){
  mvp_return=weight%*%t(month_df)
  mvp_fit<- fitdistr(mvp_return, "normal")
  mean_mvp=mvp_fit$estimate[1]
  sd_mvp=mvp_fit$estimate[2]
  p_var=AUM*mean_mvp-AUM*1.645*sd_mvp
  es1=dnorm(-1.645)/0.05
  p_es=AUM*(-mean_mvp+sd_mvp*es1)
  percentile5=as.numeric(quantile(mvp_return,probs=c(0.05)))
  IEVaR = (mvp_return < percentile5)
  no_p_var=-AUM*percentile5
  no_p_es=-AUM * sum(mvp_return * IEVaR) / sum(IEVaR)
  result_list=list(p_var,p_es,no_p_var,no_p_es)
  return(result_list)
}


var_and_es_t=function(weights){
  data=weights%*%t(month_df)
  fitt = fitdistr(data, "t")
  param = as.numeric(fitt$estimate)
  mean = param[1]
  df = param[3]
  sd = param[2] * sqrt((df) / (df - 2))
  lambda = param[2]
  alpha=0.05
  qalpha = qt(alpha, df = df)
  quantile=mean + lambda * qalpha
  print(quantile)
  VaR_par = -AUM * (quantile)
  es1 = dt(qalpha, df = df) / (alpha)
  es2 = (df + qalpha^2) / (df - 1)
  es3 = -mean + lambda * es1 * es2
  ES_par = AUM*es3
  percentile5=as.numeric(quantile(mvp_return,probs=c(0.05)))
  IEVaR = (mvp_return < percentile5)
  no_p_var=-AUM*percentile5
  no_p_es=-AUM * sum(mvp_return * IEVaR) / sum(IEVaR)
  result_list=list(VaR_par,ES_par,no_p_var,no_p_es)
  return(result_list)
}

var_and_es(mvp_weights_nc)
var_and_es_t(mvp_weights_nc)
var_and_es_t(weights_5_20)
var_and_es_t(weights_10_20)
var_and_es_t(mvp_weights)


save_weights=function(weight){
weights=data.frame(weight)
temp_df=cbind(colnames(month_df),weights)
temp_df=t(temp_df)
temp_df[2,]=paste(round(100*as.numeric(temp_df[2,]), 2), "%", sep="")
final_df=rbind(temp_df[,1:12],temp_df[,13:24])
write.csv(final_df,'mvp_weights.csv')
}
save_weights(mvp_weights)

#sharpe

sharpe=function(muP,sdP){
  mufree = 0.00054
  points(0,mufree,cex=3,col="blue",pch="*")
  sharpe =(muP-mufree)/sdP
  ind = (sharpe == max(sharpe)) # locates the tangency portfolio
  t_weight=weights[ind,] 
  t_weight=data.frame(t_weight)
  t_weight=cbind(t_weight,colnames(month_df))
  tangency_sharpe=max(sharpe)
  result_list=list(t_weight,tangency_sharpe)
  return(result_list)
}
sharpe(muP_constrain,sd_constrain)
sharpe=rep(NA,24)
for (i in 1:24){
  sharpe[i]=(mean[i]-mufree)/sd[i]
}
#
sharpe=data.frame(sharpe)
sharpe=cbind(sharpe,colnames(month_df))
write.csv(sharpe,'sharpe.csv')




'''
#PCA
pca=prcomp(month_df)
plot(pca,type="l")
summary(pca)
#

#daily return
#
total_networth=1000000
single_asset=total_networth/length(my.tickers)
'''

library(corrplot)
corrplot(cor(month_df))
par(mfrow=c(1,1))

#simulation to get VAR and expected shortfall different distribution


var_es_skewed_t=function(fit){
  mean=fit$estimate[1]
  sd=fit$estimate[2]
  nu=fit$estimate[3]
  xi=fit$estimate[4]
  randomed=rstd(1000000,mean=mean,sd=sd,nu=nu,xi=xi)
  quantile=qstd(0.05,mean,sd,nu,xi)
  var=-AUM*(quantile)
  es=AUM*mean(randomed[which(randomed<quantile)])
  return(c(var,es))
}
var_es_skewed_ged=function(fit){
  mean=fit$par[1]
  sd=fit$par[2]
  nu=fit$par[3]
  xi=fit$par[4]
  randomed=rsged(1000000,mean,sd,nu,xi)
  quantile=qsged(0.05,mean,sd,nu,xi)
  var=-AUM*(quantile)
  es=AUM*mean(randomed[which(randomed<quantile)])
  return(c(var,es))
}
var_es_ged=function(fit){
  mean=fit$par[1]
  sd=fit$par[2]
  nu=fit$par[3]
  randomed=rged(1000000,mean,sd,nu)
  quantile_ged=qged(0.05,mean,sd,nu)
  var=-AUM*(quantile_ged)
  es=AUM*mean(randomed[which(randomed<quantile_ged)])
  return(c(var,es))
}


var_es_t1=function(fit){
  mean=fit$par[1]
  sd=fit$par[2]
  nu=fit$par[3]
  randomed=mean+sd*rt(10000000,nu)
  qa=qt(0.05,nu)
  quantile=mean+sd*qa
  print(quantile)
  var=-AUM*quantile
  es=AUM*mean(randomed[which(randomed<quantile)])
  return(c(var,es))
}
aic_df=data.frame(rep(NA,7))



for (i in 1:24){
  t_dist=stdFit(data[,i])
  #var_es_t1=
  skewed_t_dist=sstdFit(data[,i])
  ged_dist=gedFit(data[,i])
  skewed_ged_dist=sgedFit(data[,i])
  aic_t=AIC_t(t_dist)
  aic_skew_t=AIC_skew_t(skewed_t_dist)
  aic_ged=AIC_ged(ged_dist)
  aic_skew_ged=AIC_skewed_ged(skewed_ged_dist)
  temp_aic=c(aic_t,aic_skew_t,aic_ged,aic_skew_ged)
  distribution_indicator=match(min(temp_aic),temp_aic)
  if(distribution_indicator==1){
    distribution='t_distribution'
    temp_var_es=var_es_t1(t_dist)
  }
  else if(distribution_indicator==2){
    distribution='skewed t distribution'
    temp_var_es=var_es_skewed_t(skewed_t_dist)
  }
  else if(distribution_indicator==3){
    distribution='ged distribution'
    temp_var_es=var_es_ged(ged_dist)
  }
  else{
    distribution='skewed ged distribution'
    temp_var_es=var_es_skewed_ged(skewed_ged_dist)
  }
  temp_aic=c(temp_aic,temp_var_es[1],temp_var_es[2],distribution)
  aic_df=cbind(aic_df,temp_aic)
}

aic_df=aic_df[,2:25]
colnames(aic_df)=colnames(data)
aic_df
write.csv(aic_df,'aic_df1.csv')



my.tickers <- c('AEP','AMZN','BA','BAC','DUK','GE','GOOGL','INTC','JNJ','JPM',
                'KO','MMM','NGG','NKE','NVS','PCLN','PFE','RDS-B','RY','T','TOT','UL','UPS','XOM')

my.env <- new.env()

# download data and save to my.env
getSymbols(Symbols = my.tickers, env = my.env)
final_df=data.frame()

#generate close price df
for(i in ls(my.env)){
  temp_df=get(i,my.env)
  
  final_df=cbind(final_df,temp_df[,6])
}
#read riskfree rate
mufree1 <- read.csv("R/FRB_H15-21.csv",header = F)
riskfree=mufree1/1200

# generate return dataframe
tail(final_df)
head(final_df)

#monthly return
month_df=data.frame()
for(i in 1:ncol(final_df)){
  temp_month_return=monthlyReturn(final_df[,i])
  month_df=cbind(month_df,temp_month_return)
}
tail(month_df)


#s&p500
sp500=new.env()
getSymbols(Symbols = '^GSPC', env = sp500)
sp_500=sp500$GSPC[,6]
sp_500_month=monthlyReturn(sp_500)
month_df=cbind(month_df,sp_500_month)

month_df=month_df[1:131,]           # get monthly data from 2007-01 to 2017-11, 2017-12 excluded

lable1=c('AEP','AMZN','BA','BAC','DUK','GE','GOOGL','INTC','JNJ','JPM',
         'KO','MMM','NGG','NKE','NVS','PCLN','PFE','RDS-B','RY','T','TOT',
         'UL','UPS','XOM','S&P500')
colnames(month_df)=lable1

#allocation with $100000
#estimate mean and coviance from hist data
mean_vect = colMeans(month_df)
cov_mat = cov(month_df)
sd_vect = sqrt(diag(cov_mat))

#target 6% only risky asset

M = length(mean_vect)
Amat = cbind(rep(1,M),mean_vect,diag(1,nrow=M),-diag(1,nrow=M))

weights = matrix(0,nrow=1,ncol=M)


result=solve.QP(Dmat=cov_mat,dvec=rep(0,M), Amat=Amat, c(1,0.005,rep(-0,M),rep(-1,M)), meq=2)
sd= sqrt(2*result$value)
weights= result$solution

hist_portf_retern_nonrf= month_df%*%weights

#95% Value at Risk with no rf asset
VaR_non_rf=100000*(qnorm(0.95)*sd-0.005)
VaR_non_rf

#tangency portfolio with no short sell
muP = seq(min(mean_vect)+0.00001,max(mean_vect)-0.00001,length=501)
sdP = muP
weight = matrix(0,nrow=501,ncol=M)

r = 0.015 #target return of 1.5% monthly

for (i in 1:length(muP))
{
  result =
    solve.QP(Dmat=cov_mat,dvec=rep(0,M), Amat=Amat,
             c(1,muP[i],rep(-0,M),rep(-1,M)), meq=2)
  sdP[i] = sqrt(2*result$value)
  weight[i,] = result$solution
}

mufree=colMeans(riskfree)
plot(sdP,muP,type="l",xlim=c(0,0.15),ylim=c(0,.04))
title(main='no short sell allowed in Portfolio')
points(0,mufree,cex=3,col="blue",pch="*")

sharpe =( muP-mufree)/sdP
ind_noss = (sharpe == max(sharpe)) # locates the tangency portfolio with no short sell

lines(c(0,1.5*sdP[ind_noss]),c(mufree,1.5*muP[ind_noss]-0.5*mufree)
      ,col="red",lwd=3,lty=3)
points(sdP[ind_noss],muP[ind_noss],col="blue",cex=3,pch="*")

ind = (abs(muP-r) == min(abs(muP-r)))
points((r-mufree)*sdP[ind]/(muP[ind]-mufree),r,
       col='brown',cex=3,pch='*')

ind2 = (sdP == min(sdP)) # locates the minimum variance portfolio
points(sdP[ind2],muP[ind2],col="green",cex=3,pch="*")

ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],lty=2,xlim=c(0,.15),
      ylim=c(0,.04),col="purple",lwd=3)

legend("bottomright",c("efficient frontier","efficient portfolios",
                       "tangency portfolio","min var portfolio",
                       'OPT:return of 0.02'),
       lty=c(2,3,NA,NA,NA),
       lwd=c(3,3,1,1,1),
       pch=c("","","*","*",'*'),
       col=c("purple","red","blue","green",'brown'),
       pt.cex=c(1,1,3,3,3)
)


weight_star_noss=weight[ind_noss,]  # weights for tangency portfolio with no short sell
mu_tan_noss=t(weight_star_noss)%*%mean_vect
sd_tan_noss=sqrt(t(weight_star_noss)%*%cov_mat%*%weight_star_noss)

weight_risk_free_noss=(muP[ind_noss]-r)/(muP[ind_noss]-mufree)

mu_star_noss=(1-weight_risk_free_noss)*mu_tan_noss+weight_risk_free_noss*mufree
sd_star_noss=(1-weight_risk_free_noss)*sd_tan_noss

hist_portf_retern_noss= (1-weight_risk_free_noss)*month_df%*%weight_star_noss+weight_risk_free_noss*riskfree
VaR_noss=(qnorm(0.95)*sd_star_noss[1,1]-r)*100000
VaR_noss

#tangency portf with short sell
muP = seq(min(mean_vect),1.2*max(mean_vect),length=501)
sdP = muP
weight = matrix(0,nrow=501,ncol=M)

for (i in 1:length(muP))
{
  result =
    solve.QP(Dmat=cov_mat,dvec=rep(0,M), Amat=Amat,
             c(1,muP[i],rep(-0.5,M),rep(-0.5,M)), meq=2)
  sdP[i] = sqrt(2*result$value)
  weight[i,] = result$solution
}



sharpe =( muP-mufree)/sdP
ind_ss = (sharpe == max(sharpe)) # locates the tangency portfolio with short sell


plot(sdP,muP,type="l",xlim=c(0,0.15),ylim=c(0,.05))
title(main='short sell allowed in Portfolio')
points(0,mufree,cex=3,col="blue",pch="*")


lines(c(0,1.5*sdP[ind_noss]),c(mufree,1.5*muP[ind_noss]-0.5*mufree)
      ,col="red",lwd=3,lty=3)
points(sdP[ind_ss],muP[ind_ss],col="blue",cex=3,pch="*")

ind = (abs(muP-r) == min(abs(muP-r)))
points((r-mufree)*sdP[ind]/(muP[ind]-mufree),r,
       col='brown',cex=3,pch='*')

ind2 = (sdP == min(sdP)) # locates the minimum variance portfolio
points(sdP[ind2],muP[ind2],col="green",cex=3,pch="*")

ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],lty=2,xlim=c(0,.15),
      ylim=c(0,.05),col="purple",lwd=3)

legend("bottomright",c("efficient frontier","efficient portfolios",
                       "tangency portfolio","min var portfolio",
                       'OPT:return of 0.02'),
       lty=c(2,3,NA,NA,NA),
       lwd=c(3,3,1,1,1),
       pch=c("","","*","*",'*'),
       col=c("purple","red","blue","green",'brown'),
       pt.cex=c(1,1,3,3,3)
)

weight_star_ss=weight[ind_ss,]  # weights for tangency portfolio with no short sell
mu_tan_ss=t(weight_star_ss)%*%mean_vect
sd_tan_ss=sqrt(t(weight_star_ss)%*%cov_mat%*%weight_star_ss)

weight_risk_free_ss=(muP[ind_ss]-r)/(muP[ind_ss]-mufree)

mu_star_ss=(1-weight_risk_free_ss)*mu_tan_ss+weight_risk_free_ss*mufree
sd_star_ss=(1-weight_risk_free_ss)*sd_tan_ss

hist_portf_retern_ss= (1-weight_risk_free_ss)*month_df%*%weight_star_ss+weight_risk_free_ss*riskfree
VaR_ss=(qnorm(0.95)*sd_star_ss[1,1]-r)*100000
VaR_ss




#output

Asset_Allocation_norf=cbind(t(weights),matrix(c(0,sd),ncol=2,nrow=1))
Asset_Allocation_noss=cbind(t(weight_star_noss*(1-weight_risk_free_noss)),matrix(c(weight_risk_free_noss,sd_star_noss),ncol=2, nrow=1))
Asset_Allocation_ss=cbind(t(weight_star_ss*(1-weight_risk_free_ss)),matrix(c(weight_risk_free_ss,sd_star_ss),ncol=2, nrow=1))
Asset_Allocation=rbind(Asset_Allocation_norf,Asset_Allocation_noss,Asset_Allocation_ss)

colnames(Asset_Allocation)=cbind(t(lable1),'riskfree','sd')
rownames(Asset_Allocation)=c('Portf1','Portf2','Portf3')

write.csv(Asset_Allocation,file='Asset Allocation.csv')
#construct hist data for 3 portfolio
hist_port=cbind(hist_portf_retern_nonrf,hist_portf_retern_noss,hist_portf_retern_ss)
lable2=c('port_nonrf','port_noss','port_ss')
colnames(hist_port)=lable2

#PCA
pca_25=princomp(month_df)
names(pca_25)
summary(pca_25)
barplot(pca_25$sdev)
pca_accumulate=rep(0,25)
for (i in 1:25)
{
  pca_accumulate[i]=sum(pca_24$sdev[1:i])
}

barplot(pca_accumulate)

#Risk Management


ES=function(mean,sd,alpha)  #calculate ES given mean, sd and alpha
{
  VaR_alpha=function(alpha)
  {
    VaR=-(qnorm(alpha)*sd1+mean1)*100000
    VaR
  }
  ans=c()
  for (i in 1:length(mean))
  {
    mean1=mean[i]
    sd1=sd[i]
    integrand=integrate(VaR_alpha,10^(-5),alpha)
    ans=cbind(ans,integrand$value/alpha)
  }
  ans
}



#VaR and ES by estimated mean and standard deviation

VaR_asset=(qnorm(0.95)*sqrt(diag(cov_mat))-mean_vect)*100000
VaR_port=data.frame(matrix(c(VaR_non_rf,VaR_noss,VaR_ss),ncol=3,nrow=1))
ES_asset=ES(mean_vect,sqrt(diag(cov_mat)),0.05)
ES_port=data.frame(matrix(c(ES(r,sd,0.05),ES(0.005,sd_star_noss,0.05),ES(r,sd_star_ss,0.05)),ncol=3,nrow=1))

flag=(VaR_asset==min(VaR_asset))
VaR_asset[flag]
flag=(VaR_asset==max(VaR_asset))
VaR_asset[flag]

flag=(ES_asset==min(ES_asset))
ES_asset[flag]
flag=(ES_asset==max(ES_asset))
ES_asset[flag]

colnames(VaR_asset)=lable1
colnames(ES_asset)=lable1
colnames(VaR_port)=lable2
colnames(ES_port)=lable2

#VaR and ES by non-parameter 
S=100000
VaR_asset_np=data.frame()
ES_asset_np=data.frame()
VaR_port_np=data.frame()
ES_port_np=data.frame()

for (i in 1:3)
{
  temp_1=hist_port[,i]
  VaR_port_np[1,i]=-quantile(temp_1,0.05)*S
  ES_port_np[1,i]=-S*sum(temp_1[-temp_1>VaR_port_np[1,i]/S])/sum(as.numeric(-temp_1>VaR_port_np[1,i]/S))
}

for (i in 1:M)
{
  temp_1=month_df[,i]
  VaR_asset_np[1,i]=-quantile(temp_1,0.05)*S
  ES_asset_np[1,i]=-S*sum(temp_1[-temp_1>VaR_asset_np[1,i]/S])/sum(as.numeric(-temp_1>VaR_asset_np[1,i]/S))
}

colnames(VaR_asset_np)=lable1
colnames(ES_asset_np)=lable1
colnames(VaR_port_np)=lable2
colnames(ES_port_np)=lable2


#parametric method, fit by t
VaR_asset_pa=data.frame()
ES_asset_pa=data.frame()
VaR_port_pa=data.frame()
ES_port_pa=data.frame()

alpha=0.05

for (i in 1:M)
{
  fitt = fitdistr(month_df[,i], "t")
  param = as.numeric(fitt$estimate)
  mean_t = param[1]
  df_t = param[3]
  sd_t = param[2] * sqrt((df_t) / (df_t - 2))
  
  lambda = param[2]
  qalpha = qt(alpha, df = df_t)
  VaR_asset_pa[1,i] = -100000 * (mean_t + lambda * qalpha)
  es1 = dt(qalpha, df = df_t) / (0.05)
  es2 = (df_t + qalpha^2) / (df_t - 1)
  es3 = -mean_t + lambda * es1 * es2
  ES_asset_pa[1,i] = 100000*es3
}

for (i in 1:3)
{
  temp_1=hist_port[,i]
  fitt = fitdistr(temp_1, "t")
  param = as.numeric(fitt$estimate)
  mean_t = param[1]
  df_t = param[3]
  sd_t = param[2] * sqrt((df_t) / (df_t - 2))
  
  lambda = param[2]
  qalpha = qt(alpha, df = df_t)
  VaR_port_pa[1,i] = -100000 * (mean_t + lambda * qalpha)
  es1 = dt(qalpha, df = df_t) / (0.05)
  es2 = (df_t + qalpha^2) / (df_t - 1)
  es3 = -mean_t + lambda * es1 * es2
  ES_port_pa[1,i] = 100000*es3
}

colnames(VaR_asset_pa)=lable1
colnames(ES_asset_pa)=lable1
colnames(VaR_port_pa)=lable2
colnames(ES_port_pa)=lable2

#bootstrap
library(boot)
#conf_intv for VaR
ValueAtRisk.boot<-function(x,idx,p=0.05, w=100000)
{
  VaR=-w*(mean(x[idx])+qnorm(p)*sd(x[idx]))
  VaR
}
conf_intv_asset=matrix(0,ncol=M,nrow=3)
for (i in 1:M)
{
  Asset.VaR.boot=boot(month_df[,i],statistic=ValueAtRisk.boot,R=500)
  conf_intv_asset[1,i]=boot.ci(Asset.VaR.boot, conf=0.95, type=c("norm","perc") )$normal[[2]]
  conf_intv_asset[2,i]=boot.ci(Asset.VaR.boot, conf=0.95, type=c("norm","perc") )$normal[[3]]
  conf_intv_asset[3,i]=boot.ci(Asset.VaR.boot, conf=0.95, type=c("norm","perc") )$t0
}
colnames(conf_intv_asset)=lable1

conf_intv_port=matrix(0,ncol=3,nrow=3)
for (i in 1:3)
{
  Port.VaR.boot=boot(hist_port[,i],statistic=ValueAtRisk.boot,R=500)
  conf_intv_port[1,i]=boot.ci(Port.VaR.boot, conf=0.95, type=c("norm","perc") )$normal[[2]]
  conf_intv_port[2,i]=boot.ci(Port.VaR.boot, conf=0.95, type=c("norm","perc") )$normal[[3]]
  conf_intv_port[3,i]=boot.ci(Port.VaR.boot, conf=0.95, type=c("norm","perc") )$t0
}

colnames(conf_intv_port)=lable2

#conf_intv for ES
ES.boot<-function(x,idx,p=0.05, w=100000)
{
  ExpectS=ES(mean(x[idx]),sd(x[idx]),p)
  ExpectS
}

conf_intv_ES_asset=matrix(0,ncol=M,nrow=3)
for (i in 1:M)
{
  Asset.ES.boot=boot(month_df[,i],statistic=ES.boot,R=500)
  conf_intv_ES_asset[1,i]=boot.ci(Asset.ES.boot, conf=0.95, type=c("norm","perc") )$normal[[2]]
  conf_intv_ES_asset[2,i]=boot.ci(Asset.ES.boot, conf=0.95, type=c("norm","perc") )$normal[[3]]
  conf_intv_ES_asset[3,i]=boot.ci(Asset.ES.boot, conf=0.95, type=c("norm","perc") )$t0
  
}
colnames(conf_intv_ES_asset)=lable1

conf_intv_ES_port=matrix(0,ncol=3,nrow=3)
for (i in 1:3)
{
  Port.ES.boot=boot(hist_port[,i],statistic=ES.boot,R=500)
  conf_intv_ES_port[1,i]=boot.ci(Port.ES.boot, conf=0.95, type=c("norm","perc") )$normal[[2]]
  conf_intv_ES_port[2,i]=boot.ci(Port.ES.boot, conf=0.95, type=c("norm","perc") )$normal[[3]]
  conf_intv_ES_port[3,i]=boot.ci(Port.ES.boot, conf=0.95, type=c("norm","perc") )$t0
}

colnames(conf_intv_ES_port)=lable2

Riskmanagement_asset=rbind(VaR_asset,ES_asset,VaR_asset_np,ES_asset_np,VaR_asset_pa,ES_asset_pa,conf_intv_asset,conf_intv_ES_asset)
Riskmanagement_port=rbind(VaR_port,ES_port,VaR_port_np,ES_port_np,VaR_port_pa,ES_port_pa,conf_intv_port,conf_intv_ES_port)
Riskmanagement=cbind(Riskmanagement_asset,Riskmanagement_port)
rownames(Riskmanagement)=c('VaR_estimate','ES_estimate','VaR_np','ES_np','VaR_pa','ES_pa','CI_L','CI_U','CI_C','CI_ES_L','CI_ES_U','CI_ES_C')
write.csv(Riskmanagement,file='Riskmanagement.csv')


#copula
est=data.frame()
est=matrix(0,ncol=25,nrow=3)
for (i in 1:25)
{
  est[,i]= as.numeric( fitdistr(month_df[,i],"t")$estimate )
  est[2,i] = est[2,i] * sqrt(est[3,i] / (est[3,i]-2) )
}


data=data.frame()
library(fGarch)
for (i in 1:25)
{
  data=cbind(data,pstd(month_df[,i],est[1,i],est[2,i],est[3,i]))
}
data=data.frame(data)

fnorm = fitCopula(copula=normalCopula(dim=25),data=data,method="ml")
-2*fnorm@loglik + 2*length(fnorm@estimate)

ft = fitCopula(copula=tCopula(dim=25),data=data,method="ml")
-2*ft@loglik + 2*length(ft@estimate)

ffrank = fitCopula(copula = frankCopula(3, dim = 25),
                   data = data, method = "ml")
-2*ffrank@loglik + 2*length(ffrank@estimate)

fclayton = fitCopula(copula = claytonCopula(1, dim=25),
                     data = data, method = "ml")
-2*fclayton@loglik + 2*length(fclayton@estimate)

fgumbel = fitCopula(copula = gumbelCopula(3, dim=25),
                    data = data, method = "ml")
-2*fgumbel@loglik + 2*length(fgumbel@estimate)

fjoe = fitCopula(copula=joeCopula(2,dim=25),data=data,method="ml")
-2*fjoe@loglik + 2*length(fjoe@estimate)




################################################################################################

