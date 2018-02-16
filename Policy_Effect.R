# In this project I try to reproduce Miyao(2002, Journal of Money) and update it using recent data
rm(list = ls())
#### Import & preprocessing ####
library(tidyverse)
library(magrittr)
library(zoo)

call=read.csv('callrate.csv', fileEncoding = 'cp932',
              skip=1, na.strings=c('NA     ',''),
              stringsAsFactors = F) %>% 
  set_colnames(c('date','collaterised','uncolRate' ))
call$date=as.yearmon(call$date, '%Y/%m')

money=read.csv('money.csv', fileEncoding = 'cp932',
               skip=1, stringsAsFactors = F) %>% 
  set_colnames(c('date','m2cd','m2cd.2','cd','m2'))
money$date=as.yearmon(money$date, '%Y/%m')

stock= read.csv('N225.csv', stringsAsFactors = F) %>% 
  extract(c(1,6)) %>% 
  set_colnames(c('date','stock'))
stock$date=as.yearmon(stock$date,'%Y-%m-%d')

IP=as.data.frame(t(read.csv('ip.csv', fileEncoding = 'cp932', 
            header=F,skip=2, row.names = 2)[1,-c(1,2)]))
colnames(IP)= IP[2,]
IP=as.data.frame(t(read.csv('ip.csv', skip=2, fileEncoding = 'cp932',
            header=F,row.names = 2)[1:2,-c(1,2)])) %>% 
  set_colnames(c('date','IIP')) %>% 
  set_rownames(NULL)
IP$date= IP$date %>% 
  gsub('\\.0', '' ,. ) %>% 
  as.yearmon('%Y%m')
IP$IIP=as.numeric(as.character(IP$IIP))
IP2=read.csv('ip_past.csv', fileEncoding = 'cp932', skip=1)[-c(1:2),1:2] %>% 
  set_colnames(c('date', 'IIP'))
IP2$date=as.yearmon(IP2$date, '%Y%m')

IIP=IP2 %>% 
  full_join(IP)

dat=reduce(list(call, money, stock, IIP), full_join)

rm(list = ls()[!ls() %in% c('dat')])

qplot(date,stock, data=dat, geom='line')

## untill 1998:4
dat_miyao= dat[37:280,c(1:4,8:9)] %>% 
  set_rownames(NULL)
ave_col= mean(dat_miyao$collaterised[!is.na(dat_miyao$uncolRate)])
ave_uncol= mean(dat_miyao$uncolRate, na.rm=T)
diff_coluncol=ave_uncol-ave_col
for (i in 1:nrow(dat_miyao)){
  if (is.na(dat_miyao$uncolRate[i])){
    dat_miyao$call[i]=dat_miyao$collaterised[i]+diff_coluncol
  } else {
    dat_miyao$call[i]=dat_miyao$uncolRate[i]
  }
}

qplot(date,stock,data=dat_miyao,geom='line')

dat_miyao_log=dat_miyao %>% 
  mutate(money= log(m2cd)*100,
         stock= log(stock)*100) %>% 
  dplyr::select(date,call,money, stock, IIP)

qplot(date,stock,data=dat_miyao_log,geom='line')


## full information
dat_update=dat[37:515,] %>% 
  set_rownames(NULL)
ave_col_u=mean(dat_update$collaterised[!is.na(dat_update$uncolRate)], na.rm=T)
ave_uncol_u= mean(dat_update$uncolRate[!is.na(dat_update$collaterised)], na.rm=T)
diff_coluncol_u= ave_uncol_u - ave_col_u
for (i in 1:nrow(dat_update)){
  if (is.na(dat_update$uncolRate[i])) {
    dat_update$call[i]=dat_update$collaterised[i]+diff_coluncol_u
  } else{
    dat_update$call[i]=dat_update$uncolRate[i]
  }
}

ave_m2cd_11=mean(dat_update$m2cd[!is.na(dat_update$m2cd.2)], na.rm = T)
ave_m2cd_12=mean(dat_update$m2cd.2[!is.na(dat_update$m2cd)], na.rm=T)
diff_m2cd_1=ave_m2cd_12 - ave_m2cd_11

ave_m2cd_21= mean(dat_update$m2cd.2[!is.na(dat_update$m2)], na.rm = T)
ave_m2cd_22= mean((dat_update$m2[!is.na(dat_update$m2cd.2)]+dat_update$cd[!is.na(dat_update$m2cd.2)]), na.rm = T)
diff_m2cd_2=ave_m2cd_22 - ave_m2cd_21

for (i in 1:nrow(dat_update)) {
  if (is.na(dat_update$m2cd.2[i]) & is.na(dat_update$m2[i])) {
    dat_update$m2cd.2[i]=dat_update$m2cd[i]+diff_m2cd_1
  }
}

for (i in 1:nrow(dat_update)) {
  if (is.na(dat_update$m2[i])) {
    dat_update$money[i]=dat_update$m2cd.2[i]+diff_m2cd_2
  } else {
    dat_update$money[i]=dat_update$cd[i]+ dat_update$m2[i]
  }
}

qplot(date, stock, data=dat_update, geom='line')

dat_update_log= dat_update %>% 
  mutate(money=log(money*100),
         stock=log(stock)*100) %>% 
  dplyr::select(date, call, money, stock, IIP)

qplot(date, money, data=dat_update_log, geom='line')

rm(list = ls()[!ls() %in% c('dat', 'dat_miyao', 'dat_miyao_log', 'dat_update', 'dat_update_log')])

#### Unit Root and cointegration ####
library(tseries)

dat_test=dat_miyao_log
# dat_test=dat_update_log
# adf.test in levels
for (i in 2:5){
  print(adf.test(dat_test[,i]))
}

adf.test(dat_update_log$money, k=12)
# create diff's
dat_miyao_return= dat_miyao_log %>% 
  mutate(d_call=c(NA,diff(call)),
         l_money=c(NA,diff(money)),
         l_stock=c(NA,diff(stock)),
         d_IIP=c(NA,diff(IIP))) %>% 
  dplyr::select(date, d_call, l_money, l_stock, d_IIP) %>% 
  filter(!is.na(d_call))

qplot(date, l_stock, data=dat_miyao_return, geom='line')
write.csv(dat_miyao_return, 'dat_miyao.csv', row.names = F)

dat_update_return= dat_update_log %>% 
  mutate(d_call=c(NA,diff(call)),
         l_money=c(NA,diff(money)),
         l_stock=c(NA,diff(stock)),
         d_IIP=c(NA,diff(IIP))) %>% 
  dplyr::select(date, d_call, l_money, l_stock, d_IIP) %>% 
  filter(!is.na(d_call))

qplot(date, l_stock, data=dat_update_return, geom='line')
write.csv(dat_update_return, 'dat_update.csv', row.names = F)

# adf.test in diff
dat_test=dat_miyao_return # dat_test= dat_update_return

adf.test(dat_test$call, k=1)

for (i in 2:5){
  print(adf.test(dat_test[-1,i], k=12))
}

for (i in 1:12){
  print(adf.test(dat_test$l_money, k=i))
}


bic=urca::ur.df(dat_test$l_stock,selectlags = 'BIC')
bic@lags



# Johansen test 
library(urca)
summary(ca.jo(dat_test[2:5],type='eigen',K=6 ))
summary(ca.jo(dat_test[2:5],type='eigen',K=10))
summary(ca.jo(dat_test[2:5],type='eigen',K=12))

rm(list = ls()[!ls() %in% c('dat', 'dat_miyao_return', 'dat_update_return')])

### SVAR
library(vars)
var=VAR(dat_miyao_return[2:5], p=12)
amat=matrix(c(1,NA,NA,NA,0,1,NA,NA,0,0,1,NA,0,0,0,1),nrow=4);amat
bmat=matrix(c(NA,0,0,0,0,NA,0,0,0,0,NA,0,0,0,0,NA), nrow=4);bmat
svar=SVAR(var, Amat=amat, Bmat = bmat)
irf=irf(svar, n.ahead=36)
plot(irf)


### VAR and AR forecast
data=dat_update_return
rm(list = ls()[!ls() %in% c('data')])

# Recursive forecast starting from 1998:5 (based on data from 1978:2 till 1998:4)

# VAR forecast
ind=as.integer(rownames(data[data$date==' 4 1998',]))
call_mse_var=c()
money_mse_var=c()
stock_mse_var=c()
iip_mse_var=c()
for (i in ind:(nrow(data)-1)){
  var=VAR(data[1:i,2:5], ic='AIC', lag.max = 12)
  pred=predict(var, n.ahead = 1)$fcst
  err_call=(pred$d_call[1]-data[i+1,2])^2; call_mse_var=append(call_mse_var, err_call)
  err_money=(pred$l_money[1]-data[i+1,3])^2; money_mse_var=append(money_mse_var, err_money)
  err_stock=(pred$l_stock[1]-data[i+1,4])^2; stock_mse_var=append(stock_mse_var, err_stock)
  err_iip=(pred$d_IIP[1]-data[i+1,5])^2; iip_mse_var=append(iip_mse_var, err_iip)
}
rm(list = ls()[!ls() %in% c('data','call_mse_var','stock_mse_var','money_mse_var','iip_mse_var')])

# AR forecast 
attach(data)
library(forecast)
ind=as.integer(rownames(data[data$date==' 4 1998',]))
call_mse_ar=c()
money_mse_ar=c()
stock_mse_ar=c()
iip_mse_ar=c()
for (i in ind:(nrow(data)-1)){
  ar_call=ar(d_call[1:i], order.max=12) # AIC in default
  fcst_c=forecast(ar_call,1)
  call_mse_ar=append(call_mse_ar, (fcst_c$mean[1]-d_call[i+1])^2)
  
  ar_money=ar(l_money[1:i], order.max=12)
  fcst_m= forecast(ar_money,1)
  money_mse_ar=append(money_mse_ar, (fcst_m$mean[1]-l_money[i+1])^2)
  
  ar_stock=ar(l_stock[1:i], order.max=12)
  fcst_s=forecast(ar_stock,1)
  stock_mse_ar=append(stock_mse_ar, (fcst_s$mean[1]-l_stock[i+1])^2)
  
  ar_iip=ar(d_IIP[1:i], order.max=12)
  fcst_i=forecast(ar_iip,i)
  iip_mse_ar=append(iip_mse_ar, (fcst_i$mean[1]-d_IIP[i+1])^2)
}

rm(list = ls()[!ls() %in% c('data','call_mse_var','stock_mse_var','money_mse_var','iip_mse_var',
                            'call_mse_ar','stock_mse_ar','money_mse_ar','iip_mse_ar', 'ind')])

sum(iip_mse_ar)

qplot(money_mse_ar, money_mse_var, geom='point')

