
############ Version #####################
##
##    2015-07-28   14:50 verion
##    ·可初始化仓位、起始时间
##    ·按交易所交易日结算
##    ·从起始日期计算到今天收盘
##    ·填入合约名称
##    ·添加 margin, implied_volatility
##    ·自动读取合约名称
##    ·连接sumnmary
##    ·修正parameter:S in Greeks.value
## 
##########################################


## install.packages("NMOF")
library("NMOF")
library(WindR)
w.start()
setwd("D:/csv_data_0727/input_data");                          ## 设置input工作环境
option_data <- read.table("20150728查询csv.csv",sep=",",header=T,stringsAsFactors = FALSE);   ## 读取csv数据
init_data <- read.table("起始仓位.csv",sep=",", header = T,stringsAsFactors = FALSE);   

setwd("D:/csv_data_0727/output_data");                          ## 设置output工作环境
init_summary <- read.table("Summary.csv",sep=",", header = T,stringsAsFactors = FALSE);   

scale_num = 10000

########### initial  function ##############

init_array <- function(x)
{
  n = length(x);
  y = x;
  for(i in c(1:n)){
    y[i] = 0;
  }
  return (y);
}






#####  去掉preDate之前的交易记录 #####

pre_date <- init_data$起始日期[1];
origin_date <- option_data$成交日期;
total_len <- length(origin_date);
del_counter <- 0;
for (i in c(1:total_len)){
  del_counter = del_counter+1;
  if (origin_date[i] < pre_date){
    option_data = option_data[-1,]
  }
}




#######################################




##### vol calculation ###########
start.date <- "2015-01-01"
end.date <- Sys.Date()
SpotPrice <- w.wsd("510050.SH","close", start.date, end.date, "Fill=Previous;PriceAdj=B")$Data$CLOSE
Tdiff <- difftime(end.date, start.date, units = 'days')
Tdiff <- as.numeric(Tdiff)
vol <- sd(SpotPrice) * sqrt(365/Tdiff)

##### other parameters
r <- 0.04; q <- 0.03; 
# S <- w.wsq("510050.SH","rt_last")$Data$RT_LAST #Spot

#####   获取所有期权列表 
date <- Sys.Date()
sdate <- paste(substr(date, 1, 4), substr(date, 6, 7), substr(date, 9, 10), sep = '')
AllOptionData <- w.wset('OptionChain',paste('date=', sdate, ';us_code=510050.SH;option_var=510050OP;
                                            month=全部;call_put=全部', sep = ''))$Data
colnames(AllOptionData)


#### option data  initializing #############

trade_length <- length(option_data$成交日期);

option_code <- option_data$合约代码;

#### 合并 option_code  ####

init_code <- init_data$合约代码;
option_code <- c(option_code, init_code);

########### zeros_arrays ###############

option_code <- unique(option_code);         ##获取合约代码集合
option_num <- length(option_code);


zeros_array <- init_array(c(1:option_num));
option.pnl <- zeros_array;
option.Total <- zeros_array;

curr_pos <- zeros_array;                 ## 初始化option pos,pnl,closePrice, total
curr_close <- zeros_array;
prePos_data <-zeros_array;
preClose_data <- zeros_array;
curr_close<-  zeros_array;
greeks_value <- zeros_array;
greeks_delta <- zeros_array;
greeks_gamma <- zeros_array;
greeks_theta <- zeros_array;
greeks_vega <- zeros_array;
greeks_rho <- zeros_array;
option_name <- zeros_array;
margin_data <- zeros_array;
implied_vol <- zeros_array;

#### 将起始仓位数据读入curr_pos,prePos_data,preClose ######
init_pos <- init_data$起始仓位;
init_counter <- 0;
for (i in init_code){
  init_counter = init_counter+1;
  option_counter <- 0;
  for (j in option_code){
    option_counter = option_counter+1;
    if ( i==j){
      curr_pos[option_counter] <- init_pos[init_counter];
    }
  }
}
## 读入当前closePrice 存入 preClose
code_counter <- 0;
for ( code_tmp in option_code){
  code_counter = code_counter+1;
  close_data <- w.wss(paste(code_tmp,'.SH',sep = ""),'pre_close', paste("tradeDate=",pre_date,sep=""),
                      'priceAdj=B','cycle=D');
  close_data <- close_data$Data;
  closePrice <- close_data[1,2]*scale_num;  
  curr_close[code_counter] = closePrice;
}


###### 读取交易日数据 #############


# trading_date = option_data$成交日期
# trading_date = unique(trading_date)  ##日期去重复

today_date <- strftime(Sys.Date(), "%Y%m%d");
pre_date_str <- paste(pre_date, sep = "");
tdays_data <- w.tdays(pre_date_str,today_date);
tdays_data <- tdays_data$Data$DATETIME;
trading_date <- c(1: length(tdays_data));
for( i in c(1:length(tdays_data)) ){
  trading_date[i] = as.numeric(strftime(tdays_data[i],"%Y%m%d"));
}



##  去掉最后一天
##  trading_date <- trading_date[-length(trading_date)];





## 定义全局DataFrame记录每个合约position，和accumulative pnl

sum_pnl <- init_array(c(1:length(trading_date)));
sum_frame <- data.frame(date = trading_date, sum_PnL = sum_pnl);


sum_pnl_tmp = 0;
date_counter = 0;


##### 写入期权名称 option_name  ##
# w_wset_data<-w.wset('OptionChain','date=20150728;us_code=510050.SH;option_var=;month=全部;
#                     call_put=全部;field=option_code,option_name')

tmp_str = paste('date=',sdate,';us_code=510050.SH;option_var=;month=全部;call_put=全部;field=option_code,option_name',sep='')
w_option_name <- w.wset('OptionChain', tmp_str);
w_option_name <- w_option_name$Data

for (i in 1:length(option_code)){
  tmp_name = paste(option_code[i],'.SH',sep='')
  for (j in 1:nrow(w_option_name)){
    if (tmp_name == (w_option_name$option_code[j])){
      option_name[i] = w_option_name$option_name[j];
    }
  }
}

####### 开始对每个交易日循环计算 ##############


for(date_tmp in trading_date){
  date_counter = date_counter+1;
  ## 更新 preClose_data, prePos_data
  prePos_data= curr_pos;   
  # 更新 preClose_data
  preClose_data = curr_close;
  
  frame_option <- data.frame(code = option_code, position = curr_pos, PnL = option.pnl,   # 3
                             Close =curr_close,Daily_Total_PnL = option.Total,            # 5
                             PrePos = prePos_data, PreClose = preClose_data,              # 7
                             Value = greeks_value,                                        # 8
                             Delta = greeks_delta,                                        # 9
                             Gamma = greeks_gamma,                                        # 10
                             Theta = greeks_theta,                                        # 11
                             Vega = greeks_vega,                                          # 12
                             Rho = greeks_rho,                                            # 13
                             合约名称 = option_name,                                      # 14
                             维持保证金 = margin_data,                                    # 15
                             隐含波动率 = implied_vol                                          # 16
  );
  
  
  ###  计算每次交易Pnl #####
  for (i in c(1:trade_length)){
    cur_date = option_data$成交日期[i];
    cur_code = option_data$合约代码[i];
    code_counter = 0;
    if(cur_date == date_tmp){
      for (code_tmp in option_code){
        code_counter = code_counter+1;
        if(code_tmp == cur_code){        ##日期和代码均匹配
          
          multiplier = -1;
          trade_type <- option_data$业务类别[i];
          if(trade_type == '卖出开仓' || trade_type == '卖出平仓')
          {multiplier = 1;}
          vol_trade = option_data$成交数量[i];
          cash_trade = vol_trade*option_data$成交价格[i];
          # 计算pnl
          frame_option[code_counter,3] = frame_option[code_counter,3] + multiplier*cash_trade*scale_num;
          ## 手续费 
          commission_rate =  3.3
          if (trade_type == '卖出开仓'){
            commission_rate = 1.0
          }
          frame_option[code_counter,3] = frame_option[code_counter,3] - commission_rate*vol_trade;              
          # 记录position
          curr_pos[code_counter] = curr_pos[code_counter] - multiplier*vol_trade; 
          frame_option[code_counter,2] = curr_pos[code_counter];
          
        }
      }
    }
  }
  
  
  
  
  
  
  ## 按收盘和持仓价结算净收益
  code_counter = 0;
  total_pnl = 0;
  for (code_tmp in option_code){
    code_counter = code_counter+1;
    ## 读取code_tmp 的收盘价
    close_data <- w.wss(paste(code_tmp,'.SH',sep = ""),'close', paste("tradeDate=",date_tmp,sep=""),
                        'priceAdj=B','cycle=D');
    
    close_data <- close_data$Data;
    closePrice <- close_data[1,2]*scale_num;
    curr_close[code_counter] = closePrice;
    frame_option[code_counter,4] = curr_close[code_counter];
    
    ## 结算收益
    frame_option[code_counter,3] = frame_option[code_counter,3] + frame_option[code_counter,4]*frame_option[code_counter,2];
    
    frame_option[code_counter,3] = frame_option[code_counter,3] - frame_option[code_counter,7]*frame_option[code_counter,6]
    
    total_pnl = total_pnl + frame_option[code_counter,3];
  }
  
  
  
  
  
  
  ### 当日总PnL #############
  
  sum_pnl_tmp = sum_pnl_tmp + total_pnl;
  sum_frame[date_counter,2] = sum_pnl_tmp;
  
  ######  连接 summary_pnl  #####
  
  last_date = init_summary$date[nrow(init_summary)]
  init_sum_pnl = init_summary$sum_PnL[nrow(init_summary)]
  if(length(init_summary)>2) init_summary = init_summary[,-1]
  if (date_tmp > last_date ){
    
    #  init_summary$date[nrow(init_summary)+1] = date_tmp;
    #  init_summary$sum_PnL[nrow(init_summary)+1] = init_sum_pnl + total_pnl;
    row_tmp = c(date_tmp,init_sum_pnl + total_pnl)
    init_summary = rbind(init_summary, row_tmp)
  }
  rownames(init_summary) = 1: nrow(init_summary)
  
  
  
  
  ####  计算 margin ########
  
  code_counter <- 0;
  for (tmp_code in option_code){
    code_counter = code_counter+1;
    
    query_name = paste(tmp_code,".SH",sep="");
    # 结算价: settle 
    date_str = paste(date_tmp,sep="");
    settle_data <- w.wsd(query_name,"margin,settle","ED0D",date_str,"Fill=Previous");   #当日数据
    settle_price <- settle_data$Data$SETTLE;
    ##如果没有结算价，用收盘价代替
    if(is.na(settle_price)){
      close_data <- w.wss(query_name,'close', paste("tradeDate=",date_tmp,sep=""),
                          'priceAdj=B','cycle=D');
      close_data <- close_data$Data;
      closePrice <- close_data[1,2];
      settle_price = closePrice;
    }
    
    #行权价：exe_price
    exe_data <- w.wss(query_name,'exe_price',paste('tradeDate=',date_tmp,sep=""))    
    exe_price <- exe_data$Data$EXE_PRICE
    
    # EFT 价格 
    ETF_data <- w.wsd("510050.SH","close","ED0D",date_str,"Fill=Previous")
    ETF_price <- ETF_data$Data$CLOSE
    
    ## 判断认沽和认购  ##
    trading_name <- frame_option[code_counter,14];
    GG_type = substr(trading_name,6,6);
    
    ## 计算 margin_price##
    if (GG_type == "购"){
      virtual_price = max(exe_price - ETF_price,0)
      margin = max(0.12*ETF_price - virtual_price, 0.07*ETF_price )
      margin = (margin + settle_price) * abs(frame_option[code_counter,2])
    }
    else{
      virtual_price = max(ETF_price - exe_price,0)
      margin = max(0.12*ETF_price -virtual_price, 0.07*exe_price)
      margin = min(settle_price+margin, exe_price) * abs(frame_option[code_counter,2])
    }
    
    ## 写入 margin 到frame_option
    margin = scale_num * margin
    frame_option[code_counter, 15] <- margin
    
    
    
  } # end of Margin calculation
  
  
  
  
  
  ##################################
  
  ##### greeks ############
  S <- w.wss('510050.SH','close',paste('tradeDate=',date_tmp,sep=''),'priceAdj=B','cycle=D')
  S <- S$Data$CLOSE[1]
  for(i in 1: length(option_code))
  {
    code <- paste(option_code[i], '.SH', sep= '')
    X <- AllOptionData[AllOptionData$option_code == code, 'strike_price']
    last_tradedate <- w.asDateTime( AllOptionData[AllOptionData$option_code == code, 'last_tradedate']) 
    month <- AllOptionData[AllOptionData$option_code == code, 'month']
    tau <- as.numeric(difftime(last_tradedate, date, units = "days") + 15/24) / 365
    call_put <- AllOptionData[AllOptionData$option_code == code, 'call_put']
    type <- ifelse(call_put == "认购", 'call', 'put')
    greeks.value <- round(unlist(vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = type)), 4)
    
    ## implied volatility
    close_vol <- w.wss(code,'close', paste("tradeDate=",date_tmp,sep=""),
                       'priceAdj=B','cycle=D');
    
    close_vol <- close_vol$Data;
    price <- close_vol[1,2];
    impVol_tmp = 0
    #impVol_tmp <- vanillaOptionImpliedVol(exercise = "european", price,S,X,tau,r,q=0,
    #                                      tauD=0,D=0,type="call",M=101,uniroot.control=list())
    
    ## Greeks 写入 frame_option ####
    
    for (j in c(8:13)){
      frame_option[i,j] = greeks.value[j-7]*frame_option[i,2];
    }
    ## 写入 impVol
    frame_option[i,16] = impVol_tmp;
    
  }
  
  
  
  
  
  ##### 清除没有交易记录的合约  ######
  
  
  ## 写入当日总利润##
  
  frame_option[1,5] = total_pnl;
  
  
  frame_write = frame_option;
  ori_len = length(frame_option$code);
  delete_num = c(1:ori_len);
  
  for (i in c(1:ori_len)){
    if (frame_write[i,3] == 0){
      delete_num[i] = 0;    }
  }
  
  
  #####  去掉没有PnL的条目 ##########
  
  counter = 0;
  for (i in c(1:ori_len)){
    counter = counter+1;
    if (delete_num[i] == 0){
      frame_write = frame_write[-counter,];
      counter = counter -1;
    }
  }
  
  ## 计算Sum_Greeks
  sum_greeks = c(NA,NA,NA,NA,NA,NA,NA,0,0,0,0,0,0)
  for (greek_num in c(8:13)){
    sum_greeks[greek_num] =sum(frame_write[,greek_num])
  }
  
  frame_write = rbind(frame_write, sum_greeks)
  
  
  ## 输出每日结果
  
  # frame_write = frame_write[,-6];
  # frame_write = frame_write[,-6];
  rownames(frame_write) = 1: nrow(frame_write)
  file_name_tmp = paste("option_pnl_",date_tmp,".csv", sep = "")
  write.csv(frame_write ,file = file_name_tmp)
  
  
  
}

## 输出总结果
period_str <- paste( trading_date[1],'-',trading_date[length(trading_date)],sep='');
file_name = paste("pnl_in_",period_str,".csv", sep = "");
write.csv(sum_frame, file = file_name);
write.csv(init_summary ,file = 'Summary.csv')

## 输出日志
log_name <- paste("update_log_",Sys.Date(),".txt", sep = "");

log_data <- data.frame("Update Date" = trading_date);
write.csv(log_data, file = log_name);





##########################






'This is the end'
