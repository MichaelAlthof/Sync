## 0. Preparation

rm(list = ls(all = TRUE))

#----------------------------------------START UPDATE----------------------------------------

wdir = "D:\\Files in Google Drive\\FRM"

#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", etc
channel = "Asia"

#Data source
date_start_source = 20190102 
date_end_source = 20201030
#Index output, varying companies
date_start = 20190416
date_end = 20201030
#Network output, fixed companies
date_start_fixed = 20200831
date_end_fixed = 20201030
#Note: fixed companies are needed to produce network gif and for analysis
#Note: allow min of s days in between date_start_source 
#and date_start, date_start_fixed

#boxplot_labels = c(20150601, 20160601, 20170601, 20180601, 20190601, 20200601)
boxplot_labels = c(20200201, 20200301, 20200401, 20200501, 20200601, 20200701, 20200801)

quantiles = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50, 0.25)

#Estimation window size
s = 63      
#Tail risk level
tau = 0.05 
if (channel == "ER") tau = 1 - tau
#Number of iterations
I = 20   
#CoStress top and bottom L 
L = 5

#Number of largest companies 
#Highlighted node for network graph
if (channel == "Americas") {
  stock_main = "??"
  J = 100} else 
if (channel == "Europe") {
  stock_main = "??"
  J = 100} else     
if (channel == "Crypto") {
  stock_main = "BTC"
  J = 15} else 
if (channel == "ER") {
  stock_main = "??"
  J = 11}
if (channel == "Asia") {
  stock_main = "X601288.CH.EQUITY"
  J = 25}
#-----------------------------------------END UPDATE-----------------------------------------

setwd(wdir)

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
output_path = paste0("Output/", channel) 
#, "/Sensitivity/tau=10/s=63"

#TODO choose between quantile and expectile in the header
source("FRM_Statistics_Algorithm.R")

library(ggplot2)
library(data.table)
library(igraph)
require(timeDate)
library(stringr)
library(graphics)
library(magick)
library(scales)
library(tidyr)

## 1. Data Preprocess

mktcap = read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                date_end_source, ".csv"), header = TRUE)
stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"), header = TRUE)
macro = read.csv(file = paste0(input_path, "/", channel, "_Macro_", 
                               date_end_source, ".csv"), header = TRUE)

colnames(mktcap) == colnames(stock_prices)

M_stock = ncol(mktcap)-1
M_macro = ncol(macro)-1
M = M_stock+M_macro

colnames(mktcap)[1] = "ticker"
colnames(stock_prices)[1] = "ticker"
colnames(macro)[1] = "ticker"

#Load the stock prices and macro-prudential data matrix
# all_prices = merge(stock_prices, macro, by = "ticker", all.x = TRUE)
all_prices = merge(stock_prices, macro, by = "ticker", all.x = F)

#AL START
#all_prices[is.na(all_prices)] = 0
#AL END

#TODO: exceptions that break crypto algorithm and result in large lambda
#all_prices = all_prices[-c(377,603,716,895,896),]

ticker_str = as.data.frame(all_prices$ticker[-1])
colnames(ticker_str) = "ticker"
ticker = as.numeric(gsub("-", "", ticker_str$ticker))

N = nrow(ticker_str)

#Align mktcap rows to all_return
# mktcap = merge(ticker_str, mktcap, by = "ticker", all.x = TRUE)
mktcap = merge(ticker_str, mktcap, by = "ticker", all.x = F)
mktcap[is.na(mktcap)] = 0

mktcap$ticker == ticker_str$ticker

if (channel == "Americas") {
  all_prices$USGG3M10YR = exp(all_prices$USGG3M10YR)
  all_prices$USGG3M.INDEX = exp(all_prices$USGG3M.INDEX)
  all_prices$MOODCBAA10YRSPD = exp(all_prices$MOODCBAA10YRSPD)} else
#AL START
#AL convert discount rate into daily rate
if (channel == "Asia") {
  TED_SP=all_prices$TED.SP
  all_prices$CGB3M = exp(all_prices$CGB3M)
  all_prices$CN3M10YSLOPE = exp(all_prices$CN3M10YSLOPE)
  all_prices$CREDIT.SP = exp(all_prices$CREDIT.SP)} else
#AL END
if (channel == "Europe") {
  All_prices$BTPSDBR10 = exp(All_prices$BTPSDBR10)
  All_prices$EUAggCorp10 = exp(All_prices$EUAggCorp10)
  All_prices$DBR0110 = exp(All_prices$DBR0110)} else
if (channel == "Crypto") {
  all_prices$BV010082.INDEX = exp(all_prices$BV010082.INDEX)} else
if (channel == "ER") {
  all_prices$SLOPE_DBR = exp(all_prices$SLOPE_DBR)
  all_prices$LIQ = exp(all_prices$LIQ)
  all_prices$GE2 = exp(all_prices$GE2)
  all_prices$BE = exp(all_prices$BE)
  all_prices$SP = exp(all_prices$SP)
  all_prices$IT = exp(all_prices$IT)
  all_prices$AT = exp(all_prices$AT)
  all_prices$GE = exp(all_prices$GE)
  all_prices$FR = exp(all_prices$FR)
  all_prices$PO = exp(all_prices$PO)
  all_prices$IR = exp(all_prices$IR)
  all_prices$FI = exp(all_prices$FI)
  all_prices$NE = exp(all_prices$NE)
  all_prices$GR = exp(all_prices$GR)}

#Calculate the daily return and differences matrix of all selected financial 
#companies and macro-prudential variables; use exponential function for selected
#macro-prudential variables that are expressed in first order differences

#AL START
all_prices[, -1] = sapply(as.matrix(all_prices[, -1]), as.numeric)
mktcap[,-1] = sapply(as.matrix(mktcap[, -1]), as.numeric)
#AL END

all_return = diff(log(as.matrix(all_prices[, -1])))
if (channel == "Asia")all_return[,"TED.SP"]=as.numeric(TED_SP)[-1]
all_return[is.na(all_return)] = 0
stock_return = all_return[, 1:M_stock]
macro_return = all_return[, (M_stock+1):M]

#Sorting the market capitalization data
FRM_sort = function(data){sort(data, decreasing = TRUE, index.return = TRUE)}
#Determining the index number of each company
#according to decreasing market capitalization
mktcap_index = matrix(0, N, M_stock)
mktcap_sort = apply(mktcap[, -1], 1, FRM_sort)
#AL START
for (t in 1:N) mktcap_index[t,1:length(mktcap_sort[[t]]$ix)] = mktcap_sort[[t]]$ix
#AL END

#AL START
#omit rows where # of non-zero Mktcap companies < J
#stock_return  macro_return  ticker   mktcap_index
#c(date_start, date_end, date_start_fixed, date_end_fixed) %in% ticker
# market_vacation=rep(1,N)
# for(i in 1:N)if(mktcap[i,mktcap_index[,J]+1]==0)market_vacation[i]=0
# row_to_keep = which(market_vacation==1)
# ticker=ticker[row_to_keep]
# stock_return=stock_return[row_to_keep,]
# macro_return=macro_return[row_to_keep,]
# mktcap_index=mktcap_index[row_to_keep,]
#AL END

mktcap_index = cbind(ticker, mktcap_index)


## 2. Estimation

#Row index corresponding to date_start and date_end
N0 = which(ticker == date_start)
N1 = which(ticker == date_end)

N0_fixed = which(ticker == date_start_fixed)
N1_fixed = which(ticker == date_end_fixed)

N_upd = N1-N0+1
N_fixed = N1_fixed-N0_fixed+1
  
## 2.1 Varying companies or coins

FRM_individ = vector(mode = "list")
J_dynamic = matrix(0, 1, N_upd)

for (t in N0:N1) {
  #Biggest companies at each time point
  biggest_index = as.matrix(mktcap_index[t, 2:(J+1)])
  data = cbind(stock_return[(t-s+1):t, biggest_index], 
               # macro_return[(t-s):(t-1),])
#AL START               
macro_return[(t-s+1):(t-1+1),])
#AL END
  #J_dynamic needed for data available for less than J stocks:
  #relevant for crypto channel before 2014
  data = data[, colSums(data != 0) > 0]
  M_t = ncol(data)
  J_t = M_t - M_macro
  J_dynamic[t-N0+1] = J_t
  #Initialize adjacency matrix
  adj_matix = matrix(0, M_t, M_t) 
  est_lambda_t = vector()
  #FRM quantile regression
  for (k in 1:M_t) {
    est = FRM_Quantile_Regression(as.matrix(data), k, tau, I)
    est_lambda = abs(data.matrix(est$lambda[which(est$Cgacv == min(est$Cgacv))]))
    est_beta = t(as.matrix(est$beta[which(est$Cgacv == min(est$Cgacv)),]))
    adj_matix[k, -k] = est_beta
    est_lambda_t = c(est_lambda_t, est_lambda)
  }
  #List of vectors of different size with different column names
  est_lambda_t = t(data.frame(est_lambda_t[1:J_t]))
  colnames(est_lambda_t) = colnames(data)[1:J_t]
  FRM_individ[[t-N0+1]] = est_lambda_t
  #Save adjacency matrix
  colnames(adj_matix) = colnames(data)
  rownames(adj_matix) = colnames(data)
  write.csv(adj_matix, paste0(output_path, "/Adj_Matrices/adj_matix_", 
                                    ticker[t], ".csv"), quote = FALSE)
}

## 2.2 Fixed companies or coins 

#AL START
#error when 20200804-20200924
#AL END

#Make companies constant, select the biggest companies 
biggest_index_fixed = as.matrix(mktcap_index[N0_fixed, 2:(J+1)])

#Note: since J is constant, don't run for crypto before 2017
M_J = J+M_macro
adj_matix_fixed = matrix(0, M_J, M_J) 
FRM_individ_fixed = matrix(0, N_fixed, J+1)
FRM_individ_fixed[, 1] = ticker[N0_fixed:N1_fixed]

for (t in N0_fixed:N1_fixed) { 
  data_fixed = cbind(stock_return[(t-s+1):t, biggest_index_fixed], 
                     macro_return[(t-s):(t-1),])
  #FRM quantile regression
  for (k in 1:M_J) {
    est_fixed = FRM_Quantile_Regression(as.matrix(data_fixed), k, tau, I)
    est_lambda_fixed = abs(data.matrix(est_fixed$lambda[
      which(est_fixed$Cgacv == min(est_fixed$Cgacv))]))
    est_beta_fixed = t(as.matrix(est_fixed$beta[
      which(est_fixed$Cgacv == min(est_fixed$Cgacv)),]))
    adj_matix_fixed[k, -k] = est_beta_fixed
    if (k <= J) FRM_individ_fixed[t-N0_fixed+1, k+1] = est_lambda_fixed  
  }
  #Save adjacency matrix
  colnames(adj_matix_fixed) = colnames(data_fixed)
  rownames(adj_matix_fixed) = colnames(data_fixed)
  write.csv(adj_matix_fixed, paste0(output_path, "/Adj_Matrices/Fixed/adj_matix_", 
                                    ticker[t], ".csv"), quote = FALSE) 
}


## 3. Updated FRM index

names(FRM_individ) = ticker[N0:N1]


#AL START
#rds shouldn't exist for Asia
#AL END

#Append R dataset to the historical file
{if (file.exists(paste0(output_path, "/Lambda/FRM_", channel, ".rds"))) {
  FRM_history_prev = readRDS(paste0(output_path, "/Lambda/FRM_", channel, ".rds"))
  FRM_history = c(FRM_history_prev, FRM_individ)} 
else FRM_history = FRM_individ}
#Note: be careful with different values for the same day
FRM_history = FRM_history[unique(names(FRM_history))]
N_h = length(FRM_history)
saveRDS(FRM_history, paste0(output_path, "/Lambda/FRM_", channel, ".rds"))

#Transform the list of lambdas into a wide dataset
stock_names = vector()
for (t in 1:N_h) stock_names = c(stock_names, attributes(FRM_history[[t]])$dimnames[[2]])
stock_names = unique(stock_names)
N_names = length(stock_names)
lambdas_wide = matrix(0, N_h, N_names+1)
lambdas_wide[, 1] = names(FRM_history)
for (k in 1:N_names) 
  for (t in 1:N_h) 
    if (stock_names[k] %in% attributes(FRM_history[[t]])$dimnames[[2]]) 
      lambdas_wide[t, k+1] = FRM_history[[t]][, stock_names[k]]
colnames(lambdas_wide) = c("date", stock_names)
write.csv(lambdas_wide, paste0(output_path, "/Lambda/lambdas_wide.csv"), 
          row.names = FALSE, quote = FALSE)

#Saved fixed lambdas for the specified period
colnames(FRM_individ_fixed) = c("date", colnames(data_fixed)[1:J])
write.csv(FRM_individ_fixed, paste0(output_path, "/Lambda/Fixed/lambdas_fixed_", 
                            date_start_fixed, "_", date_end_fixed, ".csv"),
          row.names = FALSE, quote = FALSE)

#Calculate FRM index as the average
FRM_index = sapply(1:N_h, function(i) mean(FRM_history[[i]]))
FRM_index = data.frame(date = names(FRM_history), FRM = FRM_index)
write.csv(FRM_index, paste0(output_path, "/Lambda/FRM_", channel, "_index.csv"),
          row.names = FALSE, quote = FALSE)

#Daily maximum
FRM_max = sapply(1:N_h, function(i) max(FRM_history[[i]]))
name_max = sapply(1:N_h, function(i) 
  attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_max[i])[1]])
FRM_max = data.frame(date = names(FRM_history), name = name_max, lambda = FRM_max)
write.csv(FRM_max, paste0(output_path, "/Lambda/max_lambda.csv"), 
          row.names = FALSE, quote = FALSE)

#Daily minimum
FRM_min = sapply(1:N_h, function(i) min(FRM_history[[i]]))
name_min = sapply(1:N_h, function(i) 
  attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_min[i])])
FRM_min = data.frame(date = names(FRM_history), name = name_min, lambda = FRM_min)
write.csv(FRM_min, paste0(output_path, "/Lambda/min_lambda.csv"), 
          row.names = FALSE, quote = FALSE)

#Quantiles
for (q in quantiles) {
  FRM_q = sapply(1:N_h, function(i) quantile(FRM_history[[i]], q))
  FRM_q = data.frame(date = names(FRM_history), quantile = FRM_q)
  write.csv(FRM_q, paste0(output_path, "/Lambda/Quantiles/q", q*100, "_lambda.csv"), 
            row.names = FALSE, quote = FALSE)
}


## 4. Top and bottom L companies based on lambda

#TODO: needed? if so, rewrite for the new structure of FRM_individ

# top_lambda = matrix(0, N_upd, 2*L+1)
# bottom_lambda = matrix(0, N_upd, 2*L+1)
# lambda_sort = apply(FRM_individ[, -1], 1, FRM_sort)
# for (t in 1:N_upd) {
#   top_lambda[t,] = c(ticker[N0-1+t], colnames(FRM_individ)[-1]
#                      [lambda_sort[[t]]$ix][1:L], lambda_sort[[t]]$x[1:L])
#   bottom_lambda[t,] = c(ticker[N0-1+t], colnames(FRM_individ)[-1]
#                         [lambda_sort[[t]]$ix][(J-L+1):J], lambda_sort[[t]]$x[(J-L+1):J])
# }
# write.csv(top_lambda, paste0(output_path, "/Top/Top_", L, "_", date_start, "_",
#                              date_end, ".csv"), row.names = FALSE, quote = FALSE) 
# write.csv(bottom_lambda, paste0(output_path, "/Top/Bottom_", L, "_", date_start, "_", 
#                                 date_end, ".csv"), row.names = FALSE, quote = FALSE) 
# #Number of days in top/bottom L over N0:N1
# top_lambda_overall = sort(table(top_lambda[, 2:(L+1)]), decreasing = TRUE)
# bottom_lambda_overall = sort(table(bottom_lambda[, 2:(L+1)]), decreasing = TRUE)
# write.csv(top_lambda_overall, paste0(output_path, "/Top/Top_", L, "_overall_", 
#                                      date_start, "_", date_end, ".csv"), 
#                                      row.names = FALSE, quote = FALSE) 
# write.csv(bottom_lambda_overall, paste0(output_path, "/Top/Bottom_", L,
#                                         "_overall_", date_start, "_", date_end, 
#                                         ".csv"), row.names = FALSE, quote = FALSE) 

#Top 10 companies based on lambda at date_end for the website 

top_10 = FRM_individ[[N_upd]]
top_10 = top_10[, order(top_10, decreasing = T)]
top_10 = top_10[1:10]
write.csv(top_10, paste0(output_path, "/Top/top10_", date_end, "_", channel, ".csv"))


## 5.1 Boxplot

# png(paste0(output_path, "/Boxplot/Boxplot_", date_start, "_", date_end, "_",
#            channel, ".png"), width = 900, height = 600, bg = "transparent")
# 
# boxplot(FRM_individ, col = "white", xaxt = "n")
# lines(tail(FRM_index$FRM, N_upd), col = "blue", lwd = 2)
# lines(tail(FRM_max$lambda, N_upd), col = "red", lwd = 2)
# 
# #AL START
# boxplot_labels=ticker[c(seq(N0,N1,21),N1)]
# #AL END
# ll = which(names(FRM_individ) %in% boxplot_labels)
# axis(1, at = ll, labels = boxplot_labels)
# 
# dev.off()


# requires ticker of full history
date_start_bp="20190416"
FRM_bp=vector(mode="list")
N_bp=N1-which(ticker==date_start_bp)+1
for(i in 1:N_bp)FRM_bp[[i]]=FRM_history[[N1-62-N_bp+i]]
names(FRM_bp)=ticker[(N1-N_bp+1):N1]

png(paste0(output_path, "/Boxplot/Boxplot_", date_start_bp, "_", date_end, "_",
           channel, ".png"), width = 900, height = 600, bg = "transparent")

boxplot(FRM_bp, col = "white", xaxt = "n")
lines(tail(FRM_index$FRM, N_bp), col = "blue", lwd = 2)
lines(tail(FRM_max$lambda, N_bp), col = "red", lwd = 2)


boxplot_labels=ticker[c(seq(N1-N_bp+1,N1-20,21),N1)]

ll = which(names(FRM_bp) %in% boxplot_labels)
axis(1, at = ll, labels = boxplot_labels)

dev.off()

## 5.2 Accumulated boxplot



## 6. Network

N0_fixed_net = which(names(FRM_history) == date_start_fixed)
N1_fixed_net = which(names(FRM_history) == date_end_fixed)

fig = image_graph(width = 1000, height = 1000, res = 96, bg = "transparent")

for (t in N0_fixed_net:N1_fixed_net) {
  adj0 = read.csv(file=paste0(output_path, "/Adj_Matrices/Fixed/Adj_Matix_", 
                              names(FRM_history)[t], ".csv"),
                  header = TRUE, sep = "," , row.names = 1)
  
  adj0 = as.matrix(adj0)[1:J, 1:J] 
  adj0 = apply(adj0, 2, as.numeric)
  netw1 = graph_from_adjacency_matrix(adj0, mode = "directed", weighted = TRUE)
  V(netw1)$color = ifelse(V(netw1)$name == stock_main, "orange", "lightgrey")
  colors = rep("Gray", alpha.f = .8, length(E(netw1)))
  colors = ifelse(head_of(netw1, E(netw1))$name == stock_main, 'blue', colors) #inflow
  colors = ifelse(tail_of(netw1, E(netw1))$name == stock_main, 'orange', colors) #outflow
  
  plot(netw1, layout = layout_in_circle, vertex.label = colnames(adj0), edge.width = 0.8, 
       edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1, 
       vertex.size = 1000*FRM_individ_fixed[t-N0_fixed_net+1, -1])

  title(xlab = paste0(FRM_index$date[t], "\n FRM: ", round(FRM_index$FRM[t], 5)), 
        cex.lab = 1.15, font.lab = 2, line = -0.5)
}

dev.off()

animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(output_path, "/Network/Network_", date_start_fixed, "_", 
                              date_end_fixed, "_", channel, ".gif"))


## 7. Macro influence
Macro_labels=ticker[c(seq(N0,N1,63),N1)]  
macro_inf = matrix(0, N_h, M_macro+1)
macro_inf[, 1] = names(FRM_history)
colnames(macro_inf) = colnames(macro)
colnames(macro_inf)[1] = "date"

for (t in 1:N_h) {
  adj0 = read.csv(file=paste0(output_path, "/Adj_Matrices/Adj_Matix_", 
                              names(FRM_history)[t], ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  k1 = ncol(adj0)-M_macro
  for (k in 1:M_macro) macro_inf[t, k+1] = sum(adj0[1:k1, k1+k]!=0)/k1
}

write.csv(macro_inf, paste0(output_path, "/Macro/macro_influence.csv"))

macro_inf_long = gather(as.data.frame(macro_inf), macro, inf_idx, -date, 
                        convert = TRUE, factor_key = TRUE)

png(paste0(output_path, "/Macro/macro_inf.png"), 
    width = 900, height = 600, bg = "transparent")

ggplot(macro_inf_long, aes(date, as.numeric(inf_idx), group=macro)) +
  geom_line(aes(color = macro), size = 1) + ylab("normalised # of non-zero betas") +
  scale_x_discrete(breaks = Macro_labels, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.border = element_blank(),
        legend.key=element_blank(),
        panel.grid=element_blank())

dev.off()

#Smooth the values
macro_inf_smooth = macro_inf

for (k in 1:M_macro) {
  ss = smooth.spline(macro_inf[,"date"], macro_inf[, k+1])$y
  macro_inf_smooth[, k+1] = ifelse(ss > 0, ss, 0)
}

macro_inf_long_smooth = gather(as.data.frame(macro_inf_smooth), macro, inf_idx, -date, 
                               convert = TRUE, factor_key = TRUE)

png(paste0(output_path, "/Macro/macro_inf_smooth.png"), 
    width = 900, height = 600, bg = "transparent")

ggplot(macro_inf_long_smooth, aes(date, as.numeric(inf_idx), group=macro)) +
  geom_line(aes(color = macro), size = 1) + ylab("normalised # of non-zero betas") +
  scale_x_discrete(breaks = Macro_labels, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.border = element_blank(),
        legend.key=element_blank(),
        panel.grid=element_blank())

dev.off()
