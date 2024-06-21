rm(list = ls())
require(fredr)
library(urca)      
library(tseries)
library(vars)
library(ggplot2)
library(ggfortify)
library(data.table)
library(ggpubr) 
###Important: set own API key at https://fredaccount.stlouisfed.org/apikey to retrieve data from FRED
fredr_set_key("InsertKEY") 
#1.Download first set of data---------------------------------
#First we calculate leverage ratio to compare it with the data on Bruno and Shin (2015)
#Security Brokers and Dealers; Equity Capital, Level
EQUITY = fredr(
  series_id = "BOGZ1FL665080003Q",
  observation_start = as.Date("1995-09-01"),
  observation_end = as.Date("2011-09-01"),
  frequency = "q", # quarterly
  aggregation_method = "eop" 
)
#Security Brokers and Dealers; Total Liabilities, Level
LIABILITIES = fredr(
  series_id = "BOGZ1FL664190005Q",
  observation_start = as.Date("1995-09-01"),
  observation_end = as.Date("2011-09-01"),
  frequency = "q", # quarterly
  aggregation_method = "eop" 
)
#Leverage calculation 
LEVERAGE= (EQUITY[3]+LIABILITIES[3])/EQUITY[3]
#Plot leverage
leverageplot = autoplot(ts(data=LEVERAGE, start = c(1995,1), frequency = 4), ylab = "",) +
  theme_bw(base_size = 18) +
  theme(plot.title    = element_text(size = 12, face = 'bold'),
        plot.caption  = element_text(hjust = 1, face = 'italic'))+
  scale_x_date(date_labels = '%b-%y', date_breaks = '9 months')+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))
leverageplot
#2.Download second bulk of data-------------------------------------
#All data is taken as end of period
#Consumer Price Index for All Urban Consumers
CPI = fredr(
    series_id = "CPIAUCSL",
    observation_start = as.Date("1995-09-01"),
    observation_end = as.Date("2007-12-01"),
    units = "pc1",
    frequency = "q" # monthly
  )
#FED Funds Target rate
  FEDFUNDS = fredr(
    series_id = "DFEDTAR",
    observation_start = as.Date("1995-09-01"),
    observation_end = as.Date("2007-12-01"),
    frequency = "q", # quarterly
    aggregation_method = "eop" 
  )
#Real FED Funds Target rate
  FFUNDS = FEDFUNDS[3]-CPI[3]
#Security Brokers and Dealers; Equity Capital, Level
EQUITY = fredr(
    series_id = "BOGZ1FL665080003Q",
    observation_start = as.Date("1995-09-01"),
    observation_end = as.Date("2007-12-01"),
    frequency = "q", # quarterly
    aggregation_method = "eop" 
  )
#Real Broad Effective Exchange Rate for United States
REER = fredr(
    series_id = "RBUSBIS",
    observation_start = as.Date("1995-09-01"),
    observation_end = as.Date("2007-12-01"),
    frequency = "q", # quarterly
    aggregation_method = "eop" 
)
#CBOE Volatility Index: VIX
VIX = fredr(
  series_id = "VIXCLS",
  observation_start = as.Date("1995-09-01"),
  observation_end = as.Date("2007-12-01"),
  frequency = "q", # quarterly
  aggregation_method = "eop"
)
#3.Dataset--------------------------------------
#Cut leverage series
LEVERAGE= LEVERAGE[1:50,]
#Join dataset
data=cbind(FEDFUNDS[1], FFUNDS, LEVERAGE, VIX[3], REER[3])
colnames(data)=c("DATE", "FFUNDS", "LEVERAGE", "VIX", "REER")
#Data transformation
#Log of VIX
data["VIX"]=log(data["VIX"])
data["VIXlag"]=shift(data$VIX)
ggplot(data[2:50,], aes(x=VIXlag, y=LEVERAGE))+ 
  geom_point()+ labs(x = "Lagged logarithm of VIX index", y="Leverage")+ 
  stat_cor(method = "pearson", label.x = 3.2, label.y = 35, size=7)+
  theme_classic(base_size = 18)
#Dataset with only relevant variables
datagroup=data[2:50,c(2:5)]
#Ln difference of REER
datagroup$REER = diff(log(data$REER))
#4.VAR selection--------------------------------------------
info.var <- VARselect(na.omit(datagroup), lag.max = 5, type = "both")
info.var$selection #1 lag
#5.Estimation-----------------------------------------
var.est1 <- VAR(datagroup, p = 1, type = "both", season = NULL)
summary(var.est1)
#6.VAR Diagnostics-----------------------
#Serial correlation Breusch Godfrey LM Test 
#H0 No serial correlation
Serial = serial.test(var.est1, lags.pt = 12, type = "BG")
Serial
#pvalue greater than 0.05, cannot be rejected and there is no serial correlation
#ARCH Test LM Test
#H0 No heteroskedacity in residuals
Arch = arch.test(var.est1, lags.multi = 12, multivariate.only = TRUE)
Arch
#pvalue greater than 0.05, cannot be rejected 
#Stability
Stab = stability(var.est1, type = "OLS-CUS")
plot(Stab)
#Stable VAR
#7.SVAR Estimation (recursive VAR)--------------------------------
a.mat <- diag(4)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
a.mat[4, 1] <- NA
a.mat[4, 2] <- NA
a.mat[4, 3] <- NA
print(a.mat)

svar.one <- SVAR(var.est1, Amat = a.mat, Bmat = NULL, max.iter = 10000, 
                 hessian = TRUE, lrtest = FALSE)
#8.Impulse reponse functions-------------------------------------------------------
one.int <- irf(svar.one, response = "VIX", impulse = "FFUNDS", 
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs=1000)
plot(one.int)
one.int <- irf(svar.one, response = "LEVERAGE", impulse = "VIX",
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs=1000)
plot(one.int)
one.int <- irf(svar.one, response = "LEVERAGE", impulse = "FFUNDS",
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs=1000)
plot(one.int)
one.int <- irf(svar.one, response = "REER", impulse = "FFUNDS",
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs=1000)
plot(one.int)
#9.References-------------------------------
# Bank for International Settlements, Real Broad Effective Exchange Rate for United States [RBUSBIS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/RBUSBIS
# Board of Governors of the Federal Reserve System (US), Security Brokers and Dealers; Equity Capital, Level [BOGZ1FL665080003Q], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/BOGZ1FL665080003Q
# Board of Governors of the Federal Reserve System (US), Security Brokers and Dealers; Total Liabilities, Level [BOGZ1FL664190005Q], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/BOGZ1FL664190005Q
# Chicago Board Options Exchange, CBOE Volatility Index: VIX [VIXCLS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/VIXCLS


