## Test some strategie just for fun
rm(list = ls())

## Pull libraries
list.of.packages   = 
	c(
	  "dplyr", 
	  "magrittr", 
	  "dint", 
	  "lubridate", 
	  "ggplot2",
	  "quantmod", 
	  "NMOF", 
	  "Matrix",
	  "tseries"
	)
new.packages       = 
	list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
       install.packages(new.packages)
}
lapply(list.of.packages, function(x){eval(parse(text = paste0("library(", x, ");")))})

## Deinfe directories
wdir        = "C:/Users/luigi/OneDrive/Desktop/Independent Research/Investment Strategies"
codedir     = paste0(wdir, "/code")
datadir     = paste0(wdir, "/data")
outdir      = paste0(wdir, "/output")

## Source useful functions
source(paste0(codedir, "/helper_funcs.r"))

## Relevance statistic functions
Mahalanobis_Distance_I <- function(data,vector) {
  data    = as.matrix(data)
  S       = var(data)
  mean    = apply(data, 2, FUN=mean)
  return(mahalanobis(vector,mean,S))
}

Mahalanobis_Distance_S <- function(data,vector,current) {
  data   = as.matrix(data)
  S      = var(data)
  return(-1*mahalanobis(vector,current, S))
}


##=================================
## DATA PULLS
##=================================

## Pull stock data
stocks    = c("^GSPC", "IEF", "PSP", "VFEM.L", "VEA", "BND")
quantmod::getSymbols(stocks)
stocks    = remove_special_chars(stocks)
tbill     = 
	read.csv(paste0(datadir, "/Tbill_1month.csv")) %>%
		filter(tbill != ".") %>%
			mutate(
			       tbill = as.numeric(tbill)/100,
			       month = first_of_ym(as.Date(date, format = "%m/%d/%Y"))
			       ) %>%
				group_by(month) %>%
					summarize(tbill = last(tbill)) %>%
						data.frame() %>%
							set_colnames(c("date", "tbill"))
			
wei       = 
	read.csv(paste0(datadir, "/WEI.csv")) %>% 
		mutate(date = as.Date(DATE, format = "%m/%d/%Y")) %>%
			select(-DATE) %>%
				mutate(date = first_of_ym(date)) %>%
					group_by(date) %>%
						summarize(wei   = last(WEI)) %>%
							data.frame()
nfci      = 
	read.csv(paste0(datadir, "/NFCI.csv")) %>% 
		mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
			mutate(date = first_of_ym(date)) %>%
				group_by(date) %>%
					summarize(nfci   = last(nfci)) %>%
						data.frame()

## Get minimum date for each 
for(i in 1:length(stocks)){
	eval(
	     parse(
		   text = 
			   paste0(
				  stocks[i],    " = ", stocks[i], " %>% data.frame() %>% ", 
				  " mutate(date = as.Date(rownames(.), origin = '1970-01-01' )) %>% ",
				  " select(date, ", stocks[i], ".Adjusted) %>% ", 
				  " set_colnames(c('date', '", tolower(stocks[i]), "'));      "
			   )

	     )
	)
}


##=================================
## DATA PREPARATION
##=================================

## Merge all datasets together
dat     =
	merge_datasets(stocks, rep("date", length(stocks))) 

## Compute monthly returns
dat     =
	get_ret(dat, data_freq = "daily", ret_freq = "monthly")
colnames(dat)[which(colnames(dat) == names(dat)[sapply(dat, class) == "Date"])] = "date"
dat     =
	dat %>%
		select(colnames(.)[grepl("_ret", colnames(.)) | grepl("date", colnames(.))])

## Merge stocks data with economic index and Tbill
## Lag WEI and Tbill to make sure you are using data 
## that is available at the time of investment
## Also compute excess returns
## Note that all values for the assets are end-of-month fo the previous month, meaning that 
## 2021-02-01 is actually the end-of-month value for 2021-01
## The values for WEI and Tbill correspond that we use are the lagged ones
## and we do this to make sure we use data that is available at the time 	
## of forecasting. Indeed, at time t, we want to forecast the lead(asset return)
## using wei_chg	
dat     =
	dat %>%	
		left_join(., wei, by = "date", type = "left") %>%
			left_join(., tbill, by = "date", type = "left") %>%
				left_join(., nfci, by = "date", type = "left") %>%
					mutate(
					       wei_lag         = lag(wei, 1),
					       nfci_lag        = lag(nfci, 1),
					       tbill_lag       = lag(tbill, 1),
					       wei_chg         = wei_lag/lag(wei_lag, 1)-1,
					       nfci_chg        = nfci_lag/lag(nfci_lag, 1)-1,
                                	       lead_gspc_ret   = lead(gspc_ret  , 1), 
                                	       lead_psp_ret    = lead(psp_ret   , 1), 
                                	       lead_vfem.l_ret = lead(vfem.l_ret, 1),  
                                	       lead_vea_ret    = lead(vea_ret   , 1),  
                                	       lead_bnd_ret    = lead(bnd_ret   , 1) 
					) %>%
						filter(complete.cases(.) & date < first_of_ym(lubridate::today())) 
					        	
						
##===========================================
## RELEVANCE STATISTIC STRATEGY - BACKTEST 
##===========================================

## Compute Similarity, Informativeness, and Relevance 
## Technically we want to do this for all assets in the portfolio					
## but for now we just do it for the macro indices under
## under the assumption that the underlying behavior
## of all asset returns is driven by the same set of macro factors
## which we summarize via the two macro indices. The other main assumption 
## is that the DGP of the asset return (i.e. the distribution) is 					
## time-invariant
weights              = matrix(nrow = length(which(dat$date == "2018-01-01"):(nrow(dat))), ncol = 4)  
strategy_ret         = list()
mkt_ret              = list()
mean_weights         = c(30:1/sum(1:30)) 
for(j in (which(dat$date == "2018-01-01"):(nrow(dat)))){

	## Define lists to store temp results
        macro_relevance_list = list()
        macro_info_list      = list()
        macro_sim_list       = list()

	## Define historical period and current period
	historic   = j-1 
	hist_dates = dat$date[1:historic]
	sub_asset  = 
	        dat %>%
	        	select(colnames(.)[grepl("date", colnames(.)) | (grepl("lead", colnames(.)) & grepl("_ret", colnames(.)))]) %>%
				select(-lead_gspc_ret)
	sub_macro  = 
	        dat %>%
	        	select(date, wei_lag, nfci_lag)
	sp500_ret  = dat$lead_gspc_ret[j]
	curr_macro = as.numeric(sub_macro[j, 2:3])

	## Compure statistics
	for(i in 1:historic){
	  macro_info            = Mahalanobis_Distance_I(sub_macro[1:(j-1),2:3], sub_macro[i,2:3])
	  macro_sim             = Mahalanobis_Distance_S(sub_macro[1:(j-1),2:3], sub_macro[i,2:3], curr_macro)
	  macro_relevance       = macro_info + macro_sim 
	  macro_relevance_list  = append(macro_relevance_list, macro_relevance)
	  macro_info_list       = append(macro_info_list, macro_info)
	  macro_sim_list        = append(macro_sim_list, macro_sim)
	}

	## Order by most relevant and pick 10 most relevant dates
	macro_relevance_ordered = unlist(macro_relevance_list)[order(-1*unlist(macro_relevance_list))]
	most_relevant           = macro_relevance_ordered[1:30] 

	## Extract most relevant dates
 	dates     = dat$date[which(dat$date %in% hist_dates[as.numeric(names(most_relevant))])]	

	## Obtain asset returns as of those dates
	asset_ret = 
		sub_asset %>%
			filter(date %in% dates) %>%
				select(-date)
	
	## Compute moments for optimal portfolio weights 
        exp_ret   = asset_ret %>% sapply(., function(x) weighted.mean(x, w = mean_weights)) 
	cov_ret   = cov.wt(as.matrix(asset_ret), wt = mean_weights)$cov

	## Compute portfolio weights proportionally to 
	## the size of the expected return
	opt_wgt   = exp_ret/sum(exp_ret)
        #opt_wgt   = c((solve(cov_ret) %*% rep(1, 4))/c(t(rep(1,4)) %*% solve(cov_ret) %*% rep(1,4)))	

	## Implement strategy and store returns and also store SP500 returns
	temp_ret     = opt_wgt %*% t(as.matrix(sub_asset[j,2:ncol(sub_asset)]))
	strategy_ret = append(strategy_ret, temp_ret)
	mkt_ret      = append(mkt_ret, sp500_ret)
	print(paste0("The strategy ", ifelse(temp_ret - sp500_ret > 0, "overperformed ", "underperformed "), "the S&P 500"))

	## Save weights to track evolution
	weights[j-which(dat$date == "2018-01-01")+1, ] = opt_wgt
}


##======================================
## COMPUTE SHARPE RATIOS AND COMPARE
##======================================
					
## Compute SR of SP500 and strategy
strategy_sr   = mean(unlist(strategy_ret))*12/((var(unlist(strategy_ret))^0.5)*sqrt(12))
mkt_sr        = mean(unlist(mkt_ret))*12/((var(unlist(mkt_ret))^0.5)*sqrt(12))
print(strategy_sr)
print(mkt_sr)

## Test whether difference in SR is statistically significant
ret_test      = 
	summary(lm(unlist(strategy_ret) ~ unlist(mkt_ret)))
print(ret_test)


##======================================
## ASSUMPTIONS THAT IMPACTED RESULTS 
##======================================

## 1. Assumed that WEI and NFCI are reflective of real economy and  
## financial conditions.
## 2. I assumed that asset returns are driven exclusively by these macro factors
## 3. I assumed that asset return DGP does not changes over time 
## 4. I only used 4 assets which is not enough to offer significant 
## 5. I assumed 30 critical dates is the sweet spot, but this is arbitrary and not robust
## diversification benefits. 

## The next steps involve tackling each of these assumptions and 
## if not relaxing them altogether, at least making them less binding


##======================================
## PLOTS
##======================================

## TBD: Plot rolling correlation



















