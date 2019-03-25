# clearing out
rm(list = ls())
cat("\014")

# loading packages
library(tidyverse)
library(tictoc)
library(bizdays)
library(backtestr)
library(lubridate)
library(tidyquant)
library(rugarch)


tic()
# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")

# setting dates and parameters
dt_analysis <- as.Date("2019-03-22") # this was an error for march expirations
dt_expiration <- as.Date("2019-03-29") #backtestr::monthly_expiration(2019, 03)
dbl_strangle_delta <- 0.075
int_garch_fit_days <- 2521

# getting all the options for a the day before execution
#chr_path <- "/Users/Pritam/Desktop/L3_20190314/L3_options_20190314.csv"
df_option_all <-
    backtestr::option_all(ymd(20190322))



df_low_price_und <- 
    df_option_all %>% 
    dplyr::filter(expiration == dt_expiration) %>% 
    dplyr::filter(
        (type == "put" & strike <= underlying_price) |
        (type == "call" & strike > underlying_price)
    ) %>% 
    dplyr::filter(bid > 0)


#####################################################################
## NEED TO ADD: filter that there is at least one call and one put ##
#####################################################################
# continuing to filter 
# 2) low spread < 0.10
# 3) number of options
df_universe <-
    df_low_price_und %>% 
    group_by(underlying_symbol) %>% 
    summarize(
        avg_spread = mean(ask - bid)
        , tot_volume = sum(volume)
        , num_opt = n()
    ) %>% 
    dplyr::filter(avg_spread <= 0.10) %>% 
    dplyr::filter(num_opt >= 4) %>% 
    mutate(
        analysis_date = dt_analysis
        , expiration = dt_expiration
    )



# looping through and calculating some stuff for each underlying in
# df_universe
int_d2x <- bizdays(dt_analysis, dt_expiration)
df_universe$implied_forward <- NA_real_
df_universe$bid_swap_rate <- NA_real_
df_universe$ask_swap_rate <- NA_real_
df_universe$mid_swap_rate <- NA_real_
df_universe$put_delta <- NA_real_
df_universe$call_delta <- NA_real_
df_universe$put_strike <- NA_real_
df_universe$call_strike <- NA_real_
df_universe$put_bid <- NA_real_
df_universe$call_bid <- NA_real_
lst_opt_to_trade <- list()
lst_strangle <- list()

for (ix_und in 1:nrow(df_universe)){
    chr_underlying <-
        df_universe$underlying_symbol[ix_und]
    
    
    df_opt_all <- 
        df_option_all %>% 
        dplyr::filter(underlying_symbol == chr_underlying) %>% 
        dplyr::filter(expiration == dt_expiration) %>% 
        dplyr::filter(bid > 0)
    
    # calculating implied forward        
    dbl_implied_forward <- implied_forward(df_opt_all)
    # all otm options relative to implied foward
    df_otm_all <- otm_all(df_opt_all, dbl_implied_forward)
    # removing low information options
    df_otm <- otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- greeks(df_otm, int_d2x, dbl_implied_forward)
    # swap rates
    dbl_swap_rate <- swap_rate(df_otm, int_d2x)
    
    # select for 10-delta put 
    df_put <-
        df_otm %>% 
        dplyr::filter(type == "put")  %>% 
        dplyr::filter(
            abs(delta - dbl_strangle_delta) == min(abs(delta - dbl_strangle_delta))
        )
    
    # select for 10-delta call
    df_call <-
        df_otm %>% 
        dplyr::filter(type == "call")  %>% 
        dplyr::filter(
            abs(delta - dbl_strangle_delta) == min(abs(delta - dbl_strangle_delta))
        )
    
    
    # updating df_universe
    df_universe$implied_forward[ix_und] <- dbl_implied_forward
    df_universe$bid_swap_rate[ix_und] <- dbl_swap_rate[1]
    df_universe$ask_swap_rate[ix_und] <- dbl_swap_rate[2]
    df_universe$mid_swap_rate[ix_und] <- dbl_swap_rate[3]
    df_universe$put_delta[ix_und] <- df_put$delta[1]
    df_universe$call_delta[ix_und] <- df_call$delta[1]
    df_universe$put_strike[ix_und] <- df_put$strike[1]
    df_universe$call_strike[ix_und] <- df_call$strike[1]
    df_universe$put_bid[ix_und] <- df_put$bid[1]
    df_universe$call_bid[ix_und] <- df_call$bid[1]
    
    # collecting all otm options
    lst_opt_to_trade[[ix_und]] <- df_otm
    
    # strangles that will be traded
    lst_strangle[[ix_und]] <- bind_rows(df_put, df_call)
    
    # printing progress to screen
    print(
        paste0(chr_underlying, ": ", ix_und, " of ", nrow(df_universe))
    )
} 

# converting lists into dataframes
df_opt_to_trade <- bind_rows(lst_opt_to_trade)
df_strangle <- bind_rows(lst_strangle)


## loops through the underlyings and calculates GARCH(1,1) estimates
dt_garch_start <- add.bizdays(dt_analysis, -int_garch_fit_days)
spec <- ugarchspec() # creates a garch model specification
int_d2x <- bizdays(dt_analysis, dt_expiration)
df_universe$garch_forecast <- NA_real_
for(ix_und in 1:nrow(df_universe)){
    
    chr_underlying <- df_universe$underlying_symbol[ix_und]
    
    # using tidyquant to get the prices from yahoo
    df_px <- 
        tryCatch(
            tq_get(
                chr_underlying
                , get = "stock.prices"
                , from = dt_garch_start
                , to = dt_analysis
            ) 
            , warning = function(cond) return(tibble(adjusted = NA_real_))
            , error = function(cond) return(tibble(adjusted = NA_real_))
        )
    
    # if there are no missing prices thne fitting the data
    if(sum(is.na(df_px$adjusted))==0){
        df_px <- df_px %>% mutate(ret = adjusted/lag(adjusted) - 1)
        
        # fitting the model
        fit <- ugarchfit(spec, df_px$ret[2:nrow(df_px)], solver = 'hybrid')
        
        # forcasting until expiration using the fit
        ugfore <- ugarchforecast(fit, n.ahead = int_d2x)
        
        # calculated volatility until maturity
        dbl_vol_forecast <- ugfore@forecast$sigmaFor %>% mean() * sqrt(252)
        
        df_universe$garch_forecast[ix_und] <- dbl_vol_forecast    
    }
    
    
    # printing progress to screen
    print(
        paste0(chr_underlying, ": ", ix_und, " of ", nrow(df_universe))
    )
}


df_trade_sheet <- 
    df_universe %>% 
        mutate(vol_prem = bid_swap_rate - garch_forecast) %>% 
        filter(avg_spread < 0.10) %>% 
        #filter(implied_forward > 20) %>% 
        #filter(implied_forward < 100) %>% 
        filter(!is.na(garch_forecast)) %>%
        filter(put_bid + call_bid > 0.10)

write_csv(df_trade_sheet, "trade_sheet_20190325_weekly_all.csv")
print("DONE!")
toc()



## scratch work
# df_universe %>% filter(underlying_symbol == "IWM") %>% View()
# 
# 
# 
# df_leveraged_etf <- read_csv("data_input/top_leveraged_etfs.csv")
# 
# 
# 
# 
# df_universe %>% 
#     filter(underlying_symbol %in% df_leveraged_etf$Symbol) %>% 
#     View()

