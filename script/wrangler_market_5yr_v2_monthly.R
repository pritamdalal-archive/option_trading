# clearing shit out
rm(list=ls())
cat("\014")

# loading packages
library(tidyverse)
library(lubridate)
library(bizdays)
library(tictoc)
library(backtestr)

# sourcing functions
source("function/missing_data.R")
source("function/backtest_expiration.R")


# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


# creating df_chains
df_chain <- 
    read_csv("/Users/Pritam/files/data/option_backtest/v2_universe_history.csv")

# df_chain <- 
#     df_chain %>% filter(year == 2014)

# fixing the execution date issue
df_expiration <- backtest_expiration()
df_chain <-
    df_chain %>% 
        left_join(
            df_expiration %>% select(expiration, true_execution = execution)
            , by = "expiration"
        )
df_chain <- 
    df_chain %>% 
        mutate(execution = true_execution) %>% 
        select(-true_execution)



df_chain$d2x <- NA_integer_
df_chain$exec_day_volume <- NA_integer_
df_chain$exec_day_num_opt <- NA_integer_

tic()
df_chain_hist = tibble()
df_opt_hist = tibble()
# looping through all chains
for (ix_chn in 1:10){#nrow(df_chain)){
    #ix_chn <- 1
    
    tic()
    # grabbing the execution and expriation
    chr_underlying <- df_chain$underlying_symbol[ix_chn]
    dt_execution <- df_chain$execution[ix_chn]
    dt_expiration <- df_chain$expiration[ix_chn]
    dt_last_trade <- df_chain$last_trade_date[ix_chn]
    
    # calculuating days to expiration
    int_d2x = bizdays(dt_execution, dt_expiration)
    int_exec_d2x = bizdays(dt_execution, dt_expiration)
    
    # grabbing the option chain on execution from database
    df_opt_all <- 
        option_chain(
             trade_date = dt_execution
            , underlying = chr_underlying
            , expiration = dt_expiration
            , exclude_zero_bid = TRUE
        )
    
    
    #------------------------#
    # wrangling calculations #
    #------------------------#
    # calculating implied forward        
    dbl_implied_forward <- implied_forward(df_opt_all)
    # all otm options relative to implied foward
    df_otm_all <- otm_all(df_opt_all, dbl_implied_forward)
    # removing low information options
    df_otm <- otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- greeks(df_otm, int_d2x, dbl_implied_forward)
    
    
    
    #-------------------#
    # updating df_chain #
    #-------------------#
    # recording int_d2x 
    df_chain$d2x[ix_chn] <- int_d2x
    #recording execution day volume in df_chain
    df_chain$exec_day_volume[ix_chn] <- df_opt_all$volume %>% sum()
    # recording number of options
    df_chain$exec_day_num_opt[ix_chn] <- df_otm %>% nrow()
    
    
    
    
    #------------------------#
    # updating df_chain_hist #
    #------------------------#
    dbl_swap_rate <- swap_rate(df_otm, int_d2x) #change this to named vector
    df_chain_hist <- 
        df_chain_hist %>% 
            bind_rows(
                tibble(
                    underlying = chr_underlying
                    , expiration = dt_expiration
                    , trade_date = dt_execution
                    , last_trade_date = dt_last_trade
                    , d2x = int_d2x
                    , implied_forward = dbl_implied_forward
                    , bid_swap_rate = dbl_swap_rate[1]
                    , ask_swap_rate = dbl_swap_rate[2]
                    , mid_swap_rate = dbl_swap_rate[3]
                ) 
            )

       
    #----------------------#
    # updating df_opt_hist #
    #----------------------#
    df_opt_hist <- bind_rows(df_opt_hist, df_otm)
    
    
    #------------------------------------------------------------------------#
    # looping through all trade days of this expiration and grabbing px info #
    #------------------------------------------------------------------------#
    # sequence of post-execution business days
    dt_post_exec_td <- 
        bizseq(add.bizdays(dt_execution, 1), dt_expiration)
    
    
    #loop through the trading days and grab
    #the price history for all the options in df_otm
    for (ix_td in 1:(length(dt_post_exec_td))){
        
        
        dt_trade <- dt_post_exec_td[ix_td]
        
        
        
        # calculuating days to expiration
        int_d2x = bizdays(dt_trade, dt_expiration)
        
        #debugging
        # if(int_d2x == 1) {
        #     browser()
        # }
        
        
        # grabbing all option prices for trade date
        df_opt_px_all <-
           option_chain(
                trade_date = dt_trade
               , underlying = chr_underlying
               , expiration = dt_expiration
               , exclude_zero_bid = FALSE
           )
        
        
        # calculating the implied forward price
        if(dt_trade == dt_last_trade){
            dbl_implied_forward <- 
                mean(df_opt_px_all$underlying_price[1], rm.na = TRUE)
        } else {
            dbl_implied_forward <- implied_forward(df_opt_px_all)    
        }
        
        # calculating swap rates - set to zero on expiration
        if(dt_trade == dt_last_trade){
            dbl_swap_rate <- c(0, 0, 0)
        } else {
            dbl_swap_rate <- swap_rate(df_otm, int_d2x)
        }
        
        #---------------------#
        # updating chain hist #
        #---------------------#
        df_chain_hist <- 
            df_chain_hist %>% 
            bind_rows(
                tibble(
                    underlying = chr_underlying
                    , expiration = dt_expiration
                    , trade_date = dt_trade
                    , last_trade_date = dt_last_trade
                    , d2x = int_d2x
                    , implied_forward = dbl_implied_forward
                    , bid_swap_rate = dbl_swap_rate[1]
                    , ask_swap_rate = dbl_swap_rate[2]
                    , mid_swap_rate = dbl_swap_rate[3]
                ) 
            )
        
        
        #------------------------------------------#
        # filtering for only the execution day otm #
        #------------------------------------------#
        df_opt_px <-
           df_otm %>%
               select(underlying_symbol, expiration, type, strike) %>%
               left_join( ## QUESTION: What are the implications of making 
                           ##           this an inner join?
                #inner_join(
                   df_opt_px_all
                   , by = c("underlying_symbol", "type", "strike", "expiration")
               )
        
        # filling in some missing data in case of empty prices
        df_opt_px <- missing_data(df_opt_px_all, df_opt_px, dt_trade)
        
        # recalculating greeks
        if(dt_trade == dt_last_trade){
            df_opt_px <- greeks_exp(df_opt_px)
        } else {
            df_opt_px <- greeks(df_opt_px, int_d2x, dbl_implied_forward)    
        }
        
        #----------------------#
        # updating df_opt_hist #
        #----------------------#
        df_opt_hist <- bind_rows(df_opt_hist, df_opt_px)
    }
    
    print(paste0(chr_underlying, ": ", dt_expiration))
    toc()
}
toc()


#-------------------#
# writing csv files #
#-------------------#
# write_csv(df_chain, "spy_weekly_chain_desc_5yr.csv")
# write_csv(df_chain_hist, "spy_weekly_chain_hist_5yr.csv")
# write_csv(df_opt_hist, "spy_weekly_opt_hist_5yr.csv")


#################
## data issues ##
#################
# data is missing for the 4/2/2015 expiring SPY weekly option, on their
# expiration date.  need to bring to Rick's attention.

# 12/18/15 options are listed as 12/19/2015 expiration.


# 2014 (go back and fill these data issues in properly)
# ix_chn = 353, missing option price causing a problem in greeks

# ix_chn = 404, looks like a bad price is messing up the implied vol
# calculator

# ix_chn = 724, need to look into - HOT expiring on 3/22/2014.  There is
# a change in the strike prices, but doesn't look like caused by a split.

#############################
## changes to make to code ##
#############################





##############
## old code ##
##############

## sourcing functions
# source("function/db_connection.R")
# source("function/option_chain.R") 
# source("function/implied_forward.R")
# source("function/otm_all.R") 
# source("function/otm_clean.R")
# source("function/greeks.R")
# source("function/greeks_exp.R")
# source("function/swap_rate.R")
# source("function/chain_monthly.R")

## selecting underlyings to be wrangled
# df_underlying <- 
#     read_csv("data_input/monthly_underlying.csv", col_types = cols())

## creating df_chains
# all underlying, expiration, execution dates
# df_chain <- chain_monthly(df_underlying$underlying)
# df_chain <- chain_monthly(c("SPY", "QQQ"))

