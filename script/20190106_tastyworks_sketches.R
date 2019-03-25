##############################
## cleaning out environment ##
##############################
rm(list = ls())
cat("\014")



######################
## loading packages ##
######################
library(tidyverse)
library(lubridate)
library(backtestr)
library(bizdays)
library(tictoc)


## initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")




#####################
## reading in data ##
#####################
# setting the base file path
chr_data_path <- "/Users/Pritam/files/data/tastyworks/"

# my master trades csv
# chr_file_path <- 
#     paste0(chr_data_path, "trades_master_2018.csv")
# df_spreadsheet <- read_csv(chr_file_path)

# tastyworks transactions csv
chr_file_path <- 
    paste0(
        chr_data_path
        , "tastyworks_transactions_x5849_2018-01-01_2018-12-31.csv"
    )
df_transactions <- read_csv(chr_file_path)

# refactoring Date column of tastyworks dataframe
df_transactions$Date <-
    df_transactions$Date %>% as_date()





#############################################
## extracting all trades from transactions ##
#############################################

df_trade <- 
    df_transactions %>%
        filter(Type == "Trade")


# grabbing only the columns I need and renaming them using a 
# minimal transformation so that they match my style guide
df_trade_clean <-
    df_trade %>% 
        select(
            trade_date = Date, action = Action, quantity = Quantity
            , underlying_symbol = `Underlying Symbol`
            , expiration_date = `Expiration Date`
            , strike_price = `Strike Price`
            , call_or_put = `Call or Put`
            , value = Value
            , commissions = Commissions, fees = Fees
            , multiplier = Multiplier
        )


# adding a trade price column and then rearranging columns
df_trade_clean <- 
    df_trade_clean %>% 
        mutate(
            trade_price = abs(value / multiplier)
        ) %>% 
        select(
            trade_date:call_or_put, trade_price
            , value:multiplier
        )

# switching the call_or_put column to lower case because that matches
# my convention in the rest of my code base
df_trade_clean$call_or_put <- 
    df_trade_clean$call_or_put %>% str_to_lower()

# refactoring the expiration date
df_trade_clean$expiration_date <-
    df_trade_clean$expiration_date %>% mdy()




#################################################
## extracting option history to calculate PNLs ##
#################################################
# TL;DR - I basically figured out that the "straight-forward" approach
# that I initially had in mind for querying the delta_neutral database
# directly wasn't going to work, due to slow database performance.
# I then decided to basically grab MORE information than I really needed
# using the existing functions that I have, which gave acceptable performace.


# these are all the chain that I traded, along with the first
# trade_date that I traded them
df_chain <- 
    df_trade_clean %>% 
        group_by(underlying_symbol, expiration_date) %>% 
        summarize(
            first_trade_date = min(trade_date)
        )


##############################################################
# the data that is generated in with this loop is stored    ##
# in tastyworks_opt_hist_2018.csv, it may need to be re-run ##
# from time to time.                                        ##
##############################################################
# # looping through all the chain and grabbing all the EOD prices for 
# # each trading day starting from the first_trade_date until expiration.
# df_opt_hist <- tibble()
# lst_opt_hist <- list() # figure out how to implement this
# ix_opt_hist <- 1
# for (ix_chn in 1:nrow(df_chain)){
#     tic()
#     chr_underlying <- df_chain$underlying_symbol[ix_chn]
#     dt_expiration <- df_chain$expiration_date[ix_chn]
#     dt_first_trade <- df_chain$first_trade_date[ix_chn]
#     
#     
#     dt_trade <- bizseq(dt_first_trade, dt_expiration)
#     
#     for (ix_td in 1:length(dt_trade)){
#     
#     ## updating df_opt_hist
#     # df_opt_hist <- # this is the slow way
#     #     df_opt_hist %>%
#     #     bind_rows(
#     #         backtestr::option_chain(
#     #             trade_date = dt_trade[ix_td]
#     #             , underlying = chr_underlying
#     #             , dt_expiration
#     #         )
#     #     )
# 
#     # this is the faster way, but I need to figure out
#     # how to implement it
#     lst_opt_hist[[ix_opt_hist]] <-
#         backtestr::option_chain(
#             trade_date = dt_first_trade
#             , underlying = chr_underlying
#             , dt_expiration
#         )
#     ix_opt_hist <- ix_opt_hist + 1
#     
#     }
#     print(ix_chn)
#     toc()
# }

#df_opt_hist <- bind_rows(lst_opt_hist)
## writing the df_opt_hist to a CSV
# write_csv(df_opt_hist, "tastyworks_2018_opt_hist.csv")

df_opt_hist <- read_csv(
    "/Users/Pritam/files/data/tastyworks/tastyworks_opt_hist_2018.csv"
)


##########################################
## calculating daily PNL for each trade ##
##########################################
# option payoff function
payoff <- function(type, strike , underlying_price){
    if(type == "put"){return(max(strike - underlying_price, 0))}
    if(type == "call"){return(max(underlying_price -strike, 0))}
}

# ordering df_trade_clean by trade_date and underlying
df_trade_clean <- 
    df_trade_clean %>% 
        arrange(trade_date, underlying_symbol)



df_pnl_hist <- tibble()
## loops through and calculates all the dail PNLs for all the 
## trade in df_trade and then stores them in df_pnl_hist
for(ix_trade in 1:nrow(df_trade)){
    
    #ix_trade <- 21L
    tic()
    
    # grabbing information about current trade from df_trade_clean
    dt_trade <- df_trade_clean$trade_date[ix_trade]
    chr_action <- df_trade_clean$action[ix_trade]
    if (chr_action == "SELL_TO_OPEN"){
        int_direction <- -1    
    } else {
        int_direction <- 1
    }
    chr_underlying <- df_trade_clean$underlying_symbol[ix_trade]
    dt_expiration <- df_trade_clean$expiration_date[ix_trade]
    dbl_strike <- df_trade_clean$strike_price[ix_trade]
    chr_type <- df_trade_clean$call_or_put[ix_trade] %>% str_to_lower()
    dbl_trade_price <- df_trade_clean$trade_price[ix_trade]
    dbl_commission <- df_trade_clean$commissions[ix_trade]
    dbl_fee <- df_trade_clean$fees[ix_trade]

    
    # querying history of prices from df_opt_hist
    df_trade_pnl_hist <- 
        df_opt_hist %>% 
            filter(underlying_symbol == chr_underlying) %>% 
            filter(expiration == dt_expiration) %>%
            filter(strike == dbl_strike) %>% 
            filter(type == chr_type) %>%
            filter(data_date >= dt_trade) %>% 
            arrange(data_date)
    
    # adding columns to store
    df_trade_pnl_hist$action <- chr_action
    df_trade_pnl_hist$trade_price <- dbl_trade_price
    df_trade_pnl_hist$commission <- dbl_commission # TO DO: zeros for all days but first
    df_trade_pnl_hist$fee <- dbl_fee # TO DO: zeros for all days but first
    df_trade_pnl_hist$daily_pnl <- NA_real_
    
    
    ## looping the all trade days and calculating a PNL for each of them
    for(ix_td in 1:nrow(df_trade_pnl_hist)){
        
        # first trade day for trades not on expiration day
        # TO DO: make this based on trade_date and not ix_td
        if((ix_td == 1) & !(ix_td == nrow(df_trade_pnl_hist))){
            df_trade_pnl_hist$daily_pnl[ix_td] <-
                int_direction *
                ((df_trade_pnl_hist$mid[ix_td] - dbl_trade_price) * 100) +
                dbl_commission +
                dbl_fee
        } 
        
        # trade days between first_trade_date and expiration
        # TO DO: make this based on trade_date and not ix_td
        if(!((ix_td == 1) | (ix_td == nrow(df_trade_pnl_hist)))) {
            df_trade_pnl_hist$daily_pnl[ix_td] <-
                int_direction * 
                (df_trade_pnl_hist$mid[ix_td] - 
                df_trade_pnl_hist$mid[ix_td - 1]) * 100  
        }
        
        
        # expiration date, for trades not on expiration date 
        if(ix_td == nrow(df_trade_pnl_hist) & !(ix_td == 1)){
        
            dbl_upx <- df_trade_pnl_hist$underlying_price[ix_td]
            dbl_payoff <- payoff(chr_type, dbl_strike, dbl_upx)
            df_trade_pnl_hist$bid[ix_td] <- dbl_payoff
            df_trade_pnl_hist$ask[ix_td] <- dbl_payoff
            df_trade_pnl_hist$mid[ix_td] <- dbl_payoff
            
            df_trade_pnl_hist$daily_pnl[ix_td] <-
            int_direction * 
                (df_trade_pnl_hist$mid[ix_td] - 
                     df_trade_pnl_hist$mid[ix_td - 1]) * 100  

        }
        
        
        # trades on expiration date
        if(ix_td == nrow(df_trade_pnl_hist) & (ix_td == 1)){
            
            dbl_upx <- df_trade_pnl_hist$underlying_price[ix_td]
            dbl_payoff <- payoff(chr_type, dbl_strike, dbl_upx)
            df_trade_pnl_hist$bid[ix_td] <- dbl_payoff
            df_trade_pnl_hist$ask[ix_td] <- dbl_payoff
            df_trade_pnl_hist$mid[ix_td] <- dbl_payoff
            
            df_trade_pnl_hist$daily_pnl[ix_td] <-
                int_direction *
                ((df_trade_pnl_hist$mid[ix_td] - dbl_trade_price) * 100) +
                dbl_commission +
                dbl_fee 
            
        }
        
    }
    
    df_pnl_hist <- 
        df_pnl_hist %>% 
        bind_rows(df_trade_pnl_hist)
}


## total strategy PNL: $686.42
df_pnl_hist$daily_pnl %>% sum()


## calls vs put
df_pnl_hist %>% 
    filter(expiration != "2018-06-15") %>% 
    filter(expiration != "2018-10-19") %>% 
    group_by(type) %>% 
    summarize(
        pnl = sum(daily_pnl)
    )


    



#######################
## PNL by Expiration ##
#######################
# max profit by expiration
df_max_profit <- 
    df_trade_clean %>%
        filter(action == "SELL_TO_OPEN") %>% 
        group_by(expiration_date) %>% 
        summarize(
            prem_sold = sum(value)
            , commissions = sum(commissions)
            , fees = sum(fees)
            , max = prem_sold + commissions + fees
        )
df_max_profit$max %>% sum()
df_max_profit$prem_sold %>% sum()

# total trading days
int_tot_trading_days = bizdays(ymd(20180316), ymd(20181221))

# premium sold per trading day
df_max_profit$prem_sold %>% sum() %>% `/` (int_tot_trading_days)
df_max_profit$commissions %>% sum() + 
    df_max_profit$fees%>% sum() 


# passive pnl by expiration
df_passive_pnl <- 
    df_pnl_hist %>% 
        filter(action == "SELL_TO_OPEN") %>% 
        group_by(expiration) %>% 
        summarize(
            passive = sum(daily_pnl)
        )

# realized pnl by expiration
df_realized_pnl <-
    df_pnl_hist %>% 
        group_by(expiration) %>% 
        summarize(
            realized = sum(daily_pnl)
        ) %>% 
        mutate(
            ttd = cumsum(realized)
        )

## this data frame puts it all together in a nice easy to read way
df_pnl_by_expiration <- 
    df_max_profit %>% 
        left_join(
            df_passive_pnl
            , by = c("expiration_date" = "expiration")
        ) %>% 
        left_join(
            df_realized_pnl
            , by = c("expiration_date" = "expiration")
        ) %>% 
        mutate(
            active = realized - passive
        )

# percentage of max-profit retained: 50%
(df_pnl_by_expiration$realized %>% sum()) /
    (df_pnl_by_expiration$max %>% sum())
    


###################################
## Sharpe-Ratio Using Daily PNLs ##
###################################
## amalgamating the strategy PNL so that I can caluclate a Sharpe-Ratio on it
df_strategy_pnl <- 
    df_pnl_hist %>% 
        group_by(data_date) %>% 
        summarize(
            daily_pnl = sum(daily_pnl)
        ) %>% 
        arrange(data_date) %>% 
        mutate(
            ttd_pnl = cumsum(daily_pnl)
        ) 

## ttd PNL plot, there is a two day 500 dollar down-turn that I 
## don't quite believe.  Don't show this at Securian lunch on 1/7/2019
df_strategy_pnl %>% 
    ggplot() +
    geom_line(
        aes(x = data_date, y = ttd_pnl)
    )


#  passive PNL
df_pnl_hist %>% 
    filter(action == "SELL_TO_OPEN") %>% 
    .$daily_pnl %>% 
    sum()

# Annualized Sharpe-Ratio
((df_strategy_pnl$daily_pnl %>% mean()) /
    (df_strategy_pnl$daily_pnl %>% sd())) * sqrt(252)
        


# realized returns annualized
1.068575 ^ (365/280)
1.068575 ^ (252/195)

# passive returns annualized
1.050224 ^ (365/280)

#########################################
## analyzing PNLs on a per-trade basis ##
#########################################
## TL;DR - this code was written to debug a mistake I was making in my
##         calculations.  I'll leave if for now so you can see how smart
##         I am.

# calculating total PNL by trade from df_pnl_hist
df_pnl_by_trade <-
    df_pnl_hist %>% 
        group_by(underlying_symbol, expiration, strike, type, action) %>% 
        summarize(
            first_trade_date = min(data_date)
            , total_pnl = sum(daily_pnl)
        ) %>% 
        arrange(expiration, first_trade_date)


# this is a hack to find the underlying price on expiration
df_expiration_upx <-
    df_opt_hist %>% 
        filter(data_date == expiration) %>% 
        group_by(underlying_symbol, expiration) %>% 
        summarize(
            expiration_upx = mean(underlying_price, na.rm = TRUE)
        )

# attaching the expirations upx to the df_trade_clean so that I
# can calucluate total PNL for each trade
df_trade_clean <- 
    df_trade_clean %>% 
        left_join(
            df_expiration_upx
            , by = c("underlying_symbol", "expiration_date" = "expiration")
        )

# these are inputs for the payoff function; I am putting them into a 
# dataframe so that they I can use purrr::pmap_dbl()
df_payoff_input <- 
    df_trade_clean %>%
    mutate(lcase_call_or_put = str_to_lower(call_or_put)) %>% 
    select(
        type = lcase_call_or_put
        , strike = strike_price
        , underlying_price = expiration_upx
    )

# calculating the payoff of the trades using pmap_dbl()
df_trade_clean$option_payoff <- 
    pmap_dbl(df_payoff_input, payoff) * 100


# looping through all the trades and calculating to total trade PNL that 
# takes into account the trade_price, commissions, fees, and option-payoff
df_trade_clean$trade_pnl <- NA_real_ 
for(ix_trade in 1:nrow(df_trade)){
    chr_action <- df_trade_clean$action[ix_trade]
    dbl_value<- df_trade_clean$value[ix_trade]
    dbl_commission <- df_trade_clean$commissions[ix_trade]
    dbl_fee <- df_trade_clean$fees[ix_trade]
    dbl_payoff <- df_trade_clean$option_payoff[ix_trade] 
     
    # slightly different calculation for buys and sells   
    dbl_trade_pnl <- 0
    if (chr_action == "SELL_TO_OPEN"){
        dbl_trade_pnl <- 
            dbl_value + 
            dbl_commission + 
            dbl_fee -
            dbl_payoff
    } else {
        dbl_trade_pnl <- 
            dbl_value + 
            dbl_commission + 
            dbl_fee +
            dbl_payoff
    }
    
    df_trade_clean$trade_pnl[ix_trade] <- dbl_trade_pnl
}


# total pnl
df_trade_clean$trade_pnl %>% sum() # based on this calculation
df_pnl_hist$daily_pnl %>% sum() # based on original calculation


# this query show all the trade that don't match between the two 
# methodogies; this should be empty now that I fixed everything.
df_trade_clean %>% 
    left_join(
        df_pnl_by_trade
        , by = c("underlying_symbol", "expiration_date" = "expiration"
                , "strike_price" = "strike", "call_or_put" = "type"
                , "action")
        
    ) %>% 
    filter(round(trade_pnl, 2) != round(total_pnl,2)) %>%
    select(call_or_put, strike_price, expiration_upx, value
           , commissions, fees, option_payoff, trade_pnl, total_pnl)




################
################
### OLD CODE ###
################
################

# ###################
# ## my trades csv ##
# ###################
# df_opening <-
#     df_spreadsheet %>% 
#     select(trade_date:open_additional_fee)
# df_opening$trade_date <- 
#     as.Date(df_opening$trade_date, format = "%m/%d/%Y")
# 
# 
# df_unwind <- 
#     df_spreadsheet %>% 
#     select(unwind_date:unwind_additional_fee) %>% 
#     filter(!is.na(unwind_date))
# df_unwind$unwind_date <- 
#     as.Date(df_unwind$unwind_date, format = "%m/%d/%Y")
# 
# df_inflow <-
#     df_opening %>% 
#     group_by(trade_date) %>% 
#     summarize(
#         inflow = (sum(trade_px)*100) - 
#             sum(open_commission + 
#                     open_additional_fee + 
#                     open_tran_fee)
#     )
# 
# df_outflow <-
#     df_unwind %>% 
#     group_by(unwind_date) %>% 
#     summarize(
#         outflow = -(sum(unwind_px)*100) - 
#             sum(unwind_additional_fee + 
#                     unwind_tran_fee)
#     )
# 
# 
# df_cashflows <-     
#     full_join(df_inflow
#               , df_outflow
#               , by = c("trade_date" = "unwind_date")
#     ) %>% 
#     arrange(trade_date)
# 
# df_cashflows$inflow[is.na(df_cashflows$inflow)] <- 0
# df_cashflows$outflow[is.na(df_cashflows$outflow)] <- 0
# df_cashflows <-
#     df_cashflows %>% mutate(total = inflow + outflow)
# 
# 
# #################################
# ## tastyworks transactions csv ##
# #################################
# df_tran_values <-
#     df_transactions %>% 
#     filter(Type == "Trade") %>% 
#     group_by(Date) %>% 
#     summarize(
#         values = sum(Value, na.rm = TRUE) + 
#             sum(Commissions, na.rm = TRUE) + 
#             sum(Fees, na.rm = TRUE)
#     )
# 
# 
# df_comparison <-
#     df_tran_values %>% 
#     left_join(
#         df_cashflows
#         , by = c("Date" = "trade_date")
#     ) 
# 
# df_comparison <- 
#     df_comparison %>% 
#     replace_na(list(values = 0, inflow = 0, outflow = 0, total = 0))
# 
# df_comparison %>% 
#     filter(values != total) %>% 
#     View()
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
   


