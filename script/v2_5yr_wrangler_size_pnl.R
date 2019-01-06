#############
## purpose ##
#############
# The purpose of this script is to scale the PNLs correctly.


######################
## clearing session ##
######################
rm(list=ls())
cat("\014")


######################
## loading packages ##
######################
library(tidyverse)



#####################
## reading in data ##
#####################
df_chain_desc <-
    read_csv(
        "../../data/option_backtest/v2_5yr_chain_desc_201401_201811.csv"
    )

df_chain_hist <-
    read_csv(
        "../../data/option_backtest/v2_5yr_chain_hist_201401_201811.csv"
    )

df_trade_master <- 
    read_csv(
        "../../data/option_backtest/v2_5yr_trade_master.csv"
        , col_types = cols()
    )

df_pnl_master <- 
    read_csv(
        "../../data/option_backtest/v2_5yr_pnl_master.csv"
        , col_types = cols()
    )



############################################
## let's get rid of the chains with error ##
############################################
df_chain_desc <- df_chain_desc %>% filter(error == FALSE)


#################################
## removing troublesome chains ##
#################################
df_chain_desc <- 
    df_chain_desc %>% 
    filter( # removed because there are no calls
        !(underlying_symbol == "CMCSK" & expiration == "2014-07-19")
    ) %>% 
    filter( # removed because there are no calls
        !(underlying_symbol == "DBD" & expiration == "2016-02-19")
    )




###################################
## adding in correct d2x columns ##
###################################
# I realized that the d2x column of the df_chain_desc was not being populated
# correctly, so I want to correct for this.  I have made the proper change in
# the code.
df_chain_desc <- 
    df_chain_desc %>% 
    arrange(expiration, underlying_symbol)

int_d2x <-
    df_chain_hist %>%
    left_join(
        df_chain_desc %>% filter(error == FALSE)
        , by = c("underlying"="underlying_symbol", "expiration")
    ) %>% 
    filter(trade_date == execution) %>% 
    arrange(expiration, underlying) %>% 
    .$d2x.x


df_chain_desc$d2x <- int_d2x



##################################
## creating df_position_scaling ##
##################################
df_call <- df_trade_master %>% dplyr::filter(type == "call")
df_put <- df_trade_master %>% dplyr::filter(type == "put")    

# The main idea behind this section is that we want to sell about $1 in premium
# every month to make sure that all the position sizes are similar. I decided
# to use the mid prices to scale the positions, rather than the bid, because
# I didn't want to take especially large positions in an option, just because
# it has a very wide bid/ask spread.
df_position_scaling <-
    df_chain_desc %>% 
        left_join(
            df_put
            , by = c("underlying_symbol", "expiration", "execution")
            , suffix = c(".put", "put1")
        ) %>%
        select(
            variation, underlying_symbol, expiration, execution, d2x = d2x.put
            , bid, ask, mid
        ) %>% 
        inner_join(
            df_call
            , by = c("variation", "expiration", "underlying_symbol", "execution")
            , suffix = c(".put", ".call")
        ) %>% 
        select(
            variation, underlying_symbol, expiration, execution, d2x = d2x.put
            , bid.put, ask.put, mid.put
            , bid.call, ask.call, mid.call
        ) %>% 
        mutate(
            strangle_mult = 
                (d2x * 0.05) / (mid.put + mid.call)
            
            , strangle_prem_sold = 
                (bid.put + bid.call) * ((d2x * 0.05) / (mid.put + mid.call)) 
            
            , put_mult = (d2x * 0.05) / mid.put
            
            , put_prem_sold = 
                bid.put * ((d2x * 0.05) / mid.put)
            
            , call_mult = (d2x * 0.05) / mid.call
            
            , call_prem_sold = 
                bid.call * ((d2x * 0.05) / mid.call)
        )


# testing
df_position_scaling %>% 
    group_by(variation) %>% 
    summarize(
        tot_put = sum(put_prem_sold)
        , tot_call = sum(call_prem_sold)
        , tot_strangle = sum(strangle_prem_sold)
    )


#############################################################
## adding unity_mult position size scalar factor to df_pnl ##
#############################################################
# strangles
df_strangle_pnl_scaled <- 
    df_pnl_master %>% 
        left_join(
            df_position_scaling
            , by = c("variation", "expiration", "underlying_symbol")
        ) %>% 
        select(
            underlying_symbol:data_date
            , d2x = d2x.x, strike:dly_tot_mid_pnl, strangle_mult
        )

# puts
df_put_pnl_scaled <-
    df_pnl_master %>% 
        dplyr::filter(type == "put") %>% 
        left_join(
            df_position_scaling
            , by = c("variation", "expiration", "underlying_symbol")
        ) %>% 
        select(
            underlying_symbol:data_date
            , d2x = d2x.x, strike:dly_tot_mid_pnl, put_mult
        )

# calls
df_call_pnl_scaled <-
    df_pnl_master %>% 
        dplyr::filter(type == "call") %>% 
        left_join(
            df_position_scaling
            , by = c("variation", "expiration", "underlying_symbol")
        ) %>% 
        select(
            underlying_symbol:data_date
            , d2x = d2x.x, strike:dly_tot_mid_pnl, call_mult
        )



#######################
## writing CSV files ##
#######################
write_csv(df_position_scaling, "v2_5yr_position_scaling.csv")
write_csv(df_strangle_pnl_scaled, "v2_5yr_strangle_pnl_scaled.csv")
write_csv(df_put_pnl_scaled, "v2_5yr_put_pnl_scaled.csv")
write_csv(df_call_pnl_scaled, "v2_5yr_call_pnl_scaled.csv")



## looking at the worst PNLs
df_strangle_pnl_scaled %>% 
    mutate(
        scaled_daily_opt = dly_opt_pnl * strangle_mult
    ) %>% 
    group_by(underlying_symbol, expiration) %>% 
    summarize(
        pnl = sum(scaled_daily_opt)
    ) %>% 
    arrange(pnl) %>% 
    View()




