#################
## description ##
#################
# this script calculates the trades for the v2_5yr backtest


#######################
## clearning session ##
#######################
rm(list=ls())
cat("\014")


######################
## loading packages ##
######################
library(tidyverse)
library(tictoc)


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
df_opt_hist <-
    read_csv(
        "../../data/option_backtest/v2_5yr_opt_hist_201401_201811.csv"
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




############################
## df_trade initial setup ##
############################
# creating the list of all options (by parameters) that I am going to trade
# df_trade <-
#     # cartesian product of all underlying, expirations, variations
#     crossing(
#         variation = c(0.5, 0.3, 0.1)
#         , type = c("put", "call")
#         , expiration = df_chain$expiration 
#         , underlying = df_underlying$underlying    
#     ) %>%  
#     # this inner-join has the effect of adding back in the execution dates
#     inner_join(
#         df_chain %>% select(underlying, expiration, execution, last_trade_date)
#         , by = c("underlying", "expiration")
#     )

# puts
df_trade_put <- 
    df_chain_desc %>% 
        select(
            underlying_symbol, year, month
            , expiration, last_trade_date, execution 
        ) %>% 
        mutate(
            variation = 0.1
            , type = "put"
        )

# calls
df_trade_call <- 
    df_chain_desc %>% 
        select(
            underlying_symbol, year, month
            , expiration, last_trade_date, execution 
        ) %>% 
        mutate(
            variation = 0.1
            , type = "call"
        )


# all together now
df_trade <- bind_rows(df_trade_put, df_trade_call)


# arranging rows
df_trade <- 
    df_trade %>% arrange(expiration, underlying_symbol, type)

############################################################
## creating df_trades dataframe by finding actual options ##
############################################################
# looping through cartesian product of all trade parameters and choosing 
# options from df_market_history that fits each parameter
# will hold all options.
# 30 min - v2 5yr backtest, which is 25K trades
# 2 min - 50 underlyings 1.5 years of monthly
# 8 sec - SPY, 1.5 years of weekly 
df_all_options <- tibble()

tic()
# iterating through df_trades
for (ix in 1:nrow(df_trade)){
    # grabbing option from df_opt_hist
    df_option <-
        df_opt_hist %>% 
        dplyr::filter(
            underlying_symbol == df_trade$underlying_symbol[ix]) %>% 
        dplyr::filter(expiration == df_trade$expiration[ix]) %>% 
        dplyr::filter(data_date == df_trade$execution[ix]) %>% 
        dplyr::filter(type == df_trade$type[ix])  %>% 
        dplyr::filter(
            abs(delta - df_trade$variation[ix]) == 
                min(abs(delta - df_trade$variation[ix]))
        )
    
    # adding to dataframe that holds all options
    df_all_options <- bind_rows(df_all_options, df_option)
    
    #printing to screen
    if (ix %% 100 == 0){
        print(paste0(ix, " of ", nrow(df_trade)))
    }
}
toc()



#### temp - fixing an issue ####
# df_all_options <-
#     df_all_options %>% 
#         filter( # removed because there are no calls
#             !(underlying_symbol == "CMCSK" & expiration == "2014-07-19")
#         ) %>% 
#         filter( # removed because there are no calls
#             !(underlying_symbol == "DBD" & expiration == "2016-02-19")
#         )


df_all_options <- 
    df_all_options %>% 
        arrange(expiration, underlying_symbol, type)

#### testing ####
# df_trade %>% 
#     left_join(
#         df_all_options
#         , by=c("underlying_symbol", "expiration", "type")
#     ) %>% 
#     filter(is.na(underlying_price))
# 
# 
# 
# df_opt_hist %>% 
#     filter(underlying_symbol == "CMCSK") %>% 
#     filter(expiration == "2014-07-19") %>% View()
# 
# df_opt_hist %>% 
#     filter(underlying_symbol == "DBD") %>% 
#     filter(expiration == "2016-02-19") %>% View()


# adding columns of df_all_options to df_trades
df_trade <- 
    bind_cols(df_trade, df_all_options)


## checking the average min and max deltas
df_trade %>% 
    group_by(variation, type) %>%
    .$delta


#################################################
## getting option history and calculating PNLs ##
#################################################
# 6 min - 50 underlyings 1.5 years of monthly
# 11 sec - SPY, 1.5 years of weekly 
cp <- function(type){
    if (type == "put") { return(-1) }
    if (type == "call") { return(1) }
}

df_pnl <- tibble()

tic()
# iterating through all trades and grabbing all histories 
# (this takes minutes)
tic()
for (ix in 1:(nrow(df_trade))){
    
    # querying option history for price data
    df_opt <-
        df_opt_hist %>% 
        dplyr::filter(underlying_symbol == df_trade$underlying_symbol[ix]) %>% 
        dplyr::filter(expiration == df_trade$expiration[ix]) %>% 
        dplyr::filter(type == df_trade$type[ix]) %>% 
        dplyr::filter(strike == df_trade$strike[ix]) %>% 
        dplyr::filter(data_date >= df_trade$execution[ix]) %>% 
        arrange(data_date) %>% 
        mutate(
            variation = df_trade$variation[ix]
            , dly_opt_pnl = 0
            , dly_dh_pnl = 0
            , dly_opt_mid_pnl = 0
        )
    
    # iterating through price data to calculate PNLs
    for (ix_td in 1:nrow(df_opt)) {
        
        ## daily PNL
        if (ix_td == 1) {
            # selling at bid
            df_opt$dly_opt_pnl[1] <- df_opt$bid[1] - df_opt$ask[1]
            # delta-hedge
            df_opt$dly_dh_pnl[1] <- 0
            # selling at mid
            df_opt$dly_opt_mid_pnl[1] <- 0
        } else {
            # selling at bid
            df_opt$dly_opt_pnl[ix_td] <- 
                df_opt$ask[ix_td - 1] - df_opt$ask[ix_td]
            # delta-hedge
            df_opt$dly_dh_pnl[ix_td] <-
                cp(df_opt$type[ix_td]) * df_opt$delta[ix_td - 1] *
                (df_opt$underlying_price[ix_td] - df_opt$underlying_price[ix_td - 1])
            # selling at mid    
            df_opt$dly_opt_mid_pnl[ix_td] <- 
                df_opt$mid[ix_td - 1] - df_opt$mid[ix_td]
        }
        
    }
    
    # need to try something different for this
    # seems to be taking too much time
    df_pnl <- bind_rows(df_pnl, df_opt)
    
    #printing to screen
    if (ix %% 100 == 0){
        print(paste0(ix, " of ", nrow(df_trade)))
        toc()
        tic()
    }
}
toc()

# adding total_pnl columms (option plus delta-hedge)
df_pnl <-
    df_pnl %>% 
    mutate(
        dly_tot_pnl = dly_opt_pnl + dly_dh_pnl
        , dly_tot_mid_pnl = dly_opt_mid_pnl + dly_dh_pnl
    )


######################################
## preliminary graphing and testing ##
######################################
df_pnl %>% 
    #filter(data_date >= "2016-12-16" & expiration > "2016-12-16") %>% 
    #filter(data_date <= "2018-07-20" & expiration <= "2018-07-20") %>% 
    group_by(variation) %>% 
    summarize(
        total = sum(dly_opt_pnl)
    )

# testing
df_pnl %>% 
    filter(data_date >= "2016-12-16" & expiration > "2016-12-16") %>% 
    filter(data_date <= "2018-07-20") %>% 
    distinct(expiration)


# cummulative PNL graph
df_pnl %>% 
    filter(data_date > "2014-01-01") %>% 
    filter(underlying_symbol == "SPY") %>% 
    group_by(variation, data_date) %>% 
    summarize(
        daily = sum(dly_opt_pnl)
    ) %>% 
    mutate(
        ttd = cumsum(daily)
    ) %>% 
    ggplot() +
        geom_line(aes(x = data_date, y = ttd, color = factor(variation)))
 

# sharpe in 2017
df_pnl %>% 
    #dplyr::filter(data_date < "2018-01-01") %>% 
    group_by(variation) %>% 
    summarize(
        sharpe_ratio = (mean(dly_opt_pnl) / sd(dly_opt_pnl)) * sqrt(252)
    )


## writing files to CSV
write_csv(df_pnl, "v2_5yr_pnl_master.csv")
write_csv(df_trade, "v2_5yr_trade_master.csv")
























