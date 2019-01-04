# These are sketches related to some kind of smart process for chosing 
# underlyings to trade. The idea is that I will run this proces a day or
# two before execution, and then just trade the top 10 underlyings that come
# up.  Looking to the future, you could create two groups of underlyings
# "buys" and "sells" and then do a premium neutral optimization problem

library(tidyverse)
library(tictoc)
library(bizdays)
library(backtestr)

# initializing bizdays libraries
load_rmetrics_calendars(2015:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")



# getting all the options for a the day before execution
tic()
df_option_all <- option_all(as.Date("2018-03-15"))
toc()

# getting a comprehensive list of underlyings
df_etf <- read_csv("data_input/etf_list.csv")

df_etf$segment <- 
    df_etf$segment %>% str_to_lower()

df_etf <-
    df_etf %>% dplyr::filter(!str_detect(segment, "volatility"))





# getting all the options for the next expiration
# and filtering underlyings by certain criteria
# 1) ETFs only (I could probably pretty easily relax this and then
#    start introducing non-earning volatility single names.)
# 2) Low price (this seems to reduce the universe from 50 to 30)
df_low_price_und <- 
    df_option_all %>% 
        # inner_join(
        #     df_etf %>% select(symbol)
        #     , by = c("underlying_symbol" = "symbol")
        # ) %>%
        dplyr::filter(expiration == "2018-04-20") %>% 
        #dplyr::filter(underlying_price <= 75) %>% 
        dplyr::filter(
            (type == "put" & strike <= underlying_price) |
            (type == "call" & strike > underlying_price)
        ) %>% 
        dplyr::filter(bid > 0)


# continuing to filter 
# 3) low spread - max spread
# 4) minimum daily volume 
# 5) number of options
df_universe <-
    df_low_price_und %>% 
    group_by(underlying_symbol) %>% 
    summarize(
        avg_spread = mean(ask - bid)
        , tot_volume = sum(volume)
        , num_opt = n()
    ) %>% 
    dplyr::filter(avg_spread <= 0.10) %>% 
    dplyr::filter(num_opt >= 4)


#df_low_price_und %>% dplyr::filter(underlying_symbol == "AAL") %>% View()

int_d2x <- bizdays(as.Date("2018-03-15"), as.Date("2018-04-20"))
df_universe$implied_foward <- NA_real_
df_universe$bid_swap_rate <- NA_real_
df_universe$ask_swap_rate <- NA_real_
df_universe$mid_swap_rate <- NA_real_
df_universe$put_delta <- NA_real_
df_universe$call_delta <- NA_real_
df_universe$put_bid <- NA_real_
df_universe$call_bid <- NA_real_
df_opts_to_trade <- tibble()
df_strangle <- tibble()
tic()
for (ix_und in 1:nrow(df_universe)){
    chr_underlying <-
        df_universe$underlying_symbol[ix_und]
    
    
    df_opt_all <- 
        df_option_all %>% 
        dplyr::filter(underlying_symbol == chr_underlying) %>% 
        dplyr::filter(expiration == "2018-04-20") %>% 
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
            abs(delta - 0.10) == min(abs(delta - 0.10))
        )
    
    # select for 10-delta call
    df_call <-
        df_otm %>% 
        dplyr::filter(type == "call")  %>% 
        dplyr::filter(
            abs(delta - 0.10) == min(abs(delta - 0.10))
        )
    
    
    # updating df_universe
    df_universe$implied_foward[ix_und] <- dbl_implied_forward
    df_universe$bid_swap_rate[ix_und] <- dbl_swap_rate[1]
    df_universe$put_delta[ix_und] <- df_put$delta[1]
    df_universe$call_delta[ix_und] <- df_call$delta[1]
    df_universe$put_bid[ix_und] <- df_put$bid[1]
    df_universe$call_bid[ix_und] <- df_call$bid[1]
    
    # collecting all otm options
    df_opts_to_trade <-
        df_opts_to_trade %>% bind_rows(df_otm)
    
    # strangles that will be traded
    df_strangle <- 
        df_strangle %>% 
        bind_rows(df_put) %>% bind_rows(df_call)
    
    print(ix_und)
} 
toc()


# df_opts_to_trade %>% dplyr::filter(underlying_symbol == "ABT") %>% View()

#View(df_universe)


# filter to make sure the strangle is OTM enough
# make sure the at the strangle has enough value - 0.15 
df_universe %>% 
    dplyr::filter((put_delta < 0.15) & (call_delta < 0.15)) %>% 
    dplyr::filter((put_bid + call_bid) > 0.15) %>% 
    View()


# Once you have your universe, calculate a GARCH(1,1) volatility estimate
# and also the variance swap rate.  The difference between these will be your 
# vol premium estimate.  Choose the 10 with the highest vol premium estimate.
