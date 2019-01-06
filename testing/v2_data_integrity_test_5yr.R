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


######################################
## percentage of chains with errors ##
######################################
# 147 out of 12,464; 1.17%
# this seems totally reasonable and quite good
df_chain_desc %>% filter(error == TRUE) %>% nrow() /
    df_chain_desc %>% nrow()



############################################
## let's get rid of the chains with error ##
############################################
df_chain_desc_no_error <- 
    df_chain_desc %>% filter(error == FALSE)



###################################
## adding in correct d2x columns ##
###################################
# I realized that the d2x column of the df_chain_desc was not being populated
# correctly, so I want to correct for this.  I have made the proper change in
# the code.

df_chain_desc_no_error <- 
    df_chain_desc_no_error %>% 
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


df_chain_desc_no_error$d2x <- int_d2x

## Note, you could also do this all with bizdays, which might be more
## robust.  Never-the-less, I'm sure it's all fine.  But if you have time
## go back and do the analysis with bizdays

########################################################
## df_opt_hist only has one row per option/trade_date ##
########################################################
# Sat 1/5/19 - this passed
# took 18 seconds to run
tic()
df_opt_hist %>% 
    group_by(
        underlying_symbol
        , expiration
        , type
        , strike
        , data_date
    ) %>% 
    summarize(
        entries = n()
    ) %>% 
    .$entries %>% 
    max()
toc()




#######################################################
## check for correct number of rows in df_chain_hist ##
#######################################################
## this checks out, there are exactly the number of chain-hist entries
# that we would expect.

df_chain_desc_no_error %>% 
    mutate(expected_chain_hist_rows = d2x + 1) %>% 
    .$expected_chain_hist_rows %>% 
    sum()

df_chain_hist %>% nrow()


##############################################################
## check for how many missing df_opt_hist entries there are ##
##############################################################
# it looks like there are no missing options prices.  At least there a
# are entries for every option/trade-date combination

df_chain_desc_no_error %>% 
    mutate(
        expected_opt_hist_rows = (d2x + 1) * exec_day_num_opt
    ) %>% 
    .$expected_opt_hist_rows %>% 
    sum()

    
df_opt_hist %>% nrow()


#######################################
## NA value somewhere in df_opt_hist ##
#######################################
# there are only 43 and they are all expiration_dates
df_opt_NA_rows <- 
    df_opt_hist[rowSums(is.na(df_opt_hist))!=0,] %>% 


View(df_opt_NA_rows)


##############################
## NA values for bid or ask ##
##############################
# there are no rows with missing bids or missing asks.
df_opt_hist %>% filter(is.na(bid) | is.na(ask))


#########################
## checking for splits ##
#########################
# this for-loop tracks all chains in which the underlying had a greater
# than 25% move in the price.
# all possible rows of in df_opt_hist are stored in int_row_to_check
tic()
int_row_to_check <- c()
ix_cand <- 0
for (ix in 2:nrow(df_opt_hist)){
    chr_und_prev <- df_opt_hist$underlying_symbol[ix - 1]
    chr_und_curr <- df_opt_hist$underlying_symbol[ix]
    chr_upx_prev <- df_opt_hist$underlying_price[ix -1]
    chr_upx_curr <- df_opt_hist$underlying_price[ix]
    
    if(
        ((abs(chr_upx_curr/chr_upx_prev) - 1) > 0.25) &
        (chr_und_prev == chr_und_curr)
    ){
        ix_cand <- ix_cand + 1
        int_row_to_check[ix_cand] <- ix
        print(ix)
    }
}
toc()

# checking all the candidate rows of df_opt_hist
df_split_candidates <- 
    df_opt_hist[int_row_to_check, ]

# figuring out the chains
df_split_chains <-
    df_split_candidates %>% distinct(underlying_symbol, expiration)

# writing those to a csv
write_csv(df_split_chains, "v2_split_canditates_5yr.csv")


# taking a look at all the chains in chain hist
df_chain_hist %>%
    inner_join(
        df_split_chains 
        , by = c("underlying" = "underlying_symbol", "expiration")
    ) %>% 
    View()



    







