## clearing environment and console
rm(list=ls())
cat("\014")

## loading packages
library(tidyverse)
library(lubridate)
library(bizdays)
library(tictoc)
library(backtestr)

## sourcing functions
source("function/missing_data.R")
source("function/backtest_expiration.R")
source("function/otm_exec_hist_raw.R")
source("function/otm_exec_hist.R")


## initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


## creating df_chains
df_chain_desc <- 
    read_csv("/Users/Pritam/files/data/option_backtest/v2_universe_history.csv")


## fixing the execution date issue
df_expiration <- backtest_expiration()
df_chain_desc <-
    df_chain_desc %>% 
        left_join(
            df_expiration %>% select(expiration, true_execution = execution)
            , by = "expiration"
        )
df_chain_desc <- 
    df_chain_desc %>% 
        mutate(execution = true_execution) %>% 
        select(-true_execution)

int_num_chain <- df_chain_desc %>% nrow()

## initializing additional columns
df_chain_desc$d2x <- NA_integer_
df_chain_desc$exec_day_volume <- NA_integer_
df_chain_desc$exec_day_num_opt <- NA_integer_
df_chain_desc$error <- NA


## initializing history dataframes
df_chain_hist = tibble()
df_opt_hist = tibble()


tic() # big timer

## looping through all chains
for (ix_chn in (1:nrow(df_chain_desc))){
    
    tic() # little timer
    
    # grabbing inputs from chain_description
    chr_underlying <- df_chain_desc$underlying_symbol[ix_chn]
    dt_expiration <- df_chain_desc$expiration[ix_chn]
    dt_execution <- df_chain_desc$execution[ix_chn]
    
    ###########################
    ## massive function call ##
    ###########################
    lst_otm_exec_hist <- 
        otm_exec_hist(
            underlying = chr_underlying
            , expiration = dt_expiration
            , execution = dt_execution
        )
    
    # if there was no error then update all the histories
    if (!lst_otm_exec_hist$error){
        # updating chain description
        df_chain_desc$exec_day_volume[ix_chn] <-
            lst_otm_exec_hist$chain_description$exec_day_volume[1]
        df_chain_desc$exec_day_num_opt[ix_chn] <-
            lst_otm_exec_hist$chain_description$exec_day_num_opt[1]
        df_chain_desc$error[ix_chn] <- FALSE
        # updating chain history
        df_chain_hist <- 
            df_chain_hist %>%
                bind_rows(lst_otm_exec_hist$chain_history)
        # updating option history
        df_opt_hist <- 
            df_opt_hist %>%
            bind_rows(lst_otm_exec_hist$option_history)
    }
    
    # if there was an error then make a note in chain description
    if (lst_otm_exec_hist$error){
        df_chain_desc$error[ix_chn] <- TRUE
    }
    
    
    # printing an update to the screen
    print(paste0(ix_chn, " of ", int_num_chain))
    print(paste0(chr_underlying, ": ", dt_expiration))
    print(paste0("rows in option history: ", nrow(df_opt_hist)))
    toc() # little timer
}

toc() # big timer

#-------------------#
# writing csv files #
#-------------------#
tic()
 write_csv(df_chain_desc, "v2_chain_desc_5yr.csv")
  write_csv(df_chain_hist, "v2_chain_hist_5yr.csv")
 write_csv(df_opt_hist, "v2_opt_hist_5yr.csv")
toc()

tic()
df_chain_desc %>% 
    filter(expiration < "2017-05-19") %>% 
    write_csv("v2_chain_desc_5yr_201401_201704.csv")

df_chain_hist %>%  
    filter(expiration < "2017-05-19") %>% 
    write_csv("v2_chain_hist_5yr_201401_201704.csv")

df_opt_hist %>% 
    filter(expiration < "2017-05-19") %>% 
    write_csv("v2_opt_hist_5yr_201401_201704.csv")
toc()

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






