# loading packages
library(lubridate)
library(backtestr)

# sequency of dummy-dates, one per month, that will be used
# to calculate the sequence of expiration dates
dt_dummy = seq(ymd(20131201), ymd(20181201), "months")

# data-frame that stores all the chain data
df_exec_date <- 
    tibble(
        dummy_date = dt_dummy
        , year = NA_integer_
        , month = NA_integer_
        , expiration = as.Date(NA)
        , last_trade_date = as.Date(NA)
        , execution = as.Date(NA)
    )

# looping through all the chains and calcuating the dates
for (ix in 1:nrow(df_exec_date)){
    int_year <- lubridate::year(df_exec_date$dummy_date[ix])
    int_month <- lubridate::month(df_exec_date$dummy_date[ix])
    df_exec_date$year[ix] <- int_year
    df_exec_date$month[ix] <- int_month
    df_exec_date$expiration[ix] <- 
        monthly_expiration(int_year, int_month) 
    df_exec_date$last_trade_date[ix] <- 
        monthly_last_td(int_year, int_month) 
}

df_exec_date$execution <- dplyr::lag(df_exec_date$last_trade_date)
df_exec_date <- df_exec_date %>% select(-dummy_date)
df_exec_date <- df_exec_date[-1, ]
df_exec_date
