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




#####################
## reading in data ##
#####################
# setting the base file path
chr_data_path <- "/Users/Pritam/files/data/tastyworks/"

# my master trades csv
chr_file_path <- 
    paste0(chr_data_path, "trades_master_2018.csv")
df_spreadsheet <- read_csv(chr_file_path)

# tastyworks transactions csv
chr_file_path <- 
    paste0(
        chr_data_path
        , "tastyworks_transactions_x5849_2018-01-01_2018-12-31.csv"
    )
df_transactions <- read_csv(chr_file_path)

# refactoring Date column
df_transactions$Date <-
    df_transactions$Date %>% as_date()
# df_transactions$`Expiration Date` <-
#     df_transactions$`Expiration Date` %>% as_date()


#View(df_spreadsheet)
#View(df_transactions)




#############################################
## extracting all trades from transactions ##
#############################################

df_trade <- 
    df_transactions %>%
        filter(Type == "Trade")



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


df_trade_clean <- 
    df_trade_clean %>% 
        mutate(
            trade_price = value / multiplier  
        ) %>% 
        select(
            trade_date:call_or_put, trade_price
            , value:multiplier
        )

df_trade_clean$expiration_date <-
    df_trade_clean$expiration_date %>% mdy()



for (ix_trade in (1:nrow(df_trade))){
    dt_trade <- df_trade_clean$trade_date[ix_trade]
    int_quantity <- df_trade_clean$quantity[ix_trade]
    chr_underlying <- df_trade_clean$underlying_symbol[ix_trade]
    dt_expiration <- df_trade_clean$expiration_date[ix_trade]
    dbl_strike <- df_trade_clean$strike_price[ix_trade]
    chr_type <- df_trade_clean$call_or_put[ix_trade]
    dbl_trade_price <- df_trade_clean$trade_price[ix_trade]
    dbl_commission <- df_trade_clean$commissions[ix_trade]
    dbl_fee <- df_trade_clean$fees[ix_trade]

    
    chr_query <- 
        paste0("select * from option_price where DataDate>='", dt_trade 
               ,"' and UnderlyingSymbol='",  chr_underlying
               ,"' and Expiration='",  dt_expiration , "';")
               # ,"' and Strike=",  dbl_strike
               # ," and Type='", str_to_lower(chr_type), "';")

}
        
        
        
backtestr::db_option(chr_query, exclude_zero_bid = FALSE)
        
        







###################
## my trades csv ##
###################
df_opening <-
    df_spreadsheet %>% 
    select(trade_date:open_additional_fee)
df_opening$trade_date <- 
    as.Date(df_opening$trade_date, format = "%m/%d/%Y")


df_unwind <- 
    df_spreadsheet %>% 
    select(unwind_date:unwind_additional_fee) %>% 
    filter(!is.na(unwind_date))
df_unwind$unwind_date <- 
    as.Date(df_unwind$unwind_date, format = "%m/%d/%Y")

df_inflow <-
    df_opening %>% 
    group_by(trade_date) %>% 
    summarize(
        inflow = (sum(trade_px)*100) - 
            sum(open_commission + 
                    open_additional_fee + 
                    open_tran_fee)
    )

df_outflow <-
    df_unwind %>% 
    group_by(unwind_date) %>% 
    summarize(
        outflow = -(sum(unwind_px)*100) - 
            sum(unwind_additional_fee + 
                    unwind_tran_fee)
    )


df_cashflows <-     
    full_join(df_inflow
              , df_outflow
              , by = c("trade_date" = "unwind_date")
    ) %>% 
    arrange(trade_date)

df_cashflows$inflow[is.na(df_cashflows$inflow)] <- 0
df_cashflows$outflow[is.na(df_cashflows$outflow)] <- 0
df_cashflows <-
    df_cashflows %>% mutate(total = inflow + outflow)


#################################
## tastyworks transactions csv ##
#################################
df_tran_values <-
    df_transactions %>% 
    filter(Type == "Trade") %>% 
    group_by(Date) %>% 
    summarize(
        values = sum(Value, na.rm = TRUE) + 
            sum(Commissions, na.rm = TRUE) + 
            sum(Fees, na.rm = TRUE)
    )


df_comparison <-
    df_tran_values %>% 
    left_join(
        df_cashflows
        , by = c("Date" = "trade_date")
    ) 

df_comparison <- 
    df_comparison %>% 
    replace_na(list(values = 0, inflow = 0, outflow = 0, total = 0))

df_comparison %>% 
    filter(values != total) %>% 
    View()
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
   


