library(tidyverse)
library(lubridate)


df_trades <- read_csv("2018_Trades_Master.csv")
df_transactions <- read_csv("tastyworks_transactions_x5849_2018-01-01_2018-10-22.csv")
df_transactions$Date <-
    df_transactions$Date %>% as_date()


View(df_trades)
View(df_transactions)

############
## TRADES ##
############
df_opening <-
    df_trades %>% 
    select(trade_date:open_additional_fee)
df_opening$trade_date <- 
    as.Date(df_opening$trade_date, format = "%m/%d/%Y")


df_unwind <- 
    df_trades %>% 
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


##################
## TRANSACTIONS ##
##################
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