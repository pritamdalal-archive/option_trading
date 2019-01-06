library(tidyverse)


#######################
## chain description ##
#######################
df_chain_desc_201401_201704 <-
    read_csv(
        "../../data/option_backtest/v2_chain_desc_5yr_201401_201704.csv"
    )
df_chain_desc_201705_201811 <-
    read_csv(
        "../../data/option_backtest/v2_chain_desc_5yr_201705_201811.csv"
    )

df_chain_desc <- 
    bind_rows(
        df_chain_desc_201401_201704
        , df_chain_desc_201705_201811
    )


# write_csv(df_chain_desc, "v2_chain_desc_201401_201811.csv")


###################
## chain history ##
###################
df_chain_hist_201401_201704 <-
    read_csv(
        "../../data/option_backtest/v2_chain_hist_5yr_201401_201704.csv"
    )
df_chain_hist_201705_201811 <-
    read_csv(
        "../../data/option_backtest/v2_chain_hist_5yr_201705_201811.csv"
    )

df_chain_hist <- 
    bind_rows(
        df_chain_hist_201401_201704
        , df_chain_hist_201705_201811
    )


# write_csv(df_chain_hist, "v2_chain_hist_201401_201811.csv")


####################
## option history ##
####################
df_opt_hist_201401_201704 <-
    read_csv(
        "../../data/option_backtest/v2_opt_hist_5yr_201401_201704.csv"
    )
df_opt_hist_201705_201811 <-
    read_csv(
        "../../data/option_backtest/v2_opt_hist_5yr_201705_201811.csv"
    )

df_opt_hist <- 
    bind_rows(
        df_opt_hist_201401_201704
        , df_opt_hist_201705_201811
    )


# write_csv(df_opt_hist, "v2_opt_hist_201401_201811.csv")







