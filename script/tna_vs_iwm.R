library(zoo)

df_tna <- tq_get("TNA")
df_iwm <- tq_get("IWM")


df_tna <- 
    df_tna %>% 
        mutate(tna_ret = log(adjusted/lag(adjusted)))


df_iwm <- 
    df_iwm %>% 
        mutate(iwm_ret = log(adjusted/lag(adjusted)))

df_ret <-
    df_iwm %>% select(date, iwm_adj = adjusted, iwm_ret) %>% 
        left_join(
            df_tna %>% select(date, tna_adj = adjusted, tna_ret)
            , by = "date"
        )


df_ret <- 
df_ret %>% 
    mutate(
        iwm_vol = rollapply(iwm_ret, 21, sd, fill = NA, align = "right")
        , tna_vol = rollapply(tna_ret, 21, sd, fill = NA, align = "right")
        , iwm_1_mon = log(iwm_adj/lag(iwm_adj, 21))
        , tna_1_mon = log(tna_adj/lag(tna_adj, 21))
    )


df_ret %>% 
    mutate(
        vol_mult = tna_vol/iwm_vol
        , ret_mult = tna_1_mon/iwm_1_mon
    ) %>% 
    .$vol_mult %>% 
    summary()


df_ret %>% 
    filter(abs(iwm_1_mon) > 0.05) %>% 
    filter(tna_adj > 20) %>% 
    mutate(
        vol_mult = tna_vol/iwm_vol
        , ret_mult = tna_1_mon/iwm_1_mon
    ) %>% View()
    .$ret_mult %>% 
    summary()
        






    