otm_exec_hist_raw <- function(underlying
                              , expiration
                              , execution_date){

    
    # renaming underlyings according to my style guide
    chr_underlying <- underlying
    dt_expiration <- expiration
    dt_execution <- execution_date
    
    dt_last_trade <- 
        backtestr::monthly_last_td(
            year = lubridate::year(dt_expiration)
            , month = lubridate::month(dt_expiration)
        )
    
    # calculuating days to expiration
    int_d2x = bizdays::bizdays(dt_execution, dt_expiration)
    int_exec_d2x = bizdays::bizdays(dt_execution, dt_expiration)
    
    
    # grabbing the option chain on execution from database
    df_opt_all <- 
        backtestr::option_chain(
            trade_date = dt_execution
            , underlying = chr_underlying
            , expiration = dt_expiration
            , exclude_zero_bid = TRUE
        )
    
    # adding the d2x column
    df_opt_all <- 
        df_opt_all %>% dplyr::mutate(d2x = int_d2x)

    # rearranging columns
    df_opt_all <- 
        df_opt_all %>% 
            dplyr::select(
                underlying_symbol:data_date
                , d2x
                , strike:iv_mean
            )
    
    
    #------------------------#
    # wrangling calculations #
    #------------------------#
    # calculating implied forward        
    dbl_implied_forward <- backtestr::implied_forward(df_opt_all)
    # all otm options relative to implied foward
    df_otm_all <- backtestr::otm_all(df_opt_all, dbl_implied_forward)
    # removing low information options
    df_otm <- backtestr::otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- backtestr::greeks(df_otm, int_d2x, dbl_implied_forward)
    
    
    
    #-------------------#
    # updating df_chain #
    #-------------------#
    # this is one part of the output
    df_chain_desc <- 
        tibble::tibble(
            d2x = int_d2x
            , exec_day_volume = df_opt_all$volume %>% sum()
            , exec_day_num_opt = df_otm %>% nrow()
        )
    
    
    
    
    #------------------------#
    # updating df_chain_hist #
    #------------------------#
    df_chain_hist <- tibble::tibble() # intializing this dataframe
                                      # this is one part of the output            
    dbl_swap_rate <- 
        backtestr::swap_rate(df_otm, int_d2x) #change this to named vector
    
    df_chain_hist <- 
        df_chain_hist %>%
            dplyr::bind_rows(
                tibble::tibble(
                    underlying = chr_underlying
                    , expiration = dt_expiration
                    , trade_date = dt_execution
                    , last_trade_date = dt_last_trade
                    , d2x = int_d2x
                    , volume = df_opt_all$volume %>% sum()
                    , implied_forward = dbl_implied_forward
                    , bid_swap_rate = dbl_swap_rate[1]
                    , ask_swap_rate = dbl_swap_rate[2]
                    , mid_swap_rate = dbl_swap_rate[3]
                ) 
            )
        
    
    
    #----------------------#
    # updating df_opt_hist #
    #----------------------#
    df_opt_hist <- tibble::tibble() # this is one part of the output
                                    # this initializes it
    df_opt_hist <- dplyr::bind_rows(df_opt_hist, df_otm)
    
    
    #------------------------------------------------------------------------#
    # looping through all trade days of this expiration and grabbing px info #
    #------------------------------------------------------------------------#
    # sequence of post-execution business days
    dt_post_exec_td <- 
        bizseq(add.bizdays(dt_execution, 1), dt_expiration)
    
    
    #loop through the trading days and grab
    #the price history for all the options in df_otm
    for (ix_td in 1:(length(dt_post_exec_td))){
        
        
        dt_trade <- dt_post_exec_td[ix_td]
        
        
        
        # calculuating days to expiration
        int_d2x = bizdays(dt_trade, dt_expiration)
        
        
        #debugging
        # if(int_d2x == 1) {
        #     browser()
        # }
        
        
        # grabbing all option prices for trade date
        df_opt_px_all <-
            option_chain(
                trade_date = dt_trade
                , underlying = chr_underlying
                , expiration = dt_expiration
                , exclude_zero_bid = FALSE
            )
        
        
        # calculating the implied forward price
        if(dt_trade == dt_last_trade){
            dbl_implied_forward <- 
                mean(df_opt_px_all$underlying_price[1], rm.na = TRUE)
        } else {
            dbl_implied_forward <- implied_forward(df_opt_px_all)    
        }
        
        # calculating swap rates - set to zero on expiration
        if(dt_trade == dt_last_trade){
            dbl_swap_rate <- c(0, 0, 0)
        } else {
            # BUG: this is the wrong set of options to be using to 
            #      calculate this.  Need to fix!!
            dbl_swap_rate <- swap_rate(df_otm, int_d2x)
        }
        
        #---------------------#
        # updating chain hist #
        #---------------------#
        df_chain_hist <- 
            df_chain_hist %>% 
            bind_rows(
                tibble(
                    underlying = chr_underlying
                    , expiration = dt_expiration
                    , trade_date = dt_trade
                    , last_trade_date = dt_last_trade
                    , d2x = int_d2x
                    , volume = df_opt_px_all$volume %>% sum()
                    , implied_forward = dbl_implied_forward
                    , bid_swap_rate = dbl_swap_rate[1]
                    , ask_swap_rate = dbl_swap_rate[2]
                    , mid_swap_rate = dbl_swap_rate[3]
                ) 
            )
        
        
        #--------------------------------------------------#
        # filtering for only the execution day otm options #
        #--------------------------------------------------#
        df_opt_px <-
            df_otm %>%
            select(underlying_symbol, expiration, type, strike) %>%
            left_join( ## QUESTION: What are the implications of making 
                ##           this an inner join?
                #inner_join(
                df_opt_px_all
                , by = c("underlying_symbol", "type", "strike", "expiration")
            ) %>% 
            mutate(d2x = int_d2x)
        
        # filling in some missing data in case of empty prices
        df_opt_px <- missing_data(df_opt_px_all, df_opt_px, dt_trade)
        
        # recalculating greeks
        if(dt_trade == dt_last_trade){
            df_opt_px <- greeks_exp(df_opt_px)
        } else {
            df_opt_px <- greeks(df_opt_px, int_d2x, dbl_implied_forward)    
        } 
        
        df_opt_hist <- bind_rows(df_opt_hist, df_opt_px)
    }
    
    # creating output list
    lst_output <- 
        list(
            chain_description = df_chain_desc
            , chain_history = df_chain_hist
            , option_history = df_opt_hist
            , error = FALSE
        )
    
    # returning output
    lst_output
}
    
    
