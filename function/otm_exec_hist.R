otm_exec_hist <- function(underlying
                          , expiration
                          , execution_date){
   
    # initialize the error output    
    lst_error_output  <-
        list(
            chain_description = tibble()
            , chain_history = tibble()
            , option_history = tibble()
            , error = TRUE
        )
    
    lst_output <-  
        tryCatch(
            otm_exec_hist_raw(underlying, expiration, execution_date)
            , error = function(cond) return(lst_error_output)
        )    

    lst_output
}