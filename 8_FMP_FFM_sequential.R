# raw_data: mortality rate in the original scale
# smooth_data: smoothed mortality rate
# fh: forecast horizon
# fore_method: forecasting method, ets or auto.arima
# alpha: level of significance


FMP_FFM_fun_int_eval_sequential <- function(raw_data, smooth_data, fh, fore_method, alpha)
{
    n_age = dim(raw_data)[1]
    n_state = dim(raw_data)[2]
    n_year = dim(raw_data)[3]
    
    # define the test set
    
    n_validation = n_year - 11
    
    int_score_array = array(NA, dim = c((11 - fh), 3, n_state), dimnames = list(1:(11 - fh), c("ECP", "CPD", "score"), 1:n_state))
    q_alpha_mat = array(NA, dim = c(n_age, (11 - fh), n_state))
    for(iwk in 1:(11 - fh))
    {
        validation_set = 2:(n_validation + iwk - fh)
        fore_validation = array(NA, dim = c(n_age, n_state, length(validation_set)))
        for(ij in validation_set)
        {
            fore_validation[,,(ij - 1)] = FMP_FFM_fun(data_set_array = log(smooth_data[,,1:ij]), fh = fh, fmethod = fore_method)
            rm(ij)
        }
        
        # compute absolute of residuals
        
        resi = abs(raw_data[,,(2 + fh):((n_validation + iwk))] - exp(fore_validation))
        q_alpha = matrix(NA, n_age, n_state)
        for(iw in 1:n_state)
        {
            for(ij in 1:n_age)
            {
                # (1 - alpha) quantile of the absolute residuals
                
                rq_fit <- rq(resi[ij,iw,] ~ 1, tau = (1 - alpha))
                
                # use information criterion to select AR order
                
                AR_p <- max(ar(resi[ij,iw,])$order, 1)
                lastlags <- tail(x = resi[ij,iw,], AR_p)
                newdf <- as.data.frame(t(lastlags))
                names(newdf) <- paste0("lag", 1:AR_p)
                
                q_alpha[ij,iw] = predict(rq_fit, newdf)
                rm(ij)
            }
            rm(iw)
        }
        colnames(q_alpha) = 1:n_state
        rownames(q_alpha) = 1:n_age
        
        point_forecast <- exp(FMP_FFM_fun(data_set_array = log(smooth_data[,,1:(n_validation + iwk)]), fh = fh, fmethod = fore_method))
        for(iw in 1:n_state)
        {
            int_score_array[iwk,,iw] <- interval_score(holdout = raw_data[,iw,(n_validation + iwk + fh)], 
                                                       lb = (point_forecast - q_alpha)[,iw],
                                                       ub = (point_forecast + q_alpha)[,iw], 
                                                       alpha = alpha)
        }
        q_alpha_mat[,iwk,] = q_alpha
        rm(iwk)
    }
    return(list(q_alpha_array = q_alpha_mat, int_score_array = int_score_array))
}

### alpha = 0.2

## arima

# female

sequential_int_score_list_female_arima = sequential_int_score_list_male_arima = list()
for(iw in 1:10)
{
    sequential_int_score_list_female_arima[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = female_prefecture_rate_array, 
                                                                             smooth_data = female_prefecture_rate_array_smooth, 
                                                                             fh = iw, fore_method = "arima", alpha = 0.2)
 
    sequential_int_score_list_male_arima[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = male_prefecture_rate_array, 
                                                                           smooth_data = male_prefecture_rate_array_smooth, 
                                                                           fh = iw, fore_method = "arima", alpha = 0.2)
    print(iw); rm(iw)
}

sequential_int_score_list_female_arima_mat = sequential_int_score_list_male_arima_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    sequential_int_score_list_female_arima_mat[iw,] = apply(sequential_int_score_list_female_arima[[iw]]$int_score_array, 2, mean)
    sequential_int_score_list_male_arima_mat[iw,]   = apply(sequential_int_score_list_male_arima[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(sequential_int_score_list_female_arima_mat) = rownames(sequential_int_score_list_male_arima_mat) = 1:10
colnames(sequential_int_score_list_female_arima_mat) = colnames(sequential_int_score_list_male_arima_mat) = c("ECP", "CPD", "score")

## ets

# female

sequential_int_score_list_female_ets = sequential_int_score_list_male_ets = list()
for(iw in 1:10)
{
    sequential_int_score_list_female_ets[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = female_prefecture_rate_array, 
                                                                                 smooth_data = female_prefecture_rate_array_smooth, 
                                                                                 fh = iw, fore_method = "ets", alpha = 0.2)
    
    sequential_int_score_list_male_ets[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = male_prefecture_rate_array, 
                                                                               smooth_data = male_prefecture_rate_array_smooth, 
                                                                               fh = iw, fore_method = "ets", alpha = 0.2)
    print(iw); rm(iw)
}

sequential_int_score_list_female_ets_mat = sequential_int_score_list_male_ets_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    sequential_int_score_list_female_ets_mat[iw,] = apply(sequential_int_score_list_female_ets[[iw]]$int_score_array, 2, mean)
    sequential_int_score_list_male_ets_mat[iw,]   = apply(sequential_int_score_list_male_ets[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(sequential_int_score_list_female_ets_mat) = rownames(sequential_int_score_list_male_ets_mat) = 1:10
colnames(sequential_int_score_list_female_ets_mat) = colnames(sequential_int_score_list_male_ets_mat) = c("ECP", "CPD", "score")


### alpha = 0.05

## arima

# female

sequential_int_score_list_female_arima_PI_95 = sequential_int_score_list_male_arima_PI_95 = list()
for(iw in 1:10)
{
    sequential_int_score_list_female_arima_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = female_prefecture_rate_array, 
                                                                                   smooth_data = female_prefecture_rate_array_smooth, 
                                                                                   fh = iw, fore_method = "arima", alpha = 0.05)

    sequential_int_score_list_male_arima_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = male_prefecture_rate_array, 
                                                                                 smooth_data = male_prefecture_rate_array_smooth, 
                                                                                 fh = iw, fore_method = "arima", alpha = 0.05)
    print(iw); rm(iw)
}

sequential_int_score_list_female_arima_PI_95_value = sequential_int_score_list_male_arima_PI_95_value = array(NA, dim = c(47, 3, 10))
for(iw in 1:10)
{
    sequential_int_score_list_female_arima_PI_95_value[,,iw] = t(apply(sequential_int_score_list_female_arima_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    sequential_int_score_list_male_arima_PI_95_value[,,iw]   = t(apply(sequential_int_score_list_male_arima_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    rm(iw)
}

sequential_int_score_list_female_arima_PI_95_mat = sequential_int_score_list_male_arima_PI_95_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    sequential_int_score_list_female_arima_PI_95_mat[iw,] = apply(sequential_int_score_list_female_arima_PI_95[[iw]]$int_score_array, 2, mean)
    sequential_int_score_list_male_arima_PI_95_mat[iw,]   = apply(sequential_int_score_list_male_arima_PI_95[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(sequential_int_score_list_female_arima_PI_95_mat) = rownames(sequential_int_score_list_male_arima_PI_95_mat) = 1:10
colnames(sequential_int_score_list_female_arima_PI_95_mat) = colnames(sequential_int_score_list_male_arima_PI_95_mat) = c("ECP", "CPD", "score")

## ets

# female

sequential_int_score_list_female_ets_PI_95 = sequential_int_score_list_male_ets_PI_95 = list()
for(iw in 1:10)
{
    sequential_int_score_list_female_ets_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = female_prefecture_rate_array, 
                                                                                 smooth_data = female_prefecture_rate_array_smooth, 
                                                                                 fh = iw, fore_method = "ets", alpha = 0.05)
    
    sequential_int_score_list_male_ets_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = male_prefecture_rate_array, 
                                                                               smooth_data = male_prefecture_rate_array_smooth, 
                                                                               fh = iw, fore_method = "ets", alpha = 0.05)
    print(iw); rm(iw)
}

sequential_int_score_list_female_ets_PI_95_value = sequential_int_score_list_male_ets_PI_95_value = array(NA, dim = c(47, 3, 10))
for(iw in 1:10)
{
    sequential_int_score_list_female_ets_PI_95_value[,,iw] = t(apply(sequential_int_score_list_female_ets_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    sequential_int_score_list_male_ets_PI_95_value[,,iw]   = t(apply(sequential_int_score_list_male_ets_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    rm(iw)
}

sequential_int_score_list_female_ets_PI_95_mat = sequential_int_score_list_male_ets_PI_95_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    sequential_int_score_list_female_ets_PI_95_mat[iw,] = apply(sequential_int_score_list_female_ets_PI_95[[iw]]$int_score_array, 2, mean)
    sequential_int_score_list_male_ets_PI_95_mat[iw,]   = apply(sequential_int_score_list_male_ets_PI_95[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(sequential_int_score_list_female_ets_PI_95_mat) = rownames(sequential_int_score_list_male_ets_PI_95_mat) = 1:10
colnames(sequential_int_score_list_female_ets_PI_95_mat) = colnames(sequential_int_score_list_male_ets_PI_95_mat) = c("ECP", "CPD", "score")

