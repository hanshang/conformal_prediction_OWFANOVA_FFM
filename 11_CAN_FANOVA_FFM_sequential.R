################
### alpha = 0.2
################

## arima

CAN_sequential_int_score_list_female_arima = CAN_sequential_int_score_list_male_arima = list()
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_arima[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_female_prefecture_rate_array, 
                                                                                       smooth_data = CAN_female_prefecture_rate_array_smooth, 
                                                                                       fh = iw, fore_method = "arima", alpha = 0.2)
    
    CAN_sequential_int_score_list_male_arima[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_male_prefecture_rate_array, 
                                                                                     smooth_data = CAN_male_prefecture_rate_array_smooth, 
                                                                                     fh = iw, fore_method = "arima", alpha = 0.2)
    print(iw); rm(iw)
}

CAN_sequential_int_score_list_female_arima_mat = CAN_sequential_int_score_list_male_arima_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_arima_mat[iw,] = apply(CAN_sequential_int_score_list_female_arima[[iw]]$int_score_array, 2, mean)
    CAN_sequential_int_score_list_male_arima_mat[iw,]   = apply(CAN_sequential_int_score_list_male_arima[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(CAN_sequential_int_score_list_female_arima_mat) = rownames(CAN_sequential_int_score_list_male_arima_mat) = 1:10
colnames(CAN_sequential_int_score_list_female_arima_mat) = colnames(CAN_sequential_int_score_list_male_arima_mat) = c("ECP", "CPD", "score")

## ets

CAN_sequential_int_score_list_female_ets = CAN_sequential_int_score_list_male_ets = list()
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_ets[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_female_prefecture_rate_array, 
                                                                                     smooth_data = CAN_female_prefecture_rate_array_smooth, 
                                                                                     fh = iw, fore_method = "ets", alpha = 0.2)
    
    CAN_sequential_int_score_list_male_ets[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_male_prefecture_rate_array, 
                                                                                   smooth_data = CAN_male_prefecture_rate_array_smooth, 
                                                                                   fh = iw, fore_method = "ets", alpha = 0.2)
    print(iw); rm(iw)
}

CAN_sequential_int_score_list_female_ets_mat = CAN_sequential_int_score_list_male_ets_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_ets_mat[iw,] = apply(CAN_sequential_int_score_list_female_ets[[iw]]$int_score_array, 2, mean)
    CAN_sequential_int_score_list_male_ets_mat[iw,]   = apply(CAN_sequential_int_score_list_male_ets[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(CAN_sequential_int_score_list_female_ets_mat) = rownames(CAN_sequential_int_score_list_male_ets_mat) = 1:10
colnames(CAN_sequential_int_score_list_female_ets_mat) = colnames(CAN_sequential_int_score_list_male_ets_mat) = c("ECP", "CPD", "score")


#################
### alpha = 0.05
#################

## arima

CAN_sequential_int_score_list_female_arima_PI_95 = CAN_sequential_int_score_list_male_arima_PI_95 = list()
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_arima_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_female_prefecture_rate_array, 
                                                                                             smooth_data = CAN_female_prefecture_rate_array_smooth, 
                                                                                             fh = iw, fore_method = "arima", alpha = 0.05)
    
    CAN_sequential_int_score_list_male_arima_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_male_prefecture_rate_array, 
                                                                                           smooth_data = CAN_male_prefecture_rate_array_smooth, 
                                                                                           fh = iw, fore_method = "arima", alpha = 0.05)
    print(iw); rm(iw)
}

CAN_sequential_int_score_list_female_arima_PI_95_value = CAN_sequential_int_score_list_male_arima_PI_95_value = array(NA, dim = c(12, 3, 10))
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_arima_PI_95_value[,,iw] = t(apply(CAN_sequential_int_score_list_female_arima_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    CAN_sequential_int_score_list_male_arima_PI_95_value[,,iw]   = t(apply(CAN_sequential_int_score_list_male_arima_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    rm(iw)
}

CAN_sequential_int_score_list_female_arima_PI_95_mat = CAN_sequential_int_score_list_male_arima_PI_95_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_arima_PI_95_mat[iw,] = apply(CAN_sequential_int_score_list_female_arima_PI_95[[iw]]$int_score_array, 2, mean)
    CAN_sequential_int_score_list_male_arima_PI_95_mat[iw,]   = apply(CAN_sequential_int_score_list_male_arima_PI_95[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(CAN_sequential_int_score_list_female_arima_PI_95_mat) = rownames(CAN_sequential_int_score_list_male_arima_PI_95_mat) = 1:10
colnames(CAN_sequential_int_score_list_female_arima_PI_95_mat) = colnames(CAN_sequential_int_score_list_male_arima_PI_95_mat) = c("ECP", "CPD", "score")

## ets

CAN_sequential_int_score_list_female_ets_PI_95 = CAN_sequential_int_score_list_male_ets_PI_95 = list()
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_ets_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_female_prefecture_rate_array, 
                                                                                           smooth_data = CAN_female_prefecture_rate_array_smooth, 
                                                                                           fh = iw, fore_method = "ets", alpha = 0.05)
    
    CAN_sequential_int_score_list_male_ets_PI_95[[iw]] = FMP_FFM_fun_int_eval_sequential(raw_data = CAN_male_prefecture_rate_array, 
                                                                                         smooth_data = CAN_male_prefecture_rate_array_smooth, 
                                                                                         fh = iw, fore_method = "ets", alpha = 0.05)
    print(iw); rm(iw)
}

CAN_sequential_int_score_list_female_ets_PI_95_value = CAN_sequential_int_score_list_male_ets_PI_95_value = array(NA, dim = c(12, 3, 10))
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_ets_PI_95_value[,,iw] = t(apply(CAN_sequential_int_score_list_female_ets_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    CAN_sequential_int_score_list_male_ets_PI_95_value[,,iw]   = t(apply(CAN_sequential_int_score_list_male_ets_PI_95[[iw]]$int_score_array, c(2, 3), mean))
    rm(iw)
}

CAN_sequential_int_score_list_female_ets_PI_95_mat = CAN_sequential_int_score_list_male_ets_PI_95_mat = matrix(NA, 10, 3)
for(iw in 1:10)
{
    CAN_sequential_int_score_list_female_ets_PI_95_mat[iw,] = apply(CAN_sequential_int_score_list_female_ets_PI_95[[iw]]$int_score_array, 2, mean)
    CAN_sequential_int_score_list_male_ets_PI_95_mat[iw,]   = apply(CAN_sequential_int_score_list_male_ets_PI_95[[iw]]$int_score_array, 2, mean)
    rm(iw)
}
rownames(CAN_sequential_int_score_list_female_ets_PI_95_mat) = rownames(CAN_sequential_int_score_list_male_ets_PI_95_mat) = 1:10
colnames(CAN_sequential_int_score_list_female_ets_PI_95_mat) = colnames(CAN_sequential_int_score_list_male_ets_PI_95_mat) = c("ECP", "CPD", "score")

# xtable outputs

xtable(rbind(cbind(CAN_FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05_mean,
                   CAN_FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05_mean,
                   CAN_sequential_int_score_list_female_arima_PI_95_mat),
             
             cbind(CAN_FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05_mean,
                   CAN_FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05_mean,
                   CAN_sequential_int_score_list_male_arima_PI_95_mat)), digits = 3)


xtable(rbind(cbind(CAN_FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05_mean,
                   CAN_FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05_mean,
                   CAN_sequential_int_score_list_female_ets_PI_95_mat),
             
             cbind(CAN_FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05_mean,
                   CAN_FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05_mean,
                   CAN_sequential_int_score_list_male_ets_PI_95_mat)), digits = 3)

