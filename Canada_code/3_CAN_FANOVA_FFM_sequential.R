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

# save results

save(list = ls(pattern = "^CAN_sequential_int_score_list_.*_PI_95"), file = "results/CAN_sequential_95.RData")
