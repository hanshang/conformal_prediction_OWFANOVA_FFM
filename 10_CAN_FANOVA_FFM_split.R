################
### alpha = 0.2
################

## female

# arima

CAN_FMP_FFM_fun_int_score_female_array_arima_sd = CAN_FMP_FFM_fun_int_score_female_array_arima_quantile = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_female_prefecture_rate_array, 
                               smooth_data = CAN_female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.2)
    CAN_FMP_FFM_fun_int_score_female_array_arima_sd[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_female_array_arima_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_female_array_arima_sd_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_arima_sd, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_female_array_arima_quantile_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_arima_quantile, c(2, 3), mean))

# ets

CAN_FMP_FFM_fun_int_score_female_array_ets_sd = CAN_FMP_FFM_fun_int_score_female_array_ets_quantile = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_female_prefecture_rate_array, 
                               smooth_data = CAN_female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.2)
    CAN_FMP_FFM_fun_int_score_female_array_ets_sd[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_female_array_ets_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_female_array_ets_sd_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_ets_sd, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_female_array_ets_quantile_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_ets_quantile, c(2, 3), mean))

## male

# arima

CAN_FMP_FFM_fun_int_score_male_array_arima_sd = CAN_FMP_FFM_fun_int_score_male_array_arima_quantile = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_male_prefecture_rate_array, 
                               smooth_data = CAN_male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.2)
    CAN_FMP_FFM_fun_int_score_male_array_arima_sd[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_male_array_arima_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_male_array_arima_sd_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_arima_sd, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_male_array_arima_quantile_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_arima_quantile, c(2, 3), mean))

# ets

CAN_FMP_FFM_fun_int_score_male_array_ets_sd = CAN_FMP_FFM_fun_int_score_male_array_ets_quantile = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_male_prefecture_rate_array, 
                               smooth_data = CAN_male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.2)
    CAN_FMP_FFM_fun_int_score_male_array_ets_sd[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_male_array_ets_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_male_array_ets_sd_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_ets_sd, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_male_array_ets_quantile_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_ets_quantile, c(2, 3), mean))

#################
### alpha = 0.05
#################

## female

# arima

CAN_FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05 = CAN_FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05 = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_female_prefecture_rate_array, 
                               smooth_data = CAN_female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.05)
    CAN_FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05, c(2, 3), mean))

# ets

CAN_FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05 = CAN_FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05 = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_female_prefecture_rate_array, 
                               smooth_data = CAN_female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.05)
    CAN_FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05, c(2, 3), mean))

## male

# arima

CAN_FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05 = CAN_FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05 = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_male_prefecture_rate_array, 
                               smooth_data = CAN_male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.05)
    CAN_FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05, c(2, 3), mean))

# ets

CAN_FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05 = CAN_FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05 = 
  array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = CAN_male_prefecture_rate_array, 
                               smooth_data = CAN_male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.05)
    CAN_FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05[,,iw] = dum$int_score_sd
    CAN_FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

CAN_FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05, c(2, 3), mean))
CAN_FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05_mean = t(apply(CAN_FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05, c(2, 3), mean))

