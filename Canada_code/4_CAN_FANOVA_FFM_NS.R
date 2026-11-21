#################
### alpha = 0.05
#################

## female

# arima

CAN_NS_int_score_female_array_arima_alpha_0.05 = array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = CAN_female_prefecture_rate_array,
                      smooth_data = CAN_female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05)
    CAN_NS_int_score_female_array_arima_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

CAN_NS_int_score_female_array_arima_alpha_0.05_mean = t(apply(CAN_NS_int_score_female_array_arima_alpha_0.05, c(2, 3), mean))

# ets

CAN_NS_int_score_female_array_ets_alpha_0.05 = array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = CAN_female_prefecture_rate_array,
                      smooth_data = CAN_female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05)
    CAN_NS_int_score_female_array_ets_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

CAN_NS_int_score_female_array_ets_alpha_0.05_mean = t(apply(CAN_NS_int_score_female_array_ets_alpha_0.05, c(2, 3), mean))

## male

# arima

CAN_NS_int_score_male_array_arima_alpha_0.05 = array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = CAN_male_prefecture_rate_array,
                      smooth_data = CAN_male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05)
    CAN_NS_int_score_male_array_arima_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

CAN_NS_int_score_male_array_arima_alpha_0.05_mean = t(apply(CAN_NS_int_score_male_array_arima_alpha_0.05, c(2, 3), mean))

# ets

CAN_NS_int_score_male_array_ets_alpha_0.05 = array(NA, dim = c(n_states, 3, 10), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = CAN_male_prefecture_rate_array,
                      smooth_data = CAN_male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05)
    CAN_NS_int_score_male_array_ets_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

CAN_NS_int_score_male_array_ets_alpha_0.05_mean = t(apply(CAN_NS_int_score_male_array_ets_alpha_0.05, c(2, 3), mean))

# save results

save(list = ls(pattern = "^CAN_NS_int_score_.*_alpha_0.05"), file = "results/CAN_NS_95.RData")
