# the learning rate eta is searched in [eta_lower, eta_upper]

eta_lower = 0
eta_upper = 0.15

#################
### alpha = 0.05
#################

## female

# arima

CAN_AC_int_score_female_array_arima_alpha_0.05 = array(NA, dim = c(n_states, 3, 5), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:5))
CAN_AC_eta_female_arima_alpha_0.05 = matrix(NA, n_states, 5, dimnames = list(1:n_states, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = CAN_female_prefecture_rate_array,
                      smooth_data = CAN_female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    CAN_AC_int_score_female_array_arima_alpha_0.05[,,iw] = dum$int_score_AC
    CAN_AC_eta_female_arima_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

CAN_AC_int_score_female_array_arima_alpha_0.05_mean = t(apply(CAN_AC_int_score_female_array_arima_alpha_0.05, c(2, 3), mean))

# ets

CAN_AC_int_score_female_array_ets_alpha_0.05 = array(NA, dim = c(n_states, 3, 5), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:5))
CAN_AC_eta_female_ets_alpha_0.05 = matrix(NA, n_states, 5, dimnames = list(1:n_states, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = CAN_female_prefecture_rate_array,
                      smooth_data = CAN_female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    CAN_AC_int_score_female_array_ets_alpha_0.05[,,iw] = dum$int_score_AC
    CAN_AC_eta_female_ets_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

CAN_AC_int_score_female_array_ets_alpha_0.05_mean = t(apply(CAN_AC_int_score_female_array_ets_alpha_0.05, c(2, 3), mean))

## male

# arima

CAN_AC_int_score_male_array_arima_alpha_0.05 = array(NA, dim = c(n_states, 3, 5), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:5))
CAN_AC_eta_male_arima_alpha_0.05 = matrix(NA, n_states, 5, dimnames = list(1:n_states, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = CAN_male_prefecture_rate_array,
                      smooth_data = CAN_male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    CAN_AC_int_score_male_array_arima_alpha_0.05[,,iw] = dum$int_score_AC
    CAN_AC_eta_male_arima_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

CAN_AC_int_score_male_array_arima_alpha_0.05_mean = t(apply(CAN_AC_int_score_male_array_arima_alpha_0.05, c(2, 3), mean))

# ets

CAN_AC_int_score_male_array_ets_alpha_0.05 = array(NA, dim = c(n_states, 3, 5), dimnames = list(1:n_states, c("ECP", "CPD", "score"), 1:5))
CAN_AC_eta_male_ets_alpha_0.05 = matrix(NA, n_states, 5, dimnames = list(1:n_states, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = CAN_male_prefecture_rate_array,
                      smooth_data = CAN_male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    CAN_AC_int_score_male_array_ets_alpha_0.05[,,iw] = dum$int_score_AC
    CAN_AC_eta_male_ets_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

CAN_AC_int_score_male_array_ets_alpha_0.05_mean = t(apply(CAN_AC_int_score_male_array_ets_alpha_0.05, c(2, 3), mean))

# save results

save(list = ls(pattern = "^CAN_AC_(int_score|eta)_.*_alpha_0.05"), file = "results/CAN_AC_95.RData")
