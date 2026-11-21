# the learning rate eta is searched in [eta_lower, eta_upper]

eta_lower = 0
eta_upper = 0.15

# sorted_score: sorted non-conformity scores
# alpha: level of significance
# k > n: use the largest score; k < 1: use -0.5, a band of zero width

conformal_quantile <- function(sorted_score, alpha)
{
    n_score = length(sorted_score)
    k = ceiling((n_score + 1) * (1 - alpha))
    if(k < 1)
    {
        return(-0.5)
    }
    if(k > n_score)
    {
        return(sorted_score[n_score])
    }
    return(sorted_score[k])
}

# adaptive conformal prediction for the forecasts of one prefecture
# point_forecast, holdout: n_age x n_fore
# fh: forecast horizon
# AC_gamma: pointwise sd of the calibration errors
# sorted_score: sorted calibration scores
# alpha: level of significance
# eta: learning rate
# a_start: value of a before the first forecast

AC_run <- function(point_forecast, holdout, fh, AC_gamma, sorted_score, alpha, eta, a_start)
{
    n_age = nrow(point_forecast)
    n_fore = ncol(point_forecast)
    lb = ub = matrix(NA, n_age, n_fore)
    err = vector("numeric", n_fore)
    a = a_start
    for(iw in 1:n_fore)
    {
        # update a with the err of the forecast made fh years ago

        if(iw > fh)
        {
            a = a + eta * (alpha - err[iw - fh])
        }

        # band and its share of ages outside the band

        Q = conformal_quantile(sorted_score = sorted_score, alpha = a)
        AC_L = point_forecast[,iw] - AC_gamma
        AC_U = point_forecast[,iw] + AC_gamma
        AC_W = AC_U - AC_L
        lb[,iw] = AC_L - Q * AC_W
        ub[,iw] = AC_U + Q * AC_W
        err[iw] = mean(holdout[,iw] < lb[,iw] | holdout[,iw] > ub[,iw])
        rm(iw); rm(Q); rm(AC_L); rm(AC_U); rm(AC_W)
    }

    # update a with the err of the last fh forecasts

    for(iw in (n_fore - fh + 1):n_fore)
    {
        a = a + eta * (alpha - err[iw])
        rm(iw)
    }
    return(list(lb = lb, ub = ub, err = err, a_end = a))
}

# raw_data: mortality rate in the original scale
# smooth_data: smoothed mortality rate
# fh: forecast horizon (1, ..., 5)
# fore_method: forecasting method, ets or auto.arima
# alpha: level of significance
# eta_lower, eta_upper: eta is searched in [eta_lower, eta_upper]

AC_int_eval <- function(raw_data, smooth_data, fh, fore_method, alpha, eta_lower, eta_upper)
{
    n_age = dim(raw_data)[1]
    n_state = dim(raw_data)[2]
    n_year = dim(raw_data)[3]

    # calibration set

    smooth_FMP_FFM_mat_calibration = array(NA, dim = c(n_age, n_state, (12 - fh)),
                                           dimnames = list(1:n_age, 1:n_state, 1:(12 - fh)))
    for(iw in 1:(12 - fh))
    {
        smooth_FMP_FFM_mat_calibration[,,iw] <- FMP_FFM_fun(data_set_array = log(smooth_data[,,1:((n_year - 33) + iw)]), fh = fh, fmethod = fore_method)
        rm(iw)
    }
    point_forecast_calibration <- exp(smooth_FMP_FFM_mat_calibration)
    calibration_set <- array(raw_data[,,(n_year - (32 - fh)):(n_year - 21)], dim = c(n_age, n_state, (12 - fh)))
    AC_err <- calibration_set - point_forecast_calibration

    # validation set

    smooth_FMP_FFM_mat = array(NA, dim = c(n_age, n_state, (12 - fh)),
                               dimnames = list(1:n_age, 1:n_state, 1:(12 - fh)))
    for(iw in 1:(12 - fh))
    {
        smooth_FMP_FFM_mat[,,iw] <- FMP_FFM_fun(data_set_array = log(smooth_data[,,1:((n_year - 22) + iw)]), fh = fh, fmethod = fore_method)
        rm(iw)
    }
    point_forecast <- exp(smooth_FMP_FFM_mat)
    validation_set <- array(raw_data[,,(n_year - (21 - fh)):(n_year - 10)], dim = c(n_age, n_state, (12 - fh)))

    # forecasting set

    smooth_FMP_FFM_mat_fore = array(NA, dim = c(n_age, n_state, (11 - fh)),
                                    dimnames = list(1:n_age, 1:n_state, 1:(11 - fh)))
    for(iw in 1:(11 - fh))
    {
        smooth_FMP_FFM_mat_fore[,,iw] <- FMP_FFM_fun(data_set_array = log(smooth_data[,,1:((n_year - 11) + iw)]), fh = fh, fmethod = fore_method)
        rm(iw)
    }
    point_forecast_fore <- exp(smooth_FMP_FFM_mat_fore)

    # validation forecasts that depend on eta

    eta_affected = (fh + 1):(12 - fh)

    AC_eta = CPD_tuning = vector("numeric", n_state)
    lb_array_AC = ub_array_AC = array(NA, dim = c(n_age, (11 - fh), n_state), dimnames = list(1:n_age, 1:(11 - fh), 1:n_state))
    int_score_AC = matrix(NA, n_state, 3)
    for(iw in 1:n_state)
    {
        # calibration set: gamma, initial band and scores

        AC_gamma = apply(AC_err[,iw,], 1, sd)
        AC_L = point_forecast_calibration[,iw,] - AC_gamma
        AC_U = point_forecast_calibration[,iw,] + AC_gamma
        AC_W = AC_U - AC_L
        sorted_score = sort(as.vector(pmax(AC_L - calibration_set[,iw,], calibration_set[,iw,] - AC_U) / AC_W))
        rm(AC_L); rm(AC_U); rm(AC_W)

        # validation set: CPD as a function of eta

        CPD_objective <- function(eta)
        {
            AC_validation = AC_run(point_forecast = point_forecast[,iw,], holdout = validation_set[,iw,], fh = fh,
                                   AC_gamma = AC_gamma, sorted_score = sorted_score, alpha = alpha, eta = eta, a_start = alpha)
            tuning_eval = interval_score(holdout = validation_set[,iw,eta_affected], lb = AC_validation$lb[,eta_affected],
                                         ub = AC_validation$ub[,eta_affected], alpha = alpha)
            return(tuning_eval[2])
        }

        # choose eta

        eta_opt = optimize(f = CPD_objective, lower = eta_lower, upper = eta_upper)
        AC_eta[iw] = eta_opt$minimum
        CPD_tuning[iw] = eta_opt$objective

        # forecasting set: start from a at the end of the validation set

        AC_validation = AC_run(point_forecast = point_forecast[,iw,], holdout = validation_set[,iw,], fh = fh,
                               AC_gamma = AC_gamma, sorted_score = sorted_score, alpha = alpha, eta = AC_eta[iw], a_start = alpha)
        AC_fore = AC_run(point_forecast = point_forecast_fore[,iw,], holdout = raw_data[,iw,(n_year - (10 - fh)):n_year], fh = fh,
                         AC_gamma = AC_gamma, sorted_score = sorted_score, alpha = alpha, eta = AC_eta[iw], a_start = AC_validation$a_end)
        lb_array_AC[,,iw] = AC_fore$lb
        ub_array_AC[,,iw] = AC_fore$ub

        # compute interval score

        int_score_AC[iw,] = interval_score(holdout = raw_data[,iw,(n_year - (10 - fh)):n_year], lb = lb_array_AC[,,iw],
                                           ub = ub_array_AC[,,iw], alpha = alpha)
        rm(iw); rm(AC_gamma); rm(sorted_score); rm(CPD_objective); rm(eta_opt); rm(AC_validation); rm(AC_fore)
    }
    rownames(int_score_AC) = names(AC_eta) = 1:n_state
    colnames(int_score_AC) = c("ECP", "CPD", "score")
    return(list(int_score_AC = int_score_AC, AC_eta = AC_eta, CPD_tuning = CPD_tuning,
                lb_array_AC = lb_array_AC, ub_array_AC = ub_array_AC))
}

#################
### alpha = 0.05
#################

## female

# arima

AC_int_score_female_array_arima_alpha_0.05 = array(NA, dim = c(47, 3, 5), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:5))
AC_eta_female_arima_alpha_0.05 = matrix(NA, 47, 5, dimnames = list(1:47, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = female_prefecture_rate_array,
                      smooth_data = female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    AC_int_score_female_array_arima_alpha_0.05[,,iw] = dum$int_score_AC
    AC_eta_female_arima_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

AC_int_score_female_array_arima_alpha_0.05_mean = t(apply(AC_int_score_female_array_arima_alpha_0.05, c(2, 3), mean))

# ets

AC_int_score_female_array_ets_alpha_0.05 = array(NA, dim = c(47, 3, 5), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:5))
AC_eta_female_ets_alpha_0.05 = matrix(NA, 47, 5, dimnames = list(1:47, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = female_prefecture_rate_array,
                      smooth_data = female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    AC_int_score_female_array_ets_alpha_0.05[,,iw] = dum$int_score_AC
    AC_eta_female_ets_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

AC_int_score_female_array_ets_alpha_0.05_mean = t(apply(AC_int_score_female_array_ets_alpha_0.05, c(2, 3), mean))

## male

# arima

AC_int_score_male_array_arima_alpha_0.05 = array(NA, dim = c(47, 3, 5), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:5))
AC_eta_male_arima_alpha_0.05 = matrix(NA, 47, 5, dimnames = list(1:47, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = male_prefecture_rate_array,
                      smooth_data = male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    AC_int_score_male_array_arima_alpha_0.05[,,iw] = dum$int_score_AC
    AC_eta_male_arima_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

AC_int_score_male_array_arima_alpha_0.05_mean = t(apply(AC_int_score_male_array_arima_alpha_0.05, c(2, 3), mean))

# ets

AC_int_score_male_array_ets_alpha_0.05 = array(NA, dim = c(47, 3, 5), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:5))
AC_eta_male_ets_alpha_0.05 = matrix(NA, 47, 5, dimnames = list(1:47, 1:5))
for(iw in 1:5)
{
    dum = AC_int_eval(raw_data = male_prefecture_rate_array,
                      smooth_data = male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05, eta_lower = eta_lower, eta_upper = eta_upper)
    AC_int_score_male_array_ets_alpha_0.05[,,iw] = dum$int_score_AC
    AC_eta_male_ets_alpha_0.05[,iw] = dum$AC_eta
    print(iw); rm(iw); rm(dum)
}

AC_int_score_male_array_ets_alpha_0.05_mean = t(apply(AC_int_score_male_array_ets_alpha_0.05, c(2, 3), mean))

# save results

save(list = ls(pattern = "^AC_(int_score|eta)_.*_alpha_0.05"), file = "results/JPN_AC_95.RData")
