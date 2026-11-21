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

# raw_data: mortality rate in the original scale
# smooth_data: smoothed mortality rate
# fh: forecast horizon
# fore_method: forecasting method, ets or auto.arima
# alpha: level of significance

NS_int_eval <- function(raw_data, smooth_data, fh, fore_method, alpha)
{
    n_age = dim(raw_data)[1]
    n_state = dim(raw_data)[2]
    n_year = dim(raw_data)[3]

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
    NS_err <- validation_set - point_forecast

    # gamma, initial band and quantile Q

    NS_gamma = matrix(NA, n_age, n_state)
    NS_q = vector("numeric", n_state)
    for(iw in 1:n_state)
    {
        NS_gamma[,iw] = apply(NS_err[,iw,], 1, sd)
        NS_L = point_forecast[,iw,] - NS_gamma[,iw]
        NS_U = point_forecast[,iw,] + NS_gamma[,iw]
        NS_W = NS_U - NS_L
        NS_score = sort(as.vector(pmax(NS_L - validation_set[,iw,], validation_set[,iw,] - NS_U) / NS_W))
        NS_q[iw] = conformal_quantile(sorted_score = NS_score, alpha = alpha)
        rm(iw); rm(NS_L); rm(NS_U); rm(NS_W); rm(NS_score)
    }
    rm(point_forecast); rm(validation_set); rm(NS_err)

    # forecasting set

    smooth_FMP_FFM_mat_fore = array(NA, dim = c(n_age, n_state, (11 - fh)),
                                    dimnames = list(1:n_age, 1:n_state, 1:(11 - fh)))
    for(iw in 1:(11 - fh))
    {
        smooth_FMP_FFM_mat_fore[,,iw] <- FMP_FFM_fun(data_set_array = log(smooth_data[,,1:((n_year - 11) + iw)]), fh = fh, fmethod = fore_method)
        rm(iw)
    }

    point_forecast <- exp(smooth_FMP_FFM_mat_fore)
    lb_array_NS = ub_array_NS = array(NA, dim = c(n_age, (11 - fh), n_state), dimnames = list(1:n_age, 1:(11 - fh), 1:n_state))
    int_score_NS = matrix(NA, n_state, 3)
    for(iw in 1:n_state)
    {
        # initial band widened by Q times its width

        NS_L = point_forecast[,iw,] - NS_gamma[,iw]
        NS_U = point_forecast[,iw,] + NS_gamma[,iw]
        NS_W = NS_U - NS_L
        lb_array_NS[,,iw] = NS_L - NS_q[iw] * NS_W
        ub_array_NS[,,iw] = NS_U + NS_q[iw] * NS_W

        # compute interval score

        int_score_NS[iw,] = interval_score(holdout = raw_data[,iw,(n_year - (10 - fh)):n_year], lb = lb_array_NS[,,iw],
                                           ub = ub_array_NS[,,iw], alpha = alpha)
        rm(iw); rm(NS_L); rm(NS_U); rm(NS_W)
    }
    rownames(int_score_NS) = 1:n_state
    colnames(int_score_NS) = c("ECP", "CPD", "score")
    return(list(int_score_NS = int_score_NS, NS_q = NS_q, lb_array_NS = lb_array_NS, ub_array_NS = ub_array_NS))
}

#################
### alpha = 0.05
#################

## female

# arima

NS_int_score_female_array_arima_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = female_prefecture_rate_array,
                      smooth_data = female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05)
    NS_int_score_female_array_arima_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

NS_int_score_female_array_arima_alpha_0.05_mean = t(apply(NS_int_score_female_array_arima_alpha_0.05, c(2, 3), mean))

# ets

NS_int_score_female_array_ets_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = female_prefecture_rate_array,
                      smooth_data = female_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05)
    NS_int_score_female_array_ets_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

NS_int_score_female_array_ets_alpha_0.05_mean = t(apply(NS_int_score_female_array_ets_alpha_0.05, c(2, 3), mean))

## male

# arima

NS_int_score_male_array_arima_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = male_prefecture_rate_array,
                      smooth_data = male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "arima", alpha = 0.05)
    NS_int_score_male_array_arima_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

NS_int_score_male_array_arima_alpha_0.05_mean = t(apply(NS_int_score_male_array_arima_alpha_0.05, c(2, 3), mean))

# ets

NS_int_score_male_array_ets_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = NS_int_eval(raw_data = male_prefecture_rate_array,
                      smooth_data = male_prefecture_rate_array_smooth,
                      fh = iw, fore_method = "ets", alpha = 0.05)
    NS_int_score_male_array_ets_alpha_0.05[,,iw] = dum$int_score_NS
    print(iw); rm(iw); rm(dum)
}

NS_int_score_male_array_ets_alpha_0.05_mean = t(apply(NS_int_score_male_array_ets_alpha_0.05, c(2, 3), mean))

# save results

save(list = ls(pattern = "^NS_int_score_.*_alpha_0.05"), file = "results/JPN_NS_95.RData")
