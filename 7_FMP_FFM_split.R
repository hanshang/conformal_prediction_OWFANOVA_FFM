source("FFM.R")
source("choice_K.R")
source("interval_score.R")

# data_set_array: n_age x n_state x n_year
# fh: a forecast horizon
# fmethod: forecasting method, ets or auto.arima

FMP_FFM_fun <- function(data_set_array, fh, fmethod)
{
    n_age = dim(data_set_array)[1]
    n_state = dim(data_set_array)[2]
    n_year = dim(data_set_array)[3]
    
    data_set_mat = NULL
    for(ik in 1:n_state)
    {
        data_set_mat = cbind(data_set_mat, data_set_array[,ik,])
        rm(ik)
    }
    
    One_way_FMP_smooth <- One_way_median_polish(Y = t(data_set_mat), n_prefecture = n_state, year = 1:n_year, age = 1:n_age)
    One_way_FMP_smooth_grand <- One_way_FMP_smooth$grand_effect
    One_way_FMP_smooth_row <- One_way_FMP_smooth$row_effect
    
    One_way_FMP_smooth_resid <- One_way_Residuals(Y = t(data_set_mat), n_prefecture = n_state, year = 1:n_year, age = 1:n_age)
    One_way_FMP_smooth_resid_array = array(NA, dim = c(n_age, n_year, n_state), dimnames = list(1:n_age, 1:n_year, 1:n_state))
    for(iw in 1:n_state)
    {
        One_way_FMP_smooth_resid_array[,,iw] = t(One_way_FMP_smooth_resid)[,((iw-1)*n_year+1):(iw*n_year)]
        rm(iw)
    }
    
    HDFTS_factor_decomp_female <- HDFTS_factor_decomp(data = aperm(One_way_FMP_smooth_resid_array, c(2, 3, 1)))
    HDFTS_factor_decomp_female_factor_loading <- HDFTS_factor_decomp_female$factor_loading
    HDFTS_factor_decomp_female_factors <- HDFTS_factor_decomp_female$factors
    HDFTS_factor_decomp_female_residuals <- HDFTS_factor_decomp_female$error
  
    # forecasting
    
    n_factor <- ncol(HDFTS_factor_decomp_female_factors)
    HDFTS_factor_decomp_female_factors_forecast = matrix(NA, 1, n_factor)
    for(ik in 1:n_factor)
    {
        if(fmethod == "ets")
        {
            HDFTS_factor_decomp_female_factors_forecast[,ik] = forecast(ets(HDFTS_factor_decomp_female_factors[,ik]), h = fh)$mean[fh]
        }
        else if(fmethod == "arima")
        {
            HDFTS_factor_decomp_female_factors_forecast[,ik] = forecast(auto.arima(HDFTS_factor_decomp_female_factors[,ik]), h = fh)$mean[fh]
        }
        rm(ik)
    }
    
    HDFTS_factor_decomp_female_forecast <- matrix(NA, n_age, n_state)
    for(ik in 1:n_state)
    {
        HDFTS_factor_decomp_female_forecast[,ik] = HDFTS_factor_decomp_female_factors_forecast %*% HDFTS_factor_decomp_female_factor_loading[,,ik]
        rm(ik)
    }
    FMP_FFM_forecast = HDFTS_factor_decomp_female_forecast + matrix(rep(One_way_FMP_smooth_grand, n_state), n_age, n_state) + t(One_way_FMP_smooth_row)
    colnames(FMP_FFM_forecast) = 1:n_state
    rownames(FMP_FFM_forecast) = 0:(n_age - 1)
    return(FMP_FFM_forecast)
}

female_prefecture_rate_array_smooth_FMP_FFM <- FMP_FFM_fun(data_set_array = log(female_prefecture_rate_array_smooth), fh = 10, fmethod = "ets")
male_prefecture_rate_array_smooth_FMP_FFM   <- FMP_FFM_fun(data_set_array = log(male_prefecture_rate_array_smooth), fh = 10, fmethod = "ets")

##############
# calibration
##############

tune_para_select <- function(resi, tune_para, nominal, resi_sd)
{
    n_age = length(resi_sd)
    ind = matrix(NA, n_age, ncol(resi))
    for(ijk in 1:ncol(resi))
    {
        ind[,ijk] = ifelse(between(x = resi[,ijk], left = -tune_para * resi_sd, right = tune_para * resi_sd), 1, 0)
        rm(ijk)
    }
    return(abs(sum(ind)/(n_age * ncol(resi)) - nominal))
}

# training_data: 1:28
# validation_data: 29:39
# test_data: 40:49

# raw_data: mortality rate in the original scale
# smooth_data: smoothed mortality rate
# fh: forecast horizon
# fore_method: forecasting method, ets or auto.arima
# alpha: level of significance

FMP_FFM_fun_int_eval <- function(raw_data, smooth_data, fh, fore_method, alpha)
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
    FMP_FFM_err <- validation_set - point_forecast
    
    # compute pointwise sd
    
    FMP_FFM_quantile = FMP_FFM_sd = matrix(NA, n_age, n_state)
    for(iw in 1:n_state)
    {
        FMP_FFM_quantile[,iw] = apply(abs(FMP_FFM_err[,iw,]), 1, quantile, (1 - alpha))
        FMP_FFM_sd[,iw] = apply(FMP_FFM_err[,iw,], 1, sd)
        rm(iw)
    }
    colnames(FMP_FFM_quantile) = colnames(FMP_FFM_sd) = 1:n_state
    rownames(FMP_FFM_quantile) = rownames(FMP_FFM_sd) = 1:n_age
    
    # try several optimisers
    
    tune_para_value = vector("numeric", n_state)
    for(iw in 1:n_state)
    {
        obj_BFGS = optim(par = 1, fn = tune_para_select, method = "BFGS", resi = FMP_FFM_err[,iw,], 
                         nominal = 1 - alpha, resi_sd = FMP_FFM_sd[,iw])
        obj_NM = optim(par = 1, fn = tune_para_select, method = "Nelder-Mead", resi = FMP_FFM_err[,iw,], 
                       nominal = 1 - alpha, resi_sd = FMP_FFM_sd[,iw])
        obj_Brent = optim(par = 1, fn = tune_para_select, method = "Brent", resi = FMP_FFM_err[,iw,], 
                          nominal = 1 - alpha, lower = 0, upper = 10, resi_sd = FMP_FFM_sd[,iw])
        tune_para_obj = min(c(obj_BFGS$value, obj_NM$value, obj_Brent$value))
        tune_para_value[iw] = c(obj_BFGS$par, obj_NM$par, obj_Brent$par)[which.min(c(obj_BFGS$value, obj_NM$value, obj_Brent$value))]
        rm(iw)
    }
    rm(point_forecast); rm(validation_set); rm(FMP_FFM_err)
    
    # forecasting set
    
    smooth_FMP_FFM_mat_fore = array(NA, dim = c(n_age, n_state, (11 - fh)),
                                        dimnames = list(1:n_age, 1:n_state, 1:(11 - fh)))
    for(iw in 1:(11 - fh))
    {
        smooth_FMP_FFM_mat_fore[,,iw] <- FMP_FFM_fun(data_set_array = log(smooth_data[,,1:((n_year - 11) + iw)]), fh = fh, fmethod = fore_method)
        rm(iw)
    }
    
    point_forecast <- exp(smooth_FMP_FFM_mat_fore)
    lb_array_sd = ub_array_sd = lb_array_quantile = ub_array_quantile = array(NA, dim = c(n_age, (11 - fh), n_state), dimnames = list(1:n_age, 1:(11 - fh), 1:n_state))
    int_score_sd = int_score_quantile = matrix(NA, n_state, 3)
    for(iw in 1:n_state)
    {
        lb_array_sd[,,iw] = point_forecast[,iw,] - tune_para_value[iw] * FMP_FFM_sd[,iw]
        ub_array_sd[,,iw] = point_forecast[,iw,] + tune_para_value[iw] * FMP_FFM_sd[,iw]

        lb_array_quantile[,,iw] = point_forecast[,iw,] - FMP_FFM_quantile[,iw]
        ub_array_quantile[,,iw] = point_forecast[,iw,] + FMP_FFM_quantile[,iw]
        
        int_score_sd[iw,] = interval_score(holdout = raw_data[,iw,(n_year - (10 - fh)):n_year], lb = lb_array_sd[,,iw],
                                           ub = ub_array_sd[,,iw], alpha = alpha)
        int_score_quantile[iw,] = interval_score(holdout = raw_data[,iw,(n_year - (10 - fh)):n_year], lb = lb_array_quantile[,,iw],
                                           ub = ub_array_quantile[,,iw], alpha = alpha)
        rm(iw)
    }
    rownames(int_score_sd) = rownames(int_score_quantile) = 1:n_state
    colnames(int_score_sd) = colnames(int_score_quantile) = c("ECP", "CPD", "score")
    return(list(int_score_sd = int_score_sd, int_score_quantile = int_score_quantile))
}

################
### alpha = 0.2
################

## female

# arima

FMP_FFM_fun_int_score_female_array_arima_sd = FMP_FFM_fun_int_score_female_array_arima_quantile = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = female_prefecture_rate_array, 
                               smooth_data = female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.2)
    FMP_FFM_fun_int_score_female_array_arima_sd[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_female_array_arima_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_female_array_arima_sd_mean = t(apply(FMP_FFM_fun_int_score_female_array_arima_sd, c(2, 3), mean))
FMP_FFM_fun_int_score_female_array_arima_quantile_mean = t(apply(FMP_FFM_fun_int_score_female_array_arima_quantile, c(2, 3), mean))

# ets

FMP_FFM_fun_int_score_female_array_ets_sd = FMP_FFM_fun_int_score_female_array_ets_quantile = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = female_prefecture_rate_array, 
                                smooth_data = female_prefecture_rate_array_smooth, 
                                fh = iw, fore_method = "ets", alpha = 0.2)
    FMP_FFM_fun_int_score_female_array_ets_sd[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_female_array_ets_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_female_array_ets_sd_mean = t(apply(FMP_FFM_fun_int_score_female_array_ets_sd, c(2, 3), mean))
FMP_FFM_fun_int_score_female_array_ets_quantile_mean = t(apply(FMP_FFM_fun_int_score_female_array_ets_quantile, c(2, 3), mean))
        
## male

# arima

FMP_FFM_fun_int_score_male_array_arima_sd = FMP_FFM_fun_int_score_male_array_arima_quantile = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = male_prefecture_rate_array, 
                               smooth_data = male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.2)
    FMP_FFM_fun_int_score_male_array_arima_sd[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_male_array_arima_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_male_array_arima_sd_mean = t(apply(FMP_FFM_fun_int_score_male_array_arima_sd, c(2, 3), mean))
FMP_FFM_fun_int_score_male_array_arima_quantile_mean = t(apply(FMP_FFM_fun_int_score_male_array_arima_quantile, c(2, 3), mean))

# ets

FMP_FFM_fun_int_score_male_array_ets_sd = FMP_FFM_fun_int_score_male_array_ets_quantile = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = male_prefecture_rate_array, 
                               smooth_data = male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.2)
    FMP_FFM_fun_int_score_male_array_ets_sd[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_male_array_ets_quantile[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_male_array_ets_sd_mean = t(apply(FMP_FFM_fun_int_score_male_array_ets_sd, c(2, 3), mean))
FMP_FFM_fun_int_score_male_array_ets_quantile_mean = t(apply(FMP_FFM_fun_int_score_male_array_ets_quantile, c(2, 3), mean))

#################
### alpha = 0.05
#################

## female

# arima

FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05 = FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = female_prefecture_rate_array, 
                               smooth_data = female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.05)
    FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05, c(2, 3), mean))
FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05, c(2, 3), mean))

# ets

FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05 = FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = female_prefecture_rate_array, 
                               smooth_data = female_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.05)
    FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05, c(2, 3), mean))
FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05, c(2, 3), mean))

## male

# arima

FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05 = FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = male_prefecture_rate_array, 
                               smooth_data = male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "arima", alpha = 0.05)
    FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05, c(2, 3), mean))
FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05, c(2, 3), mean))

# ets

FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05 = FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05 = array(NA, dim = c(47, 3, 10), dimnames = list(1:47, c("ECP", "CPD", "score"), 1:10))
for(iw in 1:10)
{
    dum = FMP_FFM_fun_int_eval(raw_data = male_prefecture_rate_array, 
                               smooth_data = male_prefecture_rate_array_smooth, 
                               fh = iw, fore_method = "ets", alpha = 0.05)
    FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05[,,iw] = dum$int_score_sd
    FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05[,,iw] = dum$int_score_quantile
    print(iw); rm(iw); rm(dum)
}

FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05, c(2, 3), mean))
FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05_mean = t(apply(FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05, c(2, 3), mean))

#################
## female & male
#################

# arima

xtable(rbind(cbind(FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05_mean,
                   FMP_FFM_fun_int_score_female_array_arima_quantile_alpha_0.05_mean,
                   sequential_int_score_list_female_arima_PI_95_mat),

             cbind(FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05_mean,
                   FMP_FFM_fun_int_score_male_array_arima_quantile_alpha_0.05_mean,
                   sequential_int_score_list_male_arima_PI_95_mat)), digits = 3)

# ets

xtable(rbind(cbind(FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05_mean,
                   FMP_FFM_fun_int_score_female_array_ets_quantile_alpha_0.05_mean,
                   sequential_int_score_list_female_ets_PI_95_mat),
             
             cbind(FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05_mean,
                   FMP_FFM_fun_int_score_male_array_ets_quantile_alpha_0.05_mean,
                   sequential_int_score_list_male_ets_PI_95_mat)), digits = 3)

##########
# boxplot
##########

### ECP

## arima

# female

savefig("Fig_2a", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05[,1,],
        names = paste0("", 1:10),
        xlab = "",
        ylab = "ECP",
        main = "Split conformal prediction (sd)")
abline(h = 0.95, lty = 2)
dev.off()

savefig("Fig_2b", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(sequential_int_score_list_female_arima_PI_95_value[,1,],
        names = paste0("", 1:10),
        xlab = "",
        ylab = "",
        main = "Sequential conformal prediction")
abline(h = 0.95, lty = 2)
dev.off()

# male

savefig("Fig_2c", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05[,1,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "ECP",
        main = "")
abline(h = 0.95, lty = 2)
dev.off()

savefig("Fig_2d", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(sequential_int_score_list_male_arima_PI_95_value[,1,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "",
        main = "")
abline(h = 0.95, lty = 2)
dev.off()

## ets

# female

boxplot(FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05[,1,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "ECP",
        main = "Split conformal prediction (sd)")
abline(h = 0.95, lty = 2)

boxplot(sequential_int_score_list_female_ets_PI_95_value[,1,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "ECP",
        main = "Sequential conformal prediction")
abline(h = 0.95, lty = 2)

# male

boxplot(FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05[,1,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "ECP",
        main = "")
abline(h = 0.95, lty = 2)

boxplot(sequential_int_score_list_male_ets_PI_95_value[,1,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "ECP",
        main = "")
abline(h = 0.95, lty = 2)

### score

## arima

# female

savefig("Fig_3a", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05[,3,],
        names = paste0("", 1:10),
        xlab = "",
        ylab = "Mean interval score",
        main = "Split conformal prediction (sd)", ylim = c(0, 0.41))
dev.off()

savefig("Fig_3b", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(sequential_int_score_list_female_arima_PI_95_value[,3,],
        names = paste0("", 1:10),
        xlab = "",
        ylab = "",
        main = "Sequential conformal prediction")
dev.off()

# male

savefig("Fig_3c", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05[,3,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "Mean interval score",
        main = "", ylim = c(0, 0.31))
dev.off()

savefig("Fig_3d", width = 12, height = 10, toplines = 0.8, type = "png")
boxplot(sequential_int_score_list_male_arima_PI_95_value[,3,],
        names = paste0("", 1:10),
        xlab = "Horizon",
        ylab = "",
        main = "")
dev.off()

