# set parameters

ages = 0:95
n_age = length(ages)
years = 1975:2023
n_year = length(years)

female_prefecture_rate_combo_smooth_log <- log(female_prefecture_rate_combo_smooth)

One_way_FMP_smooth <- One_way_median_polish(Y = t(female_prefecture_rate_combo_smooth_log), n_prefecture = 47, year = 1975:2023, age = 0:95)
One_way_FMP_smooth_grand <- One_way_FMP_smooth$grand_effect
One_way_FMP_smooth_row <- One_way_FMP_smooth$row_effect

# graphical display

plot(0:95, One_way_FMP_smooth_grand, type = "l", xlab = "Age", ylab = "Grand effect")
plot(fts(0:95, t(One_way_FMP_smooth_row)), xlab = "Age", ylab = "Row effect")

# reconstruction

female_prefecture_rate_array_smooth_resid <- array(NA, dim = c(96, 47, 49), dimnames = list(0:95, 1:47, 1975:2023))
for(iw in 1:49)
{
    female_prefecture_rate_array_smooth_resid[,,iw] = log(female_prefecture_rate_array_smooth[,,iw]) - (matrix(rep(One_way_FMP_smooth_grand, 47), 96, 47) + t(One_way_FMP_smooth_row))
    rm(iw)
}

# double check

One_way_FMP_smooth_resid <- One_way_Residuals(Y = t(female_prefecture_rate_combo_smooth_log), n_prefecture = 47, year = 1975:2023, age = 0:95)
One_way_FMP_smooth_resid_array = array(NA, dim = c(96, 49, 47), dimnames = list(0:95, years, 1:47))
for(iw in 1:47)
{
    One_way_FMP_smooth_resid_array[,,iw] = t(One_way_FMP_smooth_resid)[,((iw-1)*n_year+1):(iw*n_year)]
    rm(iw)
}

all(female_prefecture_rate_array_smooth_resid == aperm(One_way_FMP_smooth_resid_array, c(1, 3, 2)))

