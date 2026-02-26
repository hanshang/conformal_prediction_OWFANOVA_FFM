# conformal_prediction_OWFANOVA_FFM

## Features
- High-dimensional functional time series
- Split conformal prediction method
- Sequential conformal prediction method

1. load_packages.R: required R packages
2. read_data.R: read Japanese subnational age- and sex-specific log mortality rates
3. interval_score.R: internal function for computing coverage probability difference and mean interval score
4. choice_K.R: way for selecting the number of functional principal components
5. One_way_FMP.R: One way functional analysis of variance based on median polish
6. FFM.R: functional factor model
7. FMP_FFM_split.R: split conformal prediction using one-way FANOVA and functional factor model for the Japanese data
8. FMP_FFM_sequential.R: sequential conformal prediction using one-way FANOVA and functional factor model for the Japanese data
9. CAN_read_data.R: read Canadian subnational age- and sex-specific log mortality rates
10. CAN_FANOVA_FFM_split.R: split conformal prediction using one-way FANOVA and functional factor model for the Canadian data
11. CAN_FANOVA_FFM_sequential.R: sequential conformal prediction using one-way FANOVA and functional factor model for the Canadian data
