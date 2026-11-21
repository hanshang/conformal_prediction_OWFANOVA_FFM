<br />
<div align="center">
  <img src="Macquarie_University_logo.png" alt="Macquarie University" width="161">

<h3 align="center">Conformal Prediction for High-Dimensional Functional Time Series: Applications to Subnational Mortality</h3>

  <p align="center">
    Han Lin Shang · Chang Liu
    <br />
    Department of Actuarial Studies and Business Analytics, Macquarie University
  </p>
</div>

---

# Abstract

In statistics, forecast uncertainty is often quantified using a specified statistical model, though such approaches may be vulnerable to model misspecification, selection bias, and limited finite-sample validity. While bootstrap procedures may alleviate some of these concerns, their validity is not guaranteed and they are often computationally demanding. Instead, we take a model-agnostic and distribution-free approach, namely conformal prediction, to construct pointwise prediction intervals in high-dimensional functional time series.

Among a rich family of conformal prediction methods, we study split, non-conformity score, sequential predictive, and adaptive conformal prediction. In split and non-conformity score conformal prediction, the data are divided into training, validation, and test sets, and the interval width is calibrated on the validation set so that the empirical coverage probability matches the nominal level, by tuning a coefficient of the residual standard deviation and by taking an empirical quantile of the non-conformity scores, respectively. Sequential predictive conformal prediction removes the need for a validation set by updating predictive quantiles sequentially, whereas adaptive conformal prediction updates the significance level over time according to the coverage of past intervals.

Using subnational age- and sex-specific log mortality data from Japan and Canada, we compare the finite-sample forecast performance of these methods using the empirical coverage probability and the mean interval score.

---

# Software

All results were obtained with R version 4.5.2.

---

# 1. Data

| File | Description |
|---|---|
| `data/JMD_005_001` | Raw Japanese Mortality Database files (`Mx_1x1.txt`, `Exposures_1x1.txt`) for the 47 prefectures, one folder per prefecture |
| `data/CHMD_Canada` | Raw Canadian Human Mortality Database files (death rates and exposures) for the 12 provinces and territories |
| `data/JPN_data.RData` | Raw and smoothed Japanese mortality rates, 1975–2023, ages 0–94 and 95+ |
| `data/CAN_data.RData` | Raw and smoothed Canadian mortality rates, 1950–2016, ages 0–79 and 80+ |
| `2_read_data.R` | Loads `data/JPN_data.RData`; the code that made it from the raw files is kept, commented out |
| `Canada_code/1_CAN_read_data.R` | Loads `data/CAN_data.RData`; the code that made it from the raw files is kept, commented out |

The Japanese data are from the Japanese Mortality Database, ver. 005_001, downloaded on 31 January 2026 and archived at <https://www.ipss.go.jp/p-toukei/JMD_005_001/index-en.asp>. The Canadian data are from the Canadian Human Mortality Database, <http://www.bdlc.umontreal.ca/chmd/>.

---

# 2. Point Forecasting Model

| File | Description |
|---|---|
| `1_load_packages.R` | Loads the R packages |
| `3_interval_score.R` | Empirical coverage probability, coverage probability difference and interval score |
| `4_choice_K.R` | Criteria for the number of factors |
| `5_One_way_FMP.R` | One-way functional median polish |
| `6_FFM.R` | Functional factor model |
| `7_FMP_FFM_split.R` | Point forecasts from the one-way functional ANOVA and functional factor model, followed by split conformal prediction |

---

# 3. Conformal Prediction for Japan

## Main Files

| File | Description |
|---|---|
| `7_FMP_FFM_split.R` | Split conformal prediction |
| `8_FMP_FFM_sequential.R` | Sequential predictive conformal prediction |
| `9_FMP_FFM_NS.R` | Non-conformity score conformal prediction |
| `10_FMP_FFM_AC.R` | Adaptive conformal prediction |

## Result Files

| File | Description |
|---|---|
| `results/JPN_split_95.RData` | Split conformal prediction |
| `results/JPN_sequential_95.RData` | Sequential predictive conformal prediction |
| `results/JPN_NS_95.RData` | Non-conformity score conformal prediction |
| `results/JPN_AC_95.RData` | Adaptive conformal prediction |

---

# 4. Sensitivity Analysis for Canada

## Main Files

| File | Description |
|---|---|
| `Canada_code/2_CAN_FANOVA_FFM_split.R` | Split conformal prediction |
| `Canada_code/3_CAN_FANOVA_FFM_sequential.R` | Sequential predictive conformal prediction |
| `Canada_code/4_CAN_FANOVA_FFM_NS.R` | Non-conformity score conformal prediction |
| `Canada_code/5_CAN_FANOVA_FFM_AC.R` | Adaptive conformal prediction |

## Result Files

| File | Description |
|---|---|
| `results/CAN_split_95.RData` | Split conformal prediction |
| `results/CAN_sequential_95.RData` | Sequential predictive conformal prediction |
| `results/CAN_NS_95.RData` | Non-conformity score conformal prediction |
| `results/CAN_AC_95.RData` | Adaptive conformal prediction |

---

# 5. Figures

## Main Files

| File | Description |
|---|---|
| `11_figures.R` | Figure 2 and the boxplots of ECP, CPD and mean interval score |
| `savefig.R` | `savefig()`, used for Figure 2 |

## Figure Files

| File | Description |
|---|---|
| `Figure/Japan/Fig_2a.png` to `Fig_2d.png` | Figure 2: raw and smoothed log mortality rates in Okinawa |
| `Figure/Japan/JPN_95_ECP.png` | Figure 6: ECPs for Japan |
| `Figure/Japan/JPN_95_score.png` | Figure 7: mean interval scores for Japan |
| `Figure/Canada/CAN_95_ECP.png` | Figure 8: ECPs for Canada |
| `Figure/Canada/CAN_95_score.png` | Figure 9: mean interval scores for Canada |
| `Figure/Japan/JPN_95_CPD.png`, `Figure/Canada/CAN_95_CPD.png` | CPDs for Japan and Canada |

---

# Suggested Order of Running Files

Set the working directory to the main folder and run the files in this order, in one R session:

1. `1_load_packages.R`
2. `2_read_data.R`
3. `3_interval_score.R`
4. `4_choice_K.R`
5. `5_One_way_FMP.R`
6. `6_FFM.R`
7. `7_FMP_FFM_split.R`
8. `8_FMP_FFM_sequential.R`
9. `9_FMP_FFM_NS.R`
10. `10_FMP_FFM_AC.R`
11. `Canada_code/1_CAN_read_data.R`
12. `Canada_code/2_CAN_FANOVA_FFM_split.R`
13. `Canada_code/3_CAN_FANOVA_FFM_sequential.R`
14. `Canada_code/4_CAN_FANOVA_FFM_NS.R`
15. `Canada_code/5_CAN_FANOVA_FFM_AC.R`
16. `11_figures.R`

---

# Contact

### Chang Liu

Department of Actuarial Studies and Business Analytics<br />
Macquarie University<br />
Email: chang.liu45@students.mq.edu.au

---

### Han Lin Shang

Department of Actuarial Studies and Business Analytics<br />
Macquarie University<br />
Email: hanlin.shang@mq.edu.au
