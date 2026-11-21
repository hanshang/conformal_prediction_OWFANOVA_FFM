library(demography)
library(ggplot2)
library(patchwork)
source("savefig.R")

load("data/JPN_data.RData")
load("results/JPN_split_95.RData")
load("results/JPN_sequential_95.RData")
load("results/JPN_NS_95.RData")
load("results/JPN_AC_95.RData")
load("results/CAN_split_95.RData")
load("results/CAN_sequential_95.RData")
load("results/CAN_NS_95.RData")
load("results/CAN_AC_95.RData")

# Figure 2: Okinawa

savefig("Figure/Japan/Fig_2a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(female_prefecture_rate[[47]], main = "Okinawa female data", xlab = "", ylim = c(-9.2, 0))
dev.off()

savefig("Figure/Japan/Fig_2b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(male_prefecture_rate[[47]], main = "Okinawa male data", xlab = "", ylab = "", ylim = c(-9.2, 0))
dev.off()

savefig("Figure/Japan/Fig_2c", width = 12, height = 10, toplines = 0.8, type = "png")
plot(smooth_female_prefecture_rate[[47]], main = "", ylim = c(-9.2, 0))
dev.off()

savefig("Figure/Japan/Fig_2d", width = 12, height = 10, toplines = 0.8, type = "png")
plot(smooth_male_prefecture_rate[[47]], main = "", ylab = "", ylim = c(-9.2, 0))
dev.off()

# boxplots

# result: n_state x 3 x n_horizon array of ECP, CPD and score
# method: conformal prediction method
# panel: forecasting method and sex

long_format <- function(result, method, panel)
{
    out = NULL
    for(iw in 1:dim(result)[3])
    {
        out = rbind(out, data.frame(panel = panel, method = method, h = iw,
                                    ECP = result[,1,iw], CPD = result[,2,iw], score = result[,3,iw]))
        rm(iw)
    }
    return(out)
}

boxplot_fun <- function(plot_data, measure, y_lim = NULL, nominal = NULL)
{
    plot_data$panel = factor(plot_data$panel, levels = c("ARIMA, female", "ARIMA, male", "ETS, female", "ETS, male"))
    plot_data$method = factor(plot_data$method, levels = c("Split (sd)", "Sequential", "Non-conformity Score", "Adaptive"))
    plot_data$h = factor(plot_data$h)
    plot_data$y = plot_data[,measure]

    # outliers

    q1 = ave(plot_data$y, plot_data$panel, plot_data$method, plot_data$h, FUN = function(x) quantile(x, 0.25))
    q3 = ave(plot_data$y, plot_data$panel, plot_data$method, plot_data$h, FUN = function(x) quantile(x, 0.75))
    outlier = plot_data$y < q1 - 1.5 * (q3 - q1) | plot_data$y > q3 + 1.5 * (q3 - q1)

    # values outside the y axis are shown as triangles

    plot_data$below = NA
    plot_data$above = NA
    if(measure == "ECP" | measure == "CPD")
    {
        plot_data$below[plot_data$y < y_lim[1]] = y_lim[1]
        plot_data$above[plot_data$y > y_lim[2]] = y_lim[2]
    }
    if(measure == "score")
    {
        box_top = pmax(q3, ave(ifelse(outlier, NA, plot_data$y), plot_data$panel, plot_data$method, plot_data$h, FUN = function(x) max(x, na.rm = TRUE)))
        panel_top = ave(box_top, plot_data$panel, FUN = max)
        plot_data$above[plot_data$y > panel_top] = panel_top[plot_data$y > panel_top]
    }
    plot_data$dot = ifelse(outlier & is.na(plot_data$below) & is.na(plot_data$above), plot_data$y, NA)

    shade = which(as.numeric(levels(plot_data$h)) %% 2 == 0)

    p = ggplot(plot_data, aes(x = h, y = y, fill = method)) +
        annotate("rect", xmin = shade - 0.5, xmax = shade + 0.5, ymin = -Inf, ymax = Inf, fill = "grey95")
    if(measure == "ECP")
    {
        p = p + geom_hline(yintercept = nominal, linetype = "dashed", linewidth = 0.4)
    }
    p = p +
        geom_boxplot(position = position_dodge(width = 0.8, preserve = "single"), width = 0.7,
                     linewidth = 0.3, outliers = FALSE) +
        geom_point(aes(y = dot), position = position_dodge(width = 0.8, preserve = "single"),
                   size = 0.5, colour = "grey20", na.rm = TRUE, show.legend = FALSE) +
        geom_point(aes(y = below), position = position_dodge(width = 0.8, preserve = "single"),
                   shape = 25, size = 1.6, stroke = 0.3, na.rm = TRUE, show.legend = FALSE) +
        geom_point(aes(y = above), position = position_dodge(width = 0.8, preserve = "single"),
                   shape = 24, size = 1.6, stroke = 0.3, na.rm = TRUE, show.legend = FALSE) +
        facet_wrap(~ panel, ncol = 1, scales = ifelse(measure == "score", "free_y", "fixed")) +
        scale_fill_manual(values = c("Split (sd)" = "#2a78d6", "Sequential" = "#eb6834",
                                     "Non-conformity Score" = "#1baf7a", "Adaptive" = "#eda100"), name = NULL) +
        labs(x = "Horizon", y = ifelse(measure == "score", "Mean interval score", measure)) +
        coord_cartesian(ylim = y_lim) +
        theme_bw(base_size = 10) +
        theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
    return(p)
}


# Japan

JPN_95 = rbind(long_format(FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05, "Split (sd)", "ARIMA, female"),
               long_format(sequential_int_score_list_female_arima_PI_95_value, "Sequential", "ARIMA, female"),
               long_format(NS_int_score_female_array_arima_alpha_0.05, "Non-conformity Score", "ARIMA, female"),
               long_format(AC_int_score_female_array_arima_alpha_0.05, "Adaptive", "ARIMA, female"),
               long_format(FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05, "Split (sd)", "ARIMA, male"),
               long_format(sequential_int_score_list_male_arima_PI_95_value, "Sequential", "ARIMA, male"),
               long_format(NS_int_score_male_array_arima_alpha_0.05, "Non-conformity Score", "ARIMA, male"),
               long_format(AC_int_score_male_array_arima_alpha_0.05, "Adaptive", "ARIMA, male"),
               long_format(FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05, "Split (sd)", "ETS, female"),
               long_format(sequential_int_score_list_female_ets_PI_95_value, "Sequential", "ETS, female"),
               long_format(NS_int_score_female_array_ets_alpha_0.05, "Non-conformity Score", "ETS, female"),
               long_format(AC_int_score_female_array_ets_alpha_0.05, "Adaptive", "ETS, female"),
               long_format(FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05, "Split (sd)", "ETS, male"),
               long_format(sequential_int_score_list_male_ets_PI_95_value, "Sequential", "ETS, male"),
               long_format(NS_int_score_male_array_ets_alpha_0.05, "Non-conformity Score", "ETS, male"),
               long_format(AC_int_score_male_array_ets_alpha_0.05, "Adaptive", "ETS, male"))

p = boxplot_fun(plot_data = JPN_95, measure = "ECP", y_lim = c(0.75, 1), nominal = 0.95)
ggsave("Figure/Japan/JPN_95_ECP.png", p, width = 10, height = 12, dpi = 600)

p = boxplot_fun(plot_data = JPN_95, measure = "CPD", y_lim = c(0, 0.15))
ggsave("Figure/Japan/JPN_95_CPD.png", p, width = 10, height = 12, dpi = 600)

# score: h = 1-8 and h = 9-10 on separate y axes

p_1 = boxplot_fun(plot_data = JPN_95[JPN_95$h <= 8,], measure = "score")
p_2 = boxplot_fun(plot_data = JPN_95[JPN_95$h >= 9,], measure = "score") +
    scale_y_continuous(position = "right") + labs(y = NULL) + theme(legend.position = "none")
p = p_1 + p_2 + plot_layout(widths = c(8, 2), axis_titles = "collect")
ggsave("Figure/Japan/JPN_95_score.png", p, width = 10, height = 12, dpi = 600)



# Canada

CAN_95 = rbind(long_format(CAN_FMP_FFM_fun_int_score_female_array_arima_sd_alpha_0.05, "Split (sd)", "ARIMA, female"),
               long_format(CAN_sequential_int_score_list_female_arima_PI_95_value, "Sequential", "ARIMA, female"),
               long_format(CAN_NS_int_score_female_array_arima_alpha_0.05, "Non-conformity Score", "ARIMA, female"),
               long_format(CAN_AC_int_score_female_array_arima_alpha_0.05, "Adaptive", "ARIMA, female"),
               long_format(CAN_FMP_FFM_fun_int_score_male_array_arima_sd_alpha_0.05, "Split (sd)", "ARIMA, male"),
               long_format(CAN_sequential_int_score_list_male_arima_PI_95_value, "Sequential", "ARIMA, male"),
               long_format(CAN_NS_int_score_male_array_arima_alpha_0.05, "Non-conformity Score", "ARIMA, male"),
               long_format(CAN_AC_int_score_male_array_arima_alpha_0.05, "Adaptive", "ARIMA, male"),
               long_format(CAN_FMP_FFM_fun_int_score_female_array_ets_sd_alpha_0.05, "Split (sd)", "ETS, female"),
               long_format(CAN_sequential_int_score_list_female_ets_PI_95_value, "Sequential", "ETS, female"),
               long_format(CAN_NS_int_score_female_array_ets_alpha_0.05, "Non-conformity Score", "ETS, female"),
               long_format(CAN_AC_int_score_female_array_ets_alpha_0.05, "Adaptive", "ETS, female"),
               long_format(CAN_FMP_FFM_fun_int_score_male_array_ets_sd_alpha_0.05, "Split (sd)", "ETS, male"),
               long_format(CAN_sequential_int_score_list_male_ets_PI_95_value, "Sequential", "ETS, male"),
               long_format(CAN_NS_int_score_male_array_ets_alpha_0.05, "Non-conformity Score", "ETS, male"),
               long_format(CAN_AC_int_score_male_array_ets_alpha_0.05, "Adaptive", "ETS, male"))

p = boxplot_fun(plot_data = CAN_95, measure = "ECP", y_lim = c(0.75, 1), nominal = 0.95)
ggsave("Figure/Canada/CAN_95_ECP.png", p, width = 10, height = 12, dpi = 600)

p = boxplot_fun(plot_data = CAN_95, measure = "CPD", y_lim = c(0, 0.15))
ggsave("Figure/Canada/CAN_95_CPD.png", p, width = 10, height = 12, dpi = 600)

p_1 = boxplot_fun(plot_data = CAN_95[CAN_95$h <= 8,], measure = "score")
p_2 = boxplot_fun(plot_data = CAN_95[CAN_95$h >= 9,], measure = "score") +
    scale_y_continuous(position = "right") + labs(y = NULL) + theme(legend.position = "none")
p = p_1 + p_2 + plot_layout(widths = c(8, 2), axis_titles = "collect")
ggsave("Figure/Canada/CAN_95_score.png", p, width = 10, height = 12, dpi = 600)
