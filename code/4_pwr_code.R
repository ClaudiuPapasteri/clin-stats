# Packages and settings
packs <- c("ggplot2", "papaja", "scda", "scan")
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

theme_set(papaja::theme_apa())  # set APA style theme for plots

# Get data
# Michael's data from `scda` package (can be downloaded from presentation Gihub)
data <- scda::Singh
data_Michael <- subset(data, id == "Michael")

# Plot
plot <- 
  ggplot() +
  geom_point(data = data_Michael, aes(x = time, y = score_physical)) +
  geom_rect(aes(xmin = 4.4, xmax = 4.6, ymin = 0, ymax = 3), 
            fill = "black", alpha = 0.2) +
  geom_line(data = data_Michael[data_Michael$phase == 0, ], aes(x = time, y = score_physical)) +
  geom_line(data = data_Michael[data_Michael$phase == 1, ], aes(x = time, y = score_physical)) +
  scale_x_continuous(breaks = 1:18, limits = c(1, 18)) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3)) +
  xlab("Timp") +
  ylab("Scor fizic") +
  coord_equal()

plot

# Package scda -----------------------------------------------------------
# Piecewise regression
pwr_scda <- scda::piecewiseRegr(data = data_Michael,
                           timeVar = "time",
                           yVar = "score_physical",
                           phaseVar = "phase",
                           robust = FALSE,
                           outputWidth = 1.2,
                           outputHeight = 1.2,
                           yRange = c(0,7),
                           showPlot = FALSE)
pwr_scda
pwr_scda$output$plot

# Effect size
# Swaminathan et al. (2014) compares the predicted scores from the PWR model at a given measurement point (t) in phase B with the
# prediction from the A phase extrapolated to t.
pwr_scda$output$delta_t  

# Generalized PWR 
pwrgen_scda <- scda::genPwr(data = data_Michael,
                            timeVar = "time",
                            yVar = "score_physical",
                            phaseVar = "phase")

pwrgen_scda
pwrgen_scda$output$plot


# Package scan -----------------------------------------------------------
# Transform into scdf list format
# data_scdf <- as.list(split(data, f = as.factor(data$id)))
data_Michael_scdf <- scan::scdf(
  values = data_Michael$score_physical, 
  covariate = data_Michael$score_verbal,
  mt = data_Michael$time,
  phase = LETTERS[data_Michael$phase + 1],        # data_Michael$phase
  name = "Michael"
)

plot(data_Michael_scdf)

# Piecewise regression
pwr_scan <- scan::plm(data = data_Michael_scdf, 
                      dvar = "values", pvar = "phase", mvar = "mt", 
                      model = "H-M",                                    # "B&L-B" starts the dummy var at 1, "H-M" starts at 0 which is also used by scda::piecewiseRegr
                      AR = 0, family = "gaussian",
                      trend = TRUE, level = TRUE, slope = TRUE, r_squared = TRUE)

pwr_scan

# Autocorrelation
scan::autocorr(data = data_Michael_scdf, 
               dvar = "values", pvar = "phase", mvar = "mt", lag_max = 3)

# Multivariate piecewise regression
mvpwr_scan <- scan::mplm(data = data_Michael_scdf, 
                      dvar = c("values", "covariate"), pvar = "phase", mvar = "mt", 
                      model = "H-M", trend = TRUE, level = TRUE, slope = TRUE)

mvpwr_scan
###########################################################################################

# Do it in base R
k_time <- 5   # (t-k_time) starts with 0 in B phase => k is number of first time of B phase
pwr <- lm(score_physical ~ 1 + phase + time + I(phase * (time - k_time) * (time >= k_time)), data = data_Michael)
# here phase and (time >= k_time) are both 0 in A and 1 in B ... don't need both dummy vars
summary(pwr)$coefficients  # same results as scan

pred_pwr <- predict(pwr, se.fit = TRUE, data = data_Michael)

plot_base <- 
  ggplot(data = data_Michael, aes(x = time, y = score_physical)) +
    geom_point(alpha = 0.55, color = "black") + 
    geom_line(aes(y = pred_pwr$fit), size = 1, color = "red") +
    geom_ribbon(aes(ymax = pred_pwr$fit + pred_pwr$se.fit, ymin = pred_pwr$fit - pred_pwr$se.fit), fill = "red", alpha = .1) +
    theme_minimal() 

# scda centers t so that it begins with 0 in both phases => difference in intercept between scda and scan/base solutions
# to get same result for intercept as scda:
data_Michael2 <- data_Michael
data_Michael2$time <- data_Michael2$time - 1
k_time <- 4
pwr_likescda <- lm(score_physical ~ 1 + phase + time + I(phase * (time - k_time) * (time >= k_time)), data = data_Michael2)
summary(pwr_likescda)$coefficients   # same as scda