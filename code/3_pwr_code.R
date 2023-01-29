# PWR code for slides

# Packages and settings
packs <- c("tidyverse", "latex2exp", "papaja")
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

if(!require(ggbrace)){devtools::install_github("nicolash2/ggbrace")}; library(ggbrace)

theme_set(papaja::theme_apa())  # set APA style theme for plots


# Data
df <- tibble(
  y = c(c(3, 4, 6, 5, 4), c(8, 7, 8, 9, 9)),
  AB = c(rep("A", 5), rep("B", 5)), 
  t = 1:10
)

# Stats
df_A <- df[df$AB == "A", ]
df_B <- df[df$AB == "B", ]  
mean_A <- mean(df_A$y, na.rm = TRUE)
mean_B <- mean(df_B$y, na.rm = TRUE)
intercept_A <- summary(lm(y ~ t, data = df_A))$coefficients[1, 1]
slope_A <- summary(lm(y ~ t, data = df_A))$coefficients[2, 1]
intercept_B <- summary(lm(y ~ t, data = df_B))$coefficients[1, 1]
slope_B <- summary(lm(y ~ t, data = df_B))$coefficients[2, 1]

# Plot Skeleton
plot_skeleton <- 
  ggplot() +
  geom_point(data = df, aes(x = t, y = y)) +
  geom_rect(aes(xmin = 5.4, xmax = 5.6, ymin = 1, ymax = 10), 
            fill = "black", alpha = 0.2) +
  geom_line(data = df_A, aes(x = t, y = y)) +
  geom_line(data = df_B, aes(x = t, y = y)) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  coord_equal()

# Plot Means
plot_skeleton +    
  geom_segment(aes(x = 1, xend = 5.5, y = mean_A, yend = mean_A), color = "tan1") +
  geom_segment(aes(x = 5.5, xend = 10, y = mean_B, yend = mean_B), color = "tan1")

# Plot Global Slope
plot_skeleton + 
  stat_smooth(data = df, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE, 
              linetype = "longdash", size = 1, alpha = 0.6, color = "yellow4")

# Plot Phase Slopes
plot_skeleton +
  stat_smooth(data = df_A, aes(x = t, y = y), geom = "line", 
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3") +
  stat_smooth(data = df_B, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3")

# Plot all together
plot_skeleton +
  geom_segment(aes(x = 1, xend = 5.5, y = mean_A, yend = mean_A), color = "tan1") +
  geom_segment(aes(x = 5.5, xend = 10, y = mean_B, yend = mean_B), color = "tan1") +  
  stat_smooth(data = df, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE, 
              linetype = "longdash", size = 1, alpha = 0.6, color = "yellow4") +
  stat_smooth(data = df_A, aes(x = t, y = y), geom = "line", 
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3") +
  stat_smooth(data = df_B, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3")
  
# Plot ANOVA
plot_anova <- 
  ggplot() +
  geom_point(data = df, aes(x = t, y = y)) +
  geom_line(data = df_A, aes(x = t, y = y)) +
  geom_line(data = df_B, aes(x = t, y = y)) +
  geom_rect(aes(xmin = 5.4, xmax = 5.6, ymin = 1, ymax = 10), 
            fill = "black", alpha = 0.2) +
  ggbrace::geom_brace(aes(
    label = "",
    x = c(5.2, 5.6), 
    y = c(mean_A, mean_B)), 
    rotate = 270, labelrotate = 0, labeldistance = 0.1, labelsize = 3, color = "red") +
  annotate(geom = "text", x = 4.8, y = mean(c(mean_A, mean_B)),
           label = latex2exp::TeX(r"($\beta_{1} D$)"), parse = TRUE, color = "red") +
  geom_segment(aes(x = 1, xend = 5.5, y = mean_A, yend = mean_A), color = "tan1") +
  geom_segment(aes(x = 5.5, xend = 10, y = mean_B, yend = mean_B), color = "tan1") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  coord_equal()
 
suppressWarnings(print(plot_anova))

plot(latex2exp::TeX(r"($y = \beta_{0} + \beta_{1} D + \epsilon$)"))  # raw string latex r"(...)", no escape chars needed
 

# Plot Global Slope Model
plot_oneslope <- 
  ggplot() +
  geom_point(data = df, aes(x = t, y = y)) +
  geom_rect(aes(xmin = 5.4, xmax = 5.6, ymin = 1, ymax = 10), 
            fill = "black", alpha = 0.2) +
  stat_smooth(data = df, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE, 
              linetype = "longdash", size = 1, alpha = 0.6, color = "yellow4") +
  annotate(geom = "text", x = 10, y = (intercept_B + slope_B*10 + 0.6),
           label = latex2exp::TeX(r"($\beta_{2} t$)"), parse = TRUE, color = "yellow4") +
  stat_smooth(data = df_A, aes(x = t, y = y), geom = "line", 
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3") +
  stat_smooth(data = df_A, aes(x = t, y = y), geom = "line", 
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3", fullrange = TRUE, linetype = "dashed") +
  stat_smooth(data = df_B, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3") +
  stat_smooth(data = df_B, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3", fullrange = TRUE, linetype = "dashed") +
  ggbrace::geom_brace(aes(
    label = "",
    x = c(5.2, 5.6), 
    y = c((intercept_A + slope_A*5.5), (intercept_B + slope_B*5.5))), 
    rotate = 270, labelrotate = 0, labeldistance = 0.1, labelsize = 3, color = "red") +
  annotate(geom = "text", x = 4.8, y = mean(c(mean_A, mean_B)),
           label = latex2exp::TeX(r"($\beta_{1} D$)"), parse = TRUE, color = "red") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  coord_equal()

suppressWarnings(print(plot_oneslope))
  
plot(latex2exp::TeX(r"($y = \beta_{0} + \beta_{1} D + \beta_{2} t + \epsilon$)"))


# Plot Piecewise Regression
plot_pwr1 <-
  ggplot() +
  geom_point(data = df, aes(x = t, y = y)) +
  geom_rect(aes(xmin = 5.4, xmax = 5.6, ymin = 1, ymax = 10), 
            fill = "black", alpha = 0.2) +
  geom_line(data = df_A, aes(x = t, y = y)) +
  geom_line(data = df_B, aes(x = t, y = y)) +
  stat_smooth(data = df_A, aes(x = t, y = y), geom = "line", 
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3") +
  stat_smooth(data = df_A, aes(x = t, y = y), geom = "line", 
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3", fullrange = TRUE, linetype = "dashed") +
  annotate(geom = "text", x = 3, y = (intercept_A + slope_A*3 - 0.3),
           label = latex2exp::TeX(r"($\beta_{2} t$)"), parse = TRUE, color = "yellow3") +
  stat_smooth(data = df_B, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3") +
  stat_smooth(data = df_B, aes(x = t, y = y), geom = "line",
              formula = y ~ x, method = "lm", se = FALSE,
              alpha = 0.7, size = 1, color = "yellow3", fullrange = TRUE, linetype = "dashed") +
  ggbrace::geom_brace(aes(
    label = "",
    x = c(5.2, 5.6), 
    y = c((intercept_A + slope_A*5.5), (intercept_B + slope_B*5.5))), 
    rotate = 270, labelrotate = 0, labeldistance = 0.1, labelsize = 3, color = "red") +
  annotate(geom = "text", x = 4.8, y = mean(c(mean_A, mean_B)),
           label = latex2exp::TeX(r"($\beta_{1} D$)"), parse = TRUE, color = "red") +
  geom_curve(aes(x = 8.3, xend = 8.3, y = (intercept_A + slope_A*8.3),  yend = (intercept_B + slope_B*8.3)),
             color = "red") +
  annotate(geom = "text", x = 9.7, y = mean(c((intercept_A + slope_A*8.3), (intercept_B + slope_B*8.3))),
           label = latex2exp::TeX(r"($\beta_{3} D(t-k)$)"), parse = TRUE, color = "red") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  coord_equal()

suppressWarnings(print(plot_pwr1))


plot_pwr2 <-
  ggplot() +
  geom_point(data = df, aes(x = t, y = y)) +
  geom_rect(aes(xmin = 5.4, xmax = 5.6, ymin = 1, ymax = 10), 
            fill = "black", alpha = 0.2) +
  geom_line(data = df_A, aes(x = t, y = y)) +
  geom_line(data = df_B, aes(x = t, y = y)) +
  ggbrace::geom_brace(aes(
    label = "",
    x = c(5.2, 5.6), 
    y = c(mean_A, mean_B)), 
    rotate = 270, labelrotate = 0, labeldistance = 0.1, labelsize = 3, color = "red") +
  annotate(geom = "text", x = 4.8, y = mean(c(mean_A, mean_B)),
           label = latex2exp::TeX(r"($\beta_{1} D$)"), parse = TRUE, color = "red") +
  geom_segment(aes(x = 1, xend = 5.5, y = mean_A, yend = mean_A), color = "tan1") +
  annotate(geom = "text", x = 3, y = mean_A - 0.4,
           label = latex2exp::TeX(r"($\beta_{2} t = 0$)"), parse = TRUE, color = "tan1") +
  geom_segment(aes(x = 5, xend = 10, y = mean_A, yend = mean_A), color = "tan1", linetype = "dashed") +
  geom_segment(aes(x = 5.5, xend = 10, y = mean_B, yend = mean_B), color = "tan1") +
  geom_curve(aes(x = 8, xend = 8, y = mean_A,  yend = mean_B), curvature = 0.3, color = "red") +
  annotate(geom = "text", x = 9.7, y = mean(c(mean_A, mean_B)),
           label = latex2exp::TeX(r"($\beta_{3} D(t-k)$)"), parse = TRUE, color = "red") +
  annotate(geom = "text", x = 9.7, y = mean(c(mean_A, mean_B)) - 0.4,
           label = latex2exp::TeX(r"($= 0$)"), parse = TRUE, color = "red") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  coord_equal()

suppressWarnings(print(plot_pwr2))

plot(latex2exp::TeX(r"($y = \beta_{0} + \beta_{1} D + \beta_{2} t + \beta_{3} D(t-k) + \epsilon$)"))
