# Packages and settings
packs <- c("ggplot2", "papaja")
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

theme_set(papaja::theme_apa())  # set APA style theme for plots

# Get data from package
if(!require(MultiKink)){install.packages("MultiKink")}; library(MultiKink)
data <- MultiKink::triceps
data <- subset(data, age <= 30)

# Plot the data
plot <- ggplot(data, aes(x = age, y = triceps)) +
  geom_point(alpha = .7, fill = "black", color = "grey50") + 
  xlab("Vârstă") +
  ylab("TSF")

plot

# Linear Regression
# plot + geom_smooth(formula = y ~ x, method = "lm", se = TRUE, color = "red") # fast way to plot it
lm_lin <- lm(triceps ~ age, data = data)   # fit regression model and then plot it
pred_lin <- predict(lm_lin, se.fit = TRUE, data = data) 

plot_lin <- 
  plot +
  geom_line(aes(y = pred_lin$fit), size = 1, color = "red") +
  geom_ribbon(aes(ymax = pred_lin$fit + pred_lin$se.fit, ymin = pred_lin$fit - pred_lin$se.fit), 
              fill = "red", alpha = .3) +
  geom_text(x = 5, y = 35, color = "red", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_lin)$r.squared, 3))))

suppressWarnings(print(plot_lin))

# Polynomial Regression
lm_quad <- lm(triceps ~ age + I(age^2), data = data)
lm_cub <- lm(triceps ~ age + I(age^2) + I(age^3), data = data)

pred_quad <- predict(lm_quad, se.fit = TRUE, data = data) 
pred_cub <- predict(lm_cub, se.fit = TRUE, data = data) 

plot_poly <-
  plot +
  geom_line(aes(y = pred_quad$fit), size = 1, color = "blue") +
  geom_ribbon(aes(ymax = pred_quad$fit + pred_quad$se.fit, ymin = pred_quad$fit - pred_quad$se.fit), 
              fill = "blue", alpha = .1) +
  geom_text(x = 5, y = 35, color = "blue", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_quad)$r.squared, 3)))) +
  geom_line(aes(y = pred_cub$fit), size = 1, color = "red") +
  geom_ribbon(aes(ymax = pred_cub$fit + pred_cub$se.fit, ymin = pred_cub$fit - pred_cub$se.fit), 
              fill = "red", alpha = .1) +
  geom_text(x = 5, y = 30, color = "red", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_cub)$r.squared, 3))))

suppressWarnings(print(plot_poly))

# Piecewise regression
data_k1 <- data[data$age < 5, ]
data_k2 <- data[data$age >= 5 & data$age < 10, ]
data_k3 <- data[data$age >= 10 & data$age < 20, ]   
data_k4 <- data[data$age >= 20, ]
  
lm_p1 <- lm(triceps ~ age, data = data_k1)
lm_p2 <- lm(triceps ~ age, data = data_k2)
lm_p3 <- lm(triceps ~ age, data = data_k3)
lm_p4 <- lm(triceps ~ age, data = data_k4)

pred_p1 <- predict(lm_p1, se.fit = TRUE, data = data_k1)
pred_p2 <- predict(lm_p2, se.fit = TRUE, data = data_k2)
pred_p3 <- predict(lm_p3, se.fit = TRUE, data = data_k3)
pred_p4 <- predict(lm_p4, se.fit = TRUE, data = data_k4)

plot_pwr <- 
  plot + 
  geom_line(data = data_k1, aes(y = pred_p1$fit), size = 1, color = "red") +
  geom_ribbon(data = data_k1, aes(ymax = pred_p1$fit + pred_p1$se.fit, ymin = pred_p1$fit - pred_p1$se.fit), 
              fill = "red", alpha = .1) + 
  geom_line(data = data_k2, aes(y = pred_p2$fit), size = 1, color = "red") +
  geom_ribbon(data = data_k2, aes(ymax = pred_p2$fit + pred_p2$se.fit, ymin = pred_p2$fit - pred_p2$se.fit), 
              fill = "red", alpha = .1) +
  geom_line(data = data_k3, aes(y = pred_p3$fit), size = 1, color = "red") +
  geom_ribbon(data = data_k3, aes(ymax = pred_p3$fit + pred_p3$se.fit, ymin = pred_p3$fit - pred_p3$se.fit), 
              fill = "red", alpha = .1) +
  geom_line(data = data_k4, aes(y = pred_p4$fit), size = 1, color = "red") +
  geom_ribbon(data = data_k4, aes(ymax = pred_p4$fit + pred_p4$se.fit, ymin = pred_p4$fit - pred_p4$se.fit), 
              fill = "red", alpha = .1)  

suppressWarnings(print(plot_pwr))  
  
# Linear splines
# Add the terms (x − k) when x ≥ k . 
# We will do this by adding I ((age − k)∗(age >= k)) terms to the linear model. 
# Note that (age >= k) is a logical statement that will be 0 (FALSE) or 1 (TRUE) and I() evaluates that all expression.
lm_linsp <- lm(triceps ~ age + 
                 I((age - 5) * (age >= 5)) +
                 I((age - 10) * (age >= 10)) +
                 I((age - 20) * (age >= 20)),
               data = data)
pred_linsp <- predict(lm_linsp, se.fit = TRUE, data = data)

plot_linsp <-                  
  plot +
  geom_line(data = data, aes(y = pred_linsp$fit, x = age), size = 1, col = "red") +
  geom_ribbon(data = data, aes(ymax = pred_linsp$fit + pred_linsp$se.fit, ymin = pred_linsp$fit - pred_linsp$se.fit), 
              fill = "red", alpha = .1) +
  geom_text(x = 5, y = 35, color = "red", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_linsp)$r.squared, 3))))

suppressWarnings(print(plot_linsp))

# Quadratic splines
lm_quadsp <- lm(triceps ~ age + 
                  I((age - 5) * (age >= 5)) + I((age - 5)^2 * (age >= 5)) + 
                  I((age - 10) * (age >= 10)) + I((age - 10)^2 * (age >= 10)) + 
                  I((age - 20) * (age >= 20)) + I((age - 20)^2 * (age >= 20)), 
                data = data)

pred_quadsp <- predict(lm_quadsp, se.fit = TRUE, data = data)

plot_quadsp <-
  plot +
  geom_line(data = data, aes(y = pred_quadsp$fit, x = age), size = 1, col = "red") +
  geom_ribbon(data = data, aes(ymax = pred_quadsp$fit + pred_quadsp$se.fit, ymin = pred_quadsp$fit - pred_quadsp$se.fit), 
              fill = "red", alpha = .1) +
  geom_text(x = 5, y = 35, color = "red", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_quadsp)$r.squared, 3))))

suppressWarnings(print(plot_quadsp))