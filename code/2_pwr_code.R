# Packages and settings
packs <- c("ggplot2", "papaja", "osfr", "tidyverse", "scales", "lspline", "plotly")
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)

theme_set(papaja::theme_apa())  # set APA style theme for plots

# Read data from folder -- here we will read directly from OSF repository
# csv_name <- "cognitive_dynamics_heino.csv"
# csv_path <- here::here("datasets", csv_name) 
# df <- read.csv(csv_path)   

# Read data from OSF repository
repo_url <- "https://osf.io/w9v28/"
csv_name <- "cognitive_dynamics_heino.csv"
repo <- osfr::osf_retrieve_node(repo_url)
repo_files <- osfr::osf_ls_files(repo)
# we download to a temporary file that gets deleted after
temp_dir <- tempdir()
osfr::osf_download(repo_files[repo_files$name == csv_name, ], path = temp_dir)
csv_path <- file.path(temp_dir, csv_name)
df <- read.csv(csv_path)
unlink(csv_path)   

# Transform
df_clean <- 
  df %>%
  # keep only days with record of date
  tidyr::drop_na(date) %>%
  # convert to ISO 8601 date format, format used is not consistent
  tidyr::separate(date, into = c("day", "month", "year"), sep = "\\.", remove = FALSE) %>%
  tidyr::separate(time_of_day, into = c("hour", "minute"), sep = "\\:", remove = FALSE) %>%
  # there are some typos, exclude them
  dplyr::mutate(across(c(day, month, year, hour, minute), as.numeric)) %>%
  dplyr::filter(year >= 2014, year <= 2017, month < 12, day < 31) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(year, month, day, sep = ' ')),
    date_posixct = as.POSIXct(date),
    hm = lubridate::hm(paste(hour, minute, sep = ' '))
  ) %>%
  dplyr::filter(date > lubridate::as_date("2014-07-01")) %>%
  dplyr::select(rowNumber, date, date_posixct, hm, stroop_incongruent_post_meditation) %>%
  tidyr::drop_na(stroop_incongruent_post_meditation) %>%
  dplyr::mutate(y = stroop_incongruent_post_meditation,
                moments = dplyr::row_number())

# Plot
plot <- 
  ggplot(df_clean, aes(x = date_posixct, y = y)) +
  geom_point() +
  scale_x_datetime(date_labels = "%Y-%m", breaks= scales::date_breaks("6 month")) +
  xlab("") +
  ylab("Timp răspuns Stroop inc. Post Meditație") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

plot

# Linear Regression
lm_lin <- lm(y ~ date_posixct, data = df_clean)
pred_lin <- predict(lm_lin, se.fit = TRUE, data = df_clean)

plot_lin <-                  
  plot +
  geom_line(data = df_clean, aes(y = pred_lin$fit, x = date_posixct), size = 1, col = "blue") +
  geom_ribbon(data = df_clean, aes(ymax = pred_lin$fit + pred_lin$se.fit, ymin = pred_lin$fit - pred_lin$se.fit), 
              fill = "blue", alpha = .1) +
  geom_text(x = as.POSIXct("2014-11-30"), y = 21, color = "blue", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_lin)$r.squared, 3))))

suppressWarnings(print(plot_lin))

# Linear Splines
# Instead of doing it manually, we will use `lspline` package
# knots set manually lspline()
# knots set at breaks dividing the range of x into q equal-frequency intervals qlspline()
# knots set at breaks dividing the range of x into n equal-width intervals elspline()
lm_linsp2 <- lm(y ~ lspline::qlspline(date_posixct, q = 2), data = df_clean)
lm_linsp3 <- lm(y ~ lspline::qlspline(date_posixct, q = 3), data = df_clean)
lm_linsp4 <- lm(y ~ lspline::qlspline(date_posixct, q = 4), data = df_clean)
lm_linsp5 <- lm(y ~ lspline::qlspline(date_posixct, q = 5), data = df_clean)
lm_linsp6 <- lm(y ~ lspline::qlspline(date_posixct, q = 6), data = df_clean)
lm_linsp10 <- lm(y ~ lspline::qlspline(date_posixct, q = 10), data = df_clean)

df_clean$knots2 <- predict(lm_linsp2, se.fit = TRUE, data = df_clean)$fit
df_clean$knots3 <- predict(lm_linsp3, se.fit = TRUE, data = df_clean)$fit
df_clean$knots4 <- predict(lm_linsp4, se.fit = TRUE, data = df_clean)$fit
df_clean$knots5 <- predict(lm_linsp5, se.fit = TRUE, data = df_clean)$fit
df_clean$knots6 <- predict(lm_linsp6, se.fit = TRUE, data = df_clean)$fit
df_clean$knots10 <- predict(lm_linsp10, se.fit = TRUE, data = df_clean)$fit

plot_interact <- 
  plot_ly(data = df_clean, x = ~date_posixct) %>% 
  add_markers(y = ~y, showlegend = FALSE, name = "") %>% 
  add_lines(x = ~date_posixct, y = ~knots2, visible = "legendonly", name = "2 noduri") %>%
  add_lines(x = ~date_posixct, y = ~knots3, visible = "legendonly", name = "3 noduri") %>%
  add_lines(x = ~date_posixct, y = ~knots4, visible = "legendonly", name = "4 noduri") %>%
  add_lines(x = ~date_posixct, y = ~knots5, visible = "legendonly", name = "5 noduri") %>%
  add_lines(x = ~date_posixct, y = ~knots6, visible = "legendonly", name = "6 noduri") %>%
  layout(
    xaxis = list(
      title = "", 
      tickformat = "%Y-%m"
  ))

plot_interact

# Plot PWR with different number of knots
plot_knots <- 
  plot +
  geom_line(data = df_clean, aes(y = knots5, x = date_posixct), alpha = .8, size = 1, color = "red1") +
  geom_text(x = as.POSIXct("2014-11-30"), y = 21, color = "red1", 
          label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_linsp5)$r.squared, 3)))) + 
  geom_line(data = df_clean, aes(y = knots6, x = date_posixct), alpha = .8, size = 1, color = "red3") +
  geom_text(x = as.POSIXct("2014-11-30"), y = 19.5, color = "red3", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_linsp6)$r.squared, 3)))) +
  geom_line(data = df_clean, aes(y = knots10, x = date_posixct), alpha = .8, size = 1, color = "blue") +
  geom_text(x = as.POSIXct("2014-11-30"), y = 17.5, color = "blue", 
            label = latex2exp::TeX(paste0("$\\R^2 = $", round(summary(lm_linsp10)$r.squared, 3))))

suppressWarnings(print(plot_knots))