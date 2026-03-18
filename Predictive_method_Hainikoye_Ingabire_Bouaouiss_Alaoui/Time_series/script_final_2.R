############################################
## 0. Loading Data
############################################
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tseries)
library(scales)

unemployment <- read_excel("~/Desktop/muller project/monthly_unemployment.xlsx", sheet = "Feuil1")

############################################
## 1. Prepare the database
############################################
unemployment <- unemployment %>%
  mutate(
    Date = as.Date(parse_date_time(`Time period`, orders = "Y-b"))
  ) %>%
  arrange(Date) %>%
  mutate(
    u_lag1   = lag(`Unemployment, total`, 1),
    u_lag2   = lag(`Unemployment, total`, 2),
    CPI_lag1 = lag(`CPI Index, 2015`, 1),
    # inflation approx (monthly log-diff of CPI)
    infl = 100 * (log(`CPI Index, 2015`) - log(lag(`CPI Index, 2015`, 1))),
    infl_lag1 = lag(infl, 1)
  )

############################################
## 2. Descriptive analysis
############################################

#Unemployment 

ggplot(unemployment, aes(x = Date, y = `Unemployment, total`)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Evolution of the Unemployment Rate (Monthly)",
    x = "Time",
    y = "Unemployment rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(size = 14)
  )
#CPI 
ggplot(unemployment, aes(x = Date, y = `CPI Index, 2015`)) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(
    title = "Evolution of the Consumer Price Index (CPI)",
    x = "Time",
    y = "CPI (2015 = 100)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(size = 14)
  )

#Auto correlation Function 

library(stats)

acf(
  unemployment$`Unemployment, total`,
  main = "Autocorrelation Function (ACF) of Unemployment Rate",
  na.action = na.omit
)

#PACF – Partial Autocorrelation Function
pacf(
  unemployment$`Unemployment, total`,
  main = "Partial Autocorrelation Function (PACF) of Unemployment Rate",
  na.action = na.omit
)

#differentiate serie

unemployment <- unemployment %>%
  mutate(
    du = `Unemployment, total` - lag(`Unemployment, total`)
  )

ggplot(unemployment, aes(x = Date, y = du)) +
  geom_line(color = "gray40") +
  labs(
    title = "First difference of the unemployment rate",
    x = "Time",
    y = "Change in unemployment rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

############################################
## 3. Stationarity tests (ADF)
############################################
library(tseries)

# ADF test on unemployment rate (level)
adf_u_level <- adf.test(
  na.omit(unemployment$`Unemployment, total`)
)

# ADF test on CPI (level)
adf_cpi_level <- adf.test(
  na.omit(unemployment$`CPI Index, 2015`)
)

# ADF test on first difference of unemployment
adf_u_diff <- adf.test(
  na.omit(unemployment$du)
)

adf_u_level
adf_cpi_level
adf_u_diff


############################################
## 4. Remove NA due to lags
############################################
data_model <- unemployment %>%
  filter(!is.na(u_lag1), !is.na(u_lag2), !is.na(CPI_lag1), !is.na(infl_lag1))

############################################
## 5. Train / Test split
############################################
cutoff <- as.Date("2015-12-01")

train <- data_model %>% filter(Date <= cutoff)
test  <- data_model %>% filter(Date > cutoff)

############################################
## 6. Estimate models (OLS)
############################################
m_ar1  <- lm(`Unemployment, total` ~ u_lag1, data = train)
m_ar2  <- lm(`Unemployment, total` ~ u_lag1 + u_lag2, data = train)
m_arx1 <- lm(`Unemployment, total` ~ u_lag1 + CPI_lag1, data = train)
m_arx2 <- lm(`Unemployment, total` ~ u_lag1 + infl_lag1, data = train)

summary(m_ar1)
summary(m_ar2)
summary(m_arx1)
summary(m_arx2)


############################################
## 7. Predictions + prediction intervals
############################################
pred_ar1  <- predict(m_ar1,  newdata = test, interval = "prediction")
pred_ar2  <- predict(m_ar2,  newdata = test, interval = "prediction")
pred_arx1 <- predict(m_arx1, newdata = test, interval = "prediction")
pred_arx2 <- predict(m_arx2, newdata = test, interval = "prediction")

test_pred <- test %>%
  mutate(
    y = `Unemployment, total`,
    ar1_fit  = pred_ar1[,"fit"],  ar1_lwr  = pred_ar1[,"lwr"],  ar1_upr  = pred_ar1[,"upr"],
    ar2_fit  = pred_ar2[,"fit"],  ar2_lwr  = pred_ar2[,"lwr"],  ar2_upr  = pred_ar2[,"upr"],
    arx1_fit = pred_arx1[,"fit"], arx1_lwr = pred_arx1[,"lwr"], arx1_upr = pred_arx1[,"upr"],
    arx2_fit = pred_arx2[,"fit"], arx2_lwr = pred_arx2[,"lwr"], arx2_upr = pred_arx2[,"upr"]
  )

############################################
## 8. Loss functions (MAE / RMSE / MAPE)
############################################
mae  <- function(y, yhat) mean(abs(y - yhat))
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mape <- function(y, yhat) mean(abs((y - yhat) / y)) * 100

results <- data.frame(
  Model = c("AR(1)", "AR(2)", "AR(1)+CPI", "AR(1)+Inflation"),
  MAE  = c(mae(test_pred$y, test_pred$ar1_fit),
           mae(test_pred$y, test_pred$ar2_fit),
           mae(test_pred$y, test_pred$arx1_fit),
           mae(test_pred$y, test_pred$arx2_fit)),
  RMSE = c(rmse(test_pred$y, test_pred$ar1_fit),
           rmse(test_pred$y, test_pred$ar2_fit),
           rmse(test_pred$y, test_pred$arx1_fit),
           rmse(test_pred$y, test_pred$arx2_fit)),
  MAPE = c(mape(test_pred$y, test_pred$ar1_fit),
           mape(test_pred$y, test_pred$ar2_fit),
           mape(test_pred$y, test_pred$arx1_fit),
           mape(test_pred$y, test_pred$arx2_fit))
)

results

############################################
## 9. Graph: Observed vs Predicted (choose best model after seeing results)
############################################
# Observed vs Predicted Unemployment with Prediction Interval

ggplot(test_pred, aes(x = Date)) +
  geom_line(aes(y = y, color = "Observed"), linewidth = 1) +
  geom_line(aes(y = arx1_fit, color = "Predicted"), linewidth = 1) +
  geom_ribbon(aes(ymin = arx1_lwr, ymax = arx1_upr), alpha = 0.2) +
  labs(
    title = "Unemployment Forecasting (AR(1)+CPI) with Prediction Interval",
    x = "Date",
    y = "Unemployment rate (%)",
    color = ""
  ) +
  theme_minimal()

############################################
## 10. Time-series cross-validation (rolling origin)
############################################
# Rolling one-step ahead prediction errors
rolling_cv <- function(formula, data, start_index) {
  y <- data$`Unemployment, total`
  n <- nrow(data)
  errs <- rep(NA, n)
  for (t in (start_index):(n-1)) {
    train_t <- data[1:t, ]
    test_t  <- data[t+1, , drop = FALSE]
    m <- lm(formula, data = train_t)
    yhat <- predict(m, newdata = test_t)
    errs[t+1] <- y[t+1] - yhat
  }
  errs <- na.omit(errs)
  list(
    MAE  = mean(abs(errs)),
    RMSE = sqrt(mean(errs^2))
  )
}

# choose a reasonable start (e.g., after having enough obs)
start_idx <- 200

cv_ar1  <- rolling_cv(`Unemployment, total` ~ u_lag1, data_model, start_idx)
cv_ar2  <- rolling_cv(`Unemployment, total` ~ u_lag1 + u_lag2, data_model, start_idx)
cv_arx1 <- rolling_cv(`Unemployment, total` ~ u_lag1 + CPI_lag1, data_model, start_idx)
cv_arx2 <- rolling_cv(`Unemployment, total` ~ u_lag1 + infl_lag1, data_model, start_idx)

cv_table <- data.frame(
  Model = c("AR(1)", "AR(2)", "AR(1)+CPI", "AR(1)+Inflation"),
  CV_MAE  = c(cv_ar1$MAE, cv_ar2$MAE, cv_arx1$MAE, cv_arx2$MAE),
  CV_RMSE = c(cv_ar1$RMSE, cv_ar2$RMSE, cv_arx1$RMSE, cv_arx2$RMSE)
)

cv_table
