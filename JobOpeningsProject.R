library(TSA)
library(astsa)
library(forecast)
library(vars)

# Load in data and create time series object
jobs = read.csv('https://dxl-datasets.s3.amazonaws.com/data/industrial_data.csv')
openings = ts(jobs$opening, start=c(2002, 1), frequency=12)

# Train test split for accuracy
openings_train = window(openings, start=c(2002, 1), end=c(2020,12))
openings_test = window(openings, start=2021)

plot(openings)
plot(diff(openings))

# STL model
m_stl = stl(openings_train, s.window = 'periodic')
plot(m_stl)

f_stl = forecast(m_stl, h=12)$mean

autoplot(openings_test) +
  autolayer(f_stl)

# ARIMA model
plot(openings_train)
plot(diff(openings_train))

acf2(openings_train)
eacf(openings_train)

m_arima = Arima(openings_train, order = c(0,1,3), seasonal = c(0, 0, 1), include.constant = T )
summary(m_arima)

f_arima = forecast(m_arima, h = 12)$mean

autoplot(openings_test) +
  autolayer(f_stl) +
  autolayer(f_arima)

# LINEAR
m_lin = tslm(openings_train ~ poly(trend, 3) + season) 
summary(m_lin)

f_lin = forecast(m_lin, h = 12)$mean

autoplot(openings_test) +
  autolayer(f_stl) +
  autolayer(f_arima) +
  autolayer(f_lin)

# LINEAR TS
res = m_lin$residuals

acf2(res) 
eacf(res)

arima_for_resids = Arima(res, order = c(2, 0, 1))
f_resids = forecast(arima_for_resids, h=12)$mean

f_lin_ts = f_lin + f_resids

autoplot(openings_test) +
  autolayer(f_stl) +
  autolayer(f_arima) +
  autolayer(f_lin) +
  autolayer(f_lin_ts)

# ETS
m_ets = ets(openings_train, model='ZZZ')
plot(m_ets)

f_ets = forecast(m_ets, h=12)$mean

autoplot(openings_test) +
  autolayer(f_stl) +
  autolayer(f_arima) +
  autolayer(f_lin) +
  autolayer(f_lin_ts) +
  autolayer(f_ets)

# Test for accuracy
acc = rbind(
  data.frame(accuracy(f_lin, openings_test), row.names='Linear'),
  data.frame(accuracy(f_lin_ts, openings_test), row.names='Linear TS'),
  data.frame(accuracy(f_stl, openings_test), row.names='STL'),
  data.frame(accuracy(f_ets, openings_test), row.names='ETS'),
  data.frame(accuracy(f_arima, openings_test), row.names='ARIMA')
)

acc$Model = rownames(acc)

ggplot(acc, aes(x=reorder(Model, RMSE), y=RMSE)) + # can change y to RMSE
  geom_col(fill = 'blue') + 
  coord_flip() + 
  labs(x='Model', y = 'RMSE')

# Average of two best models - Linear and Linear TS
f_avg = (f_lin + f_lin_ts)/2
f_avg

# Final forecast from Linear TS (best model)
m_lin_final = tslm(openings ~ poly(trend, 3) + season) 
summary(m_lin_final)

f_lin_final = forecast(m_lin_final, h = 12)$mean

res_final = m_lin_final$residuals

acf2(res_final) 
eacf(res_final)

arima_for_resids_final = Arima(res_final, order = c(1, 0, 2))
summary(arima_for_resids_final)

f_resids_final = forecast(arima_for_resids_final, h=12)$mean

f_lin_ts_final = f_lin_final + f_resids_final
f_lin_ts_final

autoplot(openings) +
  autolayer(f_lin_ts_final)

plot(f_lin_ts_final, col = 'coral3', lwd = 2)  

# Export forecasted values as csv
write.csv(f_lin_ts_final, file = "Forecasted_Values_2022.csv")
