### Packages
pacman::p_load(Mcomp, tidyverse, forecast, tseries, knitr, cowplot, kableExtra, forecTheta)


### Data
data(M3)
id <- 1931
period <- M3[[id]]$period
description <- M3[[id]]$description
x <- M3[[id]]$x
n <- M3[[id]]$n
h <- M3[[id]]$h
xx <- M3[[id]]$xx


### Time series plot
x %>%
  autoplot() +
  labs(x='Ano', y='Valor Observado') +
  theme_bw()


### STL decomposition
x %>%
  mstl() %>%
  autoplot() +
  labs(x='Ano') +
  theme_bw()


### ARIMA
# Diffs
diffs <- c()
if (x %>% ndiffs() == 1) {
  w <- diff(x)
  diffs <- c(diffs, 1)
  if (w %>% nsdiffs() == 1) {
    w <- diff(w, lag=12)
    diffs <- c(diffs, 12)
  }
}
cbind(Original=x, Diferenciada=w) %>%
  autoplot(facets=T) +
  labs(x='Ano', y='Valor Observado') +
  theme_bw()
w %>% adf.test()
# Manual selection
w %>%
  ggAcf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
w %>%
  ggPacf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
# p = 1
# q = 1
# P = 0, 1 ou 2
# Q = 1, 2 ou 3
p <- 1
q <- 1
P <- 0:2
Q <- 1:3
# Without Box-Cox
best_aicc <- Inf
for (Pi in P) {
  for (Qi in Q) {
    mod <- x %>% Arima(order=c(1, 1, 1), seasonal=c(Pi, 0, Qi), include.mean=F, lambda=NULL)
    if (mod$aicc < best_aicc) {
      best_mod <- mod
      best_aicc <- mod$aicc
    }
  }
} 
mod1 <- best_mod
mod1 %>% summary()
# With Box-Cox
best_aicc <- Inf
for (Pi in P) {
  for (Qi in Q) {
    mod <- x %>% Arima(order=c(1, 1, 1), seasonal=c(Pi, 0, Qi), include.mean=F, lambda='auto')
    if (mod$aicc < best_aicc) {
      best_mod <- mod
      best_aicc <- mod$aicc
    }
  }
} 
mod2 <- best_mod
mod2 %>% summary()
# Residual analysis without Box-Cox
res1 <- mod1 %>% residuals()
est1 <- res1 %>%
  autoplot() +
  labs(x='Ano', y='Resíduos') +
  theme_bw()
qq1 <- res1 %>%
  data.frame() %>%
  ggplot(aes(sample=res1)) +
  stat_qq() +
  stat_qq_line() +
  labs(x='Quantis Teóricos', y='Quantis Amostrais') +
  theme_bw()
acf1 <- res1 %>%
  ggAcf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
pacf1 <- res1 %>%
  ggPacf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
plot_grid(est1, qq1, acf1, pacf1, nrow=2)
res1 %>% adf.test() 
res1 %>% Box.test(lag=20, type='Ljung-Box', fitdf=5)
res1 %>% shapiro.test()
mod1 %>% checkresiduals()
# Residual analysis with Box-Cox
res2 <- mod2 %>% residuals()
est2 <- res2 %>%
  autoplot() +
  labs(x='Ano', y='Resíduos') +
  theme_bw()
qq2 <- res2 %>%
  data.frame() %>%
  ggplot(aes(sample=res2)) +
  stat_qq() +
  stat_qq_line() +
  labs(x='Quantis Teóricos', y='Quantis Amostrais') +
  theme_bw()
acf2 <- res2 %>%
  ggAcf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
pacf2 <- res2 %>%
  ggPacf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
plot_grid(est2, qq2, acf2, pacf2, nrow=2)
res2 %>% adf.test()
res2 %>% Box.test(lag=20, type='Ljung-Box', fitdf=5)
res2 %>% shapiro.test()
mod2 %>% checkresiduals()


### ETS
# Auto selection without Box-Cox
mod3 <- x %>% ets()
mod3 %>% summary()
# Auto selection with Box-Cox
mod4 <- x %>% ets(lambda='auto')
mod4 %>% summary()
# Residual analysis without Box-Cox
res3 <- mod3 %>% residuals()
est3 <- res3 %>%
  autoplot() +
  labs(x='Ano', y='Resíduos') +
  theme_bw()
qq3 <- res3 %>%
  data.frame() %>%
  ggplot(aes(sample=res3)) +
  stat_qq() +
  stat_qq_line() +
  labs(x='Quantis Teóricos', y='Quantis Amostrais') +
  theme_bw()
acf3 <- res3 %>%
  ggAcf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
pacf3 <- res3 %>%
  ggPacf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
plot_grid(est3, qq3, acf3, pacf3, nrow=2)
res3 %>% adf.test()
res3 %>% Box.test(lag=20, type='Ljung-Box', fitdf=17)
res3 %>% shapiro.test()
mod3 %>% checkresiduals()
# Residual analysis with Box-Cox
res4 <- mod4 %>% residuals()
est4 <- res4 %>%
  autoplot() +
  labs(x='Ano', y='Resíduos') +
  theme_bw()
qq4 <- res4 %>%
  data.frame() %>%
  ggplot(aes(sample=res4)) +
  stat_qq() +
  stat_qq_line() +
  labs(x='Quantis Teóricos', y='Quantis Amostrais') +
  theme_bw()
acf4 <- res4 %>%
  ggAcf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
pacf4 <- res4 %>%
  ggPacf(lag.max=12*5) +
  labs(title='') +
  theme_bw()
plot_grid(est4, qq4, acf4, pacf4, nrow=2)
res4 %>% adf.test()
res4 %>% Box.test(lag=20, type='Ljung-Box', fitdf=17)
res4 %>% shapiro.test()
mod4 %>% checkresiduals()


### Sliding window validation
f_arima1 <- function(y, h){
  fit <- Arima(y, order=c(1, 1, 1), seasonal=c(1, 0, 2), include.mean=F, lambda=NULL)
  forecast(fit, h)
}
f_arima2 <- function(y, h){
  fit <- Arima(y, order=c(1, 1, 1), seasonal=c(1, 0, 2), include.mean=F, lambda='auto')
  forecast(fit, h)
}
f_ets1 <- function(y, h){
  fit <- ets(y)
  forecast(fit, h)
}
f_ets2 <- function(y, h){
  fit <- ets(y, lambda='auto')
  forecast(fit, h)
}
CV_arima1 <- x %>% tsCV(forecastfunction=f_arima1, h=5, initial=n-14)
CV_arima2 <- x %>% tsCV(forecastfunction=f_arima2, h=5, initial=n-14)
CV_ets1 <- x %>% tsCV(forecastfunction=f_ets1, h=5, initial=n-14)
CV_ets2 <- x %>% tsCV(forecastfunction=f_ets2, h=5, initial=n-14)
MAE_arima1 <- CV_arima1 %>% abs() %>% colMeans(na.rm=T)
MAE_arima2 <- CV_arima2 %>% abs() %>% colMeans(na.rm=T)
MAE_ets1 <- CV_ets1 %>% abs() %>% colMeans(na.rm=T)
MAE_ets2 <- CV_ets2 %>% abs() %>% colMeans(na.rm=T)
tab <- cbind(MAE_arima1, MAE_arima2, MAE_ets1, MAE_ets2)
tab %>%
  kable(
    col.names=c('ARIMA', 'ARIMA + Box-Cox', 'ETS', 'ETS + Box-Cox'),
    caption='MAE por horizonte de predição.',
    digits=2,
    format.args=list(decimal.mark=',', big.mark='.', scientific=F),
    align='c'
  ) %>%
  kable_styling(
    position='center',
    bootstrap_options=c('striped', 'hover', 'condensed', 'responsive')
  )
tab_plot <- tab %>%
  as.data.frame() %>%
  mutate(Horizonte=1:5) %>%
  gather(key='Modelo', value='MAE', -Horizonte)
tab_plot %>%
  ggplot(aes(x=Horizonte, y=MAE)) +
  geom_line(aes(color=Modelo)) + 
  scale_color_manual(
    values=c('black', 'darkred', 'steelblue', 'darkgreen'),
    breaks=c('MAE_arima1', 'MAE_arima2', 'MAE_ets1', 'MAE_ets2'),
    labels=c('ARIMA', 'ARIMA + Box-Cox', 'ETS', 'ETS + Box-Cox')
    ) +
  theme_bw()


### Forecast
# plots
plot_preds <- function(mod, nome='') {
  vec <- c(nome, 'Observado')
  cores <- c('#0000AA', 'red')
  names(cores) <- vec
  preds <- forecast(mod, h=h, level=95)
  plot_obj <- x %>%
    autoplot() + xlab('') + ylab('') + theme_bw() +
    autolayer(preds, series=nome) +
    autolayer(xx, series='Observado') +
    scale_colour_manual(
      values=cores,
      breaks=vec,
      name='')
  return(plot_obj)
}
plot_preds(mod1, 'ARIMA')
plot_preds(mod2, 'ARIMA + Box-Cox')
plot_preds(mod3, 'ETS')
plot_preds(mod4, 'ETS + Box-Cox')
# Benchmark comparison
preds <- list(
  'ARIMA' = forecast(mod1, h=h),
  'ARIMA + Box-Cox' = forecast(mod2, h=h),
  'ETS' = forecast(mod3, h=h),
  'ETS + Box-Cox' = forecast(mod4, h=h),
  'auto.arima' = forecast(auto.arima(x), h=h),
  'SES' = ses(x, h=h),
  'Holt' = holt(x, h=h),
  'sltf' = stlf(x, h=h),
  'BATS' = forecast(bats(x), h=h),
  'TBATS' = forecast(tbats(x), h=h),
  'Bagged ETS' = forecast(baggedETS(x), h=h)
)
mae <- unlist(lapply(preds, function(m) return(mean(abs(xx - m$mean)))))
final <- data.frame(MAE=mae)
final %>%
  kable(
    caption='MAE nos dados de teste.',
    digits=2,
    format.args=list(decimal.mark=',', big.mark='.', scientific=F),
    align='c'
  ) %>%
  kable_styling(
    position='center',
    bootstrap_options=c('striped', 'hover', 'condensed', 'responsive')
  )