#-------------------------------------
library(prophet)
library(tidyverse)
library(rlang)
library(Rcpp)
#-------------------------------------

# Ler dados
dataset <- read.csv('C:/Sharepoint/OneDrive - Saque e Pague/PROJECAO/Outputs/20161231_20200130.csv', sep = ',')%>%
  arrange(DATA)

tail(dataset, 6)

#-------------------------------------
# Agrupar por data somando terminal ativo e inativo
db <- dataset %>%
  filter(FL_ATIVO == 1)%>%
  group_by(DATA)%>%
  summarise(TRN_TOTAL = sum(TRN_TOTAL))%>%
  ungroup()%>%
  rename('ds'='DATA',
         'y'='TRN_TOTAL')%>%
  mutate(ds = as.Date(strptime(ds, "%Y%m%d")))


#-------------------------------------
# Definir datas especiais
hldys1 <- data.frame(
  holiday = rep('queda', 31),
  ds = as.Date(c('2017-02-28', '2017-04-14', '2017-04-16', '2017-04-21', '2017-05-01', 
                 '2017-06-15', '2017-09-07', '2017-10-12', '2017-11-02', '2017-11-15',
                 '2018-02-12', '2018-02-13', '2018-03-30', '2018-04-01', '2018-04-21', 
                 '2018-05-01', '2018-05-31', '2018-09-07', '2018-10-12', '2018-11-02', 
                 '2018-11-15', '2019-03-04', '2019-03-05', '2019-04-19', '2019-04-21', 
                 '2019-05-01', '2019-06-20', '2019-09-07', '2019-10-12','2019-11-02',
                 '2019-11-15')),
  lower_window = 0,
  upper_window = 0
)

hldys2 = data.frame(
  holiday = rep('alta_sep', 3),
  ds = as.Date(c('2017-12-20','2018-12-20','2019-12-20')),
  lower_window = 0,
  upper_window = 2
)

hldys3 = data.frame(
  holiday = rep('especial', 7),
  ds = as.Date(c('2017-12-25','2018-01-01','2018-12-25','2019-01-01','2019-12-25','2020-01-01','2020-12-25')),
  lower_window = 0,
  upper_window = 0
)


hldys4 = data.frame(
  holiday = rep('alta', 19),
  ds = as.Date(c('2017-02-24','2017-03-01','2017-03-02', '2017-06-10', '2017-08-12', '2017-11-24', #'2017-05-13',
                 '2018-02-09','2018-02-14','2018-02-15', '2018-06-12', '2018-08-11', '2018-11-23',#'2018-05-12',
                 '2019-03-01','2019-03-06','2019-03-07','2019-05-11','2019-06-12','2019-08-10','2019-11-29')),
  lower_window = 0,
  upper_window = 0
)

hldys = rbind(hldys1, hldys2, hldys3, hldys4)

rm(hldys1, hldys2, hldys3, hldys4)

#-------------------------------------

prediction_size = 61

train_df <- db %>% 
  mutate(y = log(y))

train_df %>% tail(6)

sum(tail(train_df$y,12))

glimpse(train_df)


#-------------------------------------
## modelo otimizado para rodar intra mês, isto é, ser preciso a partir do dia 08 a 15

j = 31 #16  # weekly seasonality prior_scale
i = 46 #31  # monthly seasonality prior_scale
b = 31 #16  # seasonality prior_scale
c = 46 #50  # holidays seasonality prior_scale

m <- prophet(yearly.seasonality = FALSE,
             weekly.seasonality = FALSE,
             growth = 'linear',
             daily.seasonality = FALSE,
             holidays = hldys,
             holidays.prior.scale = c,
             seasonality.prior.scale = b,
             seasonality.mode = 'additive',
             changepoint.prior.scale = 0.99,
             changepoint.range = 0.59
)

m <- add_seasonality(m, name = 'Weekly', period = 7, fourier.order = 50, prior.scale = 2*j)
m <- add_seasonality(m, name = 'Montly', period = 30.5, fourier.order = 20, prior.scale = i)
m <- add_seasonality(m, name = 'Yearly', period = 365, fourier.order = 40, prior.scale = 1.3*i)


m <- fit.prophet(m, train_df)

future <- make_future_dataframe(m, periods = prediction_size, include_history = TRUE)

forecast <- predict(m, future)

a <- forecast %>%
  select(ds, yhat)%>%
  mutate(pred = exp(yhat))%>%
  mutate(ds = as.Date(strptime(ds, "%Y-%m-%d")))


#-------------------------------------
# Projecao
b <- dataset %>% filter(DATA %in% (20200201:20200201)) %>% summarise(sum(TRN_TOTAL))
c <- a %>% filter(between(ds, as.Date('2020-02-01'), as.Date('2020-02-29'))) %>% summarise(sum(pred))
b
c
b + c

#-------------------------------------
