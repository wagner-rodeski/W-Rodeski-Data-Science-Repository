import numpy as np
from scipy import stats
import statsmodels.api as sm
import matplotlib.pyplot as plt
from fbprophet import Prophet

import pandas as pd
from datetime import datetime
daily_df = pd.read_csv("../input/25w_dataset1.csv",infer_datetime_format=True,
                      parse_dates = ['DATA'])
daily_df.head()

daily_df.info()

#daily_df = pd.read_excel('teste.xlsx')
#daily_df.head(10)

hldys = pd.DataFrame({
  'holiday': 'publish',
  'ds': pd.to_datetime(['2018-08-12', '2018-09-07', '2018-10-12', '2018-11-02', '2018-11-15']),
  'lower_window': -1,
  'upper_window': 0,
})
hldys.head()


#daily_df.groupby('EC').sum().sort_values('TRN_TOTAL')
#daily_df = daily_df[daily_df['EC'] == 'SUPER MERCADO NAZARE']


#daily_df.groupby('CEP').sum().sort_values('TRN_TOTAL')
#daily_df = daily_df[daily_df['CEP'] == '66023000']

# usando todas as transações
daily_df = daily_df.groupby('DATA')['TRN_TOTAL'].sum()
daily_df = daily_df.resample('D').apply(sum)
plt.plot(daily_df)
plt.show()

#daily_df = daily_df[:'2018-09-30']
#daily_df.tail()
#daily_df2 = daily_df['2018-10-01':'2018-10-31']
#plt.plot(daily_df2)
#plt.show()

# usando somente transações com perfil numerário
# daily_df = daily_df.groupby('DATA')['TRN_NUM_SIM'].sum()
# daily_df = daily_df.resample('D').apply(sum)
# plt.plot(daily_df)
# plt.show()

# usando somente transações SEM perfil numerário
# daily_df = daily_df.groupby('DATA')['TRN_NUM_NAO'].sum()
# daily_df = daily_df.resample('D').apply(sum)
# plt.plot(daily_df)
# plt.show()

weekly_df = daily_df.resample('W').apply(sum)
plt.plot(weekly_df)
plt.show()

#########################################################################
df = daily_df.reset_index()
df.columns = ['ds', 'y']
prediction_size = 15
train_df = df[:-prediction_size]
train_df.tail(n=3)

# def inverse_boxcox(y, lambda_):
#     return np.exp(y) if lambda_ == 0 else np.exp(np.log(lambda_ * y + 1) / lambda_)

# train_df2 = train_df.copy().set_index('ds')
# train_df2['y'], lambda_prophet = stats.boxcox(train_df2['y'])
# train_df2.reset_index(inplace=True)

# m2 = Prophet(yearly_seasonality=False,weekly_seasonality=False,
#             daily_seasonality=False).add_seasonality(
#             name='w',period=7,fourier_order=20).add_seasonality(
#             name='m',period=30,fourier_order=20)
# m2.fit(train_df2)
# future2 = m2.make_future_dataframe(periods=prediction_size)
# forecast2 = m2.predict(future2)
# m2.plot(forecast2)

# forecast2['yhat'] = inverse_boxcox(forecast2['yhat'],lambda_prophet)

m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
            daily_seasonality=False, holidays = hldys, holidays_prior_scale = 40,
           seasonality_prior_scale = 40, seasonality_mode = 'multiplicative').add_seasonality(
            name='w',period=7,fourier_order=20, prior_scale = 20).add_seasonality(
            name='m',period=30,fourier_order=20)
m.fit(train_df)

future = m.make_future_dataframe(periods=prediction_size)
future.tail(n=3)

forecast = m.predict(future)
forecast.tail(n=5)

m.plot(forecast)
m.plot_components(forecast)

a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
a['diff'] = (a['y']-a['yhat'])/a['y']*100
######     % de erro      ########################################
#1-(a['diff'].sum()/a['y'].sum())
a['diff'].mean()

# a = forecast2.set_index('ds')[['yhat']].join(df.set_index('ds'))
# a['diff'] = a['y'] - inverse_boxcox(a['yhat'],lambda_prophet)
# ######     % de erro      ########################################
# 1-(a['diff'].sum()/a['y'].sum())

a['2018-11-01':]['y'].sum()
a.loc['2018-11-27']
#forecast.set_index('ds')['2018-11-01':]['yhat'].sum()
forecast.set_index('ds').loc['2018-11-27']['yhat']

# b = inverse_boxcox(a['yhat'],lambda_prophet)
# #b.head()
# b['2018-11-01':].sum()

forecast['yhat'][forecast['ds'] == '2018-11-26'].sum()
a['2018-10-01':'2018-10-23']['y'].sum() + forecast['yhat'][forecast['ds'] > '2018-10-23'].sum()

help(Prophet)
