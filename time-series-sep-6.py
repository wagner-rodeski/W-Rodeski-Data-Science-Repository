# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np
import matplotlib.pyplot as plt
from fbprophet import Prophet
import pandas as pd

dataset = pd.read_csv("../input/sep-dataset-4/20161231_20190207_v1.csv",infer_datetime_format=True,
                      parse_dates = ['DATA'])  

#teste = pd.read_csv("../input/sep-dataset-3/20161231_20190131_v1.csv")  
#teste['DATA'] = pd.to_datetime(teste['DATA'],format='%Y%m%d')
#teste.head()

dataset.tail()

daily_df = dataset[['DATA','TRN_TOTAL']][dataset['FL_ATIVO'] == 1]
daily_df.head()
daily_df.tail()
#daily_df = daily_df[daily_df['DATA'] <= '2018-12-31']
#daily_df = daily_df[daily_df['DATA'] >= '2016-12-31']
daily_df = daily_df.groupby('DATA')['TRN_TOTAL'].sum()
daily_df = daily_df.resample('D').apply(sum)
df = daily_df.reset_index()
df.columns = ['ds', 'y']
df.head()
df.tail()


hldys1 = pd.DataFrame({
  'holiday': 'queda',
  'ds': pd.to_datetime([
                        '2017-02-28', '2017-04-14', '2017-04-16', '2017-04-21', '2017-05-01', '2017-06-15', 
                        '2017-09-07', '2017-10-12', '2017-11-02', '2017-11-15',
                        '2018-02-13', '2018-03-30', '2018-04-01', '2018-04-21', '2018-05-01', '2018-05-31',
                        '2018-09-07', '2018-10-12', '2018-11-02', '2018-11-15']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys2 = pd.DataFrame({
  'holiday': 'alta_sep',
  'ds': pd.to_datetime(['2017-12-20','2018-12-20']),
  'lower_window': 0,
  'upper_window': 2,
})

hldys3 = pd.DataFrame({
  'holiday': 'especial',
  'ds': pd.to_datetime(['2017-12-25','2018-01-01','2018-12-25','2019-01-01']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys4 = pd.DataFrame({
  'holiday': 'alta',
  'ds': pd.to_datetime(['2017-05-13', '2017-06-10', '2017-08-12', '2017-11-24', '2018-05-12', '2018-06-12', '2018-08-11', '2018-11-23']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys = pd.concat((hldys1, hldys2, hldys3, hldys4))


prediction_size = 39
train_df = df.copy()
#train_df = df[:-prediction_size]
train_df['y'] = np.log(train_df['y'])
train_df.tail(n=6)

j = 31 #16  # weekly seasonality prior_scale
i = 46 #31  # monthly seasonality prior_scale
b = 31 #16  # seasonality prior_scale
c = 16 #50  # holidays seasonality prior_scale
m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
            daily_seasonality=False, holidays = hldys, holidays_prior_scale = c,
           seasonality_prior_scale = b, seasonality_mode = 'additive',
           changepoint_prior_scale = 0.1, changepoint_range=0.99,
           ).add_seasonality(
            name='w',period=7,fourier_order=10, prior_scale = j).add_seasonality(
            name='m',period=30.5,fourier_order=20, prior_scale = i).add_seasonality(
            name='ano',period=365,fourier_order=40, prior_scale = 1.3*i)
m.fit(train_df)

future = m.make_future_dataframe(periods=prediction_size)
future.tail(n=3)

forecast = m.predict(future)
a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
a['yhat'] = np.exp(a['yhat'])
'''
from fbprophet.plot import add_changepoints_to_plot
fig = m.plot(forecast)
h = add_changepoints_to_plot(fig.gca(), m, forecast)
'''
# m.plot_components(forecast)

b = dataset.set_index('DATA')['2019-02-01':'2019-02-07']['TRN_TOTAL'].sum()
c = a['2019-02-08':'2019-02-28']['yhat'].sum()
est = b + c
est
'''
a['2019-02-01':'2019-02-28']['yhat']
a['2019-02-01':'2019-02-28']['yhat'].sum()

a['2018-10-01':'2018-10-31']['yhat'].sum()
a['2018-10-01':'2018-10-31']['y'].sum()
a['2018-11-01':'2018-11-30']['yhat'].sum()
a['2018-11-01':'2018-11-30']['y'].sum()
a['2018-12-01':'2018-12-31']['yhat'].sum()
a['2018-12-01':'2018-12-31']['y'].sum()

a['2017-10-01':'2017-10-31']['y'].sum()
a['2017-11-01':'2017-11-30']['y'].sum()
a['2017-12-01':'2017-12-31']['y'].sum()
a['2018-01-01':'2018-01-31']['y'].sum()

a['2018-01-01':'2018-01-08']['y'].sum()



a['2019-01-01':'2019-01-31']['yhat'].sum()

a['2017-12-01':'2017-12-31']['y'].sum()
a['2018-01-01':'2018-01-31']['y'].sum()


#------------------------------------------------


import matplotlib.pyplot
fig = matplotlib.pyplot.gcf()
fig, ax = plt.subplots(figsize=(15, 8))
plt.plot(dataset.set_index('DATA')['2019-02-01':'2019-02-07']['TRN_TOTAL'].resample('D').apply(sum), '-b', label='Real')
plt.plot(a['2019-02-01':'2019-02-28']['yhat'],'--r', label='Predict')
ax.set_xlabel('qqqqqqqq',fontsize=14)
ax.set_ylabel('tttttttt',fontsize=14)
ax.set_title('kkkk',fontsize=14)
ax.legend()

fig = matplotlib.pyplot.gcf()
fig.set_size_inches(18.5, 10.5)
plt.plot(a['2018-12-01':'2019-01-31']['yhat'])

#------------------------------------------------

monthly_df = daily_df.resample('M').apply(sum)
plt.plot(monthly_df)
plt.show()

a.loc['2018-12-14']['yhat']
daily_df['2018-12-12']
daily_df
(2545712-2474360)/2474360

np.exp(forecast[['holidays']][(forecast['holidays']) < 0])
np.exp(forecast[['holidays']][(forecast['holidays']) > 0])
np.exp(2)
forecast[['ds','holidays']][(forecast['holidays']) < 0].sum()
forecast[['ds','holidays']][(forecast['holidays']) > 0].sum()
forecast

#m.plot(forecast)
#m.plot_components(forecast)
###### mean percentual error

a['diff'] = (a['y']-a['yhat'])/a['y']
print(a['diff']['2018-12-01':].mean()*100)
plt.plot(a['2018-12-01':'2018-12-31']['y'])
plt.plot(a['2018-12-01':'2018-12-31']['yhat'])
###### mean absolute error
print(a['2018-12-01':]['y'].sum())
print(a['2018-12-01':]['yhat'].sum())
print((1-a['2018-12-01':]['yhat'].sum()/a['2018-12-01':]['y'].sum())*100)

'''