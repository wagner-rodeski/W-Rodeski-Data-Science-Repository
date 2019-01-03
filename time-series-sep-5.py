# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np
import matplotlib.pyplot as plt
from fbprophet import Prophet
import pandas as pd

daily_df = pd.read_csv("../input/730d_dataset.csv",infer_datetime_format=True,
                      parse_dates = ['DATA'])
daily_df.head()
#daily_df = daily_df[daily_df['DATA'] >= '2017-12-01']
#daily_df = daily_df[daily_df['DATA'] <= '2017-12-31']
#daily_df = daily_df.groupby('DATA')['TRN_TOTAL'].sum()
#daily_df = daily_df.resample('D').apply(sum)
#plt.plot(daily_df)
#plt.show()
#daily_df.head()
#daily_df.tail()
#daily_df.info()

hldys1 = pd.DataFrame({
  'holiday': 'national',
  'ds': pd.to_datetime(['2017-10-12', '2017-11-02', '2017-11-15',
                        '2018-02-13', '2018-03-30','2017-12-24','2018-12-24','2017-12-31','2018-12-31',
                       '2018-04-01', '2018-04-21', '2018-05-01', '2018-05-13', '2018-05-31',
                       '2018-08-12', '2018-09-07', '2018-10-12', '2018-11-02', '2018-11-15','2018-11-23']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys2 = pd.DataFrame({
  'holiday': 'sep',
  'ds': pd.to_datetime(['2017-12-20','2018-12-20']),
  'lower_window': 0,
  'upper_window': 2,
})

hldys3 = pd.DataFrame({
  'holiday': 'esp',
  'ds': pd.to_datetime(['2017-12-25','2018-01-01','2018-12-25','2019-01-01']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys = pd.concat((hldys1, hldys2, hldys3))

#hldys.head()

daily_df = daily_df.groupby('DATA')['TRN_TOTAL'].sum()
daily_df = daily_df.resample('D').apply(sum)
#plt.plot(daily_df)
#plt.show()

#weekly_df = daily_df.resample('W').apply(sum)
#plt.plot(weekly_df)
#plt.show()

#monthly_df = daily_df.resample('M').apply(sum)
#plt.plot(monthly_df)
#plt.figure(figsize = [30,10])
#plt.show()

df = daily_df.reset_index()
df.columns = ['ds', 'y']

"""
#begin----------------------------------------------- hyperparameter tunning

#from dateutil import parser, relativedelta

#z = pd.DataFrame(columns=['indice1','indice2','indice3','indice4','size','diff','diff2'])
#est_ini = '2018-11-01'
#est_fim = '2018-11-30'
#prediction_size1 = (parser.parse(est_fim) - parser.parse(est_ini)).days

#for j in range (1,15,15):
#    for i in range (1,15,15):
#        for b in range (1,15,15):
#            for c in range (1,15,15):
#                for d in range (1,15,15):
#                    fim_data_train = parser.parse(est_ini) - relativedelta.relativedelta(days=d)
#                    train_df = df[df['ds'] <= fim_data_train]
#                    m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
#                                daily_seasonality=False, holidays = hldys, holidays_prior_scale = c,
#                               seasonality_prior_scale = b, seasonality_mode = 'additive',
#                               changepoint_prior_scale = 0.9).add_seasonality(
#                                name='w',period=7,fourier_order=10, prior_scale = j).add_seasonality(
#                                name='m',period=30.5,fourier_order=10, prior_scale = i)
#                    m.fit(train_df)
#                    prediction_size2 = (parser.parse(est_fim) - fim_data_train).days
#                    future = m.make_future_dataframe(periods=prediction_size2)
#                    forecast = m.predict(future)
#                    a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
#                    a.reset_index(inplace = True)
#                    y = a['y'][a['ds'].between(est_ini,est_fim)].sum()
#                    yhat = a['yhat'][a['ds'].between(est_ini,est_fim)].sum()
#                    dif = abs(y - yhat)
#                    dif2 = abs(y - yhat)/abs(y)
#                    z = z.append({'indice1':j,'indice2':i,'indice3':b,'indice4':c,'size':d,
#                                  'diff':round(dif),'diff2':dif2},ignore_index=True )
#plt.plot(z['diff2'])
#z[z['size'] > 1].sort_values('diff2')
#z.sort_values('diff2')
###### 
#x = a['y'][a['ds'].between(est_ini,est_fim)].sum()
#y = a['yhat'][a['ds'].between(est_ini,est_fim)].sum()
#print(x)
#print(y)
#print(y-x)
#print((x-y)/x)
#end----------------------------------------------- hyperparameter tunning
"""
#begin----------------------------------------------- testing choosen hyperparameters

prediction_size = 13
train_df = df[:-prediction_size]
train_df.tail(n=3)
j = 16  # weekly seasonality prior_scale
i = 31  # monthly seasonality prior_scale
b = 16  # seasonality prior_scale
c = 1   # holidays seasonality prior_scale
m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
            daily_seasonality=False, holidays = hldys, holidays_prior_scale = c,
           seasonality_prior_scale = b, seasonality_mode = 'additive',
           changepoint_prior_scale = 0.9).add_seasonality(
            name='w',period=7,fourier_order=10, prior_scale = j).add_seasonality(
            name='m',period=30.5,fourier_order=10, prior_scale = i)
m.fit(train_df)

future = m.make_future_dataframe(periods=prediction_size)
future.tail(n=3)

forecast = m.predict(future)
forecast[['ds','yhat']].tail(n=5)
m.plot(forecast)
m.plot_components(forecast)
###### mean percentual error
a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
a['diff'] = (a['y']-a['yhat'])/a['y']
print(a['diff']['2018-12-01':].mean()*100)
plt.plot(a['2018-12-01':]['y'])
plt.plot(a['2018-12-01':]['yhat'])
###### mean absolute error
print(a['2018-12-01':]['y'].sum())
print(a['2018-12-01':]['yhat'].sum())
print((1-a['2018-12-01':]['yhat'].sum()/a['2018-12-01':]['y'].sum())*100)
###### combined percentual error
plt.plot(a['2018-12-01':]['y'])
plt.plot(a['2018-12-01':]['yhat'])
b = a['2018-12-01':'2018-12-05']['y'].sum()
c = a['2018-12-06':'2018-12-13']['yhat'].sum()
est = b + c
real = a['2018-12-01':]['y'].sum()
print(a['2018-12-01':]['y'].sum())
print(c)
print(real)
print(np.around(est, decimals = 1))
print(np.around((1-est/real)*100, decimals = 1),'%')

#end----------------------------------------------- testing choosen hyperparameters
"""
#begin----------------------------------------------- testing predictions for real

daily_df = pd.read_csv("../input/730d_dataset.csv",infer_datetime_format=True,
                      parse_dates = ['DATA'])
#daily_df = daily_df[daily_df['DATA'] <= '2018-11-05']
daily_df = daily_df.groupby('DATA')['TRN_TOTAL'].sum()
daily_df = daily_df.resample('D').apply(sum)
df = daily_df.reset_index()
df.columns = ['ds', 'y']
df.tail()

hldys1 = pd.DataFrame({
  'holiday': 'national',
  'ds': pd.to_datetime(['2017-10-12', '2017-11-02', '2017-11-15',
                        '2018-02-13', '2018-03-30','2017-12-24','2018-12-24','2017-12-31','2018-12-31',
                       '2018-04-01', '2018-04-21', '2018-05-01', '2018-05-13', '2018-05-31',
                       '2018-08-12', '2018-09-07', '2018-10-12', '2018-11-02', '2018-11-15','2018-11-23']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys2 = pd.DataFrame({
  'holiday': 'sep',
  'ds': pd.to_datetime(['2017-12-20','2018-12-20']),
  'lower_window': 0,
  'upper_window': 2,
})

hldys3 = pd.DataFrame({
  'holiday': 'esp',
  'ds': pd.to_datetime(['2017-12-25','2018-01-01','2018-12-25','2019-01-01']),
  'lower_window': 0,
  'upper_window': 0,
})

hldys = pd.concat((hldys1, hldys2, hldys3))

prediction_size = 29
train_df = df.copy()
#train_df = df[:-prediction_size]
train_df['y'] = np.log(train_df['y'])
train_df.tail(n=3)

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
            name='ano',period=364.5,fourier_order=20, prior_scale = i)
m.fit(train_df)

future = m.make_future_dataframe(periods=prediction_size)
future.tail(n=3)

forecast = m.predict(future)
a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
a['yhat'] = np.exp(a['yhat'])

from fbprophet.plot import add_changepoints_to_plot
fig = m.plot(forecast)
h = add_changepoints_to_plot(fig.gca(), m, forecast)


m.plot_components(forecast)
b = a['2018-12-01':'2018-12-23']['y'].sum()
c = a['2018-12-24':'2018-12-31']['yhat'].sum()
est = b + c
est
a['2019-01-01':'2019-01-31']['yhat']
a['2019-01-01':'2019-01-31']['yhat'].sum()

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


##### linha crescente linear a partir de out/18 - parece que não pegou a caida que tem em janeiro

######### versão dados reais ate 13/12/18 ###############
# real ate 02/12:
c = a['2018-12-03':'2018-12-31']['yhat'].sum()
240232 + c 
# previsto 4.576.897,9

# real ate 09/12:
c = a['2018-12-10':'2018-12-31']['yhat'].sum()
1383030 + c # este real obtido do produção está diferente do que tenho na base de treino
# previsto 4.579.742,0

# real ate 16/12:
c = a['2018-12-17':'2018-12-31']['yhat'].sum()
2383032 + c 
# previsto 4.600.814,4

# real ate 23/12:
c = a['2018-12-24':'2018-12-31']['yhat'].sum()
3505407 + c 
# previsto 4.635.959,5
######### versão dados reais ate 13/12/18 ###############

a['2019-01-01':'2019-01-31']['yhat'].sum()

a['2017-12-01':'2017-12-31']['y'].sum()
a['2018-01-01':'2018-01-31']['y'].sum()


#------------------------------------------------

import matplotlib.pyplot
fig = matplotlib.pyplot.gcf()

fig = matplotlib.pyplot.gcf()
fig.set_size_inches(18.5, 10.5)
plt.plot(a['2017-12-15':'2018-01-02']['y'])

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
a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
a['diff'] = (a['y']-a['yhat'])/a['y']
print(a['diff']['2018-12-01':].mean()*100)
plt.plot(a['2018-12-01':]['y'])
plt.plot(a['2018-12-01':]['yhat'])
###### mean absolute error
print(a['2018-12-01':]['y'].sum())
print(a['2018-12-01':]['yhat'].sum())
print((1-a['2018-12-01':]['yhat'].sum()/a['2018-12-01':]['y'].sum())*100)
###### combined percentual error
plt.plot(a['2018-12-01':]['y'])
plt.plot(a['2018-12-01':]['yhat'])
b = a['2018-12-01':'2018-12-' + str(w)]['y'].sum()
c = a['2018-12-' + str(w+1):'2018-12-31']['yhat'].sum()
est = b + c
real = a['2018-12-01':]['y'].sum()
est
print(est)
print(real)
print(np.around(est, decimals = 1))
print(np.around((1-est/real)*100, decimals = 1),'%')

#end----------------------------------------------- testing predictions for real
"""
"""
#begin----------------------------------------------- testing variation along the month

############## NOV-18

z = pd.DataFrame(columns=['data_est','est','var','real'])
real_of = df.set_index('ds')['2018-11-01':'2018-11-30'].sum()

for i in range (1,29):
    prediction_size = 44-i
    train_df = df[:-prediction_size]
    m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
                daily_seasonality=False, holidays = hldys, holidays_prior_scale = 1,
               seasonality_prior_scale = 16, seasonality_mode = 'additive',
               changepoint_prior_scale = 0.9).add_seasonality(
                name='w',period=7,fourier_order=10, prior_scale = 16).add_seasonality(
                name='m',period=30.5,fourier_order=10, prior_scale = 31)
    m.fit(train_df)
    future = m.make_future_dataframe(periods=prediction_size)
    forecast = m.predict(future)
    a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
    real = a['2018-11-01':'2018-11-'+ str(i)]['y'].sum()
    est_par = a['2018-11-' + str(i + 1):'2018-11-30']['yhat'].sum()
    var_t = np.around((1-(real+est_par)/real_of)*100, decimals = 1)
    z = z.append({'data_est':'2018-11-' + str(i + 1),'est':real + est_par,'var':var_t,'real':real_of},ignore_index=True )
print(z['var'].mean())
plt.plot(z['var'])
plt.plot(z['est'])
plt.plot(z['real'])
plt.plot(a['2018-11-01':'2018-11-30']['y'])
plt.plot(a['2018-11-01':'2018-11-30']['yhat'])

############## OUT-18

z = pd.DataFrame(columns=['data_est','est','var','real'])
real_of = df.set_index('ds')['2018-10-01':'2018-10-31'].sum()

for i in range (1,30):
    prediction_size = 75-i
    train_df = df[:-prediction_size]
    m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
                daily_seasonality=False, holidays = hldys, holidays_prior_scale = 1,
               seasonality_prior_scale = 16, seasonality_mode = 'additive',
               changepoint_prior_scale = 0.9).add_seasonality(
                name='w',period=7,fourier_order=10, prior_scale = 16).add_seasonality(
                name='m',period=30.5,fourier_order=10, prior_scale = 31)
    m.fit(train_df)
    future = m.make_future_dataframe(periods=prediction_size)
    forecast = m.predict(future)
    a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
    real = a['2018-10-01':'2018-10-'+ str(i)]['y'].sum()
    est_par = a['2018-10-' + str(i + 1):'2018-10-31']['yhat'].sum()
    var_t = np.around((1-(real+est_par)/real_of)*100, decimals = 1)
    z = z.append({'data_est':'2018-10-' + str(i + 1),'est':real + est_par,'var':var_t,'real':real_of},ignore_index=True )
print(z['var'].mean())
plt.plot(z['var'])
plt.plot(z['est'])
plt.plot(z['real'])
plt.plot(a['2018-10-01':'2018-10-31']['y'])
plt.plot(a['2018-10-01':'2018-10-31']['yhat'])

############## SET-18

z = pd.DataFrame(columns=['data_est','est','var','real'])
real_of = df.set_index('ds')['2018-09-01':'2018-09-30'].sum()

for i in range (1,29):
    prediction_size = 105-i
    train_df = df[:-prediction_size]
    m = Prophet(yearly_seasonality=False,weekly_seasonality=False,growth = 'linear',
                daily_seasonality=False, holidays = hldys, holidays_prior_scale = 1,
               seasonality_prior_scale = 16, seasonality_mode = 'additive',
               changepoint_prior_scale = 0.9).add_seasonality(
                name='w',period=7,fourier_order=10, prior_scale = 16).add_seasonality(
                name='m',period=30.5,fourier_order=10, prior_scale = 31)
    m.fit(train_df)
    future = m.make_future_dataframe(periods=prediction_size)
    forecast = m.predict(future)
    a = forecast.set_index('ds')[['yhat']].join(df.set_index('ds'))
    real = a['2018-09-01':'2018-09-'+ str(i)]['y'].sum()
    est_par = a['2018-09-' + str(i + 1):'2018-09-30']['yhat'].sum()
    var_t = np.around((1-(real+est_par)/real_of)*100, decimals = 1)
    z = z.append({'data_est':'2018-09-' + str(i + 1),'est':real + est_par,'var':var_t,'real':real_of},ignore_index=True )
print(z['var'].mean())
plt.plot(z['var'])
plt.plot(z['est'])
plt.plot(z['real'])
plt.plot(a['2018-09-01':'2018-09-30']['y'])
plt.plot(a['2018-09-01':'2018-09-30']['yhat'])

#end----------------------------------------------- testing variation along the month
"""
#begin---------------------------------------------- EDA
daily_df = pd.read_csv("../input/500d_dataset_v2.csv",infer_datetime_format=True,
                      parse_dates = ['DATA'])

w1 = 9
w2 = 9
w3 = 30

nov17 = daily_df[daily_df['DATA'] >= '2017-' + str(w1) + '-01']
nov17 = nov17[nov17['DATA'] <= '2017-' + str(w2) + '-' + str(w3)]
nov17 = nov17.groupby('DATA')['TRN_TOTAL'].sum()
nov17 = nov17.resample('D').apply(sum)

nov18 = daily_df[daily_df['DATA'] >= '2018-' + str(w1) + '-01']
nov18 = nov18[nov18['DATA'] <= '2018-' + str(w2) + '-' + str(w3)]
nov18 = nov18.groupby('DATA')['TRN_TOTAL'].sum()
nov18 = nov18.resample('D').apply(sum)

k17 = nov17.reset_index()
k18 = nov18.reset_index()
print(k18['TRN_TOTAL'].sum() - k17['TRN_TOTAL'].sum())
plt.plot(k17['TRN_TOTAL'])
plt.plot(k18['TRN_TOTAL'])
plt.show()

k17 = nov17.reset_index()
k17
k17.loc[k17['TRN_TOTAL'].count()] = ['qqq','200000']
w4 = k17['TRN_TOTAL'].count()-1
w4
for i in range (w4,0,-1):
    k17.iloc[i] = k17.iloc[i-1]
k18