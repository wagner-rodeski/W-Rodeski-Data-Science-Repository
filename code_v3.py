# -*- coding: utf-8 -*-
"""
Created on Tue Nov 21 14:12:24 2017

@author: wagner_rodeski
"""

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
import seaborn as sns

import statsmodels.api as sm

from sklearn import preprocessing
from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_val_score
from sklearn.feature_selection import RFE

from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve

#------------------------------------------------------------------------------
# lendo os dados para train & test
base_0 = pd.read_excel ('sample_500k_seed_123321.xlsx')
base_0.head()
base = pd.DataFrame()
base = base_0.append(base)


base_0[base_0['NUM_CPF_CNPJ'] == 26997665004]
temp_0 = base_0['COD_COOPERATIVA'].map(str) + "-" + base_0['NUM_CPF_CNPJ'].map(str)
base_temp = base_0.assign(teste = base_0['COD_COOPERATIVA'].map(str) + "-" + base_0['NUM_CPF_CNPJ'].map(str))




base.drop(['COD_COOP','NUM_CPF_CNPJ'], axis=1, inplace=True)
#------------------------------------------------------------------------------
# conhecendo mais os dados
base.shape
base.columns.tolist()
base.describe()
base.groupby('PROD_POUPANCA').mean()
base.info()
#------------------------------------------------------------------------------
# verificando missing data
base.isnull().any()
base.isnull().sum(axis=0)
#------------------------------------------------------------------------------
# eliminando missing data por coluna (caso queria eliminar somente Nan de alguma col espec.) 
base=base[pd.notnull(base['RENDA_MENSAL'])]
base.isnull().sum(axis=0)
base=base[pd.notnull(base['FAIXA_RISCO'])]
base.isnull().sum(axis=0)
base=base[pd.notnull(base['IDADE'])]
base.isnull().sum(axis=0)
base.shape
# eliminando todos missing data
base=base.dropna()
base.isnull().sum(axis=0)
base.shape
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------explorando os dados--------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
base.shape
base['FAIXA_RISCO'].unique()
base.describe()
base.groupby('PROD_POUPANCA').mean()
#------------------------------------------------------------------------------
# contagem valores absolutos (vazio ou "0" pois representam o False)
base['PROD_POUPANCA'].value_counts()
#------------------------------------------------------------------------------
# contagem valores relativos (uso o "1" pois ele representa o True)
base['PROD_POUPANCA'].value_counts(1)
base.groupby('PROD_POUPANCA').mean()
#------------------------------------------------------------------------------
# visualizando os dados

sns.countplot(x='PROD_POUPANCA', data=base, palette='hls')
plt.show()

sns.countplot(y="FAIXA_RISCO", data=base_0)
plt.show()

pd.crosstab(base.FAIXA_RISCO, base.PROD_POUPANCA, normalize='index', margins=True)
pd.crosstab(base.FAIXA_RISCO, base.PROD_POUPANCA).plot(kind='bar', stacked=True)
plt.title('ipp poupança por fx risco')
plt.xlabel('fx risco')
plt.ylabel('penetração poupança')
plt.savefig('department_bar_chart')

table=pd.crosstab(base.FAIXA_RISCO, base.PROD_POUPANCA)
table.div(table.sum(1).astype(float), axis=0).plot(kind='bar', stacked=True)

base.hist(bins=10,figsize=(13,20))

sns.heatmap(base.corr())
plt.show()

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------tratando dados e variáveis-------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# diminuindo as categorias da 'faixa de risco'

base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '6_Baixo2','baixo',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '7_Baixo1','baixo',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '8_Baixíssimo','baixo',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '2_Alto2','alto',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '1_Altíssimo','alto',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '3_Alto1','alto',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '5_Médio1','medio',base['FAIXA_RISCO'])
base['FAIXA_RISCO']=np.where(base['FAIXA_RISCO'] == '4_Médio2','medio',base['FAIXA_RISCO'])
#------------------------------------------------------------------------------
# eliminando associados em prejuízo
base = base[base.FAIXA_RISCO != '0_Default']
base['FAIXA_RISCO'].unique()
base.shape
#------------------------------------------------------------------------------
# transformando variáveis categóricas em variáveis dummy

cat_vars = ['FAIXA_RISCO']
for var in cat_vars:
    cat_list ='var'+'_'+var
    cat_list = pd.get_dummies(base[var], prefix=var)
    base_temp = base.join(cat_list)
    base = base_temp
base.drop(base.columns[0], axis=1, inplace=True)
base.columns.tolist()
#------------------------------------------------------------------------------
# definindo variável resposta e variáveis preditoras

base_vars = base.columns.values.tolist()
k=['PROD_POUPANCA']
t=[i for i in base_vars if i not in k]
y=base['PROD_POUPANCA']
X=base[t]
#------------------------------------------------------------------------------
# definindo dados de trainning e test


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)
#------------------------------------------------------------------------------
# treinando modelo de regressão logística

logit_model=sm.Logit(y,X)
result=logit_model.fit()
result.summary()

logreg = LogisticRegression()
logreg.fit(X_train, y_train)


accuracy_score(y_test, logreg.predict(X_test))
#------------------------------------------------------------------------------
# treinando modelo de random forrest


rf = RandomForestClassifier(n_jobs=-1, n_estimators=30, max_features=16)
rf.fit(X_train, y_train)

accuracy_score(y_test, rf.predict(X_test))

#------------------------------------------------------------------------------
# treinando modelo com cross validation


kfold = model_selection.KFold(n_splits=10, random_state=7)
modelCV = RandomForestClassifier()
scoring = 'accuracy'
results = model_selection.cross_val_score(modelCV, X_train, y_train, cv=kfold, scoring=scoring)
results.mean()
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------feature_selection----------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# selecionando variáveis


model = RandomForestClassifier()
rfe = RFE(model, 16)
rfe = rfe.fit(X, y)
rfe.support_
rfe.ranking_
#------------------------------------------------------------------------------
# montando vetor com variáveis selecionadas

w=pd.DataFrame(t, columns=['produtos'])
w
q=pd.DataFrame(rfe.support_, columns=['boolean'])
q
a = w.join(q)
a
a['boolean']=np.where(a['boolean'] == True,'1','0')
a=a['produtos'].loc[a['boolean'] == '1']
a
#------------------------------------------------------------------------------
t_2=a.values.tolist()
X_2=base[t_2]
#------------------------------------------------------------------------------
# definindo dados de trainning e test

X_train_2, X_test_2, y_train, y_test = train_test_split(X_2, y, test_size=0.3, random_state=0)
#------------------------------------------------------------------------------
# treinando modelo de regressão logística

logreg_2 = LogisticRegression()
logreg_2.fit(X_train_2, y_train)
accuracy_score(y_test, logreg_2.predict(X_test_2))

#------------------------------------------------------------------------------
# treinando modelo de random forrest

rf_2 = RandomForestClassifier(n_jobs=-1, n_estimators=50)
rf_2.fit(X_train_2, y_train)
accuracy_score(y_test, rf_2.predict(X_test_2))

#------------------------------------------------------------------------------
# treinando modelo com cross validation

kfold_2 = model_selection.KFold(n_splits=10, random_state=7)
modelCV_2 = RandomForestClassifier()
scoring_2 = 'accuracy'
results_2 = model_selection.cross_val_score(modelCV_2, X_train_2, y_train, cv=kfold_2, scoring=scoring_2)
results_2.mean()

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------precision/recall/confusion------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# random forrest

print(classification_report(y_test, rf.predict(X_test)))

y_pred = rf.predict(X_test)

forest_cm = metrics.confusion_matrix(y_pred, y_test, [1,0])
sns.heatmap(forest_cm, annot=True, fmt='.2f',xticklabels = ["tem_poup", "n_tem_poup"] , yticklabels = ["tem_poup", "n_tem_poup"] )
plt.ylabel('True class')
plt.xlabel('Predicted class')
plt.title('Random Forest')
plt.savefig('random_forest')

#The precision is the ratio tp / (tp + fp) where tp is the number of true positives and fp the number of false positives. The precision is intuitively the ability of the classifier to not label a sample as positive if it is negative.
#The recall is the ratio tp / (tp + fn) where tp is the number of true positives and fn the number of false negatives. The recall is intuitively the ability of the classifier to find all the positive samples.
#The F-beta score can be interpreted as a weighted harmonic mean of the precision and recall, where an F-beta score reaches its best value at 1 and worst score at 0.
#The F-beta score weights the recall more than the precision by a factor of beta. beta = 1.0 means recall and precision are equally important.
#The support is the number of occurrences of each class in y_test.

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------roc_curve------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


logit_roc_auc = roc_auc_score(y_test, logreg.predict(X_test))
fpr, tpr, thresholds = roc_curve(y_test, logreg.predict_proba(X_test)[:,1])
rf_roc_auc = roc_auc_score(y_test, rf.predict(X_test))
rf_fpr, rf_tpr, rf_thresholds = roc_curve(y_test, rf.predict_proba(X_test)[:,1])
plt.figure()
plt.plot(fpr, tpr, label='Logistic Regression (area = %0.2f)' % logit_roc_auc)
plt.plot(rf_fpr, rf_tpr, label='Random Forest (area = %0.2f)' % rf_roc_auc)
plt.plot([0, 1], [0, 1],'r--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic')
plt.legend(loc="lower right")
plt.savefig('ROC')
plt.show()
#------------------------------------------------------------------------------
#---------------------features_importance--------------------------------------
#------------------------------------------------------------------------------
feature_labels = np.array(t)
importance = rf.feature_importances_
feature_indexes_by_importance = importance.argsort()
for index in feature_indexes_by_importance:
    print('{}-{:.2f}%'.format(feature_labels[index], (importance[index] *100.0)))

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------predizendo um novo assoc---------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

pred_0 = pd.read_excel ('pred.xlsx')
pred_0.head()
pred_1 = pd.DataFrame()
pred_1 = pred_1.append(pred_0)
pred_1.drop(['COD_COOP','NUM_CPF_CNPJ','PROD_POUPANCA'], axis=1, inplace=True)
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '6_Baixo2','baixo',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '7_Baixo1','baixo',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '8_Baixíssimo','baixo',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '2_Alto2','alto',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '1_Altíssimo','alto',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '3_Alto1','alto',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '5_Médio1','medio',pred_1['FAIXA_RISCO'])
pred_1['FAIXA_RISCO']=np.where(pred_1['FAIXA_RISCO'] == '4_Médio2','medio',pred_1['FAIXA_RISCO'])
pred_1 = pred_1[pred_1.FAIXA_RISCO != '0_Default']
#------------------------------------------------------------------------------
# gerando vetor/matriz para previsão
pred_cat_vars = ['FAIXA_RISCO']
for var in pred_cat_vars:
    pred_cat_list ='var'+'_'+var
    pred_cat_list = pd.get_dummies(pred_1[var], prefix=var)
    pred_1 = pred_1.join(pred_cat_list)    
pred_1.drop('FAIXA_RISCO', axis=1, inplace=True)

pred = pd.DataFrame(columns=[X.columns])
pred = pred.append(pred_1)
pred.fillna(value=0, inplace=True)
#------------------------------------------------------------------------------
# fazendo a predição
rf.predict(pred)[0]
