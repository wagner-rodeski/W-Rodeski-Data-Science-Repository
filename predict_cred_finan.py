# -*- coding: utf-8 -*-
"""
Created on Tue Dec 19 09:44:42 2017

@author: wagner_rodeski
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from sklearn import preprocessing
from sklearn.preprocessing import LabelEncoder
from sklearn import metrics
from sklearn.preprocessing import MinMaxScaler
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
from sklearn import model_selection
############################################################################################
#   Problem definition
#   Variable Identification (predictors and target)
#   Making Sense of Data
#       Univariate Analysis
#       Bi-variate Analysis
#   Missing values treatment
#   Outlier treatment
#   Feature Engineering
#       Variable transformation
#       Variable creation
#   Modeling
#   Model Evaluation/Selection
#   Deploy
############################################################################################


############################################################################################
############################################################################################
# testar:
#    isa 
#    uso de certos produtos em meses anteriores ou com recorrencia
#    saldo (atual, anterior, média, etc)
#    volumetrias para produtos do isa (talvez mc?)
#    cnae/cbo (fazer algum tipo de clustering)
#    patrimonio
#    médias ou outras métricas que agreguem comportamento histórico/tendência
#
#o que leva a contratação de um cred finan?
#o que se pode observar em alguem que contrata o prod (persona)
#
#premissa: as pessoas se não poupam, então alavancam
#    formas de alavancar:
#        /curtissimo prazo/
#            [chq_esp] >> por muita necessidade, acidentalmente, momentaneamente
#                gastos sob controle mas tem problema no fluxo de caixa (terá chq com recor mas em vol pequeno)
#                gastos fora de controle, vendendo o almoço pra comprar a janta
#        /curto/
#            [cartao_cred/cred_finan] >> aquisição de bens que ainda não possui / pgto de pequenas dívidas ou desp
#        /médio/
#            [cred_finan] >>  aquisição de bens que ainda não possui / pgto de pequenas dívidas ou desp
#        /médio/longo/
#            [cred_finan/consórcio] >> upgrade de bens ou aquisição de grandes bens que ainda não possui
#            [consórcio] >> sem pressa, legal ter mas não necessário agora
#        
#/veículos/camihoes/motos/educação/imovel/construção-reforma/
#
#idade ativa (basicamente exclui idoso)
#conomimicamente ativo (tem que ter fonte de renda/emprego)
#quem tem algum investimento não deveria ser necessariamente excluido, pois pode
#usar o investimento como 'entrada'
#
#
############################################################################################
############################################################################################

#------------------------------------------------------------------------------
# lendo os dados para train & test
base = pd.read_excel ('sample_40k_seed_123321_vShort2.xlsx')
base = pd.read_excel ('sample_40k_seed_123321.xlsx')
base.head()

#------------------------------------------------------------------------------
# lendo os dados para predição (usar mesmo nome de df pra reaproveitar os códigos de cleansing)
base = pd.read_excel ('dados_pred.xlsx')

#------------------------------------------------------------------------------
# verificando missing data
base.isnull().any()
base.isnull().sum(axis=0)
base[pd.isnull(base['DIAS_SEM_MOVIMENTO'])].groupby('PROD_CRED_FINANC').count()
base[pd.isnull(base['SCR'])].groupby('PROD_CRED_FINANC').count()
base[pd.isnull(base['FAIXA_RISCO'])].groupby('PROD_CRED_FINANC').count()

#------------------------------------------------------------------------------
# eliminando missing data por coluna (isso elimina somente Nan de alguma col espec.) 
base=base[pd.notnull(base['DIAS_SEM_MOVIMENTO'])]
base.isnull().sum(axis=0)
base=base[pd.notnull(base['IDADE'])]
base.isnull().sum(axis=0)
base=base[pd.notnull(base['FAIXA_RISCO'])]
base.isnull().sum(axis=0)
base=base[pd.notnull(base['PUBLICO_ESTRATEGICO'])]
base.isnull().sum(axis=0)

base.shape
# eliminando todos missing data
#base=base.dropna()
#base.isnull().sum(axis=0)
#base.shape

base['SCR'].unique().tolist()
base['SCR'] = base['SCR'].fillna('N_SCR')
base['SCR'].unique().tolist()
base.isnull().sum(axis=0)
base.shape
base.info()
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------explorando os dados--------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
base.columns.tolist()
base.describe()

#PROD_CREDITO_RURAL não tem nenhuma informação, vou dropar

base=base.drop('PROD_CREDITO_RURAL', axis=1)
base.columns.tolist()
base.shape

base.groupby('PROD_CRED_FINANC').mean()

#------------------------------------------------------------------------------
# contagem valores absolutos (vazio ou "0" pois representam o False)
base['PROD_CRED_FINANC'].value_counts()
#------------------------------------------------------------------------------
# contagem valores relativos (uso o "1" pois ele representa o True)
base['PROD_CRED_FINANC'].value_counts(1)
#------------------------------------------------------------------------------
###############################################################################
## visualizando os dados

sns.countplot(x='PROD_CRED_FINANC', data=base, palette='hls')
plt.show()

sns.countplot(y="FAIXA_RISCO", data=base)
plt.show()

pd.crosstab(base.FAIXA_RISCO, base.PROD_CRED_FINANC, normalize='index', margins=True)
pd.crosstab(base.FAIXA_RISCO, base.PROD_CRED_FINANC).plot(kind='bar', stacked=True)

##
pd.crosstab(base.FAIXA_RISCO, base.PROD_CRED_FINANC, normalize='index', margins=True).plot(kind='bar', stacked=True)
plt.title('ipp cred_finan por fx risco')
plt.xlabel('fx risco')
plt.ylabel('penetração cred_finan')
plt.savefig('risco_bar_chart')
##

base.hist(bins=10,figsize=(13,20))

sns.heatmap(base.corr())
plt.show()

base.corr() # sem indícios para testar colinearidade
# base.corr()['PROD_PAGAMENTOS']

base_1 = base._get_numeric_data() #drop non-numeric cols
corr=np.corrcoef(base_1, rowvar=0)
w, v = np.linalg.eig(corr)        # eigen values & eigen vectors
w # nenhum auto valor próximo de zero

##
###############################################################################

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------tratando dados e variáveis-------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

## criando categorias para variáveis contínuas para posteriormente tranformá-las em dummy

base['DIAS_SEM_MOVIMENTO'].hist()
base.loc[base['DIAS_SEM_MOVIMENTO'] <= 30, 'DIAS_SEM_MOVIMENTO'] = 1
base.loc[(base['DIAS_SEM_MOVIMENTO'] <= 60) & (base['DIAS_SEM_MOVIMENTO'] > 30)
, 'DIAS_SEM_MOVIMENTO'] = 2
base.loc[(base['DIAS_SEM_MOVIMENTO'] <= 90) & (base['DIAS_SEM_MOVIMENTO'] > 60)
, 'DIAS_SEM_MOVIMENTO'] = 3
base.loc[(base['DIAS_SEM_MOVIMENTO'] <= 180) & (base['DIAS_SEM_MOVIMENTO'] > 90)
, 'DIAS_SEM_MOVIMENTO'] = 4
base.loc[base['DIAS_SEM_MOVIMENTO'] > 180, 'DIAS_SEM_MOVIMENTO'] = 5
base['DIAS_SEM_MOVIMENTO'].unique().tolist()

base['IDADE'].hist()
base.loc[base['IDADE'] <= 18, 'IDADE'] = 1
base.loc[(base['IDADE'] <= 30) & (base['IDADE'] > 18), 'IDADE'] = 2
base.loc[(base['IDADE'] <= 40) & (base['IDADE'] > 30), 'IDADE'] = 3
base.loc[(base['IDADE'] <= 50) & (base['IDADE'] > 40), 'IDADE'] = 4
base.loc[base['IDADE'] > 50, 'IDADE'] = 5
base['IDADE'].unique().tolist()


## transformando variáveis categóricas em numéricas

# conhecendo todas as categorias das var cat
labels = pd.DataFrame([                          
base['FLG_SEXO'].unique(),
base['PORTE_PADRAO'].unique(),
base['PUBLICO_ESTRATEGICO'].unique(),
base['SCR'].unique(),
base['IDADE'].unique(),
base['FAIXA_RISCO'].unique()]).T
labels.columns=['FLG_SEXO','PORTE_PADRAO','PUBLICO_ESTRATEGICO','SCR','IDADE','FAIXA_RISCO']
labels

# enconding

le1 = preprocessing.LabelEncoder()
le2 = preprocessing.LabelEncoder()
le3 = preprocessing.LabelEncoder()
le4 = preprocessing.LabelEncoder()
le5 = preprocessing.LabelEncoder()
le6 = preprocessing.LabelEncoder()

base['FLG_SEXO'] = le1.fit_transform(base['FLG_SEXO'])
base['PORTE_PADRAO'] = le2.fit_transform(base['PORTE_PADRAO'])
base['PUBLICO_ESTRATEGICO'] = le3.fit_transform(base['PUBLICO_ESTRATEGICO'])
base['SCR'] = le4.fit_transform(base['SCR'])
base['IDADE'] = le5.fit_transform(base['IDADE'])
base['FAIXA_RISCO'] = le6.fit_transform(base['FAIXA_RISCO'])

labels_encoded = pd.DataFrame([
le1.classes_,
le2.classes_,
le3.classes_,
le4.classes_,
le5.classes_,
le6.classes_]).T
labels_encoded.columns=['FLG_SEXO','PORTE_PADRAO','PUBLICO_ESTRATEGICO','SCR','IDADE','FAIXA_RISCO']
labels_encoded

# le1.classes_.tolist()
# le2.classes_.tolist()
# le3.classes_.tolist()
# le4.classes_.tolist()
# le5.classes_.tolist()

#------------------------------------------------------------------------------
# eliminando associados em prejuízo
base = base[base.FAIXA_RISCO != 0]
base.shape
base['FAIXA_RISCO'].unique()
#------------------------------------------------------------------------------
# transformando variáveis categóricas em variáveis dummy
# pq???

cat_vars = ['FLG_SEXO','PORTE_PADRAO','PUBLICO_ESTRATEGICO','SCR','IDADE','FAIXA_RISCO']
for var in cat_vars:
    cat_list ='var'+'_'+var
    cat_list = pd.get_dummies(base[var], prefix=var)
    base_temp = base.join(cat_list)
    base = base_temp
    
base = base.drop(['FLG_SEXO','PORTE_PADRAO','PUBLICO_ESTRATEGICO','SCR','IDADE','FAIXA_RISCO'], axis=1)

base.columns.tolist()
#------------------------------------------------------------------------------
# definindo variável resposta e variáveis preditoras

base_vars = base.columns.values.tolist()
k=['PROD_CRED_FINANC']
t=[i for i in base_vars if i not in k]
y=base['PROD_CRED_FINANC']
X=base[t]
#------------------------------------------------------------------------------
# definindo dados de trainning e test


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)
#------------------------------------------------------------------------------
# treinando modelo de regressão logística

######### normalizando dados
scaler = MinMaxScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)
####################################

######### logit
logit_model=sm.Logit(y,X)
result=logit_model.fit()
result.summary()
####################################

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
modelCV = LogisticRegression()
scoring = 'accuracy'
results = model_selection.cross_val_score(modelCV, X_train, y_train, cv=kfold, scoring=scoring)
results.mean()
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------feature_selection----------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# selecionando variáveis


model = LogisticRegression()
lr = RFE(model, 30)
lr = lr.fit(X, y)
lr.support_
lr.ranking_
#------------------------------------------------------------------------------
# montando vetor com variáveis selecionadas

w=pd.DataFrame(t, columns=['produtos'])
w
q=pd.DataFrame(lr.support_, columns=['boolean'])
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
modelCV_2 = LogisticRegression()
scoring_2 = 'accuracy'
results_2 = model_selection.cross_val_score(modelCV_2, X_train_2, y_train, cv=kfold_2, scoring=scoring_2)
results_2.mean()

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------precision/recall/confusion-------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

print(classification_report(y_test, logreg.predict(X_test)))
print(classification_report(y_test, rf.predict(X_test)))

y_pred_1 = logreg.predict(X_test)
y_pred_2 = rf.predict(X_test)

logreg_cm = metrics.confusion_matrix(y_pred_1, y_test, [1,0])
rf_cm = metrics.confusion_matrix(y_pred_2, y_test, [1,0])

sns.heatmap(logreg_cm, annot=True, fmt='.2f',xticklabels = ["tem_cred_finan", "n_tem_cred_finan"] , yticklabels = ["tem_cred_finan", "n_tem_cred_finan"] )
plt.ylabel('True class')
plt.xlabel('Predicted class')
plt.title('Reg Log')

sns.heatmap(rf_cm, annot=True, fmt='.2f',xticklabels = ["tem_cred_finan", "n_tem_cred_finan"] , yticklabels = ["tem_cred_finan", "n_tem_cred_finan"] )
plt.ylabel('True class')
plt.xlabel('Predicted class')
plt.title('Random Forest')
# plt.savefig('random_forest')

a=logreg_cm
a=rf_cm

a[0,0]/(a[0,0]+a[0,1]) # TPR (precision 1) (ROC >melhor)
a[0,1]/(a[0,0]+a[0,1]) # FNR
a[1,1]/(a[1,0]+a[1,1]) # TNR (precision 0) (1-FPR)
a[1,0]/(a[1,0]+a[1,1]) # FPR (ROC) (1-TNR <melhor)
[a[0,0]/(a[0,0]+a[0,1]),a[1,1]/(a[1,0]+a[1,1])] # acertos
[a[1,0]/(a[1,0]+a[1,1]),a[0,1]/(a[0,0]+a[0,1])] # erros

# para modelagem de oferta de produto vou querer alto TPR e baixo FNR
[a[0,0]/(a[0,0]+a[0,1]),a[0,1]/(a[0,0]+a[0,1])]


#         precision
# 0       TN (=1-FPR(ROC))
# 1       TP (=TPR->ROC)

# confusion matrix
# TP  FN
# FP  TN


# PRECISION: tp / (tp + fp), the ability of the classifier not to label as positive a sample that is negative.
#   percent of positive classifications that are truly positive

# RECALL: tp / (tp + fn), the ability of the classifier to find all the positive samples.
#   percent of truly positive instances that were classified as such

# F1: média hamônica de precisione e recall

## minha interpretação:
##   -> precision: olha só os true, isto é, avalia a capacidade de acertar
                  

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#---------------------roc_curve------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


logit_roc_auc = roc_auc_score(y_test, logreg.predict(X_test))
fpr, tpr, thresholds = roc_curve(y_test, logreg.predict_proba(X_test)[:,1])
rf_roc_auc = roc_auc_score(y_test, rf.predict(X_test))
roc_auc_score(y_test, rf.predict(X_test))
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
# plt.savefig('ROC')
plt.show()


logit_roc_auc = roc_auc_score(y_test, logreg_2.predict(X_test_2))
fpr, tpr, thresholds = roc_curve(y_test, logreg_2.predict_proba(X_test_2)[:,1])
rf_roc_auc = roc_auc_score(y_test, rf_2.predict(X_test_2))
roc_auc_score(y_test, rf_2.predict(X_test_2))
rf_fpr, rf_tpr, rf_thresholds = roc_curve(y_test, rf_2.predict_proba(X_test_2)[:,1])
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
# plt.savefig('ROC')
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
#------------------------------------------------------------------------------

pred = pd.DataFrame(columns=[X.columns])
pred = pred.append(base)
pred.fillna(value=0, inplace=True)

pred_vf1 = pd.DataFrame(rf.predict(pred))
pred_vf1.hist()
pred_vf1.sum()/pred_vf1.count()
pred_vf2 = pd.DataFrame(logreg.predict(pred))
pred_vf2.hist()
pred_vf2.sum()/pred_vf2.count()
