
import numpy as np
import pandas as pd
import csv
import matplotlib.pyplot as plt
import scipy as sp
from sklearn import datasets, model_selection, neighbors, svm, preprocessing, metrics, feature_extraction
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import SGDClassifier
from sklearn.model_selection import cross_val_score, KFold, ShuffleSplit
from sklearn.pipeline import make_pipeline
from scipy.stats import sem
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.feature_extraction.text import TfidfVectorizer, HashingVectorizer, CountVectorizer
from sklearn import metrics

spam = pd.read_csv("C:\\Users\\Daniel\\Documents\\Git Repos\\MM_spam_nodata\\Data\\fraud-messages-master.csv", encoding = "ISO-8859-1")
ham = pd.read_csv("C:\\Users\\Daniel\\Documents\\Git Repos\\MM_spam_nodata\\Data\\combined-good-messages-012016-072017.csv", encoding = "ISO-8859-1")

wordcounts = pd.read_csv("C:\\Users\\Daniel\\Documents\\Git Repos\\MM_spam_nodata\\Data\\test_counts_0203.csv", encoding = "ISO-8859-1")

clf_5 = make_pipeline(TfidfVectorizer(vocabulary = wordcounts["token"]), MultinomialNB(alpha=0.01))


ham['target'] = 0
spam['target'] = 1

spam = spam[['message', 'target']]
ham = ham[['message', 'target']]

data = pd.concat([ham, spam])
data['message'] = data['message'].astype('str')

X_train, X_test, y_train, y_test = model_selection.train_test_split(data['message'], data['target'], test_size = 0.15, random_state = 42)

fulltable = pd.concat([X_test, y_test], axis = 1, ignore_index = True)

clf_5.fit(X_train, y_train)

y_pred = clf_5.predict(X_test)

metrics.accuracy_score(y_pred, y_test)
pd.crosstab(y_pred, y_test)

