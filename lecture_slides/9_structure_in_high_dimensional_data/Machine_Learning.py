
from pandas import read_csv
url = "https://raw.githubusercontent.com/jbrownlee/Datasets/master/iris.csv"
names = ['sepal-length', 'sepal-width', 'petal-length', 'petal-width', 'species']
data = read_csv(url, names = names)

url

data.head(20)

data.describe()

data.shape

data.groupby('species').size()

from matplotlib import pyplot

data.plot(kind = 'box')
pyplot.show()

data.hist()
pyplot.show()


from pandas.plotting import scatter_matrix
scatter_matrix(data)
pyplot.show()

from seaborn import pairplot
pairplot(data, hue = "species")
pyplot.show()

from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold

from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import matthews_corrcoef
from sklearn.metrics import cohen_kappa_score
from sklearn.metrics import make_scorer

from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC

array = data.values
x = array[:, 0:4]
y = array[:, 4]

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.20, random_state = 1)
  # random_state = allows you to repeat the same "random" split again and again,
  # delete to permit true randomness or change to another integer to get a different split

x_test.shape

models = []
models.append(('LR', LogisticRegression(max_iter = 1000)))
models.append(('LDA', LinearDiscriminantAnalysis()))
models.append(('KNN', KNeighborsClassifier()))
models.append(('CART', DecisionTreeClassifier()))
models.append(('NB', GaussianNB()))
models.append(('SVM', SVC(gamma = 'auto')))
print(models)

results = []
names = []
for name, model in models:
    kfold = StratifiedKFold(n_splits = 10, random_state = 1, shuffle = True)
      # random_state = allows you to repeat the same "random" split again and again,
      # delete to permit true randomness or change to another integer to get a different split
    cv_results = cross_val_score(model, x_train, y_train, cv = kfold, scoring = make_scorer(matthews_corrcoef))
    results.append(cv_results)
    names.append(name)
    print('%s: %f (%f)' % (name, cv_results.mean(), cv_results.std()))
from pandas import DataFrame
results_data = DataFrame(results, index = names)
results_data

pyplot.boxplot(results, labels = names)
pyplot.title('Algorithm Comparison')
pyplot.show()

from seaborn import stripplot
reshaped_data = (results_data
                 .stack()
                 .reset_index()
                 .rename(columns={'level_0': 'name', 0: 'result'})
                )
stripplot(x = 'name', y = 'result', data = reshaped_data, jitter = 0.25)
results_data2 = results_data.transpose()
results_data2.hist()
pyplot.show()

model = SVC(gamma = 'auto')
model.fit(x_train, y_train)
predictions = model.predict(x_test)

print('MCC', matthews_corrcoef(y_test, predictions))
print('Accuracy', accuracy_score(y_test, predictions))
print('kappa', cohen_kappa_score(y_test, predictions))


print(confusion_matrix(y_test, predictions))

model = KNeighborsClassifier()
model.fit(x_train, y_train)
predictions = model.predict(x_test)

print('MCC', matthews_corrcoef(y_test, predictions))
print('Accuracy', accuracy_score(y_test, predictions))
print('kappa', cohen_kappa_score(y_test, predictions))

print(confusion_matrix(y_test, predictions))