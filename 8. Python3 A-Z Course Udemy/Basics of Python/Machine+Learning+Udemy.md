

```python
# Import the essential libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

```


```python
# Import the dataset
df = pd.read_csv("C:\\Users\\sohail.ahmad\\Desktop\\Machine Learning A-Z Template Folder\\Part 1 - Data Preprocessing\\Data.csv")
df.dtypes
```




    Country       object
    Age          float64
    Salary       float64
    Purchased     object
    dtype: object




```python
# 1st double dot means we take all the rows, :-1 means we take all the columns except the last one.
# Here we make the matrix of the independent variable
X = df.iloc[:, :-1].values
X
```




    array([['France', 44.0, 72000.0],
           ['Spain', 27.0, 48000.0],
           ['Germany', 30.0, 54000.0],
           ['Spain', 38.0, 61000.0],
           ['Germany', 40.0, nan],
           ['France', 35.0, 58000.0],
           ['Spain', nan, 52000.0],
           ['France', 48.0, 79000.0],
           ['Germany', 50.0, 83000.0],
           ['France', 37.0, 67000.0]], dtype=object)




```python
Y = df.iloc[:, 3].values
Y
```




    array(['No', 'Yes', 'No', 'No', 'Yes', 'Yes', 'No', 'Yes', 'No', 'Yes'], dtype=object)




```python
# CHecking the missing values column-wise. 2nd and 3rd (1,2) columns have misisng values
df.isnull().sum()
```




    Country      0
    Age          1
    Salary       1
    Purchased    0
    dtype: int64




```python
# Taking care of missing values
from sklearn.preprocessing import Imputer
imputer = Imputer(missing_values = "NaN", strategy="mean", axis=0)

# Here we're taking only 2nd and 3rd columns (indexes 1 & 2 since python starts counting with zero) since they've missing values
imputer = imputer.fit(X[:, 1:3])     # imputer object is fitted to matrix x
X[:,1:3] = imputer.transform(X[:,1:3])   #THis will replace the missing values with mean of the column
X
```




    array([['France', 44.0, 72000.0],
           ['Spain', 27.0, 48000.0],
           ['Germany', 30.0, 54000.0],
           ['Spain', 38.0, 61000.0],
           ['Germany', 40.0, 63777.77777777778],
           ['France', 35.0, 58000.0],
           ['Spain', 38.77777777777778, 52000.0],
           ['France', 48.0, 79000.0],
           ['Germany', 50.0, 83000.0],
           ['France', 37.0, 67000.0]], dtype=object)




```python
# Encoding categorical variables
from sklearn.preprocessing import LabelEncoder
labelencoder_X = LabelEncoder()
X[:,0] = labelencoder_X.fit_transform(X[:,0]) #Here we are encoding only the 1st column State which is categorical
X
# This has basically encoded states into 0,1,2. Here python will think France (0) is less than Spain (1)
# So we wont use this. Instead, we would use one hot encoding
```




    array([[0L, 44.0, 72000.0],
           [2L, 27.0, 48000.0],
           [1L, 30.0, 54000.0],
           [2L, 38.0, 61000.0],
           [1L, 40.0, 63777.77777777778],
           [0L, 35.0, 58000.0],
           [2L, 38.77777777777778, 52000.0],
           [0L, 48.0, 79000.0],
           [1L, 50.0, 83000.0],
           [0L, 37.0, 67000.0]], dtype=object)




```python

from sklearn.preprocessing import LabelEncoder, OneHotEncoder

onehotencoder = OneHotEncoder(categorical_features = [0])
X = onehotencoder.fit_transform(X).toarray()
X
```




    array([[  1.0e+00,   0.0e+00,   0.0e+00,   4.4e+01,   7.2e+04],
           [  0.0e+00,   0.0e+00,   1.0e+00,   2.7e+01,   4.8e+04],
           [  0.0e+00,   1.0e+00,   0.0e+00,   3.0e+01,   5.4e+04],
           [  0.0e+00,   0.0e+00,   1.0e+00,   3.8e+01,   6.1e+04],
           [  0.0e+00,   1.0e+00,   0.0e+00,   4.0e+01,   6.4e+04],
           [  1.0e+00,   0.0e+00,   0.0e+00,   3.5e+01,   5.8e+04],
           [  0.0e+00,   0.0e+00,   1.0e+00,   3.9e+01,   5.2e+04],
           [  1.0e+00,   0.0e+00,   0.0e+00,   4.8e+01,   7.9e+04],
           [  0.0e+00,   1.0e+00,   0.0e+00,   5.0e+01,   8.3e+04],
           [  1.0e+00,   0.0e+00,   0.0e+00,   3.7e+01,   6.7e+04]])




```python
# One hot encoding for the dependent variable as well
labelencoder_y = LabelEncoder()
Y = labelencoder_y.fit_transform(Y)
Y
```




    array([0, 1, 0, 0, 1, 1, 0, 1, 0, 1], dtype=int64)




```python
# Splitting the dataset into training and test set
#We've mentioned test size as 20% so train would be automatically 80%, random_state is same as set.seed in R
from sklearn.cross_validation import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size=0.2, random_state=100)
X_train
```


```python
#Feature Scaling
# For training set we need to fit and transform the data. FOr test set, we only need to transform
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)

# All the variables seem to be on the same scale. We have done feature scaling because so many models use euclidean distance.
# Model wont be biased after feature scaling if the model uses euclidean distnace. FS also makes the convergence easier.
# We've done FS for dependent variable because it is binary. it doesnt need.
X_train
```


```python
X_train
```




    array([[-1.  ,  2.65, -0.77,  0.26,  0.12],
           [ 1.  , -0.38, -0.77, -0.25,  0.46],
           [-1.  , -0.38,  1.29, -1.98, -1.53],
           [-1.  , -0.38,  1.29,  0.05, -1.11],
           [ 1.  , -0.38, -0.77,  1.64,  1.72],
           [-1.  , -0.38,  1.29, -0.08, -0.17],
           [ 1.  , -0.38, -0.77,  0.95,  0.99],
           [ 1.  , -0.38, -0.77, -0.6 , -0.48]])


