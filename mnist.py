import numpy as np

# train - number of train data for each digit
# test - number of tet data for each digit
# maxnumber - get digits from 0 to this number, default 0-9
# norm - perform normalization or not (-1 to 1)
def get_data(train=200,test=100,maxnumber=9, norm = False):
    x_train=[]
    y_train=[]
    x_test=[]
    y_test=[]
    for i in range(maxnumber+1):
        x = np.genfromtxt("data/mnist_digit_" +str(i) + ".csv",max_rows=test+train)
        x_train.append(x[0:train,:])
        y_train.append(np.array([i] * train))
        y_test.append(np.array([i] * test))
        x_test.append(x[-1:-test-1:-1,:])
    x_train = np.concatenate(x_train)
    x_test = np.concatenate(x_test)
    y_train = np.concatenate(y_train)
    y_test = np.concatenate(y_test)    
    if norm:
        x_train = normalizedData(x_train)
        x_test = normalizedData(x_test)

    return x_train, y_train,  x_test, y_test


def normalizedData(l):
	return 2*l / 255 - 1

