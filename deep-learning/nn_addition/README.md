# Neural Network Addition
A (silly) neural network that tries to learn addition for numbers from 0-9.

The training data consits of an (n x 2) matrix where n is the size of the training set.
Each row contains integers a,b where a + b <= 9. The label per row is a one-hot vector of
a+b, e.g. 0 -> [1, 0, ...], 1 -> [0, 1, ...], etc. 

The network consists of only one fully connected hidden layer.

The results, so far, are not great. The training accuracy <= 70%. No effort was made to check
validation/test accuracy, or to improve the overall accuracy of the network (for obvious
reasons).

