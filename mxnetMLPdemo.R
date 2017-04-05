library(mlbench)
library(mxnet)

data(Sonar, package="mlbench") #Load data set

Sonar[,61] = as.numeric(Sonar[,61]) - 1 #Set target column to numeric
train.ind = c(1:50, 100:150) #Create index to split train/test sets
train.x = data.matrix(Sonar[train.ind, 1:60]) #Create training set from index
train.y = Sonar[train.ind, 61] #Class for training set
test.x = data.matrix(Sonar[-train.ind, 1:60]) #Create test set from index
test.y = Sonar[-train.ind, 61] #Class for test set

##Multi-layer Perceptron
mx.set.seed(0)

model <- mx.mlp(train.x, 
                train.y, 
                hidden_node = 10, 
                out_node = 2, 
                out_activation = "softmax",
                num.round = 20, 
                array.batch.size = 15, 
                learning.rate = 0.07, 
                momentum = 0.9,
                eval.metric = mx.metric.accuracy)

graph.viz(model$symbol)

preds <- predict(model, test.x)
pred.label <- max.col(t(preds)) - 1
table(pred.label, test.y)
