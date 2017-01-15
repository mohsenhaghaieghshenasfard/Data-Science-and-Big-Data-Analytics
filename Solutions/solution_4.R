
#--------------------------- Classification with Naive Bayes and Decision Trees
#--------------------------- A.1 ---------------------------

install.packages("e1071")
install.packages("party")

require(e1071)
require(party)

# Inspect the first few rows of the dataset
head(iris)

# Inspect the data type and levels of the attributes of the dataset
str(iris)

# To check the distribution of the data values in a particular attribute
table(iris$Species)

summary(iris)

#--------------------------- A.2 ---------------------------
set.seed(101)
# percentage of training set
alpha     <- 0.67 
inTrain   <- sample(1:nrow(iris), alpha * nrow(iris))
train.set <- iris[inTrain,]
test.set  <- iris[-inTrain,]

# To check the distribution of the data values in a particular attribute
table(train.set$Species)
table(test.set$Species)

#--------------------------- A.3 ---------------------------
# Training a Naive Base Classifier:
# It is possible to use . instead of X1 + X2 + ...
naive_model <- naiveBayes(Species ~ Sepal.Length + Sepal.Width
                          + Petal.Length + Petal.Width, 
                          data = train.set)

# Evaluate the results of Naive Base Classifier:
naive_predictions <- predict(naive_model, test.set)
naive_predictions
summary(naive_predictions)
table(naive_predictions,test.set$Species)


#--------------------------- A.4 ---------------------------
# Training a Decision Tree (Conditional Inference Tree):
irisct <- ctree(Species ~ ., data = train.set)
irisct

# Plotting the model:
plot(irisct)

# Estimated class probabilities, a list
tree_res <- treeresponse(irisct, newdata = test.set)
tail(tree_res)

# Evaluate the results of Decision Tree Classifier:
irisct_predictions <- predict(irisct, test.set)
summary(irisct_predictions)
table(irisct_predictions, test.set$Species)








