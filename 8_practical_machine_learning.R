== Practical Machine Learning ==

-- Week 1

[Prediction Motivation]

- This course covers the basic ideas behind machine learning/prediction
  + Study design: training vs. test sets
  + Conceptual issues - out of sample error, ROC curves
  + Practical implementation - the caret package

[What is Prediction]

Components of a predictor:
question -> input data -> features -> algorithm -> parameters -> evaluation

library(kernlab)
data(spam)
head(spam)

plot(density(spam$your[spam$type == 'nonspam']),
    col = 'blue', main = '', xlab = "Frequency of 'your'")
lines(density(spam$your[spam$type == 'spam']), col = 'red')

Our algorithm:
- Find a value C
- Frequency of 'your' > C --> then predict 'spam'

abline(v=0.5, col='black')
prediction <- ifelse(spam$your > 0.5, 'spam', 'nonspam')
table(prediction, spam$type)/length(spam$type)

[Relative Importance of Steps]

question > data > features > algorithms

1 - Question

An important point:
"The combination of some data and an achin desire for an answer does not
  ensure that a reasonable answer can be extracted from a given body of data."

  - John Tukey

The message is knowing when to give up; when the data is not sufficient to answer the question.

2 - Input Data

Garbage in = Garbage out

- May be easy (movie ratings -> new movie ratings) # having data on the exact same thing you're trying to predict
- May be harder (gene expression data -> disease)
- Depends on what is a 'good prediction'
- Often more data > better models
- The most important step!

3 - Features

Features matter!

- Properties of good features:
  + Lead to data compression
  + retain relevant information
  + Are created based on expert application knowledge

- Common mistakes:
  + Trying to automate feature selection
  + Not paying attention to data specific quirks
  + Throwing away information unnecessarily

May be automated with care

3 - Algorithms

Matter less than you would think

Issues to Consider

The best Machine Learning Method:
- Interpretable
- Simple
- Accurate
- Fast (to train & test)
- Scalable

Prediction is about accuracy tradeoffs:
- Interpretability vs accuracy
- Speed vs accuracy
- Simplicity vs accuracy # when you don't understand your own features, you can't explain misfires!
- Scalability vs # the winning solution of the Netflix 1M challenge never got implemented due to scalability issues

[In Sample and Out of Sample Errors]

- In Sample Error: The error rate you get on the same data set you used to build your predictor.
  Sometimes called resubstitution error.
- Out of Sample Error: The error rate you get on a new data set.
  Sometimes called generalization error.

Key ideas:
1 - Out of sample error is what you care about
2 - In sample error < out of sample error
3 - The reason is overfitting: matching your algorithm too much on the data you have

library(kernlab)
data(spam)
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size=10),]
spamLabel <- (smallSpam$type="spam")*1 + 1
plot(smallSpam$capitalAve, col=spamLabel)

Overfitting Example

Prediction Rule 1
- capitalAve > 2.7 = 'spam' # Average Number of Capital Letters
- CapitalAve < 2.4 = 'nonspam'
- capitalAve between 2.40 and 2.45 = 'spam' # nonintuitive, but meant to caputre these outliers
- capitavlAve between 2.45 and 2.7 = 'nonspam'

rule1 <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.7] <- 'spam'
    prediction[x < 2.4] <- 'nonspam'
    prediction[(x >= 2.40 & x <= 2.45)] <- 'spam'
    prediction[(x > 2.45 & x <= 2.70)] <- 'nonspam'
    return(prediction)
}

table(rule1(smallSpam$capitalAve), smallSpam$type)

Prediction Rule 2
- capitalAve > 2.8 = 'spam'
- capitalAve <= 2.8 = 'nonspam'

rule2 <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.8] <- 'spam'
    prediction[x <= 2.8] <- 'nonspam'
    return(prediction)
}

table(rule2(smallSpam$capitalAve), smallSpam$type)

Apply to complete spam data
table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)

Look at accuracy

sum(rule1(spam$capitalAve)==spam$type) # 3366
sum(rule2(spam$capitalAve)==spam$type) # 3395

Rule 2 would miss certain values in training set,
but generalizes better on new data.

What is going on? Overfitting!

- Data have two parts:
  + Signal
  + Noise
- The goal of a predictor is to find signal
- You can always design a perfect in-sample predictor
- You capture both signal + noise when you do that
- Predictor will not perform as well on new samples

[Prediction Study Design]

1 - Define your error rate
2 - Split data into:
    + Training
    + Testing
    + Validation (optional) # validation != cross-validation
3 - On the training set pick features
    + Do cross-validation within training set to optimize
4 - On the training set pick prediction function
    + Do cross-validation within training set to optimize
5 - If no Validation Set
    + Apply 1x to test set # cannot use it to optimize your algorithm!
6 - If validation
    + Apply to test set and further refine (again) # you use the test set to optimize your algorithm
    + Apply 1x to validation # to get final prediction

# Check out Kaggle

Avoid small sample sizes
- Suppose you are predicting a binary outcome
  + Diseased/healthy
  + Click on add/not click on add
- One classifier is flipping a coin
- Probability of perfect classification is approximately:
  + (1/2)^sample_size
  + n = 1 flipping coin 50% chance of 100% accuracy
  + n = 2 flipping coin 25% chance of 100% accuracy
  + n = 10 flipping coin 0.10% chance of 100% accuracy
    --> Easy to get perfect accuracy within small samples!

Rules of thumb for prediction study design

- If you have a large sample size:
  + 60% training
  + 20% test
  + 20% validation
- If you have a medium sample size:
  + 60% training
  + 40% testing
- If you have a small sample size:
  + Do cross validation # tbd
  + Report caveat of small sample size

Some principles to remember
- Set the test/validation aside and do not look at it
- In general randomly sample training and test
- Your data sets must reflect structure of the problem
  + If predictions evolve with time, then split train/test in time chunks (called backtesting in finance)
    * E.g. a training/test set for 2014, one for 2015, etc.
- All subsets should reflect as much diversity as possible
  + Random assignment does this
  + You can also try to balance by features - but this is tricky

[Types of Errors]

-- Basic Terms

In general, positive = identified and negative = rejected. Therefore:

- True Positive = correctly identified
- False Positive = incorrectly identified
- True Negative = correctly rejected
- False Negative = incorrectly rejected

Medical testing example:

- True Positive = Sick people correctly diagnosed as sick
- False Positive = Healthy people incorrectly identified as sick
- True Negative = Healty people correctly indentified as healthy
- False Negative = Sick people incorrectly identified as healthy

Key Quantities

# Of all those that have the disease, how many did you call correctly diseased
- Sensitivity: Pr(positive test | disease) # TP / (TP + FN)
- Specificity: Pr(negative test | no disease) # TN / (FP + TN)
# Of all those you predicted as diseased, how many actually are diseased
- Positive Predictive Value: Pr(disease | positive test) # TP / (TP + FP)
- Negative Predictive Value: Pr(no disease | negative test) # TN / (FN + TN)
- Accuracy: Pr(correct outcome) # (TP + TN) / (TP + FP + FN + TN)

1 - Mean squared error (or root mean squared error)
    + Continuous data, sensitive to outliers
      * (1/n * ∑ (Predictioni - Truthi)^2)^0.5
2 - Median absolute deviation
    + Continuous data, often more robust
3 - Sensitivity (recall) # https://en.wikipedia.org/wiki/Sensitivity_and_specificity
    + True positive rate
4 - Specificity
    + True negative rate
5 - Accuracy
    + Weights false positives/negatives equally
6 - Concordance
    + One example is Kappa

[ROC Curves]

Why a curve?

- In binary classification you are predicting one of two categories
  + Alive/dead
  + Click on ad/do not click
- But your predictions are often quantitative
  + Probability of being alive
  + Prediction on a scale from 1 to 10
- The cutoff you choose gives different results

Area under the curve (AUC)

- AUC = 0.5: random guessing
- AUC = 1: perfect classifier
- In general AUC of above 0.8 considered 'good'

[Cross Validation]

Key idea

1 - Accuracy on the training set (resubstitution accuracy) is optimistic
2 - A better estimate comes from an independent set (test set accuracy)
3 - But we cannot use the test set when building the model or it becomes part of the training set
4 - So we estimate the test set accuracy with the training set

Cross-Validation

Approach:

1 - Use the training set
2 - Split it into training/test sets
3 - Build a model on the training set
4 - Evaluate on the test set (sub of the training set)
5 - Repeat and average the estimated errors

Used for:
1 - Picking variables to include in a model
2 - Picking the type of prediction function to use
3 - Picking the parameters in the prediction function
4 - Comparing different predictors

How to divide the training set into test/train sets:
- Random subsampling
- K-fold
  + Break into K equal sized datasets
  + Train on 2/3, test on 1/3
    * iterate through all combinations
- Leave one out
  + Build model on all samples except one, use that one to predict
  + Go through all samples 1 by 1 (train on rest, predict on the one left out)
  + Average the accuracy

Considerations

- For time series data data must be used in 'chunks'
- For k-fold cross validation:
  + Larger k = less bias, more variance
  + Smaller k = more bias, less variance
- Random sampling must be done without replacement
- Random sampling with replacement is the bootstrap
  + Underestimates of the error # some samples will occur more than once, if you get those right you'll get more right on average, which is an overestimation of accuracy
  + Can be corrected, but it is complicated
- If you cross-validate to pick predictors estimate you must estimate errors
  on independent data

-- Week 2

[Caret Package] # http://caret.r-forge.r-project.org/

- Some preproccessing (cleaning)
  + preProcess
- Data splitting
  + createDataPartition
  + createResample
  + createTimeSlices
- Training/testing functions
  + train
  + predict
- Model comparison
  + confusionMatrix

Machine Learning algorithms in R

- Linear discriminant analysis
- Regression
- Naive Bayes
- Support vector machines
- Classification and regression trees
- Random forests
- Boosting
- etc.

SPAM Example: 1 - Data splitting

library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

SPAM Example: 2 - Fit a Model

set.seed(32343)
modelFit <- train(type ~., data = training, method = 'glm')
modelFit # show model summary
modelFit$finalModel # show me the final model values (weights for features!!)

SPAM Example: 3 - Prediction

predictions <- predict(modelFit, newdata=testing)
predictions # show all predictions

SPAM Example: 4 - Confusion Matrix

confusionMatrix(predictions, testing$type)

Further Information

- Caret tutorials
  + http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
  + http://www.r-project.org/web/packages/caret/vignettes/caret.pdf
- A paper introducing the caret package
  + http://www.jstatsoft.org/v28/i05/paper

[Data Slicing]

SPAM Example: 1 - Data Splitting

library(caret)
library(kernlab) # to load the spam data
data(spam)
inTrain <- createDataPartition(y = spam$type, # outcome variable
                               p = 0.75,      # 75% goes in train set
                               list = FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]
dim(training)

SPAM Example: 2 - K-fold

set.seed(32323)
folds <- createFolds(y = spam$type, # specify outcome value
                     k = 10,        # number of folds
                     list = TRUE,   # return the set of indices for all folds as a list (can also be a matrix)
                     returnTrain = TRUE) # return the training set
sapply(folds, length) # show the length of the folds
folds[[1]][1:10] # it is a list, so first fold, first 10 elements

folds <- createFolds(y = spam$type,
                     k = 10,
                     list = TRUE,
                     returnTrain = FALSE) # return the test set

SPAM Example: 3 - Resampling wit Replacement

set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds, length)

SPAM Example: 3 - Time Slices

set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme,
                         initialWindow = 20, # first 20 are in train
                         horizon = 10) # next 10 are test (& repeat, 31-50 in train, 51 - 60 in test, etc.)
names(folds)
folds$train[[1]] # show first train
folds$test[[1]] # show first test

[Training Options]

library(caret)
library(kernlab) # to load the spam data
data(spam)
inTrain <- createDataPartition(y = spam$type, # outcome variable
                               p = 0.75,      # 75% goes in train set
                               list = FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]
modelFit <- train(type ~., data = training, method = 'glm')

args(train.default)

function (x,
          y,
          method = 'rf',
          preProcess = NULL, # to set a bunch of preprocessing options
          weights = NULL, # upweigh or downweigh certain observations (useful for unbalanced training sets where you have way more observations of one type than another type)
          metric = ifelse(is.factor(y), 'Accuracy', 'RMSE'),
          maximize = ifelse(metric == 'RMSE', FALSE, TRUE), # which metric to use to optimise; accuracy for categorical variables, root mean squared error for continuous
          trControl = trainControl(), # set control options
          tuneGrid = NULL,
          tuneLength = 3)

Metric Options

- Continuous outcomes:
  + RMSE = Root Mean Squared Error
  + Rsquared = R^2 from regression models # 'linear agreement' between the variables you are predicting and those you predict with
- Categorical outcomes:
  + Accuracy = Fraction correct
  + Kappa = A measure of concordance

trainControl # allows you to be much more precise in the way you train models
args(trainControl)

function (
  method = 'boot', # which method to use for resampling (boot = bootstrap -hence with replacement-), alternative is cross-validation
  number = ifelse(method %in% c('cv', 'repeatedcv'), 10, 25), # number of times to do bootstrapping (or cross-validation)
  repeats = ifelse(method %in% c('cv', 'repeatedcv'), 1, number), # how many times to repeat the whole process
  p = 0.75, # size of the training set
  initialWindow = NULL, # for time data
  horizon = 1, # number of time points you are predicting
  fixedWindow = TRUE,
  verboseIter = FALSE,
  returnData = TRUE,
  returnResamp = 'final',
  savePredictions = FALSE, # from all iterations
  classProbs = FALSE,
  summaryFunction = defaultSummary, # use a different summary
  selectionFunction = 'best',
  custom = NULL,
  preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),
  index = NULL,
  indesOut = NULL,
  timingSamps = 0,
  predictionBounds = rep(FALSE, 2),
  seeds = NA, # set the seeds
  allowParallel = TRUE) # training on multiple cores

trainControl resampling

- method
  + 'boot' = bootstrapping
  + 'boot632' = bootstrapping with adjustment
  + 'cv' = cross-validation
  + 'repeatedcv' = repeated cross-validation
  + 'LOOCV' = leave one out cross validation
- number
  + for boot/cross validation
  + Number of subsamples to take
- repeats
  + Number of times to repeate subsampling
  + If big this can slow things down!

Setting the seed
- It is often useful to set an overall seed # the same random numbers get generated each time, now you can track enhancements
- You can also set a seed for each resample
- Seeding each resample is useful for parallel fits

set.seed(12345)
modelFit2 <- train(type ~ ., data = training, method = 'glm')
modelFit2

[Plotting Predictors]

library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)

1 - Creating Test & Train Sets

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
training <- Wage[-inTrain,]
dim(training)
dim(testing)

2 - Feature plot (caret package)

featurePlot(x = training,[,c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs') # using caret

qplot(age, wage, data = training) # using ggplot2

# Look for explanations of strange patterns
qplot(age, wage, colour = jobclass, data = training)

# Add regression smoothers
qq <- qplot(age, wage, colour = education, data = training)
qq + geom_smooth(method = 'lm', formula = y ~ x)

#2 cut2, making factors (Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage, g = 3) # create 3 groups, generates factors based on quantiles
table(cutWage)

#Boxplots with cut2
p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c('boxplot'))
p1

# Boxplots with points overlayed
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c('boxplot', 'jitter'))
grid.arrange(p1, p2, ncol = 2) # so both plots are showing

# Handy: map out a continuous variable in buckets and tabulate againt another dimension
t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1, 1) # get the proportions of t1, across rows (1) or colums (2) & across entire table (either 0 or 3?)

Density Plots # useful for overlaying multiple distributions & comparing them

qplot(wage, colour = education, data = training, geom = 'density')

Notes and Further Reading

- Make your plots only in the training set
  + Do not use the test set for exploration!
- Things you should be looking for
  + Imbalance in outcomes/predictors
  + Outliers
  + Groups of points not explained by a predictor
  + Skewed variables

[Preprocessing]

1 - Why preprocess?

library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]
hist(training$capitalAve, main = '', xlab = 'ave. capital run length')
mean(training$capitalAve) # 4.709
sd(training$capitalAve) # 25.48

2 - Standardizing/Normalising Features

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve) # mean will be 0, sd will be 1

When you then standardize the features in the test set, you need to use the sd & mean of the training set!

testCapAve <- test$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)

Now the mean might not be zero and the sd might not be 1!

3 - Standardizing - preProcess function

preObj <- preProcess(training[, -58], # feed all data except column 58, which is the outcome variable
                     method = c('center', 'scale')) # center all variables (subtract mean) & scale them (divide by sd)
trainCapAves <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAves) # 0
sd(trainCapAves) # 1

testCapAveS <- predict(preObj, testing[, -58])$capitalAve # use training preObj to predict on test set instead
mean(testCapAveS) # again here mean != 0 and sd != 1 (but hopefully they are close)

4 - Standardizing - preProcess argument

set.seed(32343)
modelFit <- train(type ~ ., data = training, preProcess = c('center', 'scale'), method = 'glm') # feed preProcess arg directly into train function
modelFit

5 - Standardizing - Box-Cox transforms

preObj <- preProcess(training[, -58],
                     method = c('BoxCox')) # trying to make continous data look more normal, using some sort of maximum likelihood calculations
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow = c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

6 - Standardizing - Imputing data

set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[, -58], method = 'knnImpute') # k-nearest-neighbours; knn:
# knn finds the k most similar rows/observations and uses their average value of that is misisng to impute the variable that is missing!
capAve <- predict(preObj, training[, -58])$capAve # now you predict something (in this case the training set) including the variable with the imputed variables

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(CapAveTruth)

# Cross checking the imputed variables with the imputed ones to check differences

quantile(capAve - capAveTruth) # overall difference
quantile((capAve - capAveTruth)[selectNA]) # difference of only imputed values to true values
quantile((capAve - capAveTruth)[!selectNA]) # difference of non imputed values to true values

Notes and Further Reading

- Training and test set must be processed in the same way
- Test transformations will likely be imperfect, as you must use the parameters of the training set
  + Especially if the test/training sets collected at different times
- Careful when transforming factor variables!

[Covariate Creation] # also called predictors or features

Two Levels of Covariate Creation

- Level 1: From raw data to covariate
- Level 2: Transforming tidy covariates

library(kernlab)
data(spam)
spam$capitalAveSq <- spam$capitalAve^2

Level 1, Raw Data --> Covariates

- Depends heavily on application
- The balancing act is summarization vs. information loss
- Examples:
  + Text files: fequency of words, frequency of phrases (Google ngrams), frequency of capital letters.
  + Images: edges, corners, blobs, ridges (computer vision feature selection)
  + Webpages: Number and type of images, position of elements, colors, videos (A/B Testing)
  + People: Height, weight, hair color, sex, country of origin
- The more knowledge of the system you have the better the job you will do
- When in doubt, err on the side of more features
- Can be automated, but use caution!

Level 2, Tiday covariates --> new covariates

- More necessary for some methods (regression, svms) than for others (classification trees). # i.e. classification trees do not depend on a certain model (i.e. normal) ~ the data need not to look in a certain way
- Should be done only on the training set
- The best approach is through exploratory analysis (plotting/tables)
- New covariates should be added to data frames

Load example data

library(ISLR)
library(caret)
data(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE) # list = FALSE so matrix is returned
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

Common covariates to add, dummy variables # usually the case with factor variables, for each class get a binary dummy
# Basic idea is to convert factor variables to indicator variables

table(training$jobclass) # jobclass has 2 levels: industrial & information
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

Removing Zero Covariates # find features without variability, so you can remove them

nsv <- nearZeroVar(training,
                   saveMetrics = TRUE) # so we can see the values it uses to calculate the near-zero value
nsv

Spline Basis

library(splines)
bsBasis <- bs(training$age, df = 3) # to fit curved lines, uses polynomials, df = 3 means up to third-degree polynomial
- will return three scaled colums: age, age^2 & age^3
- now you allow for curvey model fitting:
  + lm1 <- lm(wage ~ bsBasis, data = training)
  + plot(training$age, training$wage, pch = 19, cex = 0.5)
  + points(training$age, predict(lm1, newdata = training), col = 'red', pch = 19, cex = 0.5)

Then you need to create the same values in the test-set:

predict(bsBasis, age=testing$age) # be sure to use the model from the training set, to create the values in the test set

Notes and Further Reading

- Level 1 feature creation (raw data to covariates)
  + Science is key. Google 'feature extraction for [data type]'
  + Err on overcreation of features
  + In some applications (images, voices) automated feature creation is possible/necessary
  + http://www.cs.nyu.edu/~yann/talks/lecun-ranzato-icml2013.pdf
- Level 2 feature creation (covariates to new covariates)
  + The function preProcess in caret will handle some preprocessing
  + Create new covariates if you think they will improve fit
  + Use exploratory analyses on the training set for creating them
  + Be careful about overfitting!
- Preprocessing with caret
- If you want to fit spline models, use the gam method in the caret package which
  allows smoothing of multiple variables
- More on feature creation/data tidying in the Obtaining Data course from the Data Science
  specialisation

[Preprocessing with PCA]

Correlated predictors

library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
M <- abs(cor(training[,-58])) # leave out col 58 (outcome)
diag(M) <- 0 # correlation of 1 with themselves are removed (or, set to 0)
which(M > 0.8, arr.ind = T)
names(spam)[c(34,32)] # you get the col numbers from the previous output
plot(spam[,34], spam[,32]) # to visualize

Basic PCA Idea

- We might not need every predictor
- A weighted combination of predictors might be better
- We should pick this combination to capture the 'most information' possible
- Benefits
  + Reduced number of predictors
  + Reduced noise (due to averaging)

We Could Rotate the Plot

X = 0.71 * training$num415 + 0.71 * training$num857 # adding together
Y = 0.71 * training$num415 - 0.71 * training$num857 # subtracting
plot(X,Y)

By plotting this you will note that most variability is captured by looking at
addition, not subtraction.

Related problems

- You have multivariate variables X1, ..., Xn so X1 = (X11, ..., X1m)
  i.  Find a new set of multivariate variables that are uncorrelated and explain as much
      variance as possible
  ii. If you put all the variables together in one matrix, find the best matrix created
      with fewer variales (lower rank) that explains the original data.

The first goal is statistical, and the second goal is data compression.

Related Solutions - PCA/SVD

SVD

If X is a matrix with each variable in a column and each observation in a row then the SVD
is a 'matrix decomposition'.

X = UDV^T

where the columns of U are orthogonal (left singular vectors), the columns of V are orthogonal
(right singular vectors) and D is a diagonal matrix (singular values).

PCA

The principal components are equal to the right singular values if you first scale
(subtract the mean, divide by the standard deviation) the variables.

Principal Components in R - prcomp

smallSpam <- spam[,c(34, 32)] # take the highly correlated cols
prComp <- prcomp(smallSpam) # apply principal components to it
plot(prComp$x[,1],prComp$x[,2])

PCA on SPAM data

typeColor <- ((spam$type == 'spam')*1 + 1) # spam == red (2) / nonspam == black (1)
prComp <- prcomp(log10(spam[, -58]+1)) # make the data look more gaussian
plot(prComp$x[,1], prComp$x[,2], col = typeColor, xlab = 'PC1', ylab = 'PC2')

- PC1 is the combination of features that explains the most variability
- PC2 is the combination of features that explains the second most variability
- PC3 ... etc.

PCA with caret

preProc <- preProcess(log10(spam[,-58]+1), method = 'pca', pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col = typeColor)

Preprocessing with PCA
# 1 - Here you get the PCA model/object
preProc <- preProcess(log10(training[,-58]+1), method = 'pca', pcaComp = 2) # pcaComp == how many components to compute, so here just 2
# 2 - Here you use the PCA model to transform you training set
trainPC <- predict(preProc, log10(training[,-58]+1)) # make the data look more gaussian
# 3 - Now you train a model using the PCA remodeled training set
modelFit <- train(training$type ~ ., method = 'glm', data = trainPC)
# 4 - Here you create the testPC set using the PCA model of the training set
testPC <- predict(preProc, log10(testing[, -58]+1)) # use the PCA model of the training set to reduce the test set
# 5 - Here you look at the accuracy, applying modelFit (based on training set) on test data restructured according to outcome training set PCA
confusionMatrix(testing$type, predict(modelFit, testPC)) # here you measure accuracy

--> You will see that accuracy is at 0.921, so still very high, whereas we only used 2 principal components! # I believe that in the SVD diagonal (middle) matrix you have variability explained % of each component

Alternative Way

modelFit <- train(training$type ~ ., method = 'glm', preProcess = 'pca', data = training)
confusionMatrix(testing$type, predict(modelFit, testing)) # it will now automcatically transform the testing set by using the PCA model of the training set

Final thoughts on PCs

- Most useful for linear-type models
- Can make it harder to interpret predictors
- Watch out for outliers!
  + Transform first (with lobs/Box Cox)
  + Plot predictors to identify problems

[Predicting with Regression]

Key ideas

- Fit a simple regression model
- Plug in new covariates and multiply by the coefficients
- Useful when the linear model is (nearly) correct

Pros:

- Easy to implemented
- Easy to interpret

Cons:

- Often poor performance in nonlinear settings

Example: Old faithful eruptions

library(caret)
data(faithful)
set.seed(333)

inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)

Eruption Duration vs. Waiting Time

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'waiting', ylab = 'Duration')

Fit a linear model

lm1 <- lm(eruptions ~ waiting, data = trainFaith) # predicted ~ predictor
summary(lm1)

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab  = 'waiting', ylab = 'duration')
lines(trainFaith$waiting, lm$fitted, lwd = 3)

Predict a New Value

coef(lm1)[1] + coef(lm1)[2] * 80 # predict for new value 80
#=> 4.119

Alternatively:
newdata <- data.frame(waiting = 80)
predict(lm1, newdata)
#=> 4.119

Plot predictions - training and test

par(mfrow = c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'waiting', ylab = 'duration')
lines(trainFaith$waiting, predict(lm1), lwd = 3)
plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = 'blue', xlab = 'waiting', ylab = 'duration')
# Use train model on newly unseen data in test set
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lwd = 3)

Get Training Set / Test Set Errors

# predict(model, on_data)

# Calculate RMSE on training
sqrt(sum((lm1$fitted - trainFaith$eruptions)^2))
# Calculate RMSE on test
sqrt(squm((predict(lm1, newdata = testFaith) - testFaith$eruptions)^2))

Prediction Intervals

pred1 <- predict(lm1, newdata = test$Faith, interval = 'prediction') # also return prediction intervals
ord <- order(testFaith$waiting) # so you order your data the same way
plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = 'blue') # plotting the dots
matlines(testFaith$waiting[ord], pred1[ord,], type = 'l', col = c(1,2,2), lty = c(1,1,1), lwd = 3) # plotting the lines

Same process with caret

modFit <- train(eruptions ~ waiting, data = trainFaith, method = 'lm')
summary(modFit$finalModel)

Notes and Futher Reading

- Regression models with multiple covariates can be included
- Often useful in combination with other models

[Predicting with Regression Multiple Covariates]

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
Wage <- subset(Wage, select = -c(logwage)) # remove logwage (outcome variable)
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x = training[,c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')

qplot(age, wage, data = training)
qplot(age, wage, colour = jobclass, data = training)
qplot(age, wage, colour = education, data = training)

Fit a Linear Model

modFit <- train(wage ~ age + jobclass + education,
                method = 'lm', data = training)
finMod <- modFit$finalModel
print(modFit)

Diagnostics

plot(finMod, 1, pch = 19, cex = 0.5, col = '#0000000010')

Color by those variables that are not used by the model (so you can see if you missed something)

qplot(finMod$fitted, finMod$residuals, colour = race, data = training)

Plot by index # which row the observation belongs to

plot(finMod$residuals, pch = 19)
# if you find a trend, check how the data is sorted
# you might find that that variable actually can explain something, so you can include it in model
# could potentially also be time, if the data is simply ordered by the time at which it was gathered!

Predicted versus truth in test set

pred <- predict(modFit, testing)
qplot(wage, pred, colour = year, data = testing) # ideally you want a 45 degree here, so wage equals predicted wage
# only use the test-set for this as a post-mortem
# cannot use it to update your model --> then the test set becomes part of the training set

If you want to use all covariates

modFitAll <- train(wage ~ ., data = training, method = 'lm')
pred <- predict(modFitAll, testing)
qplot(wage, pred, data = testing)

Notes and Further Reading

- Often useful in combination with other models

-- Week 3

[Predicting with Trees]

Key Ideas

- Iteratively split variables into groups
- Evaluate 'homogeneity' within each group
- Split again if necessary

Pros:
- Easy to interpret
- Better performance in non-linear settings

Cons:
- Without pruning/cross-validation can lead to overfitting
- Harder to estimate uncertainty
- Results may be variable

Basic Algorithm
1 - Start with all variables in one group
2 - Find the first variable/split that best separates the outcomes
3 - Divide the data into two groups ('leaves') on that split ('node')
4 - Within each split, find the best variable/split that separates the outcome
5 - Continue until the groups are too small or sufficiently 'pure'

Measures of Impurity

Example: 1/16 variables is incorrect

- Misclassification Error | Example: 1 - 15/16 == 1/16 == 0.06
  + 0 = perfect purity
  + 0.5 = no purity # higher than 0.5 would mean purity towards the other class, hence 0.5 is the equilibrium & no purity
- Gini Index | Example: 1 - [(1/16)^2 + (15/16)^2] == 0.12 # != Gini Coefficient from Economics
  + 0 = perfect purity
  + 0.5 = no purity
- Deviance/Information gain | Example: -[1/16 * log2(1/16) + log2(15/16)] = 0.34
  + 0 = perfect purity
  + 1 = no purity

# Example: Iris Data

data(iris)
library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets

inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

# Plot the data

qplot(Petal.Width, Sepal.Width, colour = Species, data = training)

# Iris petal widths/sepal width

library(caret)
modFit <- train(Species ~ ., method = 'rpart', data = training) # rpart is a package for classification trees
print(modFit$finalModel)

# Plot tree

plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)

# Prettier Plots

library(rattle)
fancyRpartPlot(modFit$finalModel)

# Predicting new values

predict(modFit, newdata = testing)

Notes and Further Resources
- Classification trees are non-linear models
  + They use interactions between variables
  + Data transformations may be less important (monotone transformations)
  + Trees can also be used for regression problems (continuous outcome)
- Note that there are multiple tree building options in R both in the caret package
  + party
  + rpart
    * Outside the caret package: tree

[Bagging] # Bootstrap Aggregating

Basic Idea:

1 - Resample cases with replacement and recalculate predictions
2 - Average or majority vote

Notes:

- Similar bias
- Reduced variance
- More useful for non-linear functions

# Example

Ozone Data
library(ElemStatLearn)
data(ozone, package = 'ElemStatLearn')
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess # loess is a smooth curve which you fit here below

ll <- matrix(NA, nrow = 10, ncol = 155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace = T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2) # span sets the smoothness of the loess curve
  ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:155)) # newdata = data.frame(ozone = 1:155) --> just create new dataframe with values to predict on
}

# Now we average our 10 loess curves to get a lower variance (yet similar bias) curve!

plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5) # plot the axis & dots
for(i in 1:10){lines(1:155, ll[i,], col = 'grey', lwd = 2)} # plot all lines individually in grey
lines(1:155, apply(ll, 2, mean), col = 'red', lwd = 2) # plot the average line in red

Bagging in caret

- Some models perform bagging for you, in train function consider 'method' options:
  + bagEarth
  + treebag
  + bagFDA
- Alternatively you can bag any model you choose using the bag function

# Using the bag() function from the caret package

predictors <- data.frame(ozone = ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B = 10, # B = number of replications
               # BagControl = How are you going to fit the model &&
               bagControl = bagControl(fit = ctreeBag$fit, # fit = the function that is going to be applied to fit the model every time (could be a call to the train() function)
                                       predict = ctreeBag$pred, # Given a particular modelFit how to predict new values (could be a call to the predict() function from a trained model)
                                       aggregate = ctreeBag$aggregate)) # The way we put the predictions together, i.e. average or median
# ctree = conditional regression tree model
# http://www.inside-r.org/packages/cran/caret/docs/nbBag

plot(ozone$ozone, temperature, col = 'lightgrey', pch = 19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch = 19, col = 'red') # single fit
points(ozone$ozone, predict(treebag, predictors), pch = 19, col = 'blue') # all fits aggregated

# Parts of Bagging

ctreeBag$fit # returns the modelFit from the ctree function
ctreeBag$pred # takes in a modelFit object (given by ctreeBag$fit) & a new dataset to give a prediction
ctreeBag$aggregate # gets the predictions of all modelFits and binds them together in one matrix & then takes the median of each value

Notes and Further Resources

- Bagging is most useful for nonlinear models
- Often used with trees - an extension is random forests
- Several models use bagging in the caret() train function

[Random Forests] # an extension to bagging for classification and regression trees

1 - Bootstrap samples # we resample our entire training set with replacement, and build several trees on each of those samples
2 - At each split, bootstrap variables # each time we split, we also bootstrap the variables (only a subset of the variables is considered at each potential split). This makes for a diverse set of potential trees that can be built.
3 - Grow multiple trees and vote # vote or average trees

Pros:
- Accuracy

Cons:
- Speed
- Interpretability
- Overfitting

# Example: Iris Data

data(iris)
library(ggplot2)
library(caret)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# Random Forests

modFit <- train(Species ~ ., data = training, method = 'rf', prox = TRUE) # rf = random forest | prox = TRUE --> produces a little bit more information which can be used later on
modFit

# Getting a single tree

getTree(modFit$finalModel, k = 2) # k = 2 --> give me the second tree

- Each row is a particular split
- Indicates the ROW where the Left or Right daughter is
- Which variable do we split on
- On what value do we split
- Is the node terminal (-1) or not (1)
- What is the prediction for this specific node --> 0 if this node is NOT terminal, if it is terminal, it != 0 and has a k-class number

# Class 'centers'

irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox) # here you call the prox element, to get the centers
irisP <- as.date.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col = Species, data = training) # here you plot the dots, coloured by species
p + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5, shape = 4, data = irisP) # here you plot the centers, coloured by species

# Predit new values

pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)

# Plot values you misclassified

qplot(Petal.Width, Petal.Length, colour = predRight, data = testing, main = 'newdata predictions')

Notes and Further Resources:

- Random forests are usually one of the two top performing algorithms along with boosting in prediction contests.
- Random forests are difficult to interpret but often very accurate.
- Care should be taken to avoid overfitting (see rfcv() function).

[Boosting]

Basic Idea

1 - Take lots of (possibly) weak predictors # i.e. prediction models (!= features)
2 - Weight them in a way that takes advantage of their strengths and add them up
3 - Get a stronger predictor

Basic Idea Behing Boosting

1. Start with a set of classifiers h1, ..., hk
   + Examples: all possible trees, all possible regression models, all possible cutoffs
                                                                                  t
2. Create a classifier that combines classification functions together f(x) = sgn(Σ αt * ht(x)) # f(x) is a prediction for new point!
                                                                                 t=1
   + Goal is to minimize error (on training set)
   + Iterative, select one h at each step
   + Calculate weights based on errors
   + Upweight missed classifications and select next h

Boosting in R

- Boosting can be used with any subset of classifiers
- One large subclass is gradient boosting
- R has multiple boosting libraries. Differences include the choice of basic classification functions
  and combination rules.
  + gmb: boosting with trees
  + mboost: model based boosting
  + ada: statistical boosting based on additive logistic regression
  + gamBoost: for boosting generalized additive models
- Most of these are available in the caret package

- the calculation of alpha depends on the errorfunction. Reason is you want to select alpha in a way that minimizes the errorfunction.
- for adaboost: 1/2 * log( (1-eps)/eps ); which means for the first slide log(0.7/0.3)/2 = 0.423...

library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <-Wage[inTrain,]
testing <- Wage[-inTrain]

modFit <- train(wage ~ ., method = 'gbm', data = training, verbose = FALSE) # gbm = boosting with trees
print(modFit)

qplot(predict(modFit, testing), wage, data = testing)

Notes and Further Reading

- A couple of nice tutorials for boosting:
  + Freund and Shapire - http://www.cc.gatech.edu/~thad/6601-gradAI-fall2013/boosting.pdf
  + Ron Meir - http://webee.technion.ac.il/people/rmeir/BoostingTutorial.pdf
- Boosting, random forests, and model ensembling are the most common tools that win Kaggle and other prediction contests
  - http://www.netflixprize.com/assets/GrandPrize2009_BPC_BigChaos.pdf

[Model Based Prediction]

Basic Idea

1 - Assume the data follow a probabilistic model
2 - Use Bayes theorem to identify optimal classifiers

Pros:
- Can take advantage of structure of the data
- May be computationally convenient
- Are reasonably accurate on real problems

Cons:
- Make additional assumptions about the data
- When the model is incorrect you may get reduced accuracy

Model Based Approach

1 - Our goal is to build a parametric model for conditional distribution P(Y = k|X = x)
2 - A typical appraoch is to apply Bayes theorem: https://en.wikipedia.org/wiki/Bayes%27_theorem
3 - Typically prior probabilites πk are set in advance
4 - A common choice for fk(x) is the Gaussian distribution # or multivariate Gaussian distribution
5 - Estimate paramters (μk, σ^2k) from the data
6 - Classify to the class with the highest value of P(Y = k|X = x) # maximum likelihood

Classify Using the Model

A range of models use this approach
- Linear discriminant analysis assumes fk(x) is multivariate Gaussian with some covariances
- Quadratic discriminant analysis assumes fk(x) is multivariate with different covariances
- Model based prediction assumes more complicated versions for the covariance matrix
- Naive Bayes assumes independence between features for model building
  + http://statweb.stanford.edu/~tibs/ElemStatLearn

# Example: Iris Data

data(iris)
library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets

inTrain <- createDatapartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

# Build predictions

modlda <- train(Species ~ ., data = training, method = 'lda') # linear discriminant analysis
modnb <- train(Species ~ ., data = training, method = 'nb') # naive bayes
plda <- predict(modlda, testing)
pnb <- predict(modnb, testing)
table(plda, pnb)

# Comparison of results

equalPredictions <- (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour = equalPredictions, data = testing)

-- Week 4

[Regularized Regression]

Basic Idea

1 - Fit a regression model
2 - Penalize (or shrink) large coefficients

Pros:
+ Can help with the bias/variance tradeoff
+ Can help with model selection

Cons:
- May be computationally demanding on large data sets
- Does not perform as well as random forests and boosting

# Prostate Cancer

library(ElemStatLearn)
data(prostate)
str(prostate)

small <- prostate[1:5,]
lm(lpsa ~ ., data = small)

Y = β0 + β1*X1 + β2*X2 + ε

where X1 and X2 are nearly perfectly correlated (co-linear). You can approximate this model by:

Y = β0 + (β1 + β2)X1 + ε

The result is:
- You will get a good estimate of Y
- The estimate (of Y) will be biased
- We may reduce variance in the estimate

== Model Selection Approach: Split Samples ==

- No method better when data/computation time permits it

Approach

I. Divide data into training/test/validation
II. Treat validation as test data, train all competing models on the train data and pick the best one on validation.
III. To appropriately assess performance on new data apply to test set
IV. You may re-split and reperform steps 1-3

Two common problems:
- Limited data
- Computational complexity

== Decomposing expected prediction error ==

Assume Yi = f(Xi) + ε

EPE(λ) = E[{Y - ^fλ(X)}^2]

Suppose ^fλ is the estimate from the training data and look at a new data point X = x
- ^fλ is the estimate based on the training set, using tuning parameter λ
- ^ == estimate!

E[{Y - ^fλ(x^*)}^2] = σ^2 + {E[^fλ(x^*)] - f(x^*)}^2 + var[^fλ(x0)]
- E[^fλ(x^*)] == expected prediction
- f(x^*) == truth

= Irreducible error + Bias^2 + Variance

== Hard thresholding ==

- Model $Y = f(X) + \epsilon$
- Set ^fλ(x) = x'β'
- Constrain only λ coefficients to be nonzero.
- Selection problem is after chosing λ figure out which p - λ coefficients to make nonzero

== Regularized Regression ==

If the βjs are unconstrained:
- They can explode
- And hence are susceptible to very high variance

To control variance, we might regularize/shrink the coefficients.

PRSS(β) = Σ{j=1}^n (Yj - Σ{i=1}^m β{1i} * X{ij})^2 + P(λ; β)

where PRSS is a penalized form of the sum of squares. Things that are commonly looked for:
- Penalty reduces complexity
- Penalty reduces variance
- Penalty respects structure of the problem

Ridge regression

Solve:

$$ \sum_{i=1}^N \left(y_i - \beta_0 + \sum_{j=1}^p x_{ij}\beta_j \right)^2 + \lambda \sum_{j=1}^p \beta_j^2$$

equivalent to solving

$\sum_{i=1}^N \left(y_i - \beta_0 + \sum_{j=1}^p x_{ij}\beta_j \right)^2$ subject to $\sum_{j=1}^p \beta_j^2 \leq s$ where $s$ is inversely proportional to $\lambda$

Inclusion of $\lambda$ makes the problem non-singular even if $X^TX$ is not invertible.

== Tuning parameter λ ==

λ controls the size of the coefficients
λ controls the amount of {\bf regularization}
As λ --> 0 we obtain the least square solution
As λ --> Inf we have $\hat{\beta}_{\lambda=\infty}^{ridge} = 0

== Lasso ==

$\sum_{i=1}^N \left(y_i - \beta_0 + \sum_{j=1}^p x_{ij}\beta_j \right)^2$ subject to $\sum_{j=1}^p |\beta_j| \leq s$

also has a lagrangian form

$$ \sum_{i=1}^N \left(y_i - \beta_0 + \sum_{j=1}^p x_{ij}\beta_j \right)^2 + \lambda \sum_{j=1}^p |\beta_j|$$

For orthonormal design matrices (not the norm!) this has a closed form solution

$$\hat{\beta}_j = sign(\hat{\beta}_j^0)(|\hat{\beta}_j^0 - \gamma)^{+}$$ but not in general.

Notes and Further Reading

- In caret methods are:
  + ridge
  + lasso
  + relaxo

[Combining Predictors]

Key ideas:
- You can combine classifiers by averaging/voting
- Combining classifiers improves accuracy
- Combining classifiers reduces interpretability
- Boosting, bagging, and random forests are variants on this theme # difference: same kind of classifier

== Basic intuition - majority vote ==

Suppose we have 5 completely independent classifiers

If accuracy is 70% for each:

- 10 * (0.7)^3 * (0.3)^2 + 5 * (0.7)^4 * (0.3)^1 + (0.7)^5
  + 83.7% majority vote accuracy
- With 101 independent classifiers
  + 99.9% majority vote accuracy

== Approaches for combining classifiers ==

1 - Bagging, boosting, random forests
    + Usually combine similar classifiers
2 - Combining different classifiers
    + Model stacking
    + Model ensembling

# Example with Wage data

library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select = -c(logwage)) # leave out logwage variable (since it would be a great predictor of wage..)

# Create a building data set and validation set

inBuild <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y = buildData$wage, p = 0.7, list = FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]

# Create training, test and validation sets

dim(training)
dim(testing)
dim(validation)

# Build two different models

mod1 <- train(wage ~., method= 'glm',data = training)
mod2 <- train(wage ~., method= 'rf', data = training, trControl = trainControl(method= 'cv'), number = 3)

# Predict on the testing set

pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour = wage, data = testing)

# Fit a model that combines predictors

predDF <- data.frame(pred1, pred2, wage = testing$wage)
combModFit <- train(wage ~., method = 'gam', data = predDF)
combPred <- predict(combModFit, predDF)

# Testing errors

sqrt(sum((pred1 - testing$wage)^2)) # 827.1
sqrt(sum((pred2 - testing$wage)^2)) # 866.8
sqrt(sum((combPred - testing$wage)^2)) # 813.9

# Predict on validation data set

pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1 = pred1V, pred2 = pred2V)
combPredV <- predict(combModFit, predVDF)

# Evaluate on validation

sqrt(sum((pred1V - validation$wage)^2)) # 1003
sqrt(sum((pred2V - validation$wage)^2)) # 1068
sqrt(sum((combPredV - validation$wage)^2)) # 999.9

Notes and Further Resources

- Even simple blending can be useful
- Typical model for binary/multiclass data
  + Build an odd number of models
  + Predict outcome with each model
  + Predict the class by majority vote
- This can get dramatically more complicated
  + Simple blending in caret: caretEnsemble (use at your own risk!)
  + Wikipedia ensemble learning

[Forecasting]

What is different?

- Data are dependent over time
- Specific pattern types
  + Trends - long term increase or decrease
  + Seasonal patterns - patterns related to time of week, month, year, etc.
  + Cycles - patterns that rise and fall periodically
- Subsampling into training/test is more complicated
- Similar issues arise in spatial data
  + Dependency between nearby observations
  + Location specific effects
- Typically goal is to predict one or more observations into the future.
- All standard predictions can be used (with caution!)

# Google data

library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src = "google", from = from.dat, to = to.dat)
head(GOOG)

# Summarize monthly and store as time series

mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency=12) # ts = timeSeries
plot(ts1, xlab="Years+1", ylab="GOOG")

Example time series decomposition
- Trend: Consistently increasing pattern over time
- Seasonal: When there is a pattern over a fixed period of time that recurs.
- Cyclic: When data rises and falls over non fixed periods

# Decompose a time series into parts

plot(decompose(ts1), xlab = "Years+1")

# Training and test sets

ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
ts1Train

# Simple moving average

plot(ts1Train)
lines(ma(ts1Train,order=3),col="red") # ma = moving average

# Exponential smoothing

ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col="red")

# Get the accuracy

accuracy(fcast, ts1Test)

Notes and Further Resources

- Forecasting and timeseries prediction is an entire field
- Rob Hyndmans Forecasting: principles and practice is a good place to start
- Cautions
  + Be wary of spurious correlations
  + Be careful how far you predict (extrapolation)
  + Be wary of dependencies over time (seasonal effects, etc.)
- See quantmod or quandl packages for finance-related problems

[Unsupervised Prediction]

Key ideas

- Sometimes you do not know the labels for prediction
- To build a predictor
  + Create clusters
  + Name clusters
  + Build predictor for clusters
- In a new data set
  + Predict clusters

# Iris example ignoring species labels

data(iris)
library(ggplot2)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Cluster with k-means

kMeans1 <- kmeans(subset(training, select=-c(Species)), centers=3) # subset could be replaced with: training[,-c('Species')]
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)

# Compare to real labels

table(kMeans1$cluster,training$Species)

# Build predictor

modFit <- train(clusters ~.,data=subset(training,select=-c(Species)),method="rpart") # classification tree
table(predict(modFit,training),training$Species)

# Apply on test

testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)

Notes and further reading

- The cl_predict function in the clue package provides similar functionality
- Beware over-interpretation of clusters!
- This is one basic approach to recommendation engines

