== Regression Models ==

-- Week 1

[Introduction to Regression]

- Inventor: Francis Galton

Questions for this class
- To use parents height to predict childrens heights
- To try to find a parsimonious, easily described mean relationship between
  parent and childrens heigts
- To investigate the variation in childrens heights that appears unrelated
  to parents heights (residual variation)
- To quantify what impact genotype information has beyond parental height in
  explaining child height
- To figure out how/whether and what assumptions are needed to generalize findings
  beyond the data in question (statistical inference)
- Why do children of very tall parents tend to be tall, but a little shorter than
  their parents and why children of very short parents tend to be short, but a little
  taller than their parents?
  + This is a famous question called 'Regression to the Mean/Mediocrity'

[Introduction: Basic Least Squares]

- Lets look at the data first, used by Francis Galton in 1885
- Galton was a statistician who invented the term and concepts of regression and
  correlation, founded the journal Biometrika, and was the cousin of Charles Darwin
- You may need to run install.packages('UsingR') if the UsingR library is not installed
- Lets look at the marginal (parents disregarding children and children disregarding parents)
  distribution first
  + Parent distribution is all heterosexual couples
  + Correction for gender via multiplying female heights by 1.08
  + Overplotting is an issue from discretization

library(UsingR)
data(galton)
library(reshape)
long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth = 1)
g <- g + facet_grid(. ~ variable)
g

Finding the middle via least squares

- Consider only the childrens heights
  + How could one describe the 'middle'?
  + One definition, let Yi be the height of child i for i = 1, ... , n = 928, then
    define the middle as the value of μ that minimizes:

    n
    Σ (Yi - μ)^2
   i=1

- This is the physical center of mass of the histogram
- You might have guessed that the answer is μ = Yi

library(manipulate)
myHist <- function(mu) {
    mse <- mean((galton$child - mu)^2) # as opposed to SSE (SUM)
    g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = 'salmon', colour = 'black', binwidth = 1)
    g <- g + geom_vline(xintercept = mu, size = 3)
    g <- g + ggtitle(paste('mu =', mu, ', MSE = ', round(mse, 2), sep = ''))
    g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

The least squares est. is the empirical mean
g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = 'salmon', colour = 'black', binwidth = 1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

[Introductory Data Example]

Comparing childrens heights and their parents heights

ggplot(galton, aes(x = parent, y = child)) + geom_point()

Size of point represents number of points at that (X, Y) combination (See the Rmd file for the code).

library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g

Regression through the origin
- Suppose that Yi are the parents heights
- Consider picking the slope β that minimizes:

    n
    Σ (Yi - Xi*β)^2
   i=1

- This is exactly using the origin as a pivot point picking the line that minimizes the sum
  of the squared vertical distances of the points to the line.
- Use R studios manipulate function to experiment
- Subtract the means so that the origin is the mean of the parent and childrens heights
  + This yields the same result as including the intercept in the minimalization function

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent) # recentering the origin (instead of including the intercept in the minimalization objective)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
    g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
    g <- g  + scale_size(range = c(2, 20), guide = "none" )
    g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
    g <- g + geom_point(aes(colour=freq, size = freq))
    g <- g + scale_colour_gradient(low = "lightblue", high="white")
    g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
    mse <- mean( (y - beta * x) ^2 )
    g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
    g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

The Solution
- In the next few lectures we will talk about why this is the solution

lm(I(child - mean(child))~ I(parent- mean(parent)) - 1, data = galton)

Call:
lm(formula = I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton) # -1 means "get rid of the intercept", since we do a regression to the origin

Coefficients:
I(parent - mean(parent)) = 0.646 # with every increment of x, y goes up with 0.646

[Notation and Background]

Some basic definitions
- In this module, we will cover some basic definition and notation used throughout the class
- We will try to minimize the amount of mathematics required for this class
- No calculus required

Notation for data
- We write X1, X2, ..., Xn to describe n data points
- As an example, consider the data set {1, 2, 5} then
  + X1 = 1, X2, 2, X3 = 5 and n = 3
- We often use a different letter than X, such as Y1, ..., Yn
- We will typically use Greek letters for things we do not know. Such as μ,
  a mean that we would like to estimate.
  + We use non-greek letters for variables we can simply observe!

The empirical mean
- Define the empirical mean as X (X-bar)
- Notice if we sutract the mean from data points, we gat data that has mean 0. That is,
  if we define:
  + Xi = Xi - X
- The mean of the Xi is 0.
- The process is called 'centering' the random variables
- Recall from the previous lecture that the mean is the least squares solution for minimizing:

    n
    Σ (Yi - Xi*β)^2
   i=1

The empirical standard deviation and variance
- Define the empirical variance as

                  n
S^2 = 1 / (n - 1) Σ (Xi - X)^2 == 1 / (n - 1) * (Σ Xi^2 - nX^2)
                 i=1

- The empirical standard deviation is defined as X = (S^2)^0.5.
  + Notice that the standard deviation has the same units as the data.
- The data defined by Xi/S (standard deviation) have empirical standard deviation 1.
  + This is called 'scaling' the data.

Normalization
- The data defined by:

Zi = (Xi - X) / S

  have empirical mean zero and empirical standard deviation 1.
- The process of centering (Xi - X) then scaling (/S) is called 'normalizing' the data.
- Normalized data are centered at 0 and have units equal to standard deviations of the original data.
- Example, a value of 2 from normalized data means that data point was two standard deviations larger than the mean.
  + By normalising you can make non-comparable datasets comparable

The empirical covariance/correlation
- Consider now when we have pairs of data: (Xi, Yi)
- Their empirical covariance is

                       n                                     n
Cov(X,Y) = 1 / (n - 1) Σ (Xi - X)*(Yi - Y) == 1 / (n - 1) * (Σ XiYi - nXY)
                      i=1                                   i=1

- The correlation is defined as:

Cor(X,Y) = Cov(X,Y) / SxSy # dividing by SxSy makes it 'unit free'

where Sx and Sy are the estimates of standard deviations for the X observations and Y observations, respectively.

Some facts about correlation
- Cor(X,Y) = Cor(Y,X)
- -1 <= Cor(X,Y) <= 1
- Cor(X,Y) = 1 and Cor(X,Y) = -1 only when the X or Y observations fall perfectly on a positive or negative
  sloped line, respectively.
- Cor(X,Y) measures the strength of the linear relationship between the X and Y data, with stronger relationships
  as Cor(X,Y) heads towards -1 or 1
- Cor(X,Y) = 0 implies no linear relationship

[Least Squares Estimation of Regression Lines]

Fitting the best line
- Let Yi be the ith childs height and Xi be the ith (average over the pair of) parents heights
- Consider finding the best line
  + Childs Height = β0 + Parents Hieght β1
- Use least squares

     n
min  Σ {Yi - (β0 + β1Xi)}^2
 β  i=1

Results
- The least squares model fit to the lin Y = β0 + β1X through the data pairs (Xi, Yi) with Yi as the
  outcome obtains the Y = ^β0 + ^β1X where: # ^β is 'estimated' beta

^β1 = Cor(X,Y) * (Sd(Y)/Sd(X))
^β0 = Y - ^β1X

- β1 has the units of Y/X, β0 has the units of Y
- The line passes through the point (X,Y)
- The slope of the regression line with X as the outcome and Y as the predictor (reversed) is Cor(Y,X) * (Sd(X)/Sd(Y))
- The slope is the same one you would get if you centered the data (Xi - X, Yi - Y), and did regression through the origin (β0 = 0)
- If you normalized the data, {(Xi - X )/ Sd(X), (Yi - Y)/Sd(Y)}, the slope is Cor(Y,X)

[Linear Least Squares Coding Example]

y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) * (sd(y) / sd(x))
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x))) # these values are the same (lm == regression function)

# Now we center the data

yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc^2) # this is the formula for beta1 when its passing through the origin
c(beta1, coef(lm(y ~ x)[2]))
lm(yc ~ xc - 1) # - 1 gets rid of the intercept

# Normalizing variables results in the slope being the correlation

yn <- (y - mean(y)) / sd(y)
xn <- (x - mean(x)) / sd(x) # units are standard deviations to the mean (of 0), stdev = 1
c(cor(y, x), cor(yn, yx), coef(lm(yn ~ xn))[2])

# Adding a regression line to the plot

g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g <- g + geom_smooth(method="lm", formula=y~x) # if you omit 'formula =' it will assume y ~ x
g

[Regression to the Mean]

A historically famous idea, Regression to the Mean
- Why is it that the children of tall parents tend to be tall, but not as tall as their parents?
- Why do children of short parents tend to be short, but not as short as their parents?
- Why do parents of very short children, tend to be short, but not as short as their child? And the
  same with parents of very tall children?
- Why do the best performing athletes this year tend do to a little worse the following year?

Regression to the mean
- These phenomena are all examples of so-called regression to the mean
- Invented by Francis Galton in the paper "Regression towards mediocrity in hereditary stature" in The
  Journal of the Anthropological Institute of Great Britain and Ireland, Vol. 15, (1885)
- Think of it this way, imagine if you simulated pairs of random normals
  + The largest first ones would be the largest by chance, and the probability that there are
    smaller for the second simulation is high
  + In other words P(Y < x | X = x) gets bigger as x heads into very large values
  + Similarly P(Y > z | X = x) gets bigger as heads to very small values
- Think of the regression line as the intrinsic part
  + Unless Cor(X,Y) = 1 the intrinsic part is not perfect

- Suppose that we normalize X (childs height) and Y (parents height) so that they both have mean 0 and variance 1
- Then, recall, our regression line passes through (0,0) (the mean of the X and Y)
- Then the slope of the regression line is Cor(Y,X), regardless of which variable is the outcome
  (recall, both standard deviations are 1)
- Notice if X is the outcome and you create a plot where X is the horizontal axis, the slope of the
 least squares line that you plot is 1/Cor(Y,X)
 # Outcome should be on Y-axis
 # If you reverse this you can get to the slope by dividing 1 over the Correlation

library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x,y) # rho is the standard greek letter used to denote correlations

library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1) # identity line (perfect correlation)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2) # reverse the predictor by 1 / rho
g

-- Week 2

-- Statistical linear regression models --

[Statistical Linear Regression Models]

Basic regression model with additive Gaussian errors.
- Least squares is an estimation tool, how do we do inference?
- Consider developing a probabilistic model for linear regression

Yi = β0 + β1Xi + εi # adding gaussian iid error variables makes it a statistical linear regression

To interpret these independent errors: they are the accumulation of a lot variables that you did not model but you should have,
that act together on the response in a way that can be modeled as if iid gaussian errors.

- Here the εi are assumed iid N(0, σ^2)
- Note, E[Yi | Xi = xi] = μi =  β0 + β1xi
  + The Expected value of the response given a particular value of the regressor is simply the line at that regressor (β0 + β1xi)
- Note, Var(Yi | Xi = xi) = σ^2
  + The variance of the response at any particular value of the regressor is σ^2

[Interpreting Coefficients]

Interpreting regression coefficients
- β0 is the expected value of the response when the predictor is 0

E[Y | X = 0] = β0 + β1 * 0 = β0

- Note this is not always of interest, for example when X = 0 is impossible or far outside of the range of data
  (X is blood pressure, or height, etc.).
- Consider that:

Yi = β0 + β1Xi + εi = β0 + αβ1 + β1(Xi - α) + εi # the effect of a is 0 (you add an subtract)
   = ~β0 + β1(Xi - α) + εi # ~β0 == β0 + αβ1

So, shifting your X values by value α changes the intercept, but not the slope.

- Often a is set to X-bar (average of the X variable) so that the intercept is interpretted as the expected response at the average X-value.

Interpreting regression coefficients, the slope

- β1 is the expected change in response for a 1 unit change in the predictor

E[Y | X = x + 1] - E[Y | X = x] == β0 + β1(x + 1) - (β0 + β1x) == β1

- Consider the impact of changing the units of X

Yi = β0 + β1Xi + εi == β0 + β1/α * (Xi * α) + εi == β0 + ~β1 * (Xi * α) + εi

- Therefore, multiplication of X by a factor of α results in dividing the coefficient by a factor of α.
- Example: X is height in m and Y is weight in kg. Then β1 is kg/m. Converting X to cm implies multiplying
  X by 100 (cm/m). To get β1 in the right units, we have to divide by 100 cm/m.

Xm * 100cm / m == (100X)cm and β1 * (kg/m) * (1m/100cm) = (β1 / 100) * (kg / cm)

[Linear Regression for Prediction]

## Plot the data

library(UsingR)
data(diamond)
library(ggplot2)
g <- ggplot(diamond, aes(x = carat, y = price),)
g <- g + xlab('Mass (carats)')
g <- g + ylab('Price (SIN $)')
g <- g + geom_point(size = 6, colour = 'black', alpha = 0.2)
g <- g + geom_point(size = 5, colour = 'blue', alpha = 0.2) # these two lines are used to create the same points
g <- g + geom_smooth(method = 'lm', colour = 'black')
g

## Fitting the linear regression model

fit <- lm(price ~ carat, data = diamond) # lm(y ~ x)
coef(fit)

fit # returns lm data
summary(fit) # returns more elaborate lm data
coef(fit) # returns only the coefficients in a vector

Intercept: -259.6259 --> The expected price of a 0 carat diamond
Carat: 3721.0249 --> We estimate an expected 3721.02 (SIN $) dollar increase in price for every carat increse in mass of diamond

## Getting a more interpretable intercept

        # Here we mean-center the predictor variable carat --> the average carat value will now become the intercept!
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
        # If you want to use arithmetic operations within equation statements inside lm() you have to wrap them in I()

coef(fit2)

Intercept: 500.0833 --> Thus $500.1 is the expected price for the average sized diamond of the data (0.2042 carats)
Carat: 3721.0249 --> same slope

# Changing the units to 1/10 of a carat (since 1 carat is kind of big)

fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)

Carat: 3721.0249 / 10 = 372.105

## Predicting the price of a diamond

- newx <- c(0.16, 0.27, 0.34)
- Manually:
  + coef(fit)[1] + coef(fit)[2] * newx
- Using predict() # this scales up better when you have many predictions
  + predict(fit, newdata = data.frame(carat = newx))
  + If you omit the newdata argument, e.g.: predict(fit), it predicts at the observed X values

-- Residuals --

Residuals represent variation left unexplained by our model.
We emphasize the difference between residuals and errors.
The errors are the unobservable true errors from the known coefficients,
while residuals are the observable errors from the estimated coefficients.
In a sense, the residuals are estimates of the errors.

[Residuals and residual variation]

Motivating example

diamond dataset from UsingR

Data is diamond prices (Singapore dollars) and diamond weight in carats (standard measure of diamond mass, 0.2g).
To get the data use library(UsingR); data(diamond);.

- Residual variation: the variation in the price variable after carat has been used to explain it (which explained a significant fraction of it).
- The distances (from the points to the regression lines) that make up this residual variation, are called the residuals.

- Model Yi = β0 + β1Xi + εi where εi ~ N(0, σ^2)
- Observed outcome i is Yi at predictor value Xi
- Predicted outcome i is ^Yi (Y-hat) at predictor value Xi is

^Y = ^β0 + ^β1Xi

- Residual e at i (ei) is the difference between the observed and predicted outcome

ei = Yi - ^Yi

- The vertical distance between the observed data point and the regression line

                          n
- Least squares minimizes Σ ei^2
                        i = 1

- The ei can be thought of as estimates of the ei

Properties of the Residuals
- E[ei] = 0
- If an intercept is included, Σ ei = 0 (sum of all the ei values)
- If a regressor variable, Xi, is included in the model, Σ ei * Xi = 0
- Residuals are useful for investigating poor model fit
- Positive residuals are above the line, negative residuals are below
- Residuals can be thought of as the outcome (Y) with the linear association of the predictor (X) removed
- One differentiates residual variation (variation after removing the predictor) from systematic variation
  (variation explained by the regression model)
- Residual plots highlight poor model fit

[Residuals, Coding Example]

data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit) # get the residuals from fit
# Another way to get the residuals:
yhat <- predict(fit) # Get the predicted fitted values for all X-values (so all the points exactly on the regression line)
# This shows you that you get the same results (it takes the largest discrepancy using max()):
max(abs(e - (y - yhat))) # e == (y - yhat)
# Here we substitute yhat with the manual calculation for the ^Y values)
max(abs(e - (y - (coef(fit[1]) + coef(fit)[2] * x)))

sum(e) ≈ 0
sum(e * x) ≈ 0

## Plot price (y) over carats (x) using base-R

plot(x, y,
     xlab = 'Mass (carats',
     ylab = 'Price (SIN $)',
     bg = 'lightblue',
     col = 'black', cex = 1.1, pch = 21, frame = FALSE)
abline(fit, lwd = 2) # add the fitted line
for (i in 1 : n) {
  lines(c(x[i], x[i]), c(y[i], yhat[i]), col = 'red', lwd = 2) # c(x-coord), c(y-coord) --> vertical lines only, so x coord is a single point
}

## Plot residuals (e) over carats (x)

plot(x, e,
     xlab = 'Mass (carats',
     ylab = 'Residuals (SIN $)',
     bg = 'lightblue',
     col = 'black', cex = 1.1, pch = 21, frame = FALSE)
abline(fit, lwd = 2) # add the fitted line
for (i in 1 : n) {
  lines(c(x[i], x[i]), c(y[i], yhat[i]), col = 'red', lwd = 2) # c(x-coord), c(y-coord) --> vertical lines only, so x coord is a single point
}

## Non-Linear data

x <- runif(100, -3, 3) # random uniform vars from -3 to 3
y <- x + sin(x) + rnorm(100, sd = 0.2)
library(ggplot2)
g <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g <- g + geom_smooth(method = 'lm', colour = 'black')
g <- g + geom_point(size = 7, colour = 'black', alpha = 0.4)
g <- g + geom_point(size = 5, colour = 'red', alpha = 0.4)
g

- We see that the linear fit is not perfect, but it is hard to see the sine effect yet

## Residual plot

g <- ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
  aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + geom_point(size = 7, colour = 'black', alpha = 0.4)
g <- g + geom_point(size = 5, colour = 'red', alpha = 0.4)
g <- g + xlab('X') + ylab('Residual')
g

- Now we can clearly see the sine pattern in the residual plot!

## Heteroskedasticity

x <- runif(100, 0, 6)
y <- x + rnorm(100, mean = 0, sd = 0.001 * x) # here you increase the sd with x, hence we get Heteroskedasticity
g <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g <- g + geom_smooth(method = 'lm', colour = 'black')
g <- g + geom_point(size = 7, colour = 'black', alpha = 0.4)
g <- g + geom_point(size = 5, colour = 'red', alpha = 0.5)
g

## Getting rid of the blank space can be helpful

g <- ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
  aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + geom_point(size = 7, colour = 'black', alpha = 0.4)
g <- g + geom_point(size = 5, colour = 'red', alpha = 0.5)
g <- g + xlab('X') + ylab('Residual')
g

# We now see a trend towards greater variability, this is called Heteroskedasticity

diamond$e <- resid(lm(price ~ carat, data = diamond))
g <- ggplot(diamond, aes(x = carat, y = e))
g <- g + xlab('Mass (carats)')
g <- g + ylab('Residual price (SIN $)') # residuals have the same unit as Y (since they measure the vertical distance to the regression line)
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + geom_point(size = 7, colour = 'black', alpha = 0.5)
g <- g + geom_point(size = 5, colour = 'blue', alpha = 0.2)
g

## Diamond data residual plot

lm(price ~ 1) --> ~ 1 means "only take the intercept, no other variables influence price"

e <- c(resid(lm(price ~ 1, data = diamond)), # variation around the average price
  resid(lm(price Z carat, data = diamond))) # variation around the regression line
fit <- factor(c(rep('Itc', nrow(diamond)), # intercept only
  rep('Itc, slope', nrow(diamond)))) # intercept + slope
g <- ggplot(data.frame(e - e, fit = fit), aes(y = e, x = fit, fill = fit))
g <- g + geom_dotplot(binaxis = 'y', size = 2, stackdir = 'center', binwidth = 10)
g <- g + xlab('Fitting approach')
g <- g + ylab('Residual price')
g

# We can see that we have explained a lot of the variation by using carat (mass)
# We can decompose total variation (the varation by the Y's by themselves, perhaps the mean) into the variation
# that is explained and the variation that is left over after accounting for the regression model
# The subtraction of the two (total - left over) is the explained variaton: R^2

[Estimating Residual Variation]

- Model Yi = β0 + β1Xi + εi where εi ~ N(0, σ^2)
- The ML estimate of σ^2 is '1/n Σ ei^2', the average squared residual
- Most people use

^σ^2 = (1 / (n - 2)) * Σ ei^2

# For large n this is irrelevant

- The 'n - 2' instead of n is so that E[^σ^2] = σ^2

Diamond Example

y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
# Same as:
sqrt(sum(resid(fit)^2) / (n - 2))

Summarizing Variation
- The total variability in our response is the variability around an intercept (think mean only regression): Σ(Yi - Y-bar)^2
- The regression variability is the variability that is explained by adding the predictor: Σ(^Yi - Y-bar)^2
- The error variability is what is leftover around the regression line: Σ(Yi - ^Yi)^2
- Neat fact:

Σ(Yi - Y-bar)^2 == Σ(Yi - ^Yi)^2 + Σ(^Yi - Y-bar)^2

Total variability == Residual Variability + Regression Variability

R-Squared

- R-squared is the percentage of the total variability that is explained by the linear relationship with the predictor:

R^2 = Σ(^Yi - Y-bar)^2 / Σ(Yi - Y-bar)^2

Some facts about R^2

- R^2 is the percentage of variation explained by the regression model
- 0 <= R^2 <= 1
- R^2 is the sample correlation squared
- R^2 can be a misleading summary of model fit
  + Deleting data can inflate R^2
  + Adding terms to a regression model always increases R^2
- Do 'data(anscombe)' and 'example(anscombe)' to see the following data
  + Basically same mean and variance of X and Y
  + Identical correlations (hence same R^2)
  + Same linear regression relationship

-- Inference in Regression --

[Inference in Regression]

Recall our model and fitted values

- Consider the model

Yi = β0 + β1Xi + εi

β0 = intercept
β1Xi = slope
εi = error term

- ε ~ N(0, σ^2)
- We assume that the true model is known
- We assume that you have seen confidence intervals and hypothesis tests before
- ^β0 = Y-bar - ^β1 * X-bar # estimation of intercept
- ^β1 = Cor(Y, X) * (Sd(Y)/Sd(X)) # estimation of slope

Review

- Statistics like (^θ - θ) / ^σθ often have the following properties:
  + It is normally distributed and has a finite sample Students T distribution if the estimated
    variance is replaced with a sample estimate (under normality assumptions)
  + Can be used to test H0: θ = θ0 versus Ha : θ >, <, != θ0.
    # ^σθ == standard error
  + Can be used to create a confidence interval for θ via ^θ +- Q1-α/2 * ^σθ
    * Where Q1-α/2 is the relevant quantile from either a normal or T distribution
- In the case of regression with iid sampling assumptions and normal errors, our inferences
  will follow very similarly to what you saw in your inference class
- We will not cover asymptotics for regression analysis, but suffice it to say that under
  assumptions on the ways in which the X values are collected, the iid sampling model, and
  mean model, the normal results hold to create intervals and confidence intervals

Results

σ^β1^2 = Var(^β1) = σ^2 / Σ(Xi - X-bar)^2 # Variance of the slope
σ^β0^2 = Var(^β0) = (1/n + (X-bar^2 / Σ(Xi - X-bar)^2)) * σ^2 # Variance of the intercept

- In practice, σ is replaced by its estimate
- It is probably not surprising that our slope/intercept estimate, under iid Gaussian errors follows a t distribution
  with n - 2 degrees of freedom and a normal distribution for large n.

(^βj - βj) / ^σ^βj # ^σ^βj == standard error

- This can be used to create confidence intervals and perform hypothesis tests.

[Coding Example]

library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x # residuals, same as y - (beta0 + beta1 * x)
sigma <- sqrt(sum(e^2) / (n-2)) # estimate of the variability around the regression line, sqrt so stdev
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma
seBeta1 <- sigma / sqrt(ssx)
# Under our hypothesis the true value is supposed to be 0, so we do not have to subtract it here from beta0 and beta1 respectively
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
# 2 * pt() because we have a != hypothesis, > or < would have been 1 * pt()
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

coefTable
fit <- lm(y ~ x);
summary(fit)$coefficients

## Getting a confidence interval

sumCoef <- summary(fit)$coefficients
# [1,1] & [2,1] are the estimates
# [1,2] & [2,2] are the standard errors
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10 # to get 0.1 carat as unit, instead of 1 carat

Prediction of Outcomes

- Consdier predicting Y at a value of X
  + Predicting the price of a diamond given the carat
  + Predicting the height of a child given the height of parents
- The obvious estimate for prediction at point x0 is: ^β0 + ^β1x0
- A standard error is needed to create a prediction interval
- There is a distinction between intervals for the regression line at point x0 and the prediction of what a y would be at point x0
- Line at x0 SE: ^σ * (1/n + ((x0 - X-bar)^2 / Σ(Xi - X-bar)^2))^0.5
- Prediction interval SE at x0: ^σ * (1 + 1/n + ((x0 - X-bar)^2 / Σ(Xi - X-bar)^2))^0.5 # --> the '1 +' causes the prediction interval to be larger
# 1 / n --> typically SE goes down with more n, so this makes sense
# (x0 - X-bar)^2 --> prediction error will be lowest when x0 is equal to X-bar, your error will be lowest when you're predicting at the average mass of a diamond
# Σ(Xi - X-bar)^2))^0.5 --> How variable your x'es are. The more variable you x'es are, the smaller this total term (a/b) becomes and the lower the SE (larger denominator)
# Prediction interval: + 1 since we are adding a new variable

## Plotting the prediction intervals

library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
# I want the interval around the estimated line at that particular value of x, not for a potential new value of y for that particular x-value
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
# Here you got the interval for the value of y around that particular x-value
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y = y), aes(x = x, y = y), size = 4)
g

-- Week 3

[Multivariable Regression Analyses]

-- Multivariable Regression Part I --

- If I were to present evidence of a relationship between breath mint usage (mints per day, X)
  and pulmonary function (measured in FEV; a measure of long function) you would be skeptical
  + Likely, you would say, 'smokers tend to use more breath mints than on smokers, smoking is
    related to a loss in pulmonary function. That is probably the culprit.'
  + If asked what would convince you, you would likely say, 'If non-smoking breath mint users had lower
    lung function than non-smoking non-breath mint users and, similarly, if smoking breath mint users
    had lower lung function than smoking non-breath mint users, I would be more inclined to believe you'.
- In other words, to even consider my results, I would have to demonstrate that they hold while holding smoking status fixed.

- An insurance company is interested in how last years claims can predict a persons time in the hospital this year.
  + They want to use an enormous amount of data contained in claims to predict a single number. Simple linear regression is not equipped to handle more than one predictor.
- How can one generalize SLR (simple linear regression; X --> Y, 2 variables) to incoporate lots of regressors for the purpose of prediction?
- What are the consequences of adding lots of regressors?
  + Surely there must be consequences to throwing variables in that are not related to Y?
  + Surely there must be consequences to omitting variables that are?

The Linear Model

- The general linear model simple linear regression (SLR) by adding terms linearly into the model.

Yi = β1*X1i + β2*X2i + ... + βp*Xpi + ϵi

- Here X1i = 1 typically, so that an intercept is included
- Least squares (and hence ML estimes under iid Gaussianity of the errors) minimize:

Σ(Yi - Σ(βj*Xki)) # it looks at the difference between the outcomes and the summation of the coefficients * predictors (which forms the prediction)

- Note, the important linearity is linearity in the coefficients. Thus:

Yi = β1*X1i^2 + β2*X2i^2 + ... + βp*Xpi^2 + ϵi

is still a linear model. You do not square the coefficients but only the X-s.

-- Multivariable Regression Part II --

How to get estimates

- Recall the the LS estimate for regression through the origin, E[Yi] = X1i * β1, was Σ(Xi*Yi)/ Σ(Xi^2)
- Lets consider two regressors, E[Yi] = X1i * β1 + X2i * β2 = μi

 n
 Σ(Yi - X1i * β1 - X2 * β2)^2
i=1

Result

^β1 = Σ(ei,Y|X2ei,X1|X2) / Σ(ei^2,X1|X2) # ^β1 == fitted coefficient of β1

- That is, the regression estimate for β1 is the regression through the origin estimate having regressed (removed) X2 out of both the response and the predictor.
  + The coefficient X1 has been 'adjusted' for the effect of X2
  + The linear effect of X2 is removed from the response Y and variable X1
    * Then isolated effect of X1 on Y is investigated
- (Similarly, the regression estimate for β2 is the regression through the origin estimate having regressed X1 out of bot the response and the predictor.)
- More generally, multivariate regression estimates are exactly those having  near relationship of the other variables from both the regressor and response.

Example with two variables, simple linear regression
- Yi = β1*X1i + β2 * X2i where X2i = 1 is an intercept term
  + To obtain β1 we need to get rid of the effect X2 on X1 & Y
- Notice the fitted coefficient of X2i on Yi is Y-bar, since X2i is an intercept
  + The residuals ei,Y|X2 = Yi - Y-bar # --> what is left to map X1 on; the centered version of Y
- Notice the fitted coefficient of X2i on X1i is X1-bar
  + The residuals ei,X1|x2 = X1i - X1-bar # --> what is left to map X1 on; the centered version of X1
- Thus,

^β1 = Σ(ei,Y|X2ei,X1|X2) / Σ(ei^2,X1|X2) == Σ((Xi - X-bar) * (Yi - Y-bar)) / Σ(Xi - X-bar)^2 == Cor(X,Y) Sd(Y)/Sd(X)

The General Case

- Least squares solutions have to minimize:

Σ(Xi- X1i*β1 - ... - Xpi*βp)^2

- The least squares estimate fo rthe coefficient of a multivariate regression model is exactly
  regression through the origin with the linear relationships with other regressors removed from
  both the regressor and outcome by taking residuals.
- In this sense, multivariate regression 'adjusts' a coefficient for the linear impact of the
  other variables.

-- Multivariable Regression Continued --

n = 100
x = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
# all coefficients are 1 & rnorm(n, sd = .1) is random noice, the error term
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
# lm by default contains an intercept, this is the residual for Y having regressed out x2 + x3 and the intercept
ey = resid(lm(y ~ x2 + x3))
# residual for x having regressed out x2 + x3 & the intercept
ex = resid(lm(x ~ x2 + x3))
# This calculates the regression through the origin estimate for x: the coefficient
sum(ey * ex) / sum(ex ^ 2)
# This is the same:
coef(lm(ey ~ ex - 1)) # - 1 to remove the coefficient
# When regressing all of them, you will find the same coefficient for x
coef(lm(y ~ x + x2 + x3))

The interpretation of a multivariate regression coefficient is the expected change in the response
per unit change in the regressor, holding all of the other regressors fixed.

Fitted values, residuals and residual variation

All of our SLR quantities can be extended to linear models:
- Model Yi = Σ(Xik*βk + ε) where εi ~ N(0,σ^2)
- Fitted responses ^Yi = Σ(Xik*^βk)
- Residuals ei = Yi - ^Yi # observed - fitted
# Average Squared Residuals
- Variance estimate ^σ^2 = 1 / n-p * Σ(ei^2) # p is the number of regressors, n the number of examples/rows
- To get predicted responses at new values, x1, ..., xp, simply plug them into the linear model:

Σ(Xk*^βk)

Linear Models
- Linear models are the single most important applied statistical and machine learning technique so far.
- Some amazing things that you can accomplish with linear models include:
  + Decompose a signal into its harmonics.
  + Flexibily fit complicated functions.
  + Fit factor variables as predictors.
  + Uncover complex multivariate relationships with the response.
  + Build accurate prediction models.

[Multivariate Regression Example]

Data set for discussion

require(datasets)
data(swiss)
?swiss # show helpfile

- Standardized fertility measure and socio-economic indicators for each of 47 French-speaking
  provinces of Switzerland at about 1888
- A data frame with 47 observations on 6 variables, each of which is in percent, i.e. in [0, 100]
  + [,1] Fertility - a common standardized fertility measure
  + [,2] Agriculture - % of males involved in agriculture as occupation
  + [,3] Examination - % of draftees receiving highest mark on army examination
  + [,4] Education - % of education beyond primary school for draftees
  + [,5] Catholic - % catholic (as opposed to protestant)
  + [,6] Infant.Mortality - live births who live less than 1 year

All variables but Fertility give proportions of the population

require(GGally) # add on to ggplot
require(ggplot2)

g <- ggpairs(swiss, lower = list(continuous = 'smooth'), params = c(method = 'loess'))
g

summary(lm(Fertility ~ ., data = swiss))$coefficients # the '.' means 'all other variables'

Example interpretation

Our models estimate an expected 0.17 decrease in standardized fertility for every 1% increase
in percentage of males involved in agriculture in holding the remaining variables constant.

H0: βagri = 0 versus Ha : βagri != 0
- Subtract the Hypothesis value from the estimate (in this case 0) and divide by the Std. Error
  to get the t-value

summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients # the '.' means 'all other variables'
- Now it went from having a negative effect, to a positive effect, when not considering other variables
- Simpsons Paradox

# How can adjustment reverse the sign of an effect? Let's try a simulation.
n <- 100
x2 <- 1:n
x1 <- 0.01 * x2 + runif(n, -.1, .1)
y <- -x1 + x2 + rnorm(n, sd = 0.01)

summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2 ))$coef

Factor variables
- When you run a regression across a factor variable, it will automatically take
  one class of the factor variable as reference level.
  + The p-values can be interpreted as whether or not that specific class is different
    from the reference class
- If you omit the intercept (using '- 1') you instead interpret as to whether or not they
  are different from 0 (!= 0).
- To relevel, so that another factor level becomes the reference level:
spray2 <- relevel(InstectSprays$spray, 'C')
summary(lm(count ~ spray2, data = InsectSprays))$coeff # now C is the reference level, not A (the lowest alphanumeric one)

# Create a binary variable
library(dplyr)
swiss <- mutate(swiss, CatholicBin = 1 * (Catholic > 50)) # 1 * makes it binary (1/0), instead of logical (TRUE/FALSE)

[Multivariable Regression]

n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
2/

Do this to investigate the bivariate relationship

library(rgl)
plot3d(x1, x2, y)

Another way is to look at residuals:
resid(lm(y ~ x2))
resid(lm(x1 ~ x2))

Removing x2 from x1 & y. Plot the residuals --> you can see that there is a strong linear effect of x1 on y after having removed the effect of x2.

-- Residuals again --

[Residuals and Diagnostics]

data(swiss);
par(mfrow = c(2,2))
fit <- lm(Fertility ~ ., data = swiss)
plot(fit)

Leverage vs Influence
- If it is far away from the the other data ('cloud of points'), it has high Leverage (measured from the x-axis)
- If however does not exert high influence, if it for instance still falls on the regression slope
  + If it is far away, and away from the slope, it has high leverage, AND high influence

Summary of the plot
- Calling a point an outlier is vauge
- Outliers can be the result of spurious or real processes
- Outliers can conform to the regression relationship (i.e. being marginally outlying in X or Y,
  but not outlying given the regression relationship)

Influence Measures
- Do ?influence.measures to see the full suite of influence measures in stats. The measures include
  + rstandard - standardized residuals, residuals divided by their standard deviations)
  + rstudent - standardized residuals, residuals divided by their standard deviations, where the ith data point was deleted in the calculation of the standard deviation for the residual to follow a t distribution
  + hatvalues - measures of leverage
  + dffits - change in the predicted response when the ith point is deleted in fitting the model. # measure of influence
  + dfbetas - change in individual coefficients when the ith point is deleted in fitting the model. # measure of influence
  + cooks.distance - overall change in teh coefficients when the ith point is deleted.
  + resid - returns the ordinary residuals
  + resid(fit) / (1 - hatvalues(fit)) where fit is the linear model fit returns the PRESS residuals, i.e. the leave one out cross validation residuals - the difference in the response and the predicted response at data point $i$, where it was not included in the model fitting.

How do I use all of these things?
- Be wary of simplistic rules for diagnostic plots and measures. The use of these tools is context specific.
  Its better to understand what they are trying to accomplish and use them judiciously.
- Not all of the measures have meaningful absolute scales. You can look at them relative to the values across the data.
- They probe your data in different ways to diagnose different problems.
- Patterns in your residual plots generally indicate some poor aspect of model fit. These can include:
  + Heteroskedasticity (non constant variance).
  + Missing model terms.
  + Temporal patterns (plot residuals versus collection order).
- Residual QQ plots investigate normality of the errors.
- Leverage measures (hat values) can be useful for diagnosing data entry errors.
- Influence measures get to the bottom line, 'how does deleting or including this point impact a particular aspect of the model'.

Case 1
n <- 100
x <- c(10, rnorm(n))
y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = 'lightblue', col = 'black')

Showing a Couple of the Diagnostic Values
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)

dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
summary(lm(V1 ~ . -1, data = dat))$coef
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')

-- Model Selection --

Multiple Variables

- We have an entire class on prediction and machine learning, so we will focus on modeling.
- Like nearly all aspects of statistics, good modeling decisions are context dependent.
  + Prediction has a different set of criteria, needs for interpretability and standards for generalizability.
  + In modeling, our interest lies in parsimonious, interpretable representations of the data that enhance our understanding of the phenomena under study.
- A model is a lense through which to look at your data. (I attribute this quote to Scott Zeger)
  + Under this philosophy, what is the right model? Whatever model connects the data to a true,
    parsimonious statement about what you are studying.
- There are nearly uncountable ways that a model can be wrong, in this lecture, we will focus on variable
inclusion and exclusion.
- A good model for prediction versus one for studying mechanisms versus one for trying to
  establish causal effects may not be the same.

The Rumsfeldian triplet

"There are known knowns. These are things we know that we know. There are known unknowns. That
is to say, there are things that we know we don't know. But there are also unknown unknowns. There
are things we don't know we don't know." Donald Rumsfeld

In our context
- (Known knowns) -  Regressors that we know we should check to include in the model and have
- (Known Unknowns) - Regressors that we would like to include in the model, but don not have
- (Unknown Unknowns) Regressors that we do not even know about that we should have included in the model

General rules
- Omitting variables results in bias in the coeficients of interest unless their regressors are uncorrelated with the omitted ones
  + This is why we randomize treatments, it attempts to uncorrelate our treatment indicator with variables that we do not have to put in the model
  + If there iss too many unobserved confounding variables, even randomization will not help you
- Including variables that we should not have increases standard errors of the regression variables
  + Actually, including any new variables increase (actual, not estimated) standard errors of other regressors. So we do not want
    to idly throw variables into the model
- The model must tend toward perfect fit as the number of nonredundant regressors approaches
- R^2 increases monotonically as more regressors are included
- The SSE decreases monotonically as more regressors are included

Variance Inflation

n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2);
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

Variance Inflation Errors

- Notice variance inflation was much worse when we included a variable that was highly related to x1.
- We do not know sigma, so we can only estimate the increase in the actual standard error of the coefficients for including a regressor.
- However, sigma drops out of the relative standard errors. If one sequentially adds variables, one can check the variance (or sd) inflation for including each one.
- When the other regressors are actually orthogonal to the regressor of interest, then there is no variance inflation.
- The variance inflation factor (VIF) is the increase in the variance for the ith regressor compared to the ideal setting where it is orthogonal to the other regressors.
  + (The square root of the VIF is the increase in the sd ...)
- Remember, variance inflation is only part of the picture. We want to include certain variables, even if they dramatically inflate our variance.

What about residual variance estimation?

- Assuming that the model is linear with additive iid errors (with finite variance), we can mathematically describe the impact of omitting necessary variables or including unnecessary ones
  + If we underfit the model, the variance estimate is biased # because we attribute certain patterns to random noise whereas we could have mapped it to a variable
  + If we correctly or overfit the model, including all necessary covariates and/or unnecessary covariates, the variance estimate is unbiased
    * However, the variance of the variance is larger if we include unnecessary variables # including more variables leads to a less reliable estimate (when predicting), as variance increases

How to do nested model testing in R

fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)

-- Week 4

[GLM]

Linear Models

- Linear models are the most useful applied statistical technique. However, they are not without their limitations.
  + Transormations are often hard to interpret
  + There is value in modeling the data on the scale that it was collected
  + Particularly interpretable transformations, natural logarithms in specifc, are not applicable for negative or zero values

A GLM has 3 components:
1 - An exponential family model for the response
2 - A systematic component via a linear predictor
3 - A link function that connects the means of the response to the linear predictor

[Logistic Regression]

- Frequently we care about outcomes that have two values
  + Alive/dead
  + Win/loss
  + Success/Failure
- Called binary, Bernoulli or 0/1 outcomes
- Collection of exchangeable binary outcomes for the same covariate data are called binomial outcomes

download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda", destfile="./data/ravensData.rda",method="curl")
load("./data/ravensData.rda")
head(ravensData)

RWi = b0 + b1*RSi + ei

RWi - 1 if a Ravens win, 0 if not
RSi - Number of points Ravens scored
b0 - probability of a Ravens win if they score 0 points
b1 - increase in probability of a Ravens win for each additional point
e - residual variation due

lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef

Odds

- Binary outcome 0/1
- Probability that the Ravens win Pr(RWi|RSi, b0, b1)
- Odds (0,∞) == Pr(RWi|RSi, b0, b1) / (1-Pr(RWi|RSi, b0, b1))
  + Odds == P / (1-P) # P == probability
  + P = O / (1+O) # O == Odds
- Log odds (-∞,∞) == log(Pr(RWi|RSi, b0, b1) / (1-Pr(RWi|RSi, b0, b1))) # Also called the logit, to go back to the probabilties use the exbit (below)

Linear vs. Logistic Regression

- Linear: RWi = b0 + b1*RSi + ei
  + E[RWi|RSi,b0,b1] = b0+b1*RSi # The expected value
- Logistic: Pr(RWi|RSi,b0,b1) == exp(b0+b1*RSi) / (1+exp(b0+b1*RSi)) # in logistic regression, the expected value == the probability
  + log(Pr(RWi|RSi, b0, b1) / (1-Pr(RWi|RSi, b0, b1))) == b0+b1*RSi
  + Formula to go back to the probability: e^a / (1+e^a) # exbit, in contrast, the formula to go back to the regression model == logit
    * e^b0+b1x / (1+e^b0+b1x)

Interpreting Logistic Regression

log(Pr(RWi|RSi, b0, b1) / (1-Pr(RWi|RSi, b0, b1))) == b0+b1*RSi

- b0: Log(odds) of a Ravens win if they score zero points # remember that odds == P / (1-P)
  + e^b0 / (1 + e^b0) == probability the ravens wins when scoring 0 points, using the exbit!
- b1: Log(odds) ratio of win probability for each point scored (compared to zero points)
- exp(b1): Odds ratio of win probability for each point scored (compared to zero points)

 == We are using natural logs here -- to the base e! ==

- With logistic regression, you again look at the effect of a covariate while holding the other covariates constant.

# Visualizing fitting logistic regression curves

x <- seq(-10, 10, length = 1000)
manipulate(
plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)),
type = "l", lwd = 3, frame = FALSE),
beta1 = slider(-2, 2, step = .1, initial = 2),
beta0 = slider(-2, 2, step = .1, initial = 0)
)

# Ravens Logistic Regression

logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family = "binomial") # dependent ~ independent
summary(logRegRavens)

# Ravens fitted values

plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win") # predicted responses put back on the probability scale, using exbit

# Odds ratios and confidence intervals

exp(logRegRavens$coeff)

(Intercept) ravensData$ravenScore
   0.1864         1.1125

-> This suggests an 11% increase in the probability of winning for every 1 point increase.

exp(confint(logRegRavens)) # exp() == e^

                         2.5 %   97.5 %
(Intercept)           0.005675   3.106
ravensData$ravenScore 0.996230   1.303

-> Even though we know scoring points leads to winning, it is not significant, because at the 5% level it drops below 1 at the lower tail (.96).

# ANOVA for logistic regression

anova(logRegRavens,test="Chisq")

Interpreting Odds Ratios

- Wikipedia on Odds Ratio: https://en.wikipedia.org/wiki/Odds_ratio
- Not probabilities
- Odds ratio of 1 = no difference in odds
- Log odds ratio of 0 = no difference in odds
- Odds ratio < 0.5 or > 2 commonly a "moderate effect"
- Relative risk often easier to interpret, harder to estimate
- For small probabilities Relative Risk (RR) ≈ Odds Ratio (OR) but they are not the same!

-- Poisson Regression --

Key ideas

- Many data take the form of counts
  + Calls to a call center
  + Number of flu cases in an area
  + Number of cars that cross a bridge
- Data may also be in the form of rates
  + Percent of children passing a test
  + Percent of hits to a website froma country
- Linear regression with transformation is an option

- The Poisson distribution is a useful model for counts and rates
- Here a rate is count per some monitoring time
- Some examples uses of the Poisson distribution
  + Modeling web traffic hits
  + Incidence rates
  + Approximating binomial probabilities with small p and large n
  + Analyzing contingency/frequency table data

The Poisson mass function

X ~ Poisson(t*lambda) if

P(X = x) = ((t*lambda)^x * e^-t*lambda) / x! # lambda = rate, t = period of time

- For x = 0, 1, ...
- The mean of the Poisson is E[X] = t*lambda, thus E[X/t] = lambda
- The variance of the Poisson is Var(X) = t*lambda
- The Poisson tends to a normal distribution as t*lambda gets large:

par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE)

-> we increase the mean (lambda) & t a lot, and it becomes more and more normal.

Sort of, showing that the mean and variance are equal:

x <- 0 : 10000; lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

- Consider the daily counts to Jeff Leeks web site: http://biostat.jhsph.edu/~jleek/
- Since the unit of time is always one day, set $t = 1$ and then the Poisson mean is interpretted as web hits per day.
  (If we set $t = 24$, it would be web hits per hour).

download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",destfile="./data/gaData.rda",method="curl")
load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")

Linear regression line

NHi = b0 + b1*JDi + ei

NHi - number of hits to the website
JDi - day of the year (Julian day)
b0 - number of hits on Julian day 0 (1970-01-01)
b1 - increase in number of hits per unit day
ei - variation due to everything we did not measure

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)

Aside, taking the log of the outcome

- Taking the natural log of the outcome has a specific interpretation
- Consider the model:

log(NHi) = b0 + b1*JDi + ei

NHi - number of hits to the website
JDi - day of the year (Julian day)
b0 - log number of hits on Julian day 0 (1970-01-01)
b1 - increase in log number of hits per unit day
ei - variation due to everything we did not measure

- The geometric mean is just exponentiating (e^) the arithmetic mean of the log data

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
abline(lm1,col="red",lwd=3); lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

# Mean-variance relationship?

plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Model agnostic standard errors

library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object); pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2; a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                               pct))
    ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}

[Hodgepodge]

- If p is the probability of an event, the associated odds are p/(1-p)
- A generalized linear model which has these properties supposes that the log odds of a win depend linearly on the score.
  That is, log(p/(1-p)) = b0 + b1*score. The link function, log(p/(1-p)), is called the logit, and the process of finding
  the best b0 and b1, is called logistic regression.

-- To assess best multivariate regression model:

bestmodel = step(lm(data = mtcars, mpg ~ .), trace=0)
summary(bestmodel)

OR

fit1 <- lm(mpg ~ factor(am), data = mtcars)
fit2 <- lm(mpg ~ factor(am) + wt, data = mtcars)
fit3 <- lm(mpg ~ factor(am) + wt + factor(cyl), data = mtcars)
fit4 <- lm(mpg ~ factor(am) + wt + factor(cyl) + disp, data = mtcars)
fitAll <- lm(mpg ~ ., data = mtcars)
anova(fit1, fit2, fit3, fit4, fitAll)

Take the last model that is signifcant.

+

But using our model with weight and number of cylinders included we get:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 33.7535920 2.8134831 11.9970836 2.495549e-12
## factor(am)1 0.1501031 1.3002231 0.1154441 9.089474e-01
## wt -3.1495978 0.9080495 -3.4685309 1.770987e-03
## factor(cyl)6 -4.2573185 1.4112394 -3.0167231 5.514697e-03
## factor(cyl)8 -6.0791189 1.6837131 -3.6105432 1.227964e-03
Here, the intercept is interpreted as a baseline where the car has an automatic transmission, 4 cylinder engine,
and mean weight, and factor(am)1 is the expected change in mpg as compared to that baseline. Here we
see that this expected difference is practically northing.













