[Statistical Inference]

Definition: "Generating conclusions about a population from a noisy sample."

-- Week 1: Why Statistical Inference?

[Probability]

- Given a random experiment (say rolling a die) a probability measure is a population quantity that summarizes the randomness.
- Specifically, probability takes a possible outcome from the experiment and:
  + Assigns it a number between 0 and 1
  + So that the probability that something occurs is 1 (the die must be rolled) and
  + So that the probability of the union of any two sets of outcomes that have nothing in common (mutually exclusive) is the sum of their respective probabilities

Rules probability must follow
- The probability that nothing occurs is 0
- The probability that something occurs is 1
- The probability of something is 1 minus the probability that the opposite occurs
- The probability of at least one of two (or more) things that can not simultaneously occur (mutually exclusive) is the sum of their respective probabilities
- If an event A implies the occurrence of event B, then the probability of A occurring is less than the probability that B occurs
- For any two events the probability that at least one occurs is the sum of their probabilities minus their intersection (else you sum the intersection twice)

Discrete values without upper bound (e.g. web traffic hits) are usually investigated with the Poisson distribution (canonical PMF to model counts). 

[PMF] #assigns probabilities to specific values for discrete random variables 

- A probability mass function evaluted at a value corresponds to the probability that a discrete random variable takes that value. To be a valid pmf function, p must satisfy:
  1 - It must always be larger than or equal to 0.
  2 - The sum of the possible values that the random variable can take has to add up to one.
- Bernoulli distribution

X = 0 represents tails
X = 1 represents heads

p(x) = (1/2)^x * (1/2)^1-x # p(0) = 0.5 & p(1) = 0.5
p(x) = θ^x * (1-θ)^1-x # p(0) = θ - 1 & p(1) = θ 

[PDF]

- A probability density function (pdf), is a function associated with a continuous random variable. It must satisfy:
  1 - It must be larger than or equal to zero everywhere.
  2 - The total area under it must be one.

x <- c(-0.5, 0, 1, 1, 1.5)
y <- c(0, 0, 2, 0, 0)
plot(x, y, lwd = 3, frame = FALSE, type = 'l')

What is the probability that 75% or fewer of calls get addressed? # We are looking at a rectangular triangle, so we can calculate the surface underneat by taking 0.5 * Base * Height

1.5 * 0.75 / 2 #=> 0.5625
pbeta(0.75, 2, 1) #=> 0.5625 // beta distribution (for triangles), returns the less or equal than x probability

CDF and survival function

- The cumulative distribution function (CDF) of a random variable, X, returns the probability that the random variable is less than or equal to the value x.
F(x) = P(X <= x)

- The survival function of a random variable X is defined as the probability that the random variable is greater than the value of x
S(x) = P(X > x)

Notice that S(x) = 1 - F(x)

pbeta(c(0.4, 0.5, 0.6), 2, 1) # probabilities, height, width
#=> 0.16 0.25 0.36

Quantiles

- The α th quantile of a distribution with distribution function F is the point xα so that F(xα) = a
  + A percentile is simply a quantile with α expressed as a percent 
  + The median is the 50th percentile

We want to solve 0.5 = F(x) = x^2, which equals sqrt(0.5) # 0.7071 // F(x) = 1/2 * x * 2x = 1/2 * 2x^2 = 2x^2 / 2 = x^2

qbeta(0.5, 2, 1) #=> 0.7071 

Summary
- You might be wondering at this point "I've heard of a median before, it didn't require integration. Where's the data?"
- What we are referring to are population quantities. Therefore, the median being discussed is the population median.
- A probability model connects the data to the population using assumptions.
- Therefore the median we are discussing is the estimand, the sample median will be the estimator.

Conditional Probability

- Let B be an event so that P(B) > 0

P(A | B) == probability of A given that event B has occurred
P(A & B) == intersection # & is the cap symbol 

P(A | B) = P(A & B) / P(B)

A = {1}
B = {1, 3, 5}

P(one given that roll is odd) = P(A|B) = 1/6 / 3/6 = 2/6 = 1/3

P(A U B) = P(A) + P(B) - P(A & B) # P(A U B) == union probability (A or B; at least one)

QUIZ

P(F U M) == 0.17
P(F) == 0.12
P(F & M) == 0.06%
0.17 = 0.12 + P(M) - 0.06

Bayes Rule

P(B | A) = [P(A | B) * P(B)] / [P(A | B) * P(B) + P(A | B^c) * P(B^c)] 

Diagnostic tests
- Let + and - be the events that the result of a diagnostic test is positive or negative respectively
- Let D and D^c (D-complement) be the event that the subject of the test has or does not have the disease respectively

Sensitivity = P(+|D)
Specificity = P(-|D^c)
Positive predictive value = P(D|+)
Negative predictive value = P(D^c|-)
Prevalence of disease = P(D)

Example:
- Sensitivity of 99.7% # P(+|D)
- Specificity of 98.5% # P(-|D^c)
- Population with a .1% prevalence of HIV # P(D)

What is the associated positive predictive value?

P(D|+) = [P(+|D) * P(D)] / [P(+|D) * P(D) + P(+|D^c) * P(D^c)]
P(D|+) = [P(+|D) * P(D)] / [P(+|D) * P(D) + {1 - P(-|D^c)} * {1 - P(D)}]

P(+ | D^c) # Chances that the test is positive when subject does not have disease
==
1 - P(- | D^c) # 1 - the chance test is negative when subject does not have disease

Similar vein: P(D^c) == 1 - P(D)

QUIZ

Sensitivity = 0.75  # P(+|D)
Specificity = 0.52  # P(-|D^c)
Prevalence = 0.30   # P(D)

Want to know: P(D|+)

P(D|+) = [0.75 * 0.3] / [0.75 * 0.3 + (1 - 0.52) * (1 - 0.3)] = 0.225 / [0.225 + 0.336] = 0.4011


[Independence]

Event A is independent of event B if

P(A | B) = P(A) where P(B) > 0
P(A & B) = P(A) * P(B)

IID random variables

iid = independent and identically distributed

Random variables are said to be iid if they are independent and identically distributed
- Independent: statistically unrelated from one another
- Identically distributed: all having been drawn from the same population distribution
- iid random variables are the default model for random samples

[Expected Values]

- The process of making conclusions about populations from noisy data that was drawn from it.

library(manipulate)
myHist <- function(mu){
	g <- ggplot(galton, aes(x = child))
	g <- g + geom_histogram(fill = "salmon", binwidth = 1, aes(y = ..density..), colour = "black"
	g <- g + geom_density(size = 2)
	g <- g + geom_vline(xintercept = mu, size = 2)
	mse <- round(mean(galton$child - mu)^2), 3)
	g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
	g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

Example of a population mean
- Suppose a coin is flip X is declared 0 or 1 corresponding to a head or a tail, respectively.
- What is the expected value of X?

E[X] = 0.5 * 0 + 0.5 * 1 = 0.5

What about a biased coin?

Suppose that a random variable, X, is so that P(X = 1) = p and P(X = 0) = (1 - p)

E[X] = 0 * (1 - p) + 1 * p = p

[Continuous Random Variables]

- For a continuous variable, X, with density, f, the expected value is again exactly the center of mass of the density

Facts about expected values
- Recall that expected values are properties of distributions (center of mass of distribution)
- Note that the average of random variables is itself a random variable and its associated distribution has an expected value
- The center of this distribution is the same as that of the original distribution

-- Week 2

[Introduction to Variability]

- The variance of a random variable is a measure of spread

Var(X) = E[(X-μ)^2] = E[X^2] - E[X]^2

- The square root of the variance is called the standard deviation

Example:
- E[X] = 3.5 (when X is the toss of a die)
- E[X^2] = 1^2 * 1/6 + 2^2 * 1/6 + ... + 6^2 * 1/6 = 15.17
- Var(X) = E[X^2] - E[X]^2 = 2.92

What is the variance from the result of the toss of a coin with probability of heads (1) of p?
E[X] = 0 * (1 - p) + 1 * p = p
E[X^2] = E[X] = p # since 0^2 = 0 & 1^2 = 1, they are the same in this case!
Var(X) = E[X^2] - E[X]^2 = p - p^2 = p(1 - p)

Recall the mean

- Recall that the average of a random sample from a population is itself a random variable
  + It has its own population mean (which is the same as the original/actual population mean) and population variance

E[X] = μ
To relate the random sample variance back to the original/actual variance: Var(X) = σ^2/n # hence the variance of the sample mean goes towards zero as n increases, since the distribution centers more and more around the actual population mean it is trying to estimate
+ This is useful because in real life you only get one sample mean, not many as we did here with these many experiments

X = sample mean
μ = population mean
σ = population stdev
S = sample stdev
σ^2 = population variance
S^2 = sample variance
standard error or the mean = σ / n^0.5 # == (s^2/n)^0.5
Var(X) = σ^2/n # variance of the original population = sample variance / n?

- We call the standard deviation of a statistic (e.g. mean/stdef) a standard error # error refers to it's (potential) difference to what the true statistc is

To Summarize

- The sample variance, S^2, estimates the population variance, σ^2
- The distribution of S^2 is centered around σ^2
  + And it gets more centered around σ^2 with more observations
- The distribution of X is centered around μ
  + And it gets more centered around μ with more observations
- The variance of the sample mean, S^2, is σ^2/n 
  + Its logical estimate is S^2 / n
  + Hence the logical estimate of the standard error = S/(n^0.5)
    * S, the sample standard deviation, talks about how variable the population/sample is
    * S/(n^0.5), the standard error, talks about how variable averages of random samples of size n from the population are

Simulation Example

- Normal distribution
# Standard normals have variance 1 (and stdev 1); means of a random sample of n standard normals have stdev 1/(n^0.5) # standard error formula

nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))
#=> 0.3156
1 / sqrt(n) 
#=> 0.3162

- Uniform Distribution
# Standard uniforms have variance 1/12, means of a random sample of n uniforms have stdev 1/((12*n)^0.5)
nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
#=> 0.09017
1 / sqrt(12*n)
#=> 0.09129

- Poisson Distribution
# Poisson(4) have variance 4; means of a random sample of n Poisson have stdev 2/(n^0.5)
nosim <- 1000
n <- 10
sd(apply(matrix(rpois(nosim * n), nosim), 1, mean))
#=> 0.6219
2 / sqrt(n)
#=> 0.6325

- Fair coin flips
# Fair coin flips have variance 0.25, means of a random sample of n coin flips have stdev 1/(2*n^0.5)
nosim <- 1000
n <- 10
sd(apply(matrix(sample(0 : 1, nosim * n, replace = TRUE), nosim), 1, mean))
#=> 0.1587
1 / (2 * sqrt(n))
#=> 0.1581

The Sample Variance

Population mean --> center of mass of the population
Sample mean --> center of mass of the observed data (sample)

Population variance --> Var(X) = E[(X-μ)^2] = E[X^2] - E[X]^2 # the expected squared distance of a random variable from the poplutation around the population mean
Sample variance --> S^2 = [Σ(Xi - X)^2] / [n - 1] # the sample variance is the average squared distance of the observed observations minus the sample mean

Variance of the Sample Variance
- The sample variance is a function of data, it is also a random variable, so:
  + It has an associate population distribution
  + Its expected value is the population variance
  + As you collect more data, the distribution of the sample variance is going to get more concentrated around the population variance

Data Example

library(UsingR); data(father.son);
x <- father.son$sheight # son's height
n <- length(x) # number of observations

round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)), 2)
#=> [1] 7.29 0.01 2.81 0.09

- 7.29 = variance (in inches^2) # variability in childrens heights
- 0.01 = the variability in the average of childrens heights (in inches^2) # no longer about variability in children's heights
- 2.81 = standard deviation (in inches) # variability in childrens heights
- 0.09 = the variability in the average of childrens heights (in inches), standard error == the standard deviation in the distribution of averages of n childrens heights (an estimate)

Summarizing what we know about variances

- The sample variance is an estimate of the population variance
- The distribution of the sample variance is centered at what it is estimating
- It gets more concentrated around the population variance with larger sample sizes
- The variance of the sample mean is the population variance divided by n Var(X) = stdev^2 / n == stdev / n^0.5

[Some Common Distributions]

-- The Bernoulli distribution

- The Bernoulli distribution arises as the result of a binary outcome (say a coinflip)
  + Bernoulli random variables take (only) the values 1 and 0 with probabilities of (say) p and 1 - p
  + The mean of a Bernoulli random variable is p, the variance is p(1 - p)
    * 1 = success
    * 0 = failure

P(X = x) = (p^x) * (1-p)^(1-x)

Binomial Distribution

- A binomial random variable is obtained as the sum of a bunch of iid Bernoulli variables
  + In specific, let X1, ..., Xn be iid Bernoulli(p);
  + then X = ΣXi is a binomial random variable
- The total number of heads on the flips of a potentially biased coin

P(X = x) = ( n ) * p^x * (1-p)^(n-x)
           ( x )
  
   ( n ) = n! / (x!(n-x)!)
   ( x ) 

Example

Suppose a friend has 8 children (oh my!), 7 of which are girls.

- If each gender has an independent 50% probability for each birth, what
  is the probability of gettting 7 or more girls out of 8 births?

( 8 )  * 0.5^7 * (1 - 0.5)^1 + ( 8 ) * 0.5^8 * (1 - 0.5)^0 = 0.04
( 7 )                          ( 8 )

   # ( 8 ) = 8! / (7!(8-7)!)
   # ( 7 ) 

Same as:

choose(8, 7) * 0.5^8 + choose(8, 8) * 0.5^8
#=> 0.03516
pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE) # greater than 6 == 7 & 8
#=> 0.03516

-- Normal Distribution

- Normal/Gaussian distribution with mean (E[X]) μ and variance (Var(X)) σ^2 # X~N(μ,σ^2)
  + Bell curve, formula: (2π^2)^(1/2) * e^(-(x-μ)^2/(2/2σ^2)
- When μ = 0 and σ = 1, we have the standard normal distribution # standard random normals are often labeled Z

If X~N(μ,σ^2) # X is from a normal distribution with mean μ and variance σ^2

Z = (X - μ) / σ ~ N(0,1)
+ We convert the units of X to standard deviations from the mean, the resulting random variable Z is a standard normal
+ Conversely, if we have a standard normal (Z), we can convert it back to a non-standard normal:
X = μ + σZ ~ N(μ,σ^2)

More facts about the normal density
1 - Approximately 68%, 95% and 99% of the normal density lies within 1, 2 and 3 standard deviations from the mean, respectively.
2 - -1.28, -1.645, -1.96 and -2.33 are the 10th, 5th, 2.5th and 1st percentiles of the standard normal distribution respectively.
3 - 1.28, 1.645, 1.96 and 2.33 are the 90th, 95th, 97.5th and 99th percentiles of the standard normal distribution respectively.
  + In units of sigma.
  + For a not standard normal, the points are re-calculated as μ - 1.28*σ & μ + 1.28*σ (everything between these two points = 80%, between the 10th and 90th percentile), etc.

Question

- What is the 95th percentile of a N(μ,σ^2) distribution (X.95)?
  + Quick answer in R: qnorm(.95, mean = mu, sd = sd)
  + Since we know 1.645 if the 95th percentile, answer = μ + 1.645*σ

- What is the probability that a N(μ,σ^2) random variable (RV) is larger than x?
  + Quick answer in R: pnorm(x, mean = μ, sd = sigma, lower.tail = FALSE) or 1 - pnorm(x, mean = μ, sd = sigma)
  + A conceptual way is to calculate how many standard deviations away x is from the mean --> (x - μ) / σ # x espressed as how many standard deviations away from the mean
  + Example:
    * Assume that the number of daily ad clicks for a company is (approximately) normally distributed with a mean of 1020 and a standard deviation of 50.
      What is the probability of getting more than 1160 clicks in a day?
      - It is not very likely, since 1160 is 2.8 standard deviations away from the mean. 
        + pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE) 
          #=> 0.002555
        + pnorm(2.8, lower.tail = FALSE) # takes the standard normal distribution when no μ/σ given by default 
          #=> 0.002555
    * Assume that the number of daily ad clicks for a company is (approximately) normally distributed with a mean of 1020 and a standard deviation of 50.
      What number of daily ad clicks would represent the one where 75% of days have fewer clicks (assuming the days are independent and identically distriubted)?
      - qnorm(0.75, mean = 1020, sd = 50)
        #=> 1054

-- Poisson

P(X = x; ) = (λ^x(e^-λ)) / x! # mass function

+ The mean of this distributino is λ
+ The variance of the distribution is also λ
+ x = strictly non-negative integer # something cannot have occured less than 0 times

Some uses for the Poisson distribution
- Modeling count data
- Modeling event-time or survival data
- Modeling contingency tables (again counts, multi-dimensional, grouped by more than 2 categories)
- Approximating binomials when n is large and p is small # for instance number of people (n), and how many people contract a certain disease (p)

Rates and Poisson random variables
- Poisson random variables are used to model rates
- X~Poisson(λt) where
  + λ = E[X/t] is the expected count per unit of time # the average/expected number of occurrences for a given unit of time t
  - t is the total monitoring time

Example

- The number of people that show up at a bus stop is Poisson with a mean of 2.5 per hour. 
  If watching the bus stop for 4 hours, what is the probability that 3 or fewer people show up for the whole time? 
  + ppois(3, lambda = 2.5 * 4)
    #=> 0.01034

Poisson approximation to the binomial

- When n is large and p is small, the Poisson distribution is an accurate approximation to the binomial distribution
- Notation
  + X~Binomial(n,p)
  + λ = n*p
  - n gets large
  - p gets small

Example

We flip a coin with success probability 0.01 five hundred times. 
What is the probability of 2 or fewer successes? 

pbinom(2, size = 500, prob = 0.01)
#=> 0.1234
ppois(2, lambda = 500 * 0.01)
#=> 0.1247

[A Trip to Asymptopia]

Asymptotics

- Asymptotics is the term for the behavior of statistics as the sample size (or some other relevant quantity) limits
  to infinity (or some other relevant number)
- Asymptotics are incredibly useful for simple statistical inference and approximations
- Asymptotics form the basis for frequency interpretation of probabilities (the long run of proportion of times an event occurs)

Limits of random variables

- These results allow us to talk about the large sample distribution of sample means of a collection of iid observations
- The first of these results, the Law of Large Numbers, we intuitively know
  + It says that the average limits to what its estimating, the population mean
    * Example Xn could be the average of the result of n coin flips (i.e. the sample proportion of heads)
    * As we flip a fair coin over and over, it eventually converges to the true probability of a head

Law of Large numbers (LLN) in action

n <- 1000
means <- cumsum(rnorm(n))/(1:n)

# as the number of simulations increases, we get closer and closer to the actual mean

means <- cumsum(sample(0:1, n, replace = TRUE)) / (1:n)

Discussion

- An estimator is consistent if it converges to what you want to estimate
- The LLN says that the sample mean of iid samples is consistent for the population mean
- Typically, good estimators are consistent; it is not too much to ask that if we go through 
  the trouble of collecting an infinite amount of data that we get the right answer
- The sample variance and sample standard deviation of iid random variables are consistent as well

The Central Limit Theorem (CLT)

- For our purposes, the CLT states that the distribution of averages of iid variables (properly normalized) becomes
  that of a standard normal as the sample size increases.

(Xn - μ) / σ/(n^0.5) = (Estimate - Mean of estimate) / Std. error of estimate

Xn = sample average

+ Has a distribution like that of a standard normal for large n
+ The useful way to think about the CLT is that Xn is approximately N(μ,σ^2/n) # mean is population mean, variance is std error of the mean

Example

- Simulate a standard normal random variable by rolling n (six sided)
  + Let Xi be the outcome for die i
  + Then note that μ = E[Xi] = 3.5
  + Var(Xi) = 2.92
  + SE (2.92/n)^0.5 = 1.71 / n^0.5
  + Lets roll n dice, take their mean, subtract off 3.5, and divide by 1.71/n^0.5 # scaling by the standard error

Coin CLT

- Let Xi be the 0 or 1 result of the i'th' flip of a possibly unfair coin
  + The sample proportion, say ^p, is the average of the coin flips
  + E[Xi] = p and Var(Xi) = p(1 - p)
  + SE of the mean is (p(1-p)/n)^0.5
  + (^p - p) / (p(1-p)/n)^0.5 # should be normally distributed

-- Asymptotics and Confidence Intervals

- X (sample mean) is approximately normally distributed with mean μ and sd σ/n^0.5 # SE
- Probability that X is bigger than μ + 2σ or smaller than μ - 2σ is 5% (technically 1.96σ)
- X +- 2σ / n^0.5 is called a 95% interval for μ

Example: Give a confidence interval for the average height of simulations

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(lenght(x)))/12 # dividing by 12 so that the confidence interval will be in feet rather than inches
#=> 5.710 5.738 --> 95% chance the average height lies between these two numbers

-- Sample proportions

- In the event that each Xi is 0 or 1 with common success probability p then σ^2 = p(1-p)
- The interval takes the form:

^p +- z[1-a/2] * ((p(1-p)/n))^0.5

- Replacing p by ^p in the standard error results in what is called a Wald confidence interval for p
- For 95% intervals:

^p +- 1/n^0.5

is a quick estimate for p

Example

- Your campaign advisor told you that in a random sample of 100 likely voters, 56 intent to vote for you. 
  + Can you relax? Do you have this race in the bag?
  + Without access to a computer or calculator, how precise is this estimate?
- 1/sqrt(100) = 0.1, so a back of the envelope calculation gives an approximate 95% interval of (0.46, 0.66)
  + Not enough for you to relax, better go do more campaigning!
- Rough guidelines, 100 for 1 decimal place, 10000 for 2, 1000000 for 3

round(1/sqrt(10^(1:6)), 3)
#=> 0.316 0.100 0.032 0.010 0.003 0.001

Binomial Interval

0.56 + c(-1, 1) * qnorm(0.975) * sqrt(0.56 * 0.44 / 100)
#=> 0.4627 0.6573

binom.test(56, 100)$conf.int
#=> 0.4572 0.6592
#=> attr(,"conf.level")
#=> 0.95

Simulation

n <- 20 # change to 100 for better results
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p)) {
  phats <- rbinom(nosim, prob = p, size = n)/n
  ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
  ul <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
  mean(ll < p & ul > p)
}

What is happening?

- n is not large enough (20) for the CLT to be applicable for many of the values of p
- Quick fix, form the interval with (X + 2)/(n + 4)
  + Add two successes and failures, Agresti/Coull interval

n <- 20 # change to 100 for better results
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p)) {
  phats <- (rbinom(nosim, prob = p, size = n) + 2) / (n + 4)
  ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n) # phat = ^p
  ul <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
  mean(ll < p & ul > p)
}

Poisson Interval

- A nuclear pump failed 5 times out of 94.32 days. Give a 95% confidence interval for the failure rate per day?
  + X ~ Poisson(λt)
  + Estimate ^λ = X/t # λ = failure rate, t = number of days
  + Var(^λ) = λ/t
  + ^λ/t is our variance estimate

x <- 5 
t <- 94.32
lambda <- x/t
round(labmda + c(-1, 1) * qnorm(0.975) * sqrt(lambda/t), 3)
#=> 0.007 0.099

poisson.test(x, T = 94.32)$conf
#=> 0.01721 0.12371
#=> attr(,"conf.level")
#=> 0.95

Summary

- The LLN states that averages of iid samples converge to the population means that they are estimating
- The CLT states that averages are approximately normal, with distributions:
  + centered at the population mean
  + with standard deviation equal to the standard error of the mean
  + CLT gives no guarantee that n is large enough
- Taking the mean and adding and subtracting the relevant normal quantile times the SE yields a confidence interval for the mean
  + Adding and subtracting 2 SEs works for 95% CIs
- CIs get wider as the coverage increases (why?)
  + The more sure you want to be the CI contains the parameter, the wider the procedure makes the interval
- The Poisson and binomial case have exact intervals that do not require the CLT
  + A quick fix for small sample size binomial calculations is to add 2 successes and failures

-- Week 3: Confidence Intervals and Testing

[T Confidence Intervals]

- In the previous lectures, we discussed creating a confidence interval using the CLT:
  + They took the form Estimate +- ZQ * SE 
    * Z quantile of the normal distribution
    * SE of the estimate
- We will now also look at the T distribution:
  + Est +- TQ * SE
    * We change the Z-Quantile to a T-Quantile, using the T-distibrution
      - This distribution has heavier tails than the normal distribution (so these intervals will be wider)
      - Whenever in doubt between Z/T, use T:
        + As you collect more data, T will become like Z anyway

Gossets t distribution
- Invented by William Gosset (under pseudonym 'Student') in 1908
- Has thicker tails than the normal distribution
- Is indexed by a degrees of freedom; gets more like a standard normal as df gets larger
- It assumes that the underlying data are iid Gaussian with the result that (X - μ) / (S / n^0.5)
  + Follows Gossets t distribution with n - 1 degrees of freedom
  + Interval is X +- t(n-1) * S/n^0.5 where t(n-1) is the relevant quantile
  + degrees of freedom = n-1

Notes about the t interval
- The t interval technically assumes that the data are iid normal, though it is robust to this assumption
- It works well whenever the distribution of the data is roughly symmetric and mound shaped
- Paired observations (Xt vs Xt+1) are often analyzed using the t interval by taking differences
- For large degrees of freedom, t quantiles become the same as standard normal quantiles; therefore this 
  interval converges to the same interval as the CLT yielded
- For skewed distributions, the spirit of the t interval assumptions are violated
  + Also, for skewed distributions, it does not make a lot of sense to center the interval at the mean
  + In this case, consider taking logs or using a different summary like the median
- For highly discrete data, like binary, other intervals are available

Example: Dependent Groups / Paired

Sleep Data
- In R typing data(sleep) brings up the sleep data originally analyzed in Gossets Biometrika paper,
  which shows the increase in hours for 10 patients for 10 patients on two soporific drugs.
  R treats the data as two groups rather than paired.

Results:

g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
m <- mean(difference); s <- sd(difference); n <- 10

m + c(1, -1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)

Example: Independent Group t confidence intervals

- Suppose that we want to compare the mean blood pressure between two groups in a 
  randomized trial; those who received the treatment to those who received a placebo.
  + We cannot use the paired t test because the groups are independent and may have different sample sizes
  + We now present methods for comparing independent groups

Confidence Interval

- Therefore a (1 - α) * 100% confidence interval μy - μx is:

Y - X +- tnx+ny-2,1-α/2 * Sp * (1/nx + 1/ny)^0.5

+ Sample mean Y - sample mean X
+ t-quantile:
  * DF: nx + ny - 2
  * 1-α/2
  * Sp^2 = {(nx - 1)*Sx^2 + (ny - 1)*Sy^2} / (nx + ny - 2) # pooled variance, its srt is the pooled sdev
  # You simply weigh them according to sample sizes, if sizes are the same, it is equal to the average
  - Remember that this interval is assuming a constant variance across the two groups
  - If there is some doubt, assume a different variance per group, which we will cover later on
+ If we collect a lot of data, nx & ny become small

Example: Comparing SBP for 8 oral contraceptive users versus 21 controls
- Xoc = 132.86 mmHg with soc = 15.34 mmHg # n = 8
- Xc = 127.44 mmHg with sc = 18.23 mmHg # n = 21
- Pooled variance estimate

sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(1, -1) * qt(0.975, 27) * sp * (1/8 + 1/21)^0.5
# OC - C --> not the other way around, since you think OC will be better than C

Another Example: Mistakenly treating the Sleep Data as Grouped

n1 <- length(g1); n2 <- length(g2)
sp <- sqrt(((n1-1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2 - 2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1 / n2) # this is the standard error (of the mean difference)!
rbind(
md + c(-1, 1) * qt(0.975, n1 + n2 - 2) * semd
t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf
t.test(g2, g1, paired = TRUE)$conf
)
#=> -0.2039  3.364
#=> -0.2039  3.364
#=>  0.7001  2.460

ChickWeight data in R

library(datasets); data(ChickWeight); library(reshape2)
## define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1: 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW, # this creates a new column, gain, in df wideCW
  gain = time21 - time0
  )

# Comparing diet 1 to diet 4

wideCW14 <- subset(wideCW, Diet %in% c(1,4))
rbind(
t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf 
t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf 
)

Unequal Variances

- When the variance between the groups is unequal, it does not follow a t-distribution
  + It can be approximated by a t-distribution, by changing the formula to calculate the degrees of freedom (no longer nx + ny - 2):

df = (Sx^2/nx + Sy^2/ny)^2 / ((Sx^2/nx)^2 / (nx - 1) + (Sy^2/ny)^2 / (ny - 1))

--> When in doubt whether or not the variances of the groups are equal, just use var.equal = FALSE.

Example

Comparing SBP for 8 oral contraceptive users versus 21 controls
- Xoc = 132.86 mmHg with soc = 15.34 mmHg
- Xc = 127.44 mmHg with sc = 18.23 mmHg
- df = 15.04, t15.04,975 = 2.13 # df calculated with the new df formula
- Interval:

132.86 - 127.44 +- 2.13 * (15.34^2/8 + 18.23^2/21)^0.5

- In R, t.test(..., var.equal = FALSE)

Comparing other kinds of data

For binomial data, there are lots of ways to compare two groups
- Relative risk, risk difference, odds ratio.
- Chi-squared tests (expected vs observed), normal approximations, exact tests.
For count data, there is also Chi-squared tests and exact tests.

[Hypothesis Testing]

- Hypothesis testing is concerned with making decisions using data
- A null hypothesis is specified that represents the status quo, usally labeled H0
  + The null hypothesis is assumed true and statistical evidence is required to reject it in favor of a research or alternative hypothesis

Example - Respiratory Disturbance Index

- A respiratory disturbance index of more than 30 events / hour, say, is considered evidence of severe sleep disordered breathing (SBD).
- Suppose that in a sample of 100 overweight subjects with other risk factors for sleep disordered breathing at a sleep clinic, the mean RDI
  was 32 events / hour with a standard deviation of 10 events / hour. 
- We might want to test the hypothesis that:
  + H0: μ = 30
  + Ha: μ > 30
  - where μ is the population mean RDI
- The alternative hypotheses are typicall of the form <, > or !=
- Note that there are four possible outcomes of our statistical decision process:

1 - Truth: H0 | Decide: H0 | Result: Correctly accept null
2 - Truth: H0 | Decide: Ha | Result: Type I error (FP)
3 - Truth: Ha | Decide: Ha | Result: Correctly reject null
4 - Truth: Ha | Decide: H0 | Result: Type II error (FN)

Type I & Type II rates are related: If Type I rate increases, Type II rate decreases, and vice-versa. 

- Consider a court of law; the null hypothesis is that the defendant is innocent
- We require a standard on the available evidence to reject to null hypothesis (convict)
- If we set a low standard, then we would increase the percentage of innocent people convicted (type I errors - FPs);
  however we would also increase the percentage of guilty people convicted (correctly, rejecting the null)
- If we set a high standard, then we increase the percentage of innocent people let free (correctly accepting the null)
  while we would also increase the percentage of guilty people let free (type II - FNs)

Example - Respiratory Disturbance Index (cntd..)

- A reasonable strategy would reject the null hypothesis if X was larger than some constant C
- Typically, C is chosen so that the probability of a Type I error (FP), α, is 0.05 (or some other relevant constant)
- α = Type I error rate = Probability of rejecting the null hypothesis when, in fact, the null hypothesis is correct # you do not want this, but you also don't want to set α too low, hence you'd never reject the null hypothesis

- Standard error of the mean = 10 / 100^0.5 = 1
- Under H0 X ~ N(30, 1)
- We want to choose C so that the P(X > C; H0) is 5%
- The 95th percentile of a normal distribution is 1.645 standard deviations from the mean
- IF C = 30 + 1 * 1.645 = 31.645
  + Then the probability that a N(30,1) is larger than it is 5%
  + So the rule "Reject H0 when X >= 31.645" has the property that the probability of rejection is 5% when H0 is true (for the μ, σ and n given)

- In general we do not convert C back to the original scale
- We would just reject because the Z-score; which is how many standard errors the sample mean is above the hypothesized mean; is greather than 1.645.

(32-30) / (10/10^0.5) = 2 --> > 1.645 # divide the difference between the sample and hypothesized mean, divide by the SE

- Or whenever n^0.5(X-μ)/s > Z1-α

Example reconsidered (T-tests)

# One-sided test

- Consider our example again. Suppose that n = 16 (rather than 100)
- The statistic (X - 30) / (s/16^0.5) follows a T distribution with 15 df under H0.
  + Under H0 the probability that it is larger than the 95th percentile of the T distribution is 5%
  + The 95th percentile of the T distribution with 15 df is 1.7531 (obtained via qt(.95, 15))
  + Our test statistic is now: 16^0.5(32 - 30)/10 = 0.8
    * We fail to reject H0 since 0.8 < 1.7531

# Two sided test

- Suppose that we would reject the null hypothesis if in fact the mean was too large or too small
- That is, we want to test the alternative Ha: μ != 30
- We will reject H0 is the test statistic, 0.8, is either too large or too small
  + Since the statistic is positive, we only need to consider the large side (positive side)
- Then we want the probability of rejecting under the null to be 5%, split equally as 2.5% in the upper tail and 2.5% in the lower tail
- Thus we reject if our test statistic is larger than qt(.975, 15) or smaller than qt(0.025, 15)
  + This is the same as saying: reject if the absolute value of our statistic is larger than qt(0.975, 15)
  + So we fail to reject the two sided test as well
  + (If you fail to reject the one-sided test, you know that you will fail to reject the two sided test)

T-test in R

One-Group Testing

library(UsingR)
data(father.son)
t.test(father.son$sheight - father.son$fheight) # paired same as t.test(father.son$sheight, father.son$fheight, paired = TRUE)

#=> t = 11.79 # if true mean = 0, our sample would fall at this test statistic, very unlikely, hence H0 rejected
#=> df = 1077 # 1178 - 1 observations 
#=> p-value < 2.2e-16
#=> conf interval: 0.831 1.163 --> this a 95% conf interval where the actual mean lies
#=> mean of x: 0.997

Connections with Confidence Intervals

- Consider testing H0: μ = μ0 versus Ha: μ != μ0 # μ0 mu not
- Take the set of all possible values for which you fail to reject H0, this set is a (1-α)100% confidence interval for μ
- The same works in reverse; if a (1-α)100% interval contains μ0, then we fail to reject H0

Two-Group Testing

- First, now you know how to do two group T tests since we already covered independent group T intervals
- Rejection rules are the same 
- Test H0 : μ1 = μ2

Example

library(datasets); data(ChickWeight); library(reshape2)
## Define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1:2)] <- paste("time", names(wideCW)[-(1:2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
  gain = time21 - time0 
  )

# Equal/Unequal Variance T test comparing diets 1 and 4

wideCW14 <- subset(wideCW, Diet %in% c(1,4))
t.test(gain ~ Diet, paried = False, var.equal = TRUE, data = wideCW14)
#=> t = -2.725
#=> df = 23
#=> p-value = 0.01207
#=> alternative hypothesis: true difference in means is not equal to 0
#=> 95% conf interval: -108.15 -14.81
#=> sample estimates: mean group 1: 136.2, mean group 4: 197.7

Exact Binomial Test

- Recall this problem, "Suppose a friend has 8 children, 7 of which are girls and none are twins"
- Perform the relevant hypothesis test: H0: p = 0.5, Ha: p > 0.5
  +  What is the relevant rejection region, so that the probability of rejecting (H0 incorrectly; FP) is less than 5%?

Rejection Region   Type I Error Rate
[0 : 8]          -         1
[1 : 8]          -         0.9961 
[2 : 8]          -         0.9648
[3 : 8]          -         0.8555
[4 : 8]          -         0.6367
[5 : 8]          -         0.3633
[6 : 8]          -         0.1455
[7 : 8]          -         0.0352 # so at 7:8, < 0.05
[8 : 8]          -         0.0039

- It is impossible to get an exact 5% level test for this case due to the discreteness of the binomial.
  + The closest rejection region is [7 : 8]
  + Any alpha level lower than 0.0039 is not attainable
- For larger sample sizes, we could do a normal approximation
- Two sided test is not obvious
  + Given a way to do two sided tests, we could take the set of values of p0 for which we fail to reject
    to get an exact binomial confidence interval (called the Clopper/Pearson interval)
    * So at 5%, this would be [0-1: 8] and [8 : 8] # the confidence interval (when fail to reject) is the inverse of this
- For these problems, people always create a P-Value rather than computing the rejection region

[P-Values]

- Most common measure of statistical significance 
- Their ubiquity, along with concern over interpretation and use makes them controversial among statisticians

What is a P-value?

- Idea: Suppose nothing is going on - how unusual is it to see the estimate we got? # How unusal is the result we got if H0 is true?
- Approach:
  1 - Define the hypothetical distribution of a data summary (statistic) when "nothing is going on" (null hypothesis)
  2 - Calculate the summary/statistic with the data we have (test statistic)
  3 - Compare what we calculated (2) to our hypothetical distribution (1) and see if the value is "extreme" (p-value)

- The P-value is the probability under the null hypothesis of obtaining evidence (test-statistic) as extreme or more extreme than that obtained
  + If the P-value is small, then either H0 is true and we have observed a rare event, or H0 is false

- Suppose that you get a T statistic of 2.5 for 15 df testing H0: μ = μ0 versus Ha: μ > μ0
  + What is the probability of getting a T statistic as large as 2.5 

pt(2.5, 15, lower.tail = FALSE)
#=> 0.01225

The attained significance level

- Our test statistic was 2 for H0 : μ0 = 30 versus Ha: μ > 30
  + Notice that we rejected the one sided thest when α = 0.05 (since that equals 1.645 [assuming normal distribution, not t-distribution]),
    would we reject if α = 0.01, how about α = 0.001?
  + The P-value therefore is the smallest value of α for which you would still reject H0

Notes 

- By reporting a P-value the reader can perform the hypothesis test at whatever α level he or she chooses
- If the P-value is less than α you can reject the null hypothesis at that signficance level
- For two sided hypothesis tests, double the smaller of the two one-sided hypothesis test P-values

Revisiting an earlier example

- Suppose a friend has  children, 7 of which are girls and none are twins
- If each gender has an independent 50% probability for each birth, what is the probability of getting 7 or more girls out of 8 births?

H0: p = 0.5 versus Ha: p > 0.5

choose(8,7) * 0.5^8 + choose(8,8) * 0.5^8
#=> 0.03516
pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE)

Poisson Example

- Suppose that a hospital has an infection rate of 10 infections per 100 person/days at risk (rate of 0.1) during the last monitoring period
- Assume that an infection rate of 0.05 is an important benchmark
- Given the model, could the observed rate being larger than 0.05 be attriubted to chance? 
  + Under H0: λ = 0.05 so that λ0100 = 5
  + Consider Ha: λ > 0.05

ppois(9, 5, lower.tail = FALSE) # 10 or more given rate of 5
#=> 0.03183 # unlikely to see this result
ppois(9, 5, upper.tail = FALSE) # 10 or less

Appending a vector in a loop: 

for (i in 1 : 4000) {
  smns <- c(smns, mean(sa))
} 

https://en.wikipedia.org/wiki/Q–Q_plot --> to check whether distributions of differing populations are the same

-- Week 4: Power, multiple comparisons, resampling

[Power]

- Power is the probability of rejecting the null hypothesis when it is false
  + Ergo, power (as its name would suggest) is a good thing; you want more power
- A type II error (a bad thing, as its name would suggest) is failing to reject the
  null hypothesis when it is false; the probability of a type II error is usually called β
  * Power = 1 - β
  * Remember that type I (false-positive) error rate is α!

Example

- Consider our previous example involving RDI
- H0: μ = 30 versus Ha: μ > 30
- Then power is:

P((X - 30)/ (s/n^0.5) > t1-α,n-1; μ=μa) # μa = 30

- Note that this is a function that depends on the specific value of μa
- Notice as μa approaches 30, the power approaches α

Calculating power for Gaussian data

- We reject if (X - 30) / σ/n^0.5 > z1-α

alpha = 0.05
mu0 = 30
mua = 32
sigma = 4
n = 16
z <- qnorm(1 - alpha)
pnorm(mu0 + z * sigma/sqrt(n), mean = mu0, sd = sigma/sqrt(n), lower.tail = FALSE)
#=> 0.05
pnorm(mu0 + z * sigma/sqrt(n), mean = mua, sd = sigma/sqrt(n), lower.tail = FALSE)
#=> 0.63876 == Power, the probability of detecting a mean as large as 32 or larger, if we conduct this experiment

- When testing Ha: μ > μ0, notice if power is 1 - β, then:

1 - β = P(X > μ0 + z1-α * σ/n^0.5, μ = μa) # this is what we do in the R-code above

- Where X ~ N(μa, σ^2/n)
- Unknowns: μa, σ, n, β
- Knowns: μ0, α
- Specify any 3 of the unknowns and you can solve for the remainder

-- The calcuation for Ha: μ < μ0 is similar
-- For Ha: μ != μ0 calcualte the onde sided power using α/2 (this is only approximately right,
   it excludes the probability of getting a large TS in the opposite direction of the truth)

- Power goes up as α gets larger (more FP but less FN)
- Power of a one sided test is greater than the power of the associated two sided test
- Power goes up as μ1 gets further away from μ0
- Power goes up as n goes up
- Power goes down as the variance increases
- Power does not need μa, σ and n, instead only (n^0.5(μa-μ0))/σ
  + The quantity (μa-μ0)/σ is called the effect size, the difference in the 
    means in standard deviation units.

[T-Test Power]

- Consider calculating power for a Gossetts T test for our example
- The power is:

P((X - μ0)/(S/(n^0.5)) > t1-α,n-1; μ=μa)

- Calculating this requires the non-central t-distribution
- power.t.test does this very well
  + Omit one of the arguments and solves for it

power.t.test(n = 16, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$power
#=> 0.604
power.t.test(n = 16, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$power
#=> 0.604
power.t.test(n = 16, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$power
#=> 0.604

- Values are the same, because they derive from: (μa - μ0) / σ # effect size
  + Note that delta = μa - μ0

power.t.test(power = 0.8, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$n
#=> 26.14
power.t.test(power = 0.8, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$n
#=> 26.14
power.t.test(power = 0.8, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$n
#=> 26.14 --> thus 27.

[Multiple Testing]

- Hypothesis testing/significance analysis is commonly overused
- Correcting for multiple testing avoids false positives or discoveries
- Two key components
  + Error measure
  + Correction

Three Eras of Statistics

1 - The age of Quetelet and his successors, in which huge census-level data sets 
    were brought to bear on simple but important questions: Are there more female births?
    Is the rate of insanity rising?

2 - The classical period of Pearson, Fisher, Neyman, Hotelling, and their successorts, intellectual
    giants who developed a theory of optimal inference capable of wringing every drop of information
    out of a scientific experiment. The questions dealt with still tended to be simple, is treatment A
    better than treatment B?

3 - The era of scientific mass production, in which new technologies typified by the microarray allow a
    single team of scientists to produce data sets of a size Quetelet would envy. But now the flood of data
    is accompanied by a deluge of questions, perhaps thousands of estimates or hypothesis tests that the 
    statistician is charged with answering together; not at all what the classical masters had in mind. Which 
    variables matter among the thousands measures? How do you relate unrelated information?

Jellybean Example:
- Say you perform 20 hypotheses tests at 5% to find a linkage between jelly beans and acne.
- The first 19 turn up insignificant. The 20th however finds a linkage at the 5%.
- However, at 5%, 1 out of 20 tests should/will turn up positive (5% * 20 = 100%).
- This is what you need to correct for!

Types of Errors
- Suppose you are testing a hypothesis that a parameter β equals zero versus the alternative that it
  does not equal zero. These are the possible outcomes:
                   
                   β = 0            β != 0             HYPOTHESES
Claim β  = 0         U                 T                  m - R
Claim β != 0         V                 S                    R
Claims               m0              m - m0                 m

- Type I error or false positive (V): say that the parameter does not equal zero when it does.
- Type II error or false negative (T): say that the parameter equals zero when it does not.  

Error Rates
- False positive rate: the rate at which false results E[V/m0]
  + The false positive rate is closely related to the type I error rate
- Family wise error rate (FWER): the probability of at least one false positive Pr(V >= 1)
- False discovery rate (FDR): the rate at which claims of significance are false E[V/R]

Controlling the False Positive Rate

If P-values are correctly calculated calling all P < α significant will control the 
false positive rate at level α on average.

Problem: Suppose that you perform 10,000 tests and β (false-negative rate) = 0 for all of them.
Suppose that you call P < 0.05 significant. 
The expected number of false positives is: 10,000 * 0.05 = 500 false positives.
How do we avoid so many false positives?

Controlling family-wise error rate (FWER)
- The Bonferroni correction is the oldest multiple testing correction.
- Basic idea:
  + Suppose you do m tests
  + You want to control FWER at level α so Pr(V >= 1) < α
  + Calculate P-values normally
  + Set αf - α / m 
  + Call all P-values less than αf significant
- Pros: easy to calculate, conservative
- Cons: May be very conservative

Controlling False Discovery Rate (FDR) # Benjamini Hochberg correction
- This is the most popular correction when performing lots of tests in say genomics,
  imagining, astronomy, or other signal processing disciplines.
- Basic idea:
  + Suppose you do m tests
  + You want to control FDR at level α so E[V/R]
  + Calculate P-values normally 
  + Order the P-values from smallest to largest P(1), ..., P(m)
  + Call any P(i) <= α * i/m significant 
    * i is the element number
    * so as we progress to larger p-values, i/m becomes smaller and smaller
    * the correction becomes less and less
- Pros: still pretty easy to calculate, less conservative (maybe much less)
- Cons: allows for more false-positives, may behave strangely under dependence

Adjusted P-values
 - One approach is to adjust the threshold α
 - A different approach is to calculate "adjustested p-values"
 - They are not p-values anymore
 - But they can be used directly without adjusting α

 Example: 
 - Suppose P-values are P1, ..., Pm
 - You could easily adjust them by taking Pi^fwer = max (m * Pi), 1 for each P-value. # multiply the p-value by the number of tests
 - Then if you call all Pi^fwer < α signfificant you will control the FWER.

Case Study I: No True Positives

set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000){
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y ~ x)$coeff[2,4])
}

# Controls false positive rate
sum(pValues < 0.05)
#=> 51 --> But there was no relation whatsoever..

# Controls FWER
sum(p.adjust(pValues, method = "bonferroni") < 0.05)
#=> 0

# Controls FDR
sum(p.adjust(pValues, method = "BH") < 0.05)
#=> 0

Case Study II: 50% True Positives

set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
  x <- rnorm(20)
  # First 500 beta=0, last 500 beta=2
  if(i <= 500){y <- rnorm(20)}else{y <- rnorm(20,mean = 2*x)}
  pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

trueStatus <- rep(c("zero", "not zero"), each = 500)
table(pValues < 0.05, trueStatus)

#=>            trueStatus | no-correction
#=>        not zero      zero
#=> FALSE     0          476
#=> TRUE     500         24 

--> 24 FPs

# Controls FWER
table(p.adjust(pValues, method = "bonferroni") < 0.05, trueStatus)

#=>            trueStatus | bonferroni
#=>        not zero      zero
#=> FALSE     23         500
#=> TRUE     477         0

--> No FPs but 23 FNs

#=>            trueStatus | BH
#=>        not zero      zero
#=> FALSE     0          487
#=> TRUE     500         13

--> No FNs, less FPs

Notes
- Multiple testing is an entire subfield
- A basic Bonferroni/BH correction is usually enough
- If there is strong dependence between tests there may be problems
  + Consider method = "BY"

-- Resampled Inference

[Bootstrap]

- The bootstrap is a tremendously useful tool for constructing confidence intervals and calculating standard
  errors for difficult statistics
- For example, how ould one derive a confidence interval for the median?

- Say you have a sample of 50 rolls of a dice.
- Mechanically what we are doing is taking each observation, each 1, 2, 3, 4, 5 or 6 we get and throwing them into a bag.
- And then we draw out samples of size 50 where we are drawing WITH replacement. # same datapoint/observation may get taken out twice!
  + So from the original sample, if we had say 10% the number 1, every observation has a 10% chance of being 1.
- We take the average of the new sample, and repeat this process over and over again.
- It is the same procedure we would have done on the actual population distribution if we could.
  + Now we use our observed data, to construct an estimated population distribution. We simulate from 
    that population distribution to figure out the distribution of a statistic that we are interested in

[Bootstrapping Example]

Consider a data set

library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n) # gives a b * n matrix of samples
resampledMedians <- apply(resamples, 1, median) # take the median per row (1)

[Notes on the Bootstrap]

The Bootstrap Principle
- Suppose that I have a statistic that estimates some population parameter, but I do not know its sampling distribution
- The bootstrap principle suggests using the distribution defined by the data to approximate its sampling distribution
- In practice, the bootstrap principle is always carried out using simulation
- We will only cover only a few aspects of bootstrap resampling (creating a confidence interval and estimating standard errors)
- The general procedure follows by first simulating complete data sets from the observed data with replacement
  + This is approximately drawing from the sampling distribution of that statistic, at least as far as 
    the data is able to approximate the true population distribution
- Calculate the statistic for each simulated data set
- Use the simulated statistic to either define a confidence interval or take the standard deviation to calculate a standard error

Nonparametric bootstrap algorithm example
- Bootstrap procedure for calculating confidence interval for the median from a dataset of n observations:

i.   Sample n observations with replacement from the observed data resulting in one simulated complete data set
ii.  Take the median of the simulated data set
iii. Repeat these two steps B times, resulting in B simulated medians # B should be 10,000 or more
iv.  These medians are approximately drawn from the sampling distribution of the median of n observations;
     therefore we can:
     * Draw a histogram of them
     * Calculate their standard deviation to estimate the standard error of the median
     * Take the 2.5th and 97.5th percentiles as confidence interval for the median 

Example Code

B <- 10000
resamples <- matrix(sample(x n * B, replace = TRUE), B, n)
medians <- apply(resamples, 1, median)
sd(medians) # estimated standard error of the median
#=> 0.08473 

quantile(medians, c(0.025, 0.975)) # generate 95% confidence interval
#=>   2.5%       97.5%
#=>  68.43       68.82

# Histogram of bootstrap resamples
g <- ggplot(data.frame(medians = medians), aes(x = medians))
g <- g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g

Notes on the Bootstrap
- The bootstrap is non-parametric # our example was the non-parametric bootstrap
- Better percentile bootstrap confidence intervals correct for bias # use the BCA: Bias Corrected and Accelerated Interval, easy to implement in the bootstrap R-package
- There are lots of variations on bootstrap procedures, the book "An Introduction to the Bootstrap" by
  Efron and Tibshirani is a great place to start for bootstrap and jackknife information

[Permutation Tests]

Group Comparisons

- Consider comparing two independent groups.
- Example, comparing sprays B and C.

Permutation Tests

- Consider the null hypothesis that the distribution of the observations from each group is the same
  + Then, the group labels are irrelevant!
- Consider a data frame with count (col 1) and spray (col 2)
- Permute the spray (group) labels
- Recalculate the statistic
  + Mean difference in counts
  + Geometric means
  + T-Statistic
- Calculate the percentage of simulations where the simulated statistic was more extreme (toward the alternative) than the observed

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
#=> 13.25
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))

mean(permutations > observedStat) # how many times was the mean of one of our permutations larger than the actual observed mean (non-permuted group labels)
#=> 0