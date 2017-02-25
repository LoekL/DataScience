
# Notes: JHU - Advanced R Programming
# Credits @ Roger D. Peng

### Week 1

## Lecture 1: Control Structures

x <- c("a", "b", "c", "d")

# seq_along(x) instead of 1:4
for(i in seq_along(x)) { 
  print(x[i])
}

# It is not necessary to use an index-type variable.

for(letter in x) {
  print(letter)
}

# For one line loops, the curly braces are not strictly necessary.

for(i in 1:4) print(x[i])

# Nested for loop:

x <- matrix(1:6, 2, 3)

# seq_len(x): generate a sequence of lengtht x
for(i in seq_len(nrow(x))) { 
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}

# next is used to skip an iteration of a loop:

for(i in 1:100) {
  if(i <= 20) {
    ## Skip the first 20 iterations
    next                 
  }
  ## Do something here
}

# break is used to exit a loop immediately, regardless of what iteration the loop may be on.

for(i in 1:100) {
  print(i)
  
  if(i > 20) {
    ## Stop loop after 20 iterations
    break  
  }     
}

## Lecture 2: Functions

library(readr)
library(dplyr)

# 1 - Code

# Download data from RStudio (if we haven't already)

if(!file.exists("data/2016-07-20.csv.gz")) {
  download.file("http://cran-logs.rstudio.com/2016/2016-07-20.csv.gz", 
                "data/2016-07-20.csv.gz")
}
cran <- read_csv("data/2016-07-20.csv.gz", col_types = "ccicccccci")
cran %>% filter(package == "filehash") %>% nrow
# > 179

# 2 - Function Interface

# Given the date and package name, the function downloads the appropriate download logs 
# from the RStudio server, reads the CSV file, and then returns the number of downloads 
# for the package.

# pkgname: package name (character)
# date: YYYY-MM-DD format (character)

num_download <- function(pkgname, date) {
  # Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  
  # Construct path for storing local file
  dest <- file.path("data", basename(src))
  
  # Don't download if the file is already there!
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

# Now we can call our function using whatever date or package name we choose.

num_download("filehash", "2016-07-20")
# > 179

# 3 - Default values

# We can set a default value for the date argument, for example, to be July 20, 2016. 
# In that case, if the date argument is not explicitly set by the user, 
# the function can use the default value.

num_download <- function(pkgname, date = "2016-07-20") {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("data", basename(src))
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

num_download("Rcpp")
# > 14761

# 4 - Refactoring Code

# It might make sense to abstract the first two things on this list into a separate function.

check_for_logfile <- function(date) {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("data", basename(src))
  if(!file.exists(dest)) {
    val <- download.file(src, dest, quiet = TRUE)
    if(!val)
      stop("unable to download file ", src)
  }
  dest
}

num_download <- function(pkgname, date = "2016-07-20") {
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}  

# 5 - Dependency Checking

# We can write a separate function to check that the packages are installed.

check_pkg_deps <- function() {
  # The require() function is similar to library(), however library() stops with an error if the 
  # package cannot be loaded whereas require() returns TRUE or FALSE depending on whether 
  # the package can be loaded or not.
  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first")
}

# Now, our updated function can check for package dependencies.

num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

# 6 - Vectorization

# The two things we need to do are:
# 1 - Adjust our call to filter() to grab rows of the data frame that fall within a vector of package names
# 2 - Use a group_by() %>% summarize() combination to count the downloads for each package.

## 'pkgname' can now be a character vector of names
num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package %in% pkgname) %>% 
    group_by(package) %>%
    summarize(n = n()) # n() is count function
}

num_download(c("filehash", "weathermetrics"))

# > A tibble: 2 × 2
# > package     n
# > <chr> <int>
# > 1       filehash   179
# > 2 weathermetrics     7

# 7 - Argument Checking

num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  
  ## Check arguments
  if(!is.character(pkgname))
    # Stop the function & print message
    stop("'pkgname' should be character")
  if(!is.character(date))
    stop("'date' should be character")
  if(length(date) != 1)
    stop("'date' should be length 1")
  
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", 
                   progress = FALSE)
  cran %>% filter(package %in% pkgname) %>% 
    group_by(package) %>%
    summarize(n = n())
}    

## Week 1 - Swirl Assignments

# 0 - Swirl FAQ

install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_course("Advanced R Programming")

# When you are at the R prompt (>):
# -- Typing skip() allows you to skip the current question.
# -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
# -- UNTIL you type nxt() which will regain swirl's attention.
# -- Typing bye() causes swirl to exit. Your progress will be saved.
# -- Typing main() returns you to swirl's main menu.
# -- Typing info() displays these options again.

# 1 - Functions

# Submitting a function without parentheses will return its details, boring_function():

boring_function <- function(x) {x + 1}
boring_function
# > function(x) {x + 1}

function(x) {
  # The last expression evaluated in a function will be returned!
  # Or you explicitly use return()
  x
}

# Setting default argument values:

increment <- function(number, by = 1) {
  number + by
}

# When you explicitly designate argument values by name, the ordering of the arguments becomes unimportant:

increment(by = 2, number = 10)
# > 12

# Type: args(increment) to examine the arguments for the increment function.
args(increment)
# > function (number, by = 1)

# Passing functions as arguments:

evaluate <- function(func, dat) {
  func(dat)
}

# 2 - Using anonymous functions:

evaluate(function(x){x+1}, 6)
# > 7

# 3 - Ellipsis / dot-dot-dot

# As you can see the first argument of paste() is `...` which is referred to as an ellipsis or simply dot-dot-dot. 
# The ellipsis allows an indefinite number of arguments to be passed into a function. 
# In the case of paste() any number of strings can be passed as arguments and paste() will return all of the strings combined into one string.

telegram <- function(...){
  paste('START', ..., 'STOP')
}

telegram('Hello', 'How', 'Are')
# > START Hello How Are STOP"

## 4 - New Binary Operators

# The syntax for creating new binary operators in R is unlike anything else in
# R, but it allows you to define a new syntax for your function. I would only
# recommend making your own binary operator if you plan on using it often!

# User-defined binary operators have the following syntax:
#      %[whatever]% 
# where [whatever] represents any valid variable name.

# Let's say I wanted to define a binary operator that multiplied two numbers and
# then added one to the product. An implementation of that operator is below:

"%mult_add_one%" <- function(left, right) { # Notice the quotation marks!
  left * right + 1
}

# I could then use this binary operator like `4 %mult_add_one% 5` which would evaluate to 21.

"%p%" <- function(a, b){ 
  paste(a, b)
}

'a' %p% 'b'
# > 'a  b'

### Week 2

## Lecture 1: Functional Programming

# 1 - What is Functional Programming?

library(purrr)

adder_maker <- function(n){
  function(x){
    n + x
  }
}

add2 <- adder_maker(2)
add3 <- adder_maker(3)

add2(5)
# > 7
add3(5)
# > 8

# 2 - Core Functional Programming Functions

# 3 - Map

# In the purrr package the map() function returns a list, while the map_lgl(), map_chr(), and map_dbl()
# functions return vectors of logical values, strings, or numbers respectively

library(purrr)

map_chr(c(5, 4, 3, 2, 1), function(x){
  c("one", "two", "three", "four", "five")[x]
})
# > "five"  "four"  "three" "two"   "one"  

map_lgl(c(1, 2, 3, 4, 5), function(x){
  x > 3
})
# > FALSE FALSE FALSE  TRUE  TRUE

# The map_if() function takes as its arguments a list or vector containing data, 
# a predicate function, and then a function to be applied. A predicate function is 
# a function that returns TRUE orFALSE for each element in the provided list or vector. 
# In the case ofmap_if(): if the predicate functions evaluates to TRUE, 
# then the function is applied to the corresponding vector element, 
# however if the predicate function evaluates to FALSE then the function is not applied.

map_if(1:5, function(x){
  x %% 2 == 0
},
function(y){
  y^2
}) %>% unlist()
# > 1  4  3 16  5

# The map_if() function always returns a list, so I’m piping (dplyr is loaded?) the result of map_if() to unlist() so 
# it look prettier. Notice how only the even numbers are squared, while the odd numbers are left alone.

# The map_at() function only applies the provided function to elements of a vector specified by their indexes.

map_at(seq(100, 500, 100), c(1, 3, 5), function(x){
  x - 10
}) %>% unlist()
# > [1] 90 200 290 400 490

# In each of the examples above we have only been mapping a function over one data structure, 
# however you can map a function over two data structures with the map2() family of functions. 
# The first two arguments should be two vectors of the same length, 
# followed by a function which will be evaluated with an element of the first vector as the first argument and 
# an element of the second vector as the second argument. 

map2_chr(letters, 1:26, paste)
# > [1] "a 1"  "b 2"  "c 3"  "d 4"  "e 5"  "f 6"  "g 7"  "h 8"  "i 9"  "j 10"
# > [11] "k 11" "l 12" "m 13" "n 14" "o 15" "p 16" "q 17" "r 18" "s 19" "t 20"
# > [21] "u 21" "v 22" "w 23" "x 24" "y 25" "z 26"

# The pmap() family of functions is similar to map2(), however instead of mapping across two vectors or lists, 
# you can map across any number of lists. The list argument is a list of lists that the function will map over, 
# followed by the function that will applied:

pmap_chr(list(
  list(1, 2, 3),
  list("one", "two", "three"),
  list("uno", "dos", "tres")
), paste)
# > [1] "1 one uno"    "2 two dos"    "3 three tres"

# 4 - Reduce

# List or vector reduction iteratively combines the first element of a vector with the second element of a vector, 
# then that combined result is combined with the third element of the vector, and so on until the end of the vector 
# is reached. The function to be applied should take at least two arguments. Where mapping returns a vector or a 
# list, reducing should return a single value.

reduce(c(1, 3, 5, 7), function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  x + y
})

# x is 1
# y is 3

# x is 4
# y is 5

# x is 9
# y is 7

# > [1] 16

# Here’s a similar example using string data:

reduce(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})

# x is a
# y is b

# x is ab
# y is c

# x is abc
# y is d

# > [1] "abcd"

# By default reduce() starts with the first element of a vector and then the second element and so on. 
# In contrast the reduce_right()function starts with the last element of a vector and then proceeds 
# to the second to last element of a vector and so on:

reduce_right(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})

# x is d
# y is c

# x is dc
# y is b

# x is dcb
# y is a

# > [1] "dcba"

# 5 - Search

# You can search for specific elements of a vector using the contains() and detect() functions. 
# contains() will return TRUE if a specified element is present in a vector, otherwise it returns FALSE:

contains(letters, "a")
# > [1] TRUE
contains(letters, "A")
# > [1] FALSE

# The detect() function takes a vector and a predicate function as arguments and it returns the first 
# element of the vector for which the predicate function returns TRUE:

detect(20:40, function(x){
  x > 22 && x %% 2 == 0
})
# > [1] 24

# The detect_index() function takes the same arguments, however it returns the index of the provided vector 
# which contains the first element that satisfies the predicate function:

detect_index(20:40, function(x){
  x > 22 && x %% 2 == 0
})
# > [1] 5

# 6 - Filter

# For keep() only the elements of the vector that satisfy the predicate function are returned 
# while all other elements are removed:

keep(1:20, function(x){
  x %% 2 == 0
})
# > [1]  2  4  6  8 10 12 14 16 18 20

# The discard() function works similarly, it only returns elements that don’t satisfy the predicate function:

discard(1:20, function(x){
  x %% 2 == 0
})
# > [1]  1  3  5  7  9 11 13 15 17 19

# The every() function returns TRUE only if every element in the vector satisfies the predicate function, 
# while the some() function returns TRUE if at least one element in the vector satisfies the predicate function:

every(1:20, function(x){
  x %% 2 == 0
})
# > FALSE

some(1:20, function(x){
  x %% 2 == 0
})
# > TRUE

# 7 - Compose 

# Finally, the compose() function combines any number of functions into one function:

n_unique <- compose(length, unique)
# The composition above is the same as:
# n_unique <- function(x){
#   length(unique(x))
# }

rep(1:5, 1:5)
# > [1] 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5

n_unique(rep(1:5, 1:5))
# > [1] 5

# 8 - Partial Application

# Using the partial() function from the purrrpackage you can specify some of the arguments of a function, 
# and then partial() will return a function that only takes the unspecified arguments.

mult_three_n <- function(x, y, z){
  x * y * z
}

mult_by_15 <- partial(mult_three_n, x = 3, y = 5)

mult_by_15(z = 4)

# By using partial application you can bind some data to the arguments of a function before using that function elsewhere.

# 9 - Side Effects

# If you want to evaluate a function across a data structure you should use the walk() function from purrr.

walk(c("Friends, Romans, countrymen,",
       "lend me your ears;",
       "I come to bury Caesar,", 
       "not to praise him."), message)

# > Friends, Romans, countrymen,
# > lend me your ears;
# > I come to bury Caesar,
# > not to praise him.

# 10 - Recursion

# Recursion is very powerful tool, both mentally and in software development, for solving problems. 
# Recursive functions have two main parts: a few easy to solve problems called “base cases,” 
# and then a case for more complicated problems where the function is called inside of itself. 
# The central philosophy of recursive programming is that problems can be broken down into 
# simpler parts, and then combining those simple answers results in the answer to a complex problem.

# Imagine you wanted to write a function that adds together all of the numbers in a vector. 
# You could of course accomplish this with a loop:
  
vector_sum_loop <- function(v){
  result <- 0
  for(i in v){
    result <- result + i
  }
  result
}

vector_sum_loop(c(5, 40, 91))
# > [1] 136

# You could also think about how to solve this problem recursively. 
# First ask yourself: what’s the base case of finding the sum of a vector? 
# If the vector only contains one element, then the sum is just the value of that element. 
# In the more complex case the vector has more than one element. 
# We can remove the first element of the vector, but then what should we do with the rest of the vector? 
# Thankfully we have a function for computing the sum of all of the elements of a vector 
# because we’re writing that function right now! So we’ll add the value of the first element 
# of the vector to whatever the cumulative sum is of the rest of the vector. 
# The resulting function is illustrated below:

vector_sum_rec <- function(v) {
  if(length(v) == 1) {
    v
  } else {
    # First round: 5 + vector_sum_rec(c(40, 91))
    # Second round: vector_sum_rec(c(40, 91)) == 40 + vector_sum_rec(c(91))
    # Third round: vector_sum_rec(c(91)) == 91
    # Result: 91 + 40 + 5
    v[1] + vector_sum_rec(v[-1])
  }
}

vector_sum_rec(c(5, 40, 91))
# > [1] 136

# Let’s write a function to compute the nth digit of the Fibonacci sequence such that 
# the first number in the sequence is 0, the second number is 1, and then all proceeding 
# numbers are the sum of the n - 1 and the n - 2 Fibonacci number.

fib <- function(n) {
  stopifnot(n > 0)
  if(n == 1) {
    0
  } else if (n == 2) {
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

# Example:
# fib(4) == fib(3) + fib(2) == fib(3) + 1
# fib(3) == fib(2) + fib(1) == 1 + 0
# fib(4) == 1 + 0 + 1 == 2

map_dbl(1:12, fib)
# > [1]  0  1  1  2  3  5  8 13 21 34 55 89

# There is one optimization that we could apply here which comes up in recursive programming often. 
# When you execute the function fib(6), within that function you’ll executefib(5) and fib(4). 
# Then within the execution of fib(5),fib(4) will be executed again.

# This duplication of computation slows down your program significantly as you calculate larger numbers 
# in the Fibonacci sequence. Thankfully you can use a technique called memoization in order to speed 
# this computation up. Memoization stores the value of each calculated Fibonacci number in table so that 
# once a number is calculated you can look it up instead of needing to recalculate it!

fib_tbl <- c(0, 1, rep(NA, 23))

fib_mem <- function(n) {
  stopifnot(n > 0)
  
  if(!is.na(fib_tbl[n])) {
    fib_tbl[n]
  } else {
    fib_tbl[n - 1] <<- fib_mem(n - 1)
    fib_tbl[n - 2] <<- fib_mem(n - 2)
    fib_tbl[n - 1] + fib_tbl[n - 2]
  }
}

map_dbl(1:12, fib_mem)
# > [1]  0  1  1  2  3  5  8 13 21 34 55 89

# It works! But is it any faster than the original fib()? 
# Below I’m going to use the microbenchmark package in order assess whether fib()or fib_mem() is faster:

library(purrr)
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)

# Microbenchmark outputs summary data in microseconds (system.time() 2.0)
# Appending $time on the function call will output the actual times in nanoseconds
fib_data <- map(1:10, function(x){microbenchmark(fib(x), times = 100)$time})
names(fib_data) <- paste0(letters[1:10], 1:10)
fib_data <- as.data.frame(fib_data)

# %<>% equals == in dplyr
fib_data %<>%
  # Create a 2-column df with num and time as cols
  gather(num, time) %>%
  group_by(num) %>%
  # Get the median for each group
  summarise(med_time = median(time))

memo_data <- map(1:10, function(x){microbenchmark(fib_mem(x))$time})
names(memo_data) <- paste0(letters[1:10], 1:10)
memo_data <- as.data.frame(memo_data)

memo_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))

plot(1:10, fib_data$med_time, xlab = "Fibonacci Number", ylab = "Median Time (Nanoseconds)",
     pch = 18, bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = 1:10)
axis(2, at = seq(0, 350000, by = 50000))
points(1:10 + .1, memo_data$med_time, col = "blue", pch = 18)
legend(1, 300000, c("Not Memorized", "Memoized"), pch = 18, 
       col = c("black", "blue"), bty = "n", cex = 1, y.intersp = 1.5)

# 11 - Summary

# - Functional programming is based on lambda calculus.
# - This approach concentrates on data, variables, functions, and function applications.
# - It’s possible for functions to be able to return other functions.
# - The core functional programming concepts can be summarized in the following categories: map, reduce, search, filter, and compose.
# - Partial application of functions allows functions to be used like data structures.
# - Side effects are difficult to debug although they motivate a huge fraction of computer programming.
# - The most important part of understanding recursion is understanding recursion.

## Lecture 2: Expressions & Environments 

# 1 - Expressions

# Expressions are encapsulated operations that can be executed by R.

two_plus_two <- quote(2 + 2) # Same as: {2 + 2}
two_plus_two
# > 2 + 2

# You can execute this expressions using the eval() function:

eval(two_plus_two)
# > [1] 4

# You might encounter R code that is stored as a string that you want to evaluate with eval(). 
# You can use parse() to transform a string into an expression:

tpt_string <- "2 + 2"

tpt_expression <- parse(text = tpt_string)

eval(tpt_expression)
# > [1] 4

# You can reverse this process and transform an expression into a string using deparse():

deparse(two_plus_two)
# > [1] "2 + 2"

# One interesting feature about expressions is that you can access and modify their contents like you a list(). 
# This means that you can change the values in an expression, or even the function being executed in the expression 
# before it is evaluated:

sum_expr <- quote(sum(1, 5))
eval(sum_expr)
# > [1] 6
sum_expr[[1]]
# > sum
sum_expr[[2]]
# > [1] 1
sum_expr[[3]]
# > [1] 5
sum_expr[[1]] <- quote(paste0)
sum_expr[[2]] <- quote(4)
sum_expr[[3]] <- quote(6)
eval(sum_expr)
# > [1] "46"

# You can compose expressions using the call() function. 
# The first argument is a string containing the name of a function, 
# followed by the arguments that will be provided to that function.

sum_40_50_expr <- call("sum", 40, 50)
sum_40_50_expr
# > sum(40, 50)
eval(sum_40_50_expr)
# > [1] 90

# You can capture the the expression an R user typed into the R console when they executed a function 
# by including match.call() in the function the user executed:

return_expression <- function(...) {
  match.call()
}

return_expression(2, col = "blue", FALSE)
# > return_expression(2, col = "blue", FALSE)

# You could of course then manipulate this expression inside of the function you’re writing.

first_arg <- function(...) {
  expr <- match.call()
  first_arg_expr <- expr[[2]]
  first_arg <- eval(first_arg_expr)
  if(is.numeric(first_arg)){
    paste("The first argument is", first_arg)
  } else {
    "The first argument is not numeric."
  }
}

first_arg(2, 4, "seven", FALSE)
# [1] "The first argument is 2"

first_arg("two", 4, "seven", FALSE)
# [1] "The first argument is not numeric."

# 2 - Environments

# You can create a new environment using new.env(). You can assign variables in that environment in a 
# similar way to assigning a named element of a list, or you can use assign(). 
# You can retrieve the value of a variable just like you would retrieve the named element of a list, 
# or you can use get(). Notice that assign() and get() are opposites:

my_new_env <- new.env()
my_new_env$x <- 4
my_new_env$x
# > [1] 4

assign("y", 9, envir = my_new_env)
get("y", envir = my_new_env)
# > [1] 9
my_new_env$y
# > [1] 9

# You can get all of the variable names that have been assigned in an environment using ls(), 
# you can remove an association between a variable name and a value using rm(), 
# and you can check if a variable name has been assigned in an environment using exists():

ls(my_new_env)
# > [1] "x" "y"
rm(y, envir = my_new_env)
exists("y", envir = my_new_env)
# > [1] FALSE
exists("x", envir = my_new_env)
# > [1] TRUE
my_new_env$x
# > [1] 4
my_new_env$y
# > NULL

# Environments are organized in parent/child relationships such that every environment keeps track of its parent, 
# but parents are unaware of which environments are their children. You can see the parents of the global environment 
# using the search() function:

search()
# > [1] ".GlobalEnv"             "package:magrittr"
# > [3] "package:tidyr"          "package:microbenchmark"
# > [5] "package:purrr"          "package:dplyr"
# > [7] "package:readr"          "package:stats"
# > [9] "package:graphics"       "package:grDevices"
# > [11] "package:utils"          "package:datasets"
# > [13] "Autoloads"              "package:base"

# As you can see package:magrittr is the parent of .GlobalEnv, and package:tidyr is parent of package:magrittr, 
# and so on. In general the parent of .GlobalEnv is always the last package that was loaded using library(). 
# Notice that after I load the ggplot2 package, that package becomes the parent of .GlobalEnv:

library(ggplot2)
search()
# > [1] ".GlobalEnv"             "package:ggplot2"
# > [3] "package:magrittr"       "package:tidyr"
# > [5] "package:microbenchmark" "package:purrr"
# > [7] "package:dplyr"          "package:readr"
# > [9] "package:stats"          "package:graphics"
# > [11] "package:grDevices"      "package:utils"
# > [13] "package:datasets"       "Autoloads"
# > [15] "package:base"

# 3 - Execution Environments

# An execution environment is an environment that exists temporarily within the scope of a function 
# that is being executed. For example if we have the following code:

x <- 10

my_func <- function(){
  x <- 5
  return(x)
}

my_func()
# > 5

# In contrast to the situation above, take a look at this variation:

x <- 10

another_func <- function(){
  return(x)
}

another_func()
# > [1] 10

# In this situation the execution environment inside of another_func() does not contain an assignment 
# for the name x, so R looks for an assignment in the parent environment of the execution environment 
# which is the global environment.

# You can use the complex assignment operator to re-assign or even create name-value bindings in the 
# global environment from within an execution environment.

x <- 10
x
# > [1] 10

assign1 <- function(){
  x <<- "Wow!"
}

assign1()
x
# > [1] "Wow!"

# You can also use <<- to assign names to values that have not been yet been defined in the global environment 
# from inside a function:

a_variable_name
# > Error in eval(expr, envir, enclos): object 'a_variable_name' not found
exists("a_variable_name")
# > [1] FALSE

assign2 <- function(){
  a_variable_name <<- "Magic!"
}

assign2()
exists("a_variable_name")
# > [1] TRUE
a_variable_name
# > [1] "Magic!"

## Lecture 3: Error Handling and Generation

# 1 - What is an error?

# Errors most often occur when code is used in a way that it is not intended to be used. 
# For example adding two strings together produces the following error:

"hello" + "world"
# > Error in "hello" + "world": non-numeric argument to binary operator

# In R there are two other constructs in R which are both related to errors: warnings and messages. 
# Warnings are meant to indicate that something seems to have gone wrong in your program which should be inspected. 
# Here's a simple example of a warning being generated:

as.numeric(c("5", "6", "seven"))
# > Warning: NAs introduced by coercion
# > [1]  5  6 NA

# Messages simply print test to the R console, though they are generated by an underlying mechanism that is similar to how errors and warning are generated. 
# Here's a small function that will generate a message:

f <- function() {
  message("This is a message.")
}

f()
# > This is a message.

# 2 - Generating Errors

# There are a few essential functions for generating errors, warnings, and messages in R. 
# The stop() function will generate an error. Let's generate an error:

stop("Something erroneous has occured!")
# > Error: Something erroneous has occured!

# If an error occurs inside of a function then the name of that function will appear in the error message:

name_of_function <- function(){
  stop("Something bad happened.")
}

name_of_function()
# > Error in name_of_function(): Something bad happened.

# The stopifnot() function takes a series of logical expressions as arguments and 
# if any of them are false an error is generated specifying which expression is false. 
# Let's take a look at an example:

error_if_n_is_greater_than_zero <- function(n){
  stopifnot(n <= 0)
  n
}

error_if_n_is_greater_than_zero(5)
# > Error: n <= 0 is not TRUE

# The warning() function creates a warning, and the function itself is very similar to the stop() function. 
# Remember that a warning does not stop the execution of a program (unlike an error.)

warning("Consider yourself warned!")
# > Warning: Consider yourself warned!

# Just like errors, a warning generated inside of a function will include the name of the function it was generated in:

make_NA <- function(x){
  warning("Generating an NA.")
  NA
}

make_NA("Sodium")
# > Warning in make_NA("Sodium"): Generating an NA.
# > [1] NA

# Messages are simpler than errors or warnings, they just print strings to the R console.
# You can issue a message with themessage() function:

message("In a bottle.")
# > In a bottle.

# 3 - When to generate errors or warnings

# Common failure conditions like providing invalid arguments to a function should be checked at the beginning of 
# your program so that the user can quickly realize something has gone wrong. This is case of checking function 
# inputs is a typical use of thestopifnot() function.

# 4 - How should errors be handled?

# The tryCatch() function is the workhorse of handling errors and warnings in R. 
# The first argument of this function is any R expression, followed by conditions which specify 
# how to handle an error or a warning. The last argument finally specifies a function or 
# expression that will be executed after the expression no matter what, even in the event of an error or a warning.

# Let's construct a simple function I'm going to call beera that catches errors and warnings gracefully.

beera <- function(expr){
  tryCatch(expr,
           # If an error occurs, execute error function
           error = function(e) {
             message("An error occurred:\n", e)
           },
           # If a warning occurs, execute warning function
           warning = function(w) {
             message("A warning occured:\n", w)
           },
           # final function is always executed
           finally = {
             message("Finally done!")
           })
}

beera({
  2 + 2
})
# > Finally done!
# > [1] 4

beera({
  "two" + 2
})
# > An error occurred:
# > Error in "two" + 2: non-numeric argument to binary operator
# > Finally done!
  
beera({
    as.numeric(c(1, "two", 3))
  })
# > A warning occured:
# > simpleWarning in doTryCatch(return(expr), name, parentenv, handler): NAs introduced by coercion
# > Finally done!

# My advice to you is to limit the number of errors your program generates as much as possible. 
# Even if you design your program so that it's able to catch and handle errors, the error handling process 
# slows down your program by orders of magnitude. Imagine you wanted to write a simple function that checks 
# if an argument is an even number. You might write the following:

is_even <- function(n) {
  n %% 2 == 0
}

is_even(768)
# > [1] TRUE

is_even("two")
# > Error in n%%2: non-numeric argument to binary operator

# You can see that providing a string causes this function to raise an error. 
# You could imagine though that you want to use this function across a list of 
# different data types, and you only want to know which elements of that list 
# are even numbers. You might think to write the following:

is_even_error <- function(n) {
  tryCatch(n %% 2 == 0,
           error = function(e) {
             FALSE
           })
}

is_even_error(714)
# > [1] TRUE

is_even_error("eight")
# > [1] FALSE

# This appears to be working the way you intended, however when applied to more data this function 
# will be seriously slow compared to alternatives. For example I could do a check that n is numeric 
# before treating n like a number:

is_even_check <- function(n){
  is.numeric(n) && n %% 2 == 0
}

is_even_check(1876)
# > [1] TRUE

# Notice below that by using `is.numeric()` before the "AND" operator (`&&`) the expression `n %% 2 == 0` is 
# never evaluated. This is a programming language design feature called "short circuiting." 
# The expression can never evaluate to `TRUE` if the left hand side of `&&` evaluates to `FALSE`, 
# so the right hand side is ignored.

is_even_check("twelve")
# > [1] FALSE

# To demonstrate the difference in the speed of the code we'll use the microbenchmark package to measure 
# how long it takes for each function to be applied to the same data.

library(microbenchmark)
microbenchmark(sapply(letters, is_even_check))

# > Unit: microseconds
# > expr    min      lq     mean  median      uq     max neval
# > sapply(letters, is_even_check) 46.224 47.7975 61.43616 48.6445 58.4755 167.091   100

microbenchmark(sapply(letters, is_even_error))

# > Unit: microseconds
# > expr     min       lq     mean   median       uq      max neval
# > sapply(letters, is_even_error) 640.067 678.0285 906.3037 784.4315 1044.501 2308.931   100

# The error catching approach is nearly 15 times slower!

# 5 - Summary

# - Expressions are a powerful tool for manipulating and executing R code.
# - Environments record associations between names and values.
# - Execution environments create a scope for variable names inside of functions.

## Week 2 - Swirl Assignments

# In the purrr package the map() function returns a list, while the map_lgl(), map_chr(), and map_dbl() functions 
# return vectors of logical values, strings, or numbers respectively.

# Covert a number into its string equivaluent.
int_to_string(4)
# > [1] "four"

map_chr(c(5, 3, 4), int_to_string)
# > [1] "five"  "three" "four" 

# gt(a, b)
# Returns TRUE if a is greater than b, otherwise returns FALSE.

# Use map_lgl() to map the function gt() to vector c(1, 2, 3, 4, 5) to see which elements of that vector are greater than 3.

map_lgl(c(1, 2, 3, 4, 5), gt, b = 3)
# > FALSE FALSE FALSE  TRUE  TRUE

# A predicate function is a function that returns TRUE or FALSE for each element in the provided list or vector. 
# In the case of map_if(): if the predicate functions evaluates to TRUE, then the function is applied to the 
# corresponding vector element, however if the predicate function evaluates to FALSE then the function is not applied.

# Use map_if() to map square() to the even elements of the vector c(1, 2, 3, 4). Use the is_even() function as a predicate.
map_if(c(1, 2, 3, 4), is_even, square)
# > [[1]]
# > [1] 1
# > [[2]]
# > [1] 4
# > [[3]]
# > [1] 3
# > [[4]]
# > [1] 16

# Notice that the map_if() function always returns a list.

# The map_at() function only applies the provided function to elements of a vector specified by their indexes. 
# Like map_if(), map_at() always returns a list.

map_at(c(4, 6, 2, 3, 8), c(1, 3, 4), square)
# > [[1]]
# > [1] 16
# > [[2]]
# > [1] 6
# > [[3]]
# > [1] 4
# > [[4]]
# > [1] 9
# > [[5]]
# > [1] 8

# In each of the previous examples we have only been mapping a function over one data structure, 
# however you can map a function over two data structures with the map2() family of functions. 
# The first two arguments should be two vectors of the same length, followed by a function which 
# will be evaluated with an element of the first vector as the first argument and an element of 
# the second vector as the second argument.

map2_chr(letters, 1:26, paste)
# > [1] "a 1"  "b 2"  "c 3"  ... "x 24" "y 25" "z 26"

# List or vector reduction iteratively combines the first element of a vector with the second element of a vector, 
# then that combined result is combined with the third element of the vector, and so on until the end of the vector 
# is reached. The function to be applied should take at least two arguments. Where mapping returns a vector or a list, 
# reducing should return a single value.

add_talk(x, y)
# Adds x and y and produces a message about which numbers are being added.

add_talk(5, 3)
# > x is 5
# > y is 3
# > [1] 8

# Reduce the vector c(1, 3, 5, 7) with the function add_talk() using the reduce() function.

reduce(c(1,3,5,7), add_talk)
# > x is 1
# > y is 3

# > x is 4
# > y is 5
 
# > x is 9
# > y is 7
 
# > [1] 16

# Reduce the vector c("a", "b", "c", "d") into one string using the paste_talk() function and the reduce() function.
reduce(c("a", "b", "c", "d"), paste_talk)
# > [1] "abcd"

# By default reduce() starts with the first element of a vector and then the second element and so on. 
# In contrast the reduce_right() function starts with the last element of a vector and then proceeds 
# to the second to last element of a vector and so on.

reduce_right(c("a", "b", "c", "d"), paste_talk)
# > [1] "dcba"

# You can search for specific elements of a vector using the contains() and detect() functions. contains() will 
# return TRUE if a specified element is present in a vector, otherwise it returns FALSE.

# Use the contains() function to see if the vector random_ints contains the number 45.
contains(random_ints, 45)
# > [1] TRUE

# The detect() function takes a vector and a predicate function as arguments and it returns the first element of the 
# vector for which the predicate function returns TRUE. Use detect() and is_even() to find the first element of 
# random_ints that is an even number.

detect(random_ints, is_even)
# > [1] 6

# The detect_index() function takes the same arguments as detect(), however it returns the index of the provided vector 
# which contains the first element that satisfies the predicate function. Use detect_index() and is_even() to find the 
# index of the first element of random_ints that is an even number.

detect_index(random_ints, is_even)
# > [1] 13

# The group of functions that includes keep(), discard(), every(), and some() are known as filter functions. 
# Each of these functions takes a vector and a predicate function as arguments.

# For keep() only the elements of the vector that satisfy the predicate function are returned while all other 
# elements are removed. Use the keep() function with random_ints and is_even() to extract the even elements of random_ints.

keep(random_ints, is_even)
# > [1]  6  8 10

# The discard() function works similarly, it only returns elements that don’t satisfy the predicate function. 
# Use discard() to filter out the even elements of random_ints.

discard(random_ints, is_even)
# > [1]  41  55  39  ... 61  37 35

# The every() function returns TRUE only if every element in the vector satisfies the predicate function, 
# while the some() function returns TRUE if at least one element in the vector satisfies the predicate function. 
# Use every() to see if every value of random_ints is less than 100.

every(random_ints, function(x) { x < 100})
# > [1] FALSE

# Finally let's talk about two functions - partial() and walk().

# Partial application of functions can allow functions to behave a little like data structures. 
# Using the partial() function from the purrr package you can specify some of the arguments of a function, 
# and then partial() will return a function that only takes the unspecified arguments. 

# Use partial() to create a new function called gt_10 which returns TRUE if its only argument is greater than ten and FALSE otherwise.

gt_10 <- partial(gt, b = 10)
gt_10(11)
# > [1] TRUE

# Side effects of functions occur whenever a function interacts with the "outside world" - reading or writing data, printing to the console, 
# and displaying a graph are all side effects. The results of side effects are one of the main motivations for writing code in the first place! 
# Side effects can be tricky to handle though, since the order in which functions with side effects are executed often matters and there are 
# variables that are external to the program (the relative location of some data).

# If you want to evaluate a function across a data structure you should use the walk() function from purrr. 
# Use walk() across the vector called mark_antony with the message function.

walk(mark_antony, message)
# > Friends, Romans, countrymen,
# > lend me your ears;
# > I come to bury Caesar,
# > not to praise him.

### Week 3

## Lecture 1 - Debugging

# 1 - Debugging Overview

# R comes with a set of built-in tools for interactive debugging that can be useful for tracking down the source of problems. 
# These functions are

# - browser(): an interactive debugging environment that allows you to step through code one expression at a time
# - debug() / debugonce(): a function that initiates the browser within a function
# - trace(): this function allows you to temporarily insert pieces of code into other functions to modify their behavior
# - recover(): a function for navigating the function call stack after a function has thrown an error
# - traceback(): prints out the function call stack after an error occurs; does nothing if there’s no error

# 2 - traceback()

check_n_value <- function(n) {
  if(n > 0) {
    stop("n should be <= 0")
  }
}
error_if_n_is_greater_than_zero <- function(n) {
  check_n_value(n)
  n
}
error_if_n_is_greater_than_zero(5)
# > Error in check_n_value(n): n should be <= 0

traceback()
# > 3: stop("n should be <= 0") at #2
# > 2: check_n_value(n) at #2
# > 1: error_if_n_is_greater_than_zero(5)

# 3 - Browsing a Function Environment

# From the traceback output, it is often possible to determine in which function and on which line of code an error occurs. 
# If you are the author of the code in question, one easy thing to do is to insert a call to the browser() function in the 
# vicinity of the error (ideally, before the error occurs). The browser() function takes no arguments and is just placed 
# wherever you want in the function. Once it is called, you will be in the browser environment, which is much like the 
# regular R workspace environment except that you are inside a function.

check_n_value <- function(n) {
  if(n > 0) {
    browser()  ## Error occurs around here
    stop("n should be <= 0")
  }
}

# Now, when we call error_if_n_is_greater_than_zero(5), we will see the following.

error_if_n_is_greater_than_zero(5)
# > Called from: check_n_value(n)f
# > Browse[1]> 

# 4 - Tracing Functions

# The simplest use of trace() is to just call trace() on a function without any other arguments.

trace("check_n_value")

# Now, whenever check_n_value() is called by any other functions, 
# you will see a message printed to the console indicating that the function was called.

error_if_n_is_greater_than_zero(5)
# > trace: check_n_value(n)
# > Error in check_n_value(n): n should be <= 0

# Here we can see that check_n_value() was called once before the error occurred. 
# But we can do more with trace(), such as inserting a call to browser() in a specific place, 
# such as right before the call tostop().

# We can obtain the expression numbers of each part of a function by calling as.list() on the body()of a function.

as.list(body(check_n_value))

# >[[1]]
# > `{`

# > [[2]]
# > if (n > 0) {
# >  stop("n should be <= 0")
# > }

# Here, the if statement is the second expression in the function 
# (the first “expression” being the very beginning of the function). 
# We can further break down the second expression as follows.

as.list(body(check_n_value)[[2]])

# > [[1]]
# > `if`

# > [[2]]
# > n > 0

# > [[3]]
# > {
# >  stop("n should be <= 0")
# > }

# Now we can see the call to stop() is the third sub-expression within the second expression of the overall function. 
# We can specify this to trace() by passing an integer vector wrapped in a list to the at argument.

trace("check_n_value", browser, at = list(c(2, 3)))
# > [1] "check_n_value"

# The trace() function has a side effect of modifying the function and converting into a new object of class “functionWithTrace”.

check_n_value
# > Object with tracing code, class "functionWithTrace"
# > Original definition: 
# >  function(n) {
# >    if(n > 0) {
# >      stop("n should be <= 0")
# >    }
# >  }

# You can see the internally modified code by calling:

body(check_n_value)
# > {
# >  if (n > 0) {
# >    .doTrace(browser(), "step 2,3")
# >    {
# >      stop("n should be <= 0")
# >    }
# >  }
# > }

# Here we can see that the code has been altered to add a call to browser() just before the call to stop().

# We can add more complex expressions to a function by wrapping them in a call to quote() within the the trace() function. 
# For example, we may only want to invoke certain behaviors depending on the local conditions of the function.

trace("check_n_value", quote({
  if(n == 5) {
    message("invoking the browser")
    browser()
  }
}), at = 2)
# > [1] "check_n_value"

# Here, we only invoke the browser() if n is specifically 5.

body(check_n_value)
# > {
# >  {
# >    .doTrace({
# >      if (n == 5) {
# >        message("invoking the browser")
# >        browser()
# >      }
# >    }, "step 2")
# >    if (n > 0) {
# >      stop("n should be <= 0")
# >    }
# >  }
# > }

# Debugging functions within a package is another key use case for trace(). 
# For example, if we wanted to insert tracing code into the glm() function within the stats package, 
# the only addition to the trace() call we would need is to provide the namespace information via the where argument.

trace("glm", browser, at = 4, where = asNamespace("stats"))
# > Tracing function "glm" in package "namespace:stats"
# > [1] "glm"

# Here we show the first few expressions of the modified glm() function.

body(stats::glm)[1:5]
# > {
# >  call <- match.call()
# >  if (is.character(family)) 
# >    family <- get(family, mode = "function", envir = parent.frame())
# >  {
# >    .doTrace(browser(), "step 4")
# >    if (is.function(family)) 
# >      family <- family()
# >    }
# >  if (is.null(family$family)) {
# >    print(family)
# >    stop("'family' not recognized")
# >  }
# > }

test <- function(a, b) {
  browser()
  t <- paste(a, b)
  t
}

test('a', 'b')

# 5 - Using debug() and debugonce()

# The debug() and debugonce() functions can be called on other functions to turn on the “debugging state” of a function. 
# Calling debug() on a function makes it such that when that function is called, 
# you immediately enter a browser and can step through the code one expression at a time.

## Turn on debugging state for 'lm' function
debug(lm)

# A call to debug(f) where f is a function is basically equivalent totrace(f, browser) which will call the browser() 
# function upon entering the function.

# The debugging state is persistent, so once a function is flagged for debugging, it will remain flagged. 
# Because it is easy to forget about the debugging state of a function, the debugonce() function turns on the 
# debugging state the next time the function is called, but then turns it off after the browser is exited.

# 6 - recover()

# The recover() function is not often used but can be an essential tool when debugging complex code. 
# Typically, you do not callrecover() directly, but rather set it as the function to invoke anytime an error occurs in code. 
# This can be done via the options()function.

options(error = recover)

# Usually, when an error occurs in code, the code stops execution and you are brought back to the usual R console prompt. 
# However, whenrecover() is in use and an error occurs, you are given the function call stack and a menu.

# > error_if_n_is_greater_than_zero(5)
# > Error in check_n_value(n) : n should be <= 0

# > Enter a frame number, or 0 to exit   

# > 1: error_if_n_is_greater_than_zero(5)
# > 2: #2: check_n_value(n)
  
# > Selection:

# Selecting a number from this menu will bring you into that function on the call stack and you will be placed in 
# a browser environment. You can exit the browser and then return to this menu to jump to another function in the call stack.

# The recover() function is very useful if an error is deep inside a nested series of function calls and it is difficult to pinpoint 
# exactly where an error is occurring (so that you might use browser() or trace()). In such cases, the debug() function is often of 
# little practical use because you may need to step through many many expressions before the error actually occurs. 
# Another scenario is when there is a stochastic element to your code so that errors occur in an unpredictable way. 
# Using recover() will allow you to browse the function environment only when the error eventually does occur.

# 7 - Summary

# - Debugging in R is facilitated with the functions browser, debug, trace, recover, and traceback.
# - These debugging tools should not be used as a crutch when developing functions.

## Lecture 2 - Profiling

# 1 - Profiling Overview

# Some of the R code that you write will be slow. 
# Slow code often isn’t worth fixing in a script that you will only evaluate a few times, 
# as the time it will take to optimize the code will probably exceed the time it takes the computer to run it. 
# However, if you are writing functions that will be used repeatedly, 
# it is often worthwhile to identify slow sections of the code so you can try to improve speed in those sections.

# 2 - microbenchmark

# The microbenchmark package is useful for running small sections of code to assess performance, 
# as well as for comparing the speed of several functions that do the same thing. 
# The microbenchmarkfunction from this package will run code multiple times (100 times is the default) and 
# provide summary statistics describing how long the code took to run across those iterations. 
# The process of timing a function takes a certain amount of time itself. The microbenchmarkfunction adjusts 
# for this overhead time by running a certain number of “warm-up” iterations before running the iterations 
# used to time the code.

# You can use the times argument in microbenchmark to customize how many iterations are used. 
# For example, if you are working with a function that is a bit slow, you might want to run the code 
# fewer times when benchmarking (although with slower or more complex code, it likely will make more 
# sense to use a different tool for profiling, likeprofvis).

# You can include multiple lines of code within a single call tomicrobenchmark. 
# However, to get separate benchmarks of line of code, you must separate each line by a comma:

library(microbenchmark)
microbenchmark(a <- rnorm(1000), 
               b <- mean(rnorm(1000)))

# > Unit: microseconds
# >             expr         min      lq     mean median       uq      max  neval
# > a <- rnorm(1000)       77.745 83.6070 114.0765 93.178  97.7680 2269.628   100
# > b <- mean(rnorm(1000)) 83.890 89.0155 100.8248 99.508 104.6265  265.553   100

# The microbenchmark function is particularly useful for comparing functions that take the same inputs and 
# return the same outputs. As an example, say we need a function that can identify days that meet two conditions: 
# (1) the temperature equals or exceeds a threshold temperature (27 degrees Celsius in the examples) and 
# (2) the temperature equals or exceeds the hottest temperature in the data before that day. 
# We are aiming for a function that can input a data frame that includes a column named temp with daily mean 
# temperature in Celsius, like this data frame:

# > date          temp
# > 2015-07-01    26.5
# > 2015-07-02    27.2
# > 2015-07-03    28.0
# > 2015-07-04    26.9
# > 2015-07-05    27.5
# > 2015-07-06    25.9
# > 2015-07-07    28.0
# > 2015-07-08    28.2

# and outputs a data frame that has an additional binary record_tempcolumn, 
# specifying if that day meet the two conditions, like this:

# > date          temp    record_temp
# > 2015-07-01    26.5    FALSE   
# > 2015-07-02    27.2    TRUE    
# > 2015-07-03    28.0    TRUE    
# > 2015-07-04    26.9    FALSE   
# > 2015-07-05    27.5    FALSE   
# > 2015-07-06    25.9    FALSE   
# > 2015-07-07    28.0    TRUE    
# > 2015-07-08    28.2    TRUE

# Below are two example functions that can perform these actions. 
# Since the record_temp column depends on temperatures up to that day, 
# one option is to use a loop to create this value. The first function takes this approach. 
# The second function instead uses tidyverse functions to perform the same tasks.

# Function that uses a loop 
find_records_1 <- function(datafr, threshold) {
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold & datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
  return(datafr)
}

# Function that uses tidyverse functions
find_records_2 <- function(datafr, threshold) {
  datafr <- datafr %>%
    mutate_(over_threshold = ~ temp >= threshold,
            cummax_temp = ~ temp == cummax(temp),
            record_temp = ~ over_threshold & cummax_temp) %>%
    select_(.dots = c("-over_threshold", "-cummax_temp"))
  return(as.data.frame(datafr))
}

# If you apply the two functions to the small example data set, you can see that they both create the desired output:

example_data <- data.frame(date = c("2015-07-01", "2015-07-02",
                                    "2015-07-03", "2015-07-04",
                                    "2015-07-05", "2015-07-06",
                                    "2015-07-07", "2015-07-08"),
                           temp = c(26.5, 27.2, 28.0, 26.9, 
                                    27.5, 25.9, 28.0, 28.2))

(test_1 <- find_records_1(example_data, 27))

# >          date temp record_temp
# > 1 2015-07-01 26.5       FALSE
# > 2 2015-07-02 27.2        TRUE
# > 3 2015-07-03 28.0        TRUE
# > 4 2015-07-04 26.9       FALSE
# > 5 2015-07-05 27.5       FALSE
# > 6 2015-07-06 25.9       FALSE
# > 7 2015-07-07 28.0        TRUE
# > 8 2015-07-08 28.2        TRUE

(test_2 <- find_records_2(example_data, 27))

# >         date temp record_temp
# > 1 2015-07-01 26.5       FALSE
# > 2 2015-07-02 27.2        TRUE
# > 3 2015-07-03 28.0        TRUE
# > 4 2015-07-04 26.9       FALSE
# > 5 2015-07-05 27.5       FALSE
# > 6 2015-07-06 25.9       FALSE
# > 7 2015-07-07 28.0        TRUE
# > 8 2015-07-08 28.2        TRUE

all.equal(test_1, test_2)
# > [1] TRUE

# The performance of these two functions can be compared usingmicrobenchmark:

record_temp_perf <- microbenchmark(find_records_1(example_data, 27), 
                                   find_records_2(example_data, 27))
record_temp_perf

# > Unit: microseconds
# >                            expr      min        lq     mean   median      uq      max    neval
# > find_records_1(example_data, 27)  674.628  704.5445  770.646  719.366  753.949 4016.827   100
# > find_records_2(example_data, 27) 1064.935 1095.5015 1183.014 1131.834  1190.596 4249.408  100

# This output gives summary statistics (min, lq, mean, median,uq, and max) describing the time it took to 
# run the two function over the 100 iterations of each function call. By default, these times are given in 
# a reasonable unit, based on the observed profiling times (units are given in microseconds in this case).

# It’s useful to check next to see if the relative performance of the two functions is similar for a bigger data set. 
# The chicagoNMMAPS data set from the dlnm package includes temperature data over 15 years in Chicago, IL.
# Here are the results when we benchmark the two functions with that data (note, this code takes a minute or two to run):

library(dlnm)
data("chicagoNMMAPS")

record_temp_perf_2 <- microbenchmark(find_records_1(chicagoNMMAPS, 27), 
                                     find_records_2(chicagoNMMAPS, 27))
record_temp_perf_2

# Unit: milliseconds
#                              expr        min         lq       mean    median         uq       max     neval
# find_records_1(chicagoNMMAPS, 27) 182.226569 197.540354 203.531151  200.767944 205.575326  321.96972   100
# find_records_2(chicagoNMMAPS, 27)   1.974453   2.197642   2.884933  2.343669   2.477938    14.34087    100

# The microbenchmark function returns an object of the “microbenchmark” class. 
# This class has two methods for plotting results, autoplot.microbenchmark and boxplot.microbenchmark. 
# To use the autoplot method, you will need to have ggplot2 loaded in your R session.

library(ggplot2)

# By default, this plot gives the “Time” axis on a log scale. 
# You can change this with the argument log = FALSE.

autoplot(record_temp_perf, log = FALSE)
boxplot(record_temp_perf, log = FALSE)

# 3 - profvis

# Once you’ve identified slower code, you’ll likely want to figure out which parts of the code are causing bottlenecks. 
# The profvis function from the profvis package is very useful for this type of profiling. 
# This function uses the RProf function from base R to profile code, and then displays it in an interactive visualization in RStudio. 
# This profiling is done by sampling, with the RProf function writing out the call stack every 10 milliseconds while running the code.

# To profile code with profvis, just input the code (in braces if it is mutli-line) into profvis within RStudio. 
# For example, we found that the find_records_1 function was slow when used with a large data set. 
# To profile the code in that function, run:

library(profvis)
datafr <- chicagoNMMAPS
threshold <- 27

profvis({
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold & datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
})

# The profvis output gives you two options for visualization: “Flame Graph” or “Data” (a button to toggle between the two is given in 
# the top left of the profvis visualization created when you profile code). The “Data” output defaults to show you the time usage of 
# each first-level function call. Each of these calls can be expanded to show deeper and deeper functions calls within the call stack. 
# This expandable interface allows you to dig down within a call stack to determine what calls are causing big bottlenecks. 
# For functions that are part of a package you have loaded with devtools::load_all, this output includes a column with the file name 
# where a given function is defined. This functionality makes this “Data” output pane particularly useful in profiling functions in 
# a package you are creating.

# The “Flame Graph” view in profvis output gives you two panels. The top panel shows the code called, with bars on the right to show 
# memory use and time spent on the line. The bottom panel also visualizes the time used by each line of code, but in this case it 
# shows time use horizontally and shows the full call stack at each time sample, with initial calls shown at the bottom of the graph, 
# and calls deeper in the call stack higher in the graph. Clicking on a block in the bottom panel will show more information about a call, 
# including which file it was called from, how much time it took, how much memory it took, and its depth in the call stack.

# Based on this visualization, most of the time is spent on line 6, filling in the record_temp vector. 
# Now that we know this, we could try to improve the function, for example by doing a better job of initializing vectors before running the loop.

# The profvis visualization can be used to profile code in functions you’re writing as part of a package. 
# If some of the functions in the code you are profiling are in a package currently loaded with loaded with devtools::load_all, 
# the top panel in the Flame Graph output will include the code defining those functions, which allows you to explore speed and 
# memory use within the code for each function. You can also profile code within functions from other packages– for more details on 
# the proper set-up, see the “FAQ” section of RStudio’s profvis documentation (https://rstudio.github.io/profvis/index.html).

# The profvis function will not be able to profile code that runs to quickly. 
# Trying to profile functions that are too fast will give you the following error message:

# > Error in parse_rprof(prof_output, expr_source) : 
# > No parsing data available. Maybe your function was too fast?

# You can use the argument interval in profvis to customize the sampling interval. 
# The default is to sample every 10 milliseconds (interval = 0.01), but you can decrease this sampling interval. 
# In some cases, you may be able to use this option to profile faster-running code. 
# However, you should avoid using an interval smaller than about 5 milliseconds, as below that you will get inaccurate estimates with profvis. 
# If you are running very fast code, you’re better off profiling with microbenchmark, which can give accurate estimates at finer time intervals.

# Here are some tips for optimizing your use of profvis:
  
# - You may find it convenient to use the “Show in new window” button on the RStudio pane with profiling results 
#   to expand this window while you are interpreting results.
# - An “Options” button near the top right gives different options for how to display the profiling results, 
#   including whether to include memory profiling results and whether to include lines of code with zero time.
# - You can click-and-drag results in the bottom visualization panel, as well as pan in and out.
# - You may need to update your version of RStudio to be able to use the full functionality of profvis. 
#   You can download a Preview version of RStudio here (https://www.rstudio.com/products/rstudio/download/preview-release-notes/).
# - If you’d like to share code profiling results from profvis publicly, you can do that by using the “Publish” button on 
#   the top right of the rendered profile visualization to publish the visualization to RPubs. The “FAQ” section of RStudio’s 
#   profvis documentation includes more tips for sharing a code profile visualization online.
# - If you get a lot of blocks labeled “<Anonymous>”, try updating your version of R. In newer versions of R, functions called 
#   usingpackage::function() syntax or list$function() syntax are labeled in profiling blocks in a more meaningful way. 
#   This is likely to be a particular concern if you are profiling code in a package you are developing, as you will often 
#   be usingpackage::function() syntax extensively to pass CRAN checks.

# 4 - Find out more

# If you’d like to learn more about profiling R code, or improving performance of R code once you’ve profiled, 
# you might find these resources helpful:
  
# - RStudio’s profvis documentation
#   + https://rstudio.github.io/profvis/index.html
# - Section on performant code in Hadley Wickham’s Advanced Rbook
#   + http://adv-r.had.co.nz/Performance.html
# - “FasteR! HigheR! StrongeR! - A Guide to Speeding Up R Code for Busy People”, an article by Noam Ross
#   + http://www.noamross.net/blog/2013/4/25/faster-talk.html

# 5 - Summary

# - Profiling can help you identify bottlenecks in R code.
# - The microbenchmark package helps you profile short pieces of code and compare functions with each other. 
#   It runs the code many times and provides summary statistics across the iterations.
# - The profvis package allows you to visualize performance across more extensive code. 
#   It can be used to profile code within functions being developed for a package, 
#   as long as the package source code has been loaded locally using devtools::load_all.

## Lecture 3 - Non-standard evaluation

# 1 - Non-standard evaluation

# Functions from packages like dplyr, tidyr, and ggplot2 are excellent for creating efficient and easy-to-read code that
# cleans and displays data. However, they allow shortcuts in calling columns in data frames that allow some room for ambiguity 
# when you move from evaluating code interactively to writing functions for others to use. The non-standard evaluation used
# within these functions mean that, if you use them as you would in an interactive session, you’ll get a lot of “no visible bindings” 
# warnings when you run CRAN checks on your package. These warnings will look something like this:

# > map_counties: no visible binding for global variable ‘fips’
# > map_counties: no visible binding for global variable ‘storm_dist’
# > map_counties: no visible binding for global variable ‘tot_precip’
# > Undefined global functions or variables:
# >  fips storm_dist tot_precip

# When you write a function for others to use, you need to avoid non-standard evaluation and so avoid all of these functions 
# (culprits include many dplyr and tidyr functions– including mutate, select,filter, group_by, summarize, gather, spread– but 
# also some functions in ggplot2, including aes). Fortunately, these functions all have standard evaluation alternatives, 
# which typically have the same function name followed by an underscore (for example, the standard evaluation version of mutate is mutate_).

# The input to the function call will need to be a bit different for standard evaluation versions of these functions. 
# In many cases, this change is as easy as using formula notation (~) within the call, but in some cases it requires something more complex, 
# including using the .dots argument.

# Here is a table with examples of non-standard evaluation calls and their standard evaluation alternatives (these are all written assuming that 
# the function is being used as a step in a piping flow, where the input data frame has already been defined earlier in the piping sequence):

#   Non-standard evaluation version	    |    Standard evaluation version
# filter(fips %in% counties)	          |  filter_(~ fips %in% counties)
# mutate(max_rain = max(tot_precip)	    |  mutate_(max_rain = ~ max(tot_precip)
# summarize(tot_precip = sum(precip))   |  summarize_(tot_precip = ~ sum(precip))
# group_by(storm_id, fips)	            |  group_by_(~ storm_id, ~ fips)
# aes(x = long, y = lat)	              |  aes_(x = ~ long, y = ~ lat)
# select(-start_date, -end_date)	      |  select_(.dots = c('start_date', 'end_date'))
# select(-start_date, -end_date)	      |  select_(.dots = c('-start_date', '-end_date'))
# spread(key, mean)	                    |  spread_(key_col = 'key', value_col = 'mean')
# gather(key, mean)	                    |  gather_(key_col = 'key', value_col = 'mean')

# If you have any non-standard evaluation in your package code (which you’ll notice because of the “no visible bindings” warnings 
# you’ll get when you check the package), go through and change any instances to use standard evaluation alternatives. 
# This change prevents these warnings when you check your package and will also ensure that the functions behave like you expect 
# them to when they are run by other users in their own R sessions.

# In this section, we’ve explained only how to convert from functions that use non-standard evaluation to those that use standard evaluation, 
# to help in passing CRAN checks as you go from coding scripts to writing functions for packages. If you would like to learn more about non-standard 
# evaluation in R, you should check out the chapter on non-standard evaluation in Hadley Wickham’s Advanced Rbook (http://adv-r.had.co.nz/Computing-on-the-language.html).

# 2 - Summary

# - Functions that use non-standard evaluation can cause problems within functions written for a package.
# - The NSE functions in tidyverse packages all have standard evaluation analogues that should be used 
#   when writing functions that will be used by others.

# 3 - Quiz:
# 1 - getting your expectations about code behavior to converge with reality
# 2 - browser()
# 3 - trace()
# 4 - returns the state of the function call stack just before an occurred
# 5 - immediately after an error occurs
# 6 - Whenever f() is called, a message is printed to the console indicating that the function was called.
# 7 - comparing the speed of several functions that do the same thing
# 8 - writes out the function call stack at user-specified intervals of time 
# 9 - displays profiling information in an interactive visualization in RStudio
# 10 - microbenchmark()
 
### Week 4

## Lecture 1 - OOP

# 1 - OOP Overview

# R has three object oriented systems because the roots of R date back to 1976, 
# when the idea of object oriented programming was barely four years old. 
# New object oriented paradigms were added to R as they were invented, 
# so some of the ideas in R about object oriented programming have gone stale in the years since. 
# It’s still important to understand these older systems since a huge amount of R code is written 
# with them, and they’re still useful and interesting! Long time object oriented programmers reading 
# this book may find these old ideas refreshing.

# The two older object oriented systems in R are called S3 and S4, and the modern system is called RC 
# which stands for “reference classes.” Programmers who are already familiar with object oriented 
# programming will feel at home using RC.

# 2 - Object Oriented Principles

# There a several key principles in object oriented programming which span across R’s object systems and 
# other programming languages. The first are the ideas of a class and an object. The world is made up of 
# physical objects - the chair you’re sitting in, the clock next to your bed, the bus you ride every day, 
# etc. Just like the world is full of physical objects, your programs can be made of objects as well. 
# A class is a blueprint for an object: it describes the parts of an object, how to make an object, 
# and what the object is able to do. If you were to think about a class for a bus (as in the public 
# buses that roam the roads) this class would describe attributes for the bus like the number of seats 
# on the bus, the number of windows, the top speed of the bus, and the maximum distance the bus can drive 
# on one tank of gas.

# Buses in general can perform the same actions, and these actions are also described in the class: 
# a bus can open and close its doors, the bus can steer, and the accelerator or the brake can be 
# used to slow down or speed up the bus. Each of these actions can be described as a method which is
# a function that is associated with a particular class. We’ll be using this class in order to create 
# individual bus objects, so we should provide a constructor which is a method where we can specify 
# attributes of the bus as arguments. This constructor method will then return an individual bus 
# object with the attributes that we specified.

# You could also imagine that after making the bus class you might want to make a special kind of class for 
# a party bus. Party buses have all of the same attributes and methods as our bus class, but they also have 
# additional attributes and methods like the number of refrigerators, window blinds that can be opened and 
# closed, and smoke machines that can be turned on and off. Instead of rewriting the entire bus class and 
# then adding new attributes and methods, it is possible for the party bus class to inherit all of the 
# attributes and methods from the bus class. In this framework of inheritance, we talk about the bus class 
# as the super-class of the party bus, and the party bus is the sub-class of the bus. What this relationship 
# means is that the party bus has all of the same attributes and methods as the bus class plus additional 
# attributes and methods.

# 3 - S3

# Conveniently everything in R is an object. By “everything” I mean every single “thing” in R including numbers, 
# functions, strings, data frames, lists, etc. If you want to know the class of an object in R you can simply 
# use the class() function:

class(2)
# > [1] "numeric"
class("is in session.")
# > [1] "character"
class(class)
# > [1] "function"

# Now it’s time to wade into some of the quirks of R’s object oriented systems. 
# In the S3 system you can arbitrarily assign a class to any object, which goes 
# against most of what we discussed in the Object Oriented Principles section. 
# Class assignments can be made using the structure() function, or you can assign 
# the class using class() and <-:

special_num_1 <- structure(1, class = "special_number")
class(special_num_1)
# > [1] "special_number"

special_num_2 <- 2
class(special_num_2)
# > [1] "numeric"
class(special_num_2) <- "special_number"
class(special_num_2)
# > [1] "special_number"

# This is completely legal R code, but if you want to have a better behaved S3 class you should 
# create a constructor which returns an S3 object. The shape_S3() function below is a constructor 
# that returns a shape_S3 object:
  
shape_s3 <- function(side_lengths){
  structure(list(side_lengths = side_lengths), class = "shape_S3")
}

square_4 <- shape_s3(c(4, 4, 4, 4))
class(square_4)
# > [1] "shape_S3"

triangle_3 <- shape_s3(c(3, 3, 3))
class(triangle_3)
# > [1] "shape_S3"

# We’ve now made two shape_S3 objects: square_4 and triangle_3, which are both instantiations of the shape_S3 class. 
# Imagine that you wanted to create a method that would return TRUE if a shape_S3 object was a square, FALSE if a 
# shape_S3 object was not a square, and NA if the object provided as an argument to the method was not a shape_s3 object. 
# This can be achieved using R’s generic methods system. A generic method can return different values based depending 
# on the class of its input. For examplemean() is a generic method that can find the average of a vector of number or 
# it can find the “average day” from a vector of dates. The following snippet demonstrates this behavior:

mean(c(2, 3, 7))
# > [1] 4
mean(c(as.Date("2016-09-01"), as.Date("2016-09-03")))
# > [1] "2016-09-02"

# Now let’s create a generic method for identifying shape_S3 objects that are squares. 
# The creation of every generic method uses theUseMethod() function in the following way with only slight variations:

# [name of method] <- function(x) UseMethod("[name of method]")

# Let’s call this method is_square:

is_square <- function(x) UseMethod("is_square")

# Now we can add the actual function definition for detecting whether or not a shape is a square by specifying 
# is_square.shape_S3. By putting a dot (.) and then the name of the class after is_square, we can create a 
# method that associates is_square with the shape_S3 class:

is_square.shape_S3 <- function(x) {
  length(x$side_lengths) == 4 &&
    x$side_lengths[1] == x$side_lengths[2] &&
    x$side_lengths[2] == x$side_lengths[3] &&
    x$side_lengths[3] == x$side_lengths[4]
}

is_square(square_4)
# > [1] TRUE
is_square(triangle_3)
# > [1] FALSE

# Seems to be working well! We also want is_square() to return NA when its argument is not a shape_S3. 
# We can specify is_square.default as a last resort if there is not method associated with the object passed to is_square().

is_square.default <- function(x){
  NA
}

is_square("square")
# > [1] NA
is_square(c(1, 1, 1, 1))
# > [1] NA

# Let’s try printing square_4:
  
print(square_4)
# > $side_lengths
# > [1] 4 4 4 4

# > attr(,"class")
# > [1] "shape_S3"

# Doesn’t that look ugly? Lucky for us print() is a generic method, so we can specify a print method for the shape_S3 class:

print.shape_S3 <- function(x) {
  if(length(x$side_lengths) == 3) {
    paste("A triangle with side lengths of", x$side_lengths[1], 
          x$side_lengths[2], "and", x$side_lengths[3])
  } else if (length(x$side_lengths) == 4) {
    if (is_square(x)) {
      paste("A square with four sides of length", x$side_lengths[1])
    } else {
      paste("A quadrilateral with side lengths of", x$side_lengths[1],
            x$side_lengths[2], x$side_lengths[3], "and", x$side_lengths[4])
    }
  } else {
    paste("A shape with", length(x$side_lengths), "sides.")
  }
}

print(square_4)
# > [1] "A square with four sides of length 4"
print(triangle_3)
# > [1] "A triangle with side lengths of 3 3 and 3"
print(shape_s3(c(10, 10, 20, 20, 15)))
# > [1] "A shape with 5 slides."
print(shape_s3(c(2, 3, 4, 5)))
# > [1] "A quadrilateral with side lengths of 2 3 4 and 5"

# Since printing an object to the console is one of the most common things to do in R, nearly every class has an associated 
# print method! To see all of the methods associated with a generic method like print() use the methods() function:

head(methods(print), 10)
# > [1] "print,ANY-method"            "print,diagonalMatrix-method"
# > [3] "print,sparseMatrix-method"   "print.acf"                  
# > [5] "print.AES"                   "print.anova"                
# > [7] "print.anova.gam"             "print.anova.lme"            
# > [9] "print.aov"                   "print.aovlist" 

# One last note on S3 with regard to inheritance. In the previous section we discussed how a sub-class can inherit attributes 
# and methods from a super-class. Since you can assign any class to an object in S3, you can specify a super class for an 
# object the same way you would specify a class for an object:

class(square_4)
# > [1] "shape_S3"
class(square_4) <- c("shape_S3", "square")
class(square_4)
# > [1] "shape_S3" "square"

# To check if an object is a sub-class of a specified class you can use the inherits() function:

inherits(square_4, "square")
# > [1] TRUE
inherits(square_4, "shape_S3")
# > [1] TRUE

# 4 - S4

# The S4 system is slightly more restrictive than S3, but it’s similar in many ways. To create a new class in S4 you 
# need to use the setClass() function. You need to specify two or three arguments for this function: 
# Class which is the name of the class as a string, slots, which is a named list of attributes for the class with the 
# class of those attributes specified, and optionally contains which includes the super-class of they class you’re specifying 
# (if there is a super-class). Take look at the class definition for a bus_S4 and a party_bus_S4 below:

setClass("bus_S4",
         slots = list(n_seats = "numeric", 
                      top_speed = "numeric",
                      current_speed = "numeric",
                      brand = "character"))

setClass("party_bus_S4",
         slots = list(n_subwoofers = "numeric",
                      smoke_machine_on = "logical"),
         contains = "bus_S4")

# Now that we’ve created the bus_S4 and the party_bus_S4 classes we can create bus objects using the new() function. 
# The new() function’s arguments are the name of the class and values for each “slot” in our S4 object.

my_bus <- new("bus_S4", n_seats = 20, top_speed = 80, current_speed = 0, brand = "Volvo")
my_bus

# > An object of class "bus_S4"
# > Slot "n_seats":
# > [1] 20
# > Slot "top_speed":
# > [1] 80
# > Slot "current_speed":
# > [1] 0
# > Slot "brand":
# > [1] "Volvo"

my_party_bus <- new("party_bus_S4", n_seats = 10, top_speed = 100, current_speed = 0, brand = "Mercedes-Benz", 
                    n_subwoofers = 2, smoke_machine_on = FALSE)
my_party_bus

# > An object of class "party_bus_S4"
# > Slot "n_subwoofers":
# > [1] 2
# > Slot "smoke_machine_on":
# > [1] FALSE
# > Slot "n_seats":
# > [1] 10
# > Slot "top_speed":
# > [1] 100
# > Slot "current_speed":
# > [1] 0
# > Slot "brand":
# > [1] "Mercedes-Benz"

# You can use the @ operator to access the slots of an S4 object:

my_bus@n_seats
# > [1] 20
my_party_bus@top_speed
# > [1] 100

# This is essentially the same as using the $ operator with a list or an environment.

# S4 classes use a generic method system that is similar to S3 classes.
# In order to implement a new generic method you need to use the setGeneric() 
# function and the standardGeneric() function in the following way:

setGeneric("new_generic", function(x) {
  standardGeneric("new_generic")
})

# Let’s create a generic function called is_bus_moving() to see if a bus_S4 object is in motion:

setGeneric("is_bus_moving", function(x) {
  standardGeneric("is_bus_moving")
})
# > [1] "is_bus_moving"
  
# Now we need to actually define the function which we can do with setMethod(). The setMethod() functions takes as
# arguments the name of the method as a string, the method signature which specifies the class of each argument
# for the method, and then the function definition of the method:

setMethod("is_bus_moving",
          c(x = "bus_S4"),
          function(x) {
            x@current_speed > 0
          })
# > [1] "is_bus_moving"

is_bus_moving(my_bus)
# > [1] FALSE
my_bus@current_speed <- 1
is_bus_moving(my_bus)
# > [1] TRUE

# In addition to creating your own generic methods, you can also create a method for your new class from an existing generic. 
# First use the setGeneric() function with the name of the existing method you want to use with your class, and then use the 
# setMethod() function like in the previous example. Let’s make a print() method for the bus_S4 class:

setGeneric("print")
# > [1] "print"

setMethod("print",
          c(x = "bus_S4"),
          function(x){
            paste("This", x@brand, "bus is traveling at a speed of", x@current_speed)
          })
# > [1] "print"

print(my_bus)
# > [1] "This Volvo bus is traveling at a speed of 1"
print(my_party_bus)
# > [1] "This Mercedes-Benz bus is traveling at a speed of 0"

# 5 - Reference Classes

# With reference classes we leave the world of R’s old object oriented systems and enter the philosophies of other 
# prominent object oriented programming languages. We can use the setRefClass() function to define a class’ fields, 
# methods, and super-classes. Let’s make a reference class that represents a student:

Student <- setRefClass("Student",
                       fields = list(name = "character",
                                     grad_year = "numeric",
                                     credits = "numeric",
                                     id = "character",
                                     courses = "list"),
                       methods = list(
                         hello = function() {
                           paste("Hi! My name is", name)
                         },
                         add_credits = function(n) {
                           credits <<- credits + n
                         },
                         get_email = function() {
                           paste0(id, "@jhu.edu")
                         }
                       ))

# To recap: we’ve created a class definition called Student which defines the student class. 
# This class has five fields and three methods. To create a Student object use the new() method:

brooke <- Student$new(name = "Brooke", grad_year = 2019, credits = 40,
                      id = "ba123", courses = list("Ecology", "Calculus III"))
roger <- Student$new(name = "Roger", grad_year = 2020, credits = 10,
                     id = "rp456", courses = list("Puppetry", "Elementary Algebra"))

# You can access the fields and methods of each object using the $ operator:

brooke$credits
# > [1] 40
roger$hello()
# > [1] "Hi! My name is Roger"
roger$get_email()
# > [1] "rp456@jhu.edu"

# Methods can change the state of an object, for instance in the case of the add_credits() function:

brooke$credits
# > [1] 40
brooke$add_credits(4)
brooke$credits
# > [1] 44

# Notice that the add_credits() method uses the complex assignment operator (<<-). 
# You need to use this operator if you want to modify one of the fields of an object with a method. 
# You’ll learn more about this operator in the Expressions & Environments section.

# Reference classes can inherit from other classes by specifying the contains argument when they’re defined. 
# Let’s create a sub-class of Student called Grad_Student which includes a few extra features:

Grad_Student <- setRefClass("Grad_Student",
                            contains = "Student",
                            fields = list(thesis_topic = "character"),
                            methods = list(
                              defend = function(){
                                paste0(thesis_topic, ". QED.")
                              }
                            ))

jeff <- Grad_Student$new(name = "Jeff", grad_year = 2021, credits = 8,
                         id = "jl55", courses = list("Fitbit Repair", 
                                                     "Advanced Base Graphics"),
                         thesis_topic = "Batch Effects")

jeff$defend()
# > [1] "Batch Effects. QED."

# 6 - Summary

# - R has three object oriented systems: S3, S4, and Reference Classes.
# - Reference Classes are the most similar to classes and objects in other programming languages.
# - Classes are blueprints for an object.
# - Objects are individual instances of a class.
# - Methods are functions that are associated with a particular class.
# - Constructors are methods that create objects.
# - Everything in R is an object.
# - S3 is a liberal object oriented system that allows you to assign a class to any object.
# - S4 is a more strict object oriented system that build upon ideas in S3.
# - Reference Classes are a modern object oriented system that is similar to Java, C++, Python, or Ruby.

## Lecture 2 - Gaining Your 'tidyverse' Citizenship

# 1 - Overview

# Many of the tools that we discuss in this book revolve around the so-called “tidyverse” set of tools. 
# These tools, largely developed by Hadley Wickham but also including a diverse community of developers, 
# have a set of principles that are adhered to when they are being developed. Hadley Wicham laid out 
# these principles in his Tidy Tools Manifesto [1], a vignette within the tidyverse package.
# [1] - https://cran.r-project.org/web/packages/tidyverse/vignettes/manifesto.html

# 2 - Reuse existing data structures

# R has a number of data structures (data frames, vectors, etc.) that people have grown accustomed to over the many years of 
# R’s existence. While it is often tempting to develop custom data structures, for example, by using S3 or S4 classes, 
# it is often worthwhile to consider reusing a commonly used structure. You’ll notice that many tidyverse functions make 
# heavy use of the data frame (typically as their first argument), because the data frame is a well-known, well-understood 
# structure used by many analysts. Data frames have a well-known and reasonably standardized corresponding file format in 
# the CSV file.

# While common data structures like the data frame may not be perfectly suited to your needs as you develop your own software, 
# it is worth considering using them anyway because the enormous value to the community that is already familiar with them. 
# If the user community feels familiar with the data structures required by your code, they are likely to adopt them more 
# quickly.

# 3 - Compose simple functions with the pipe

# One of the original principles of the Unix operating system was that every program should do “one thing well”. 
# The limitation of only doing one thing (but well!) was removed by being able to easily pipe the output of one function 
# to be the input of another function (the pipe operator on Unix was the | symbol). Typical Unix commands would contain 
# long string commands piped together to (eventually) produce some useful output. On Unix systems, the unifying concept 
# that allowed programs to pipe to each other was the use of [textual formats]. All data was rendered in textual formats 
# so that if you wrote a new program, you would not need to worry about decoding some obscure proprietary format.

# Much like the original Unix systems, the tidyverse eschews building monolithic functions that have many bells and whistles. 
# Rather, once you are finished writing a simple function, it is better to start afresh and work off the input of another 
# function to produce new output (using the %>% operator, for example). The key to this type of development is having clean 
# interfaces between functions and an expectation that the output of every function may serve as the input to another function.
# This is why the first principle (reuse existing data structures) is important, because the reuse of data structures that are
# well-understood and characterized lessens the burden on other developers who are developing new code and would prefer not to
# worry about new-fangled data structures at every turn.

# 4 - Embrace functional programming

# This can be a tough principle for people coming from other non-functional programming languages. 
# But the reality is, R is a functional programming language (with its roots in Scheme) and it’s best not to go against 
# the grain. In our section on Functional Programming, we outlined many of the principles that are fundamental to 
# functional-style programming. In particular, the purrr package implements many of those ideas.

# One benefit to functional programming is that it can at times be easier to reason about when simply looking at the code. 
# The inability to modify arguments enables us to predict what the output of a function will be given a certain input, 
# allowing for things like memoization. Functional programming also allows for simple parallelization, 
# so that we can quickly parallelize any code that uses lapply() or map().

# 5 - Design for humans

# Making your code readable and usable by people is a goal that is overlooked surprisingly often. 
# The result is things like function names that are obscure and do not actually communicate what they do. 
# When writing code, using things like good, explicit, function names, with descriptive arguments, 
# can allow for users to quickly learn your API. If you have a set of functions with a similar purpose, 
# they might share a prefix (see e.g. geom_point(), geom_line(), etc.). If you have an argument like color
# that could either take arguments 1, 2, and 3, or black, red, and green, think about which set of arguments 
# might be easier for humans to handle.
 
# To read:
# - https://cran.r-project.org/web/packages/tidyverse/vignettes/manifesto.html
# - http://adv-r.had.co.nz/Introduction.html
# - http://www.noamross.net/blog/2013/4/25/faster-talk.html