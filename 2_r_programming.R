[R Programming]

[Week 1]

1 - Console Input and Evaluation

# indicates a comment, R wil ignore it

x <- 5 	# nothing is printed
x 		# auto-printing occurs
print(x) 	# explicit printing occurs 

2 - R Objects and Attributes

5 atomic classes:
1 - character
2 - numeric
3 - integer —> x <- 1 # saves it as numeric, x <- 1L saves it as integer
4 - complex
5 - logical (true/false)

Inf = special number that equals infinity (e.g. 1/0)

vector —> can only contain objects of the same class
list —> can contain objects of differing classes

Empty vector can be created with: vector()

R objects can have attributes # attributes() —> allows you set or modify attributes
- names, dimnames
- dimensions (e.g. matrices, arrays)
- class
- length
- other user-defined attributes/metadata

3 - Vectors and Lists

x <- c(1:0i, 2+4i) # complex
x <- vector("numeric", length = 10) # ???

Mixing objects: y <- c(1.7, "a") —> coerce 1.7 to character, etc.
Explicit coercion: as.numeric, as.character, as.logical, etc.

x <- list(1, "a", TRUE) ## elements of the list will have double brackets [[]], whereas vectors only have single brackets []

4 - Matrices

Matrices are vectors with a dimension attribute. The dimension attribute is itself an integer vector of length 2: (nrow, ncol)

m <- matrix(1:6, nrow = 2, ncol = 3) # fills it up column wise
dim(m) = [1] 2 3 
attributes(m) = 2, 3

Matrices can be created straight from vectors by simply assigning dim attributes on them:
m <- 1:10
dim(m) < c(2,5)

rbind(x,y) # over rows
cbind(x,y) # over columns

4 - Data Types: Factors

Ordered [hierarchy]
Unordered [e.g. Gender]

Factors with labels are better because they are self-describing 

x <- factor(c("yes", "yes", "no", "yes", "no"))
table(x)    # frequency count
unclass(x)  # underlying structure
attr(x,"levels") = yes no

The order of the levels can be set using the levels attribute which can be important in linear modelling which uses the first level as the baseline
x <- factor(c("yes", "yes", "no", "yes", "no"), levels = c("yes","no")) —> else the levels will come in alphabetical order, which may not be what you want

5 - Missing Values

NA - can have different classes (char, int, etc.) # Not Available / Missing
NaN # Not a Number

NA = NaN | NA
NaN = NaN
NaN != NA

is.na() # check if there are NA values in object
is.nan() # test for NaN

6 - Data Frames

- df is a special type of list where every element of the list has to have the same length (table)
- each element can have a different class
- data frames also have a special attribute called row.names()
- convert to matrix by calling data.matrix(df) 

x <- data.frame(foo - 1:4, bar = c(T,T,F,F), nrow(4:8))
row.names(x) <- c(1,2,5,6)

7 - Data Types: Names Attribute

x <- 1:3
names(x) <- c("foo", "bar", "norf")

x <- list(a = 1, b = 2, c = 3) # first element is called a, etc.

m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b") , c("c", "d") # first vector equals rownames, second vector equals column names

9 - Reading Tabular Data

A - Reading Data
read.table(), read.csv() 		# for reading tabular data // returns a data frame
readLines() 					# for reading lines of a text file
source() 						# for reading in R code files
dget() 						# for reading in R code files
load() 						# for reading saved workspaces
unserialize() 				# for reading single R objects in binary form

B - Writing Data # inverse of read
write.table()
writeLines()
dump()
dput()
save()
serialize()

read.table() # args:
- file 					# the name of the file, or a connection
- header 				# logical indicating if the file has a header line
- sep 					# string which indicates how the columns are separated (tabs, spaces, comma’s, etc.)
- colClasses 			# a character vector indicating the class of each column in the dataset
- nrows 				# number of rows in the dataset
- comment.char 			# a character string indicating the comment character (i.e. ignored after #)
- skip 					# skip number of lines from the beginning
- stringsAsFactors 		# should character variables be coded as factors (T/F)

read.csv = identical to read.table() except the default separator is a comma, instead of a space

10 - Reading Large Tables

- check ?read.table
- Make rough calculation of the memory (RAM) required to store your dataset
- set comment.char = " " # blank if there are no commented lines in your file
- specifying nrows will help R save memory

Find out classes before opening entire table:

initial <- read.table("datatable.txt", nows = 100)
classes <- sapply(initial, class)
tabAll <- read.table("datable.txt", colClasses = classes)
 
Calculating Memory Requirements
df = (1,500,000, 120), all numeric

How memory is required to store the table in  memory?
1,500,000 * 120 * 8 (bytes/numeric) = 14400000000 bytes
= 14400000000 / 2^20 (bytes/mb) = 1,373.29 MB
= 1.34 GB # rule of thumb: you need twice the amount

11 - Textual Data Formats

- dump/source (1 < objects) & dput/dget (1 object)
- e.g. type of the data in each object, preserve meta-data
- dput to save file including meta-data, use dget to read it again

y <- data.frame(a = 1, b = "a")
dput(y, file = "y.R")
new.y <- dget("y.R")

Dumping R Objects # dump/source
- Can be used on multiple objects

x <- "foo"
y <- data.frame(a = 1, b = "a")
dump(c("x", "y"), file = "data.R")
rm(x,y)
source("data.R")

12 - Connections: Interfaces to the Outside World

file() 	# opens a connection to a file
gzfile() 	# opens a connection to a file compressed with gzip (.bz)
bzfile() 	# opens a connection to a file compressed with bzip2 (.bz2)
url() 	# opens a connection to a webpage

function(description = " ", open = " ", blocking = TRUE, encoding = getOption("encoding"))
open = code indicating:
- "r" = read only
- "w" = writing (and initializing new file)
- "a" = appending
- "rb"/"rw"/"ra" --> similar but in binary mode (Windows)

con <- file("foo.txt", "r")
data <- read.csv(con)
close(con)

EQUALS: data <- read.csv("foo.txt")

## Compressed
con <- gzfile("words.gz")
x <- readLines(con, 10) # read the first 10 lines of a compressed file

## URL
con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)
head(x) # shows HTML

13 - Subsetting Objects in R
 
- [ Always returns an object of the same class as the original (list returns list, etc.); can be used to select more than 1 element (with one exception) ]
- [[ Is used to extract elements of a list or a data frame; it can only be used to extract a single element and the class of the returned object will not necessarily be a list or data frame ]]
- $ is used to extract elements of a list or a data frame by name; semantics are similar to that of [[]]

# Subsetting a Vector

x <- c("a", "b", "c", "c", "d", "a")
x[1] = "a"
x[2] = "b"
x[1:4] = "a" "b" "c" "c"
x[x > "a"] = "b" "c" "c" "d"

u <- x > "a"
u = FALSE TRUE TRUE TRUE TRUE FALSE
x[u] = "b" "c" "c" "d"

# Subsetting a List

x <- List(foo = 1:4, bar = 0.6)
x[1]
$foo
[1] 1 2 3 4 
x[[1]]
1 2 3 4 
x$bar
[1] 0.6
x[["bar"]]
[1] 0.6
x["bar"]
$bar
[1] 0.6

x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[c(1,3)] # would only return foo and baz, as list - cannot use $ when you want to return 1 < items

- The [[ operator can be used with computed indices, $ only takes literal names:
x <- list(foo = 1:4, bar = 0.6, baz ="hello")
name <- "foo"
x[[name]] = 1 2 3 4 
x$name = error 	# element name does not exist
x$foo = 1 2 3 4 	# element foo (literal) does exit

Nested Elements of a List
x <- list(a = list(10, 12, 14), b = c(3.14, 2.81))
x[[c(1,3)]] = 14 		# 3rd element of the 1st element
x[[1]][[3]] = 14 		# 3rd element of the 1st element
x[[c(2,1)]] = 3.14 		# 1st element of the 2nd element
 
Subsetting Matrices
x <- matrix(1:6, 2, 3)
x[row,column] // x[1,2] = 3

blank equals return ALL 
x[,2] # everything of the second column
x[1,] # everything of the first row

By default, when a single element of a matrix is retrieved, it will return a vector of length 1 rather than a 1x1 matrix.
This can be turned off by using drop = FALSE.
x <- matrix(1:6, 2, 3)
x[1,2] = 3
x[1,2, drop = FALSE] # returns 3 in a matrix of 1x1 // drop 'drops' the dimensions

Partial Matching

- Is allowed with [[ and $

x <- list(aardvark = 1:5)
x$a = 1 2 3 4 5 # works automatically
x[["a"]] = NULL
[["a", excact = FALSE]] = 1 2 3 4 5 # need to set argument exact = FALSE

Removing NA Values
x <- c(1, 2, NA, 4, NA, 5)
bad <- is.na(x) # create logical vector which indicates where the NA's are
x[!bad] = 1 2 4 5 # only call non-NA values

What if there are multiple things and you wan to take the subset with no missing values?
x <- c(1, 2, NA, 4, NA, 5)
y <- c("a", "b", NA, "d", NA, "f")
good <- complete.cases(x, y) = TRUE TRUE FALSE TRUE FALSE TRUE # logical function, returns TRUE if both elements are non-missing
x[good] = 1 2 4 5
x[good] = a b d f 

It would also work on an entire df --> good <- complete.cases(df) # else use it with separate vectors (columns), etc.

14 - Vectorized Operations

x <- 1:4
y <- 6:9
x + y # adds the first element of x to the first element of y, etc.

Vectorized Matrix Operations

x <- matrix(1:4, 2, 2); y <- matrix(rep(10,4), 2, 2) # replicate 10 four (4) times

x * y # element-wise multiplication (like division: /)

		[,1]	[,2]
[1,]	 10 	 30
[2,]	 20      40

> truem <- x %*% y # true matrix multiplication: x[[1,1]] * y[[1,1]] + x[[1,2]] * y[[1,2]] = truem[[1,1]]

		[,1]	[,2]
[1,]	 40 	 40
[2,]	 60      60

[Week 2]

1 - Control Structures

A - if,else 	# testing a condition
B - for 		# execute a loop a fixed number of times
C - while 		# execute a loop 
D - repeat 		# execute an infinte loop
E - break 		# break the execution of a loop
F - next 		# skip an interation of a loop
G - return 		# exit a function

A - [if, else]

if(<condition>) {
	## do something
} else if(<condition2>) {
	## do something else
} else {
	## do something different -- nested additional "else if"
}

Example

if(x > 3) {y <- 10} else {y <- 0}
if(x > 3) {y <- 10} else if (x == 2) {y <- 1} else {y <- 0}
y <- if(x > 3) {10} else {0}
if(x > 3) {} --??

B - [for]

for(i in 1:10) {print(i)}

x <- c("a", "b", "c", "d")
for(i in 1:4) { print(x[i])} 			# prints a b c d
for(i in seq_along(x)) {print(x[i])} 	# base the length on the length of vector x
for(letter in x) {print(letter)}		# Just switching i (index) with letter
for(i in 1:4) print(x[i])				# with only a single expression you can omit the curly braces

x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))) { 
		for(j in seq_len(ncol(x))) {
		print(x[i,j])
	}
} # seq_len takes an integer (nrows) and creates an integer sequence of (example has 2 rows, so it creates vector c(1,2))

C - [while] # begins by testing a condition, if true, it execute the loop body. Once executed, it tests the condition again, etc.

count <- 0
while(count < 10) { print(count) count <- count + 1} # it should keep printing until count reaches 10
- Basically this structure has no limit, it can go on forever (for has set limits)

z <- 5
while ( z >= 3 && z <= 10) { print(z) 
							   coin <- rbinom(1, 1, 0.5) # rbinom(n, size, prob)
							   if(coin == 1) {
							   z <- z + 1
							   } else {
							   z <- z - 1	
							   }}

C - [repeat & break] # initiates an infinite loop, exit by calling [break]

x0 <-1
tol <- 1e-8

repeat { x1 <- computeEstimate() ## computeEstimate() is not a real function
		   if(abs(x1 - x0) < tol) 
		   {break}
           else { x0 <- x1
           }}

D - [next] # used to skip an iteration of a loop

for(i in 1:100) { 
if(x[i] <= 20) next 
print(x[i]) 
 }

[Writing Functions]

- Functions can be passed as arguments to other functions
- Functions can be nested, so that you can define a function inside of another function 
+ The return value of a function is the last expression in the function to be evaluated

Function arguments
- The formal arguments are the arguments included in the function definition # e.g. function(x,y) --> x & y
- The formals function returns a list of all the formal arguments of a function # formals(function) = x y
- Function arguments can be missing or might have default values

Argument Matching
- R functions arguments can be matched positionally or by name
+ sd(mydata) 					# nothing specified, so mydata defaults to the first argument (x)
+ sd(x = mydata) 				# specifically assigning mydata to x
+ sd(na.rm = FALSE, mydata) 	# na.rm is explicitly mentioned, afterwards mydata defaults back to first argument (in the order at which they come)
- When an argument is matched by name (e.g. data = mydata), it is "taken out" of the argument list and the remaining unnamed arguments are matched in the order that they are listed in the function definition.

args(lm)
function(formula, data, subset, weights, na.action, ...)

Following two calls are equivalent:
lm(data = mydata, y ~ x, model = FALSE, 1:100)
lm(y ~ x, mydata, 1:100, model = FALSE)

Order:
1 - Check for exact match for a named argument
2 - Check for a partial match
3 - Check for a positional match

[Defining a Function]

f <- function(a, b = 1, c = 2, d = NULL){ # a is the only argument which does not have a default value

}

[Lazy Evaluation] - Arguments are only evaluated as they are needed:

f <- function(a, b) {
	a^2
}

f(2) = 4 # function never uses/evaluates b argument, no error because 2 is positionally matched to a

f <- function(a, b){
	print(a)
	print(b)
}

f(45) = 45 + Error: argument "b" is missing, with no default # notice that "45" got printed before the error was triggered while evaluation b

The "..." Argument

myplot <- function(x, y, type = "1", ...) {
	plot(x, y, type=type, ...) # used when extending another function and you don't want to copy the entire arg list of the original function
} # this function basically only changes the default value of type argument of the plot function

args(paste) - function(..., sep = " ", collapse = NULL) # you don't know beforehand the amount of objects you want to paste together, hence you start with ...
- Catch: arguments that appear AFTER ... on the argument list must be named explicitly and cannot be partially matched:
paste("a", "b", sep = ":") = "a:b"
paste("a", "b", se = ":") = "a b :" # this makes sense because otherwise R does not know whether you are passing something to the ... or as a different argument

Examples:

add2 <- function(x, y) {
	x + y
}

add2(3, 5) = 8

above10 <- function(x){
		use <- x > 10
		x[use]
}

above <- function(x,n) {
	  use <- x > n
	  x[use]
}

x <- 1:20
above(x, 12)

above <- function(x, n = 10) { #set default to 10
	  use <- x > n
	  x[use]
}

columnmean <- function(y, removeNA = TRUE){
		   nc <- ncol(y)
		   means <- numeric(nc) # creates a numeric vector with nc elements
		   for(i in 1:nc) {
		   	means[i] <- mean(y[, i], na.rm = removeNA)
		   }
		   means
}

columnmean(airquality)
columnmean(airquality, FALSE) --> will not remove NA values

[Scoping Rules]

lm <- function(x) {x * x}
How does R know what value to assign to lm? Why doesn it not give the value of lm that is in the stats package?

R searches through a series of environments to find the appropriate value, the order is roughly:
1 - Search the global environment for a symbol name matching the one requested (i.e. lm)
2 - Search the namespaces of each of the packages on the search list

The search list can be found using: search() # the order on the search list matters! When a user loads a package using library(), the namespace of that package get put on the second place (after global), rest gets hifted down
- R uses separate namespaces for functions and non-functions so it is possible to have an object named c and a function named c

- R uses lexical or static scoping.
- Scoping rules determine how a value is associated with a free variable in a function

f <- function(x,y){   	# 2 formal arguments: x y
	x^2 + y / z			# 1 free variable: z
}

- The values of free variables are searched for in the environment in which the function was defined.

What is an environment?
- An environment (sigma: context, R e.g. global, each package, etc.) is a collection of (symbol, value) pairs, i.e. x is a symbol and 3.14 might be its values
- Every environment has a parent environment; it is possible for an environment to have multiple "children"
- The only environment without a parent is the empty environment
- A function + an environment = a closure or function closure

Searching for the value for a free variable:
- If the value of a symbol is not found in the environment in which a function was defined, then the search is continued in the parent enviroment.
- The search continues down the sequence of parent environments until we hit the top-level environment; this usually the global environment (workspace) or the namespace of a package.
- After the top-level environment, the search continues down the search list until we hit the empty environment. If a value for a given symbol cannot be found once the empty environment is arrived at, then an error is thrown.

Why does this matter?
- Typically, a function is defined in the global environment, so that the values of free variables are just found in the users workspace.
- This behavior is logical for most people and usually the "right thing" to do
- However, in R you can have functions defined inside other functions (languages like C dont let you do this)
- Now things get interesting - in this case the environment in which a function is defined is the body of another function! 

make.power <- function(n) {
	pow <- function(x) {
		x^n
	}
	pow
}

Lexical vs. Dynamic Scoping

y <- 10

f <- function(x) {
	y <- 2
	y^2 + g(x)
}

g <- function(x) {
	x * y
}

- With lexical scoping the value of y in the function g is looked up in the environment in which the function was defined, in this case the global environment, so the value of y is 10.
- With dynamic scoping, the value of y is looked up in the environment from which the function was called (somtimes referred to as the calling environment), so the value of y is 2. 

[Application: Optimization (To-do)] # !!!

[Coding Standards in R]

1 - Always use text files / text editor
2 - Indent your code (preferably indent of 8)
3 - Limit the width of your code (e.g. 80 colums?)
4 - Limit the length of your function, keep functions seperate (e.g. readdata only reads data) # debuggers will also point to functions, so splitting up functions has a secondary benefit when it comes to debugging

[Dates and Times in R]

R has developed a special representation of dates and times
- Dates are represented by the 'Date' class  
- Times are represented by the 'POSIXct' or the 'POSIXlt' classes           # you cannot mix classes when using mathematical operations, coerce one of them first in the same class (as.POSIXlt() etc.)
- Dates are stored internally as the number of days since 197-01-01 		# unclass
- Times are stored internally as the number of seconds since 1970-01-01		# unclass

x <- as.Date("1970-01-01")
unclass(x) = 0
unclass(as.Date("1970-01-02")) = 1

- POSIXct is just a very large integer under the hood; it is a useful class when you want to store times in something like a data frame
- POSIXlt is a list underneath and stores a bunch of other useful information like the day of the week, day of the year, month, day of the month

There are a number of generic functions that work on dates and times:
- weekdays() 	# give the day of the week
- months() 		# give the month name
- quarters() 	# give the quarter number ("Q1", "Q2", "Q3" or "Q4")

Times can be coerced from a character string using the as.POSIXct or as.POSIXlt functions. # Sys.time() prints the current system time.
Finally, there is the strptime function in case your dates are written in a different format (or use lubridate):

datestring <- ("January 10, 2012: 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M") # check ?strptime to see the formatting 

Can keep track of leap years, leap seconds, daylight savings, and time zones:

x <- as.POSIXct("2012-10-25 01:00:00") # equals current timezone (Sys.time())
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT")
y - x = ## Time difference of 1 hours

[Week 3]

lapply() # loop over a list and evaluate a function on each element
sapply() # same as lapply but try to simplify the result
apply()  # apply a function over the margins of an array
tapply() # apply a function over subsets of a vector
mapply   # multivariate version of lapply // table apply
split()  # auxiliary function, particularly useful in conjunction with lapply

1 - Loop Functions: lapply

lapply() takes three arguments:

I   - a list x
II  - a function (or the name of the function) FUN
III - a other arguments via its ... argument - it will take the same arguments as the function you use with lapply().

- If x is not a list, it will be coerced into one using as.list(x)

function(X, FUN, ...) {
	FUN <- match.fun(FUN) # ... wil take the args from FUN
	if (!is.vector(X) || is.object(X))
	X <- as.list(X)
	.Internal(lapply(X, FUN)) # internal in C code 
}

- lapply() always returns a list, regardless of the class of the input

x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)
$a 
[1] 3
$b
[1] 0.0296824 # in the example, as we use rnorm, own result won't be the same (very unlikely)

x <- 1:4
lapply(x, runif, min = 0, max = 10) # you can still use the args of runif, yet have to specify the names

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
lapply(x, function(elt) elt[,1]) # you write a new anonymous function within the call (elt), which only takes the first column in this case - this function dissapears after the call is done (cannot call it from a different context)

sapply() tries to simplify the result of lapply() if possible
- If the result is a list where every element is length 1, then a vector is returned
- If the result is a list where every element is a vector of the same length (>1), a matrix is returned
- If it cant figure things out, a list is returned

2 - Loop Functions: apply

apply() is used to evaluate a function (often an anonymous one) over the margins of an array
- It is most often used to apply a function to the rows or columns of a matrix
- It can be used with general arrays, e.g. taking the average of an array of matrices
- It is not really faster than writing a loop, but it works in one line (less typing, good programmers are always lazy)!

str(apply)
function (X, MARGIN, FUN, ...)

 - X is an array
 - MARGIN is an integer vector indicating which margins should be "retained"
 - FUN is a function to be applied
 - ... is for other arguments to be passed to FUN

x <- matrix(rnorm(200), 20, 10)              # Matrix with 200 normally distributed observations, 20 rows, 10 columns
apply(x, 2, mean)                            # dimension 1 has 20 rows, dimension 2 has 10 columns, 2 indicates "keep the second dimension, collapse/eliminate the 1st dimension": returns 10 values
apply(x, 1, sum)                             # preserve all the rows, collapse all the columns & sum them: returns 20 values
apply(x, 1, quantile, probs = c(0.25, 0.75)) # probs is an argument passed onto the quantile function (via ...), returns matrix (20 column, 2 rows [2 quantiles])

col/row sums and means - for sums and means of matrix dimensions, we have some shortcuts (which are much faster):
rowSums  = apply(x, 1, sum)
rowMeans = apply(x, 1, mean)
colSums  = apply(x, 2, sum)
colMeans = apply(x, 2, mean)

a <- array(rnorm(2 * 2 * 10), c(2,2,10))  # creates an array of 10 2*2 matrices stacked together
apply(a, c(1,2), mean)                    # returns the mean for [1,1], [1,2], [2,1], [2,2] across all matrices, you keep dim 1 & 2, but collapse dim 3, keeping third dimension would have been c(1,2,3)
rowMeans(a, dims = 2)

3 - Loop Functions: mapply

mapply() is a multivariate apply of sorts which applies a function in parallel over a set of arguments

str(mapply)
function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)

- FUN is a function to apply
- ... containts arguments to apply over
- MoreArgs is a list of other arguments to FUN
- SIMPLIFY indicates whether the result should be simplified

list(rep(1,4), rep(2,3), rep(3,2), rep(4,1)) = mapply(rep, 1:4, 4:1)

noise <- function(n, mean, sd) {
	rnorm(n, mean, sd)
}

noise(5, 1, 2)             # works fine
noise(1:5, 1:5, 2)         # does not work appropriately
mapply(noise, 1:5, 1:5, 2) # does this, this how you can vectorize a function which doesn't allow vector inputs
mapply(noise, 1:5, 1:5, 2) = list(noise(1,1,2), noise(2,2,2), noise(3,3,2), noise(4,4,2), noise(5,5,2))

4 - Loop Functions: tapply

tapply() is used to apply a function over subsets of a vector
str(tapply)
function(X, INDEX, FUN = NULL, ..., simplify = TRUE)

- X is a vector
- INDEX is a factor or a list of factors (or else they are coerced to factors) of the same length as X
- FUN is the function to be applied (can be anonymous)
- .. contains other arguments to be passed to FUN
- should we simplify the result? ß

x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3, 10)     # creastes a factor vector of 10 1's, 10 2's & 10 3's // gl(number of levels, size of each level)
tapply(x, f, mean) # calculates the mean of each group/level

- If you dont simplify you get returned a list

5 - [!Loop Function]: splitting

split() takes a vector or other objects and splits it into groups determined by a factor or list of factors

str(split)
function(x, f, drop = FALSE, ...)

- x is a vector (or list) or data frame
- f is a factor (or coerced into one) or a list of factors
- drop indicates whether empty factor levels should be dropped

x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3, 10)
split(x, f)

lapply(split(x,f), mean) # same as tapply(x, f, mean)

- split always returns a list, so you can use sapply or lapply on it
- split can be used on data frames, e.g. split on month

library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)  
lapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")]))               # use an anonymous function to get colMeans of three specific columns
sapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)) # returns a matrix, fixes NA issue

Splitting on More than One Level
x <- rnorm(10)
f1 <- gl(2,5) # e.g. Gender = 1 1 1 1 1 2 2 2 2 2 
f2 <- gl(5,2) # e.g. Race   = 1 1 2 2 3 3 4 4 5 5 
interaction(f1,f2) = 1.1 1.2 1.3 1.4 1.5 2.1 2.2 2.3 2.4 2.5

str(split(x, list(f1,f2))) # giving split a list of factors automatically uses interaction()
- interactions can create empty levels (when there are no instances where both factors are TRUE)
- when drop = TRUE you will drop these empty levels 

A - Debugging Tools: Diagnosing the Problem

Indications that something is not right

- message   : a generic notification/diagnostic message produced by the message function; execution of the function continues
- warning   : an indication that something is wrong but not necessarily fatal; execution of the function continues; generated by the warning function (after execution, when you "get the console back")
- error     : an indication that a fatal problem has occurred; execution stops; produced by the stop function
- condition : a generic concept for indicating that something unexpected can occur; programmers can create their own conditions

printmessage <- function(x) {

	if(x > 0)
				print("x is greater than 0")
	else
				print("x is less than or equal to zero")
	
	invisible(x)
}

printmessage2 <- function(x) {
	if(is.na(x))
					print("x is a missing value!")
	else if(x > 0)
					print("x is greater than zero")
	else 
					print("x is less than or equal to zero")
	
	invisible(x)
}

B - Debugging Tools: Basic Tools

The primary tools for debugging functions in R are:

- traceback() : prints out the function call stack after an error occurs; does nothing if there is no error # have to call it right after the error occurs, else it won't work
- debug()     : flags a function for "debug" mode which allows you to step through execution of a function one line at a time # first prints out all function code, then it prompts the Browser (the Browser environment, is the FUNCTION environment!), press n to continue. You can debug another function while in browswer mode of another function, i.e. can be nested, you can go levels deeper.
- browser     : suspends the execution of a function wherever it is called and puts the function in debug mode
- trace       : allows you to insert debugging code into a function at specific places
- recover     : allows you to modify the error behavior so that you can browse the function call stack # options(error = recover) will give you the traceback() view after an error occurs, with the option to jump into one of the traceback lines

Summary
- There are three main indications of a problem/condition: message/warning/error --> only an error is fatal
- When analyzing a function with a problem, make sure you can reproduce the problem, clearly state your expectations and how the output differs from your expectations
- Interactive debugging tools traceback, debug, browser, trace, and recover can be used to find problematic code in functions
- Debugging tools are not a substitute for thinking!

[Week 4]

str() # compact display the internal structure of an R object
- A diagnostic function and an alternative to 'summary'
- It is especially well suited to compactly display the (abbreviated) contents of (possibly nested) lists.
- Roughly one line per basic object

[Simulation]

A - Generating Random Numbers

- rnorm() # generate random Normal variates with a given mean and standard deviation
- dnorm() # evaluate the Normal probability density (with a given mean/SD) at a point (or vector of points)
- pnorm() # evaluate the cumulative distribution function for a Normal distribution
- rpois() # generate random Poisson variates with a given rate

Probability distribution functions usually have four functions associated with them. The functions are prefixed with a:
- d for density
- r for random number generation
- p for cumulative distribution
- q for quantile function

dnorm(d, mean = 0, sd = 1, log = FALSE) # if you do not specify the mean and sd, it will take mean = 0, sd = 1
pnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) # equal to the inverse of pnorm = Fi ^-1
rnorm(n, mean = 0, sd = 1)

Always use set.seed(integer) --> this ensures reproducibility for others, so they can check if they derive at the same result. It resets the sequence to back where you started.

set.seed(1)
rnorm(5)	   #1 these values are equal to #2
rnorm(5)
set.seed(1)
rnorm(5)	   #2 these values are euqal to #1

rpois(10, 1) = 3 1 0 1 0 0 1 0 1 1
ppois(2, 2)  = 0.6766764 # what is the probability that one of my random poisson values has a value of less than 2 if the rate is 2
ppois(4, 2)  = 0.947347  # probability that one of my random poisson values has a value of less than 4 if the rate is 2

B - Simulating a Linear model

set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x, y)

What if X is binary?

set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x,y)

Poisson model
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y)

C - Random Sampling

set.seed(1)
sample(1:10, 4)
sample(Letters, 5)
sample(1:10)                  # random permutation
sample(1:10, replace = TRUE)  # with replacement

Summary
- Drawing samples from specific probability distributions can be done with r* functions 
- Standard distributions are built in: Normal, Poisson, Binomial, Exponential, Gamma, etc.
- The sample function can be used to draw random samples from arbitrary vectors
- Setting the random number generator seed via set.seed is critical for reproducability

R Profiler

Why is my code so slow?
- Profiling is a systematic way to examine how much time is spend in different parts of a program 
- Useful when trying to optimize your code
- Often code runs fine once, but what if you have to put it in a loop for 1000 iterations? Is it still fast enough?
- Profiling is better than guessing

On Optimizing Your code
- Getting biggest impact on speeding up code depends on knowing where the code spends most of its time
- This cannot be done without performance analysis or profiling 

General Principles of Optimization
- Design first, then optimize # if you optimize first, you have higher chances of creating bugs
- Remember: premature optimization is the root all evil
- Measure (collect data), do not guess 
- If you are going to be scientist, you need to apply the same principles here!

Using system.time()
- Takes an arbitrary R expression as input (can be wrapped in curly braces) and returns the amount of time taken to evaluate the expression
- Computes the time (in seconds) needed to execute an expression
+ If there is an error, gives time until the error occurred
- Returns an object of class proc_time
+ User time: time charged to the CPU(s) for this expression
+ Elapsed time: "wall clock" time
- Usually, the user time and elapsed time are relatively close, for straight computing tasks
- Elapsed time may be greater than user time if the CPU spends a lot of time waiting around
- Elapsed time may be smaller than user time if your machine has multiple cores/processors (and is capable of using them)
+ Multi-threaded BLAS libraries (vecLib/Accelerate, ATLAS, ACML, MKL)
+ Parallel processing via the parallel package

system.time({
		n <- 1000
		r <- numeric(n)
		for (i in 1:n) {
			x <- rnorm(n)
			r[i] <- mean(x)
		}
	}) # within curly braces simply means "DO"! does not necessarily have to be a function (although outside this example/optimizing checks it makes little sense to do this)

Beyond system.time()
- Using system.time() allows you to test certain functions or code blocks to see if they are taking excessive amounts of time
- Assumes you already know where the problem is and can call system.time() on it
- What if you do not know where to start --> R-Profiler. 

- The Rprof() function starts the profiler in R
+ R must be compiled with profiler support (but this is usually the case)
- The summaryRprof() function summarizes the output from Rprof() (otherwise its not readable)
- DO NOT use system.time() and Rprof() together or you will be sad

- Rprof() keeps track of the function call stack at regularly sampled intervals and tabulates how much time is spend in each function
- Default sampling interval is 0.02 seconds
- NOTE: If your code runs very quickly, the profiler is not useful, but then you probably do not need it in that case 

- The summaryRprof() function tabulates the R profiler output and calculates how much time is spend in which function
- There are two methods for normalizing the data:
+ "by.total" divides the time spend in each function by the total run time
+ "by.self" does the same but first subtracts out time spent in functions above in the call stack

- Rprof() runs the profiler for analysis of performance of R code
- summaryRprof() summarizes the output of Rprof() and gives percent of time spent in each function (with two types of normalization)
- Good to break your code into functions so that the profiler can give useful information about where time is being spent
- C or Fortran code is not profiled

Example1 (Lists):  
x <- list(foo=1:4, bar=0.6)
Explicitly explain each for: 
x[1], x[[1]], x$bar, x[["bar"]], x["bar"]
x[1] returns:
$foo
[1] 1 2 3 4
x[[1]] returns:
[1] 1 2 3 4
x$bar returns:
[1] 0.6
x[["bar"]] returns:
[1] 0.6
x["bar"] returns:
[1] 0.6

Example2 (Lists):
x <- list(foo=1:4, bar=0.6, baz="hello")
Explicitly explain: x[c(1,3)]
x[c(1,3)] returns:
$foo
[1] 1 2 3 4
$baz
[1] "hello"

Example3 (Lists):
x <- list(a=list(10,12,14), b=c(3.14, 2.8))
print(x) returns:
$a
$a[[1]]
[1] 10
$a[[2]]
[1] 12
$a[[3]]
[1] 14
$b
[1] 3.14 2.80
