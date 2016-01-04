[Getting and Cleaning Data]

-- Week 1

[Obtaining Data Motivation]

- This course covers the basic ideas behind getting data ready for analysis
  + Finding and extracting raw data
  + Tidy data principles and how to make data tiny
  + Practical implementation through a range of R packages

The goal of this course:
Raw data -> Processing script -> tidy data

[Raw and Processed Data]

Definition of Data:
"Data are values of qualitative or quantitative variables, belonging to a set of items."
- Qualitative: Country of origin, sex, treatment
- Quantitative: Height, weight, blood pressure

Raw data
- The original source of the data
- Often hard to use for data anlyses
- Data analyses includes processing
- Raw data may only need to be processed once

Processed data
- Data that is ready for analysis
- Processing can include merging, subsetting, transforming, etc.
- There may be standards for processing 
- All steps should be recorded

[Components of Tidy Data]

The four things you should have:
1 - The raw data
2 - A tidy data set
3 - A code book describing each variable and its values in the tidy dataset  # metadata
4 - An explicit and exact recipe you used to go from step 1 -> 2,3. # how did you go from raw to processed data

The raw data # the rawest data you have access to
- The strange binary file your measurement machine spits outs
- The unformatted Excel file with 10 worksheets the company you contracted with sent you
- The complicated JSON data you got from scraping the Twitter API
- The hand-entered numbers you collected looking through a microscope

You know the raw data is in the right format if you:
1 - Ran no software on the data
2 - Did not manipulate any of the numbers in the data
3 - You did not remove any data from the dataset
4 - You did not summarize the data in any way

The tidy data
1 - Each variable you measure should be in one column
2 - Each different observation of that variable should be in a different row
3 - There should be one table for each "kind" of variable
4 - If you have multiple tables, they should include a column in the table that allows them to be linked

Some other important tips:
- Include a rown at the top of each file with variable names
- Make variable names human readable AgeAtDiagnosis instead of AgeDx
- In general data should be saved in one file per table

The code book
1 - Information about the variables (including units!) in the data set not contained in the tidy data
2 - Information about the summary choices you made # mean/median, why, etc.
3 - Information about the experimental study design you used to gather the data

Some other important tips
- A common format for this document is a Word/text file.
- There should be a section called "Study design" that has a thorough description of how you collected the data.
- There must be a section called "Code book" that describes each variable and its units.

The instruction list
- Ideally a computer script (in R :-), but I suppose Python is ok too..)
- The input for the script is the raw data
- The output is the processed, tidy data
- There are no parameters to the script

In some cases it will not be possible to script every step. In that case you should
provide instructions like:

Step 1 - take the raw file, run version 3.1.2 of summarize software with parameters a=1, b=2, c=3
Step 2 - run the software separately for each sample
Step 3 - take column three of outputfile.out for sample and that is the corresponding row in the output data set

[Downloading Files]

- Be aware of relative versus absolute paths:
  + Relative: setwd("./data"), setwd("../") # moves to "Users/jtleek/"
  + Absolute: setwd("Users/jtleek/data/")

Checking for and creating directories

- file.exists("directoryName") # will check to see if the directory exists
- dir.create("directoryName") # will create a directory if it does not exists

Example that checks for a 'data' directory and creating it if it does not exist:

if (!file.exists('data')) {
  dir.create('data')
}

Getting data from the Internet - download.file()
- Downloads a file from the Internet
- Even if you could do this by hand, helps with reproducibility
- Important parameters are url, destfile, method
- Useful for downloading tab-delimited, csv and other files.

fileUrl <- "https://www..."
download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl") # method = curl since url is httpS
list.files("./data")

Downloaded data can change over time, so smart to save the date:

dateDownloaded <- date()

Some notes about download.file()
- If the url starts with 'http' you can use download.file()
- If the url starts with 'https' on Mac you may need to set 'method = "curl"'
- If the file is big, it might take a while
- Be sure to record when you downloaded

[Reading Local Flat Files]

read.table()
- This is the main function for reading data into R
- Flexible and robust but requires more parameters
- Reads the data into RAM - big data can cause problems
- Important parameters: file, header, sep, row.names, nrows
- Related: read.csv(), read.csv2()

cameraData <- read.table("./data/cameras.csv", sep = ",", header = TRUE) # could also use read.csv() (sep = "," & header = TRUE)

Some more important parameters
- 'quote': you can tell R whether there are any quotated values; quote = "" means no quotes
- 'na.strings': set the character that represents a missing value.
- 'nrows': how many rows to read of the file (e.g. nrows = 10 reads 10 lines)
- 'skip': number of lines to skip before starting to read

"In my experience, the biggest trouble with reading flat files are quotation marks
' or " placed in data values, setting quote = "" often resolves these.""

[Reading Excel Files]

library(xlsx)
cameraData <- read.xlsx("./data/cameras.xlsx", sheetIndex = 1, header = TRUE) # which sheet is the data stored on

colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./data/cameras.xlsx", sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex) # only select certain rows/columns (subset)

- The write.xlsx() function will write out an Excel file with similar arguments
- read.xlsx2 is much faster than read.xlsx but for reading subsets of rows it may be slightly unstable
- The XLConnect package has more optios for writing and manipulating Excel files
- The XLConnect vignette is a good place to start for that package
- In general it is advised to store your data in either a database or in comma seperated files (.csv)
  or tab separated files (.tab/.txt) as they are easier to distribute

[Reading XML]

- Extensible markup language
- Frequently used to store structured data
- Particularly widely used in internet applications
- Extracting XML is the basis for most web scraping
- Components:
  + Markup: labels that give the text structure
  + Content: the actual text of the document

Tags, elements and attributes

- Tags correspond to general labels
  + Start tags <section>
  + End tags </section>
  + Empty tags <line-break /> # opens and closes within one tag
- Elements are specific examples of tags
  + <Greeting> Hello, world </Greeting>
- Attributes are components of the label
  + <img src="jeff.jpg", alt="instructor"/>
  + <step number="3"> Connect A to B. </step>

Read the file into R

library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
#=> breakfast_menu
names(rootNode)
#=> food food food food food food

rootNode[[1]] # list indexing, hence [[]], vector is []

<food>
  <name>Belgian Waffles</name>
  <price>$5.95</price>
  <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>
  <calories>650</calories>
</food>

rootNode[[1]][[1]]

<name>Belgian Waffles</name>

Programmatically extract parts of the file

xmlSApply(rootNode, xmlValue) # loop through all the elements of rootNode and get the xmlValue (which is a function)

XPath
- '/node': Top level node # of each element
- '//node': Node at any level
- 'node[@attr-name]': Node with an attribute name
- 'node[@attr-name='bob']': Node with attr-name = 'bob' 

xpathSApply(rootNode, '//name', xmlValue)
#=> Belgian Waffles, Strawberry Belgian Waffles, Berry-Berry Belgian Waffles, French Toast, Homestyle Breakfast

xpathSApply(rootNode, '//price', xmlValue)
#=> $.5.95, $7.95, $8.95, $4.50, $6.50

Extract content by attributes

fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore_ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE) # parsing HTML file, so use htmlTreeParse instead of xmlTreeParse
scores <- xpathSApply(doc, '//li[@class='score']', xmlValue) # check for a 'li' item (list item) and check if the class = score, when it does, return the value
teams <- xpathSApply(doc, '//li[@class='team-name']', xmlValue) # similar, but checks if the class = team-name (class is the attribute)
scores
#=> 49-27, 14-6, 30-9, etc.
teams
#=. Denver, Cleveland, Houston, etc.

[Reading JSON]

- Javascript Object Notation
- Lightweight data storage
- Common format for data from application programming interfaces (API)
- Similar structure to XML but different syntax/format
- Data stored as:
  + Numbers (double)
  + Strings (double quoted)
  + Boolean (TRUE or FALSE)
  + Array (ordered, comma separated enclosed in square brackets [])
  + Object (unordered, comma separated collection of key:value pairs in curley brackets {})

library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
names(jsonData$owner)
jsonData$owners$login

Writing data frames to JSON

myjson <- toJSON(iris, pretty = TRUE)
cat(myjson)

Convert back from JSON

iris2 <- fromJSON(myjson)
head(iris2)

[The data.table Package]

data.table
- Inherets from data.frame
  + All functions that accept data.frame work on data.table
- Written in C so it is much faster
- Much, much faster at subsetting, grouping and updating

library(data.table)
DF = data.frame(x = rnorm(9), y = rep(c('a', 'b', 'c'), each = 3), z = rnorm(9))
head(DF, 3)

DT = data.table(x = rnorm(9), y = rep(c('a', 'b', 'c'), each = 3), z = rnorm(9))
head(DT, 3)

See all data tables in memory:
tables()

Subsetting rows (similar):

DT[2,]
DT[DT$y == 'a', ]

Different:

If you subset with only one index, it will subset on rows:

DT[c(2,3)] # will return row 2 and 3

Column subsetting in data.table

- The subsetting function is modified for data.table
- The argument you pass after the comma is called an 'expression'
- In R an expression is a collection of statements enclosed in curley brackets

{
  x = 1
  y = 2
}
k = {print(10); 5}
#=> 10
print(k)
#=> 5

Calculating values for variables with expressions

DT[,list(mean(x),sum(z))] 
# pass it a list of functions
# returns two variables in two columns the mean of column x, the sum of column z

DT[,table(y)]

DT[,w:=z^2] # ads a new column 'w' which has as values z^2

Adding new columns

DT2 <- DT
DT[, y:= 2]
--> you change both DT2 AND DT now.

Multiple Operations

DT[, m:= {tmp <- (x+z); log2(tmp+5)}]
- statements are separated by a semicolon ;
- the last thing which is returned is the evaluation of the last statement
- so ultimately m will be log2((x+z)+5)

plyr like operations

DT[, a := x > 0] # returns boolean column a
DT[, b := mean(x + w), by = a] # take the mean of x + w, grouped by a

Special Variables

- '.N': An integer, length 1, containing the number of times that a particular group appears

set.seed(123);
DT <- data.table(x = sample(letters(1:3), 1E5, TRUE)) # 1E5 == 100000
DT[, .N, by = x]
#=>     x      N
#=> 1:  a    33387
#=> 2:  b    33201
#=> 3:  c    33412

Keys
DT <- data.table(x = rep(c('a', 'b', 'c'), each = 100), y = rnorm(300))
setkey(DT, x)
DT['a']
#=> returns all the variables where x == 'a'

Joins

DT1 <- data.table(x = c('a', 'a', 'b', 'dt1'), y = 1:4)
DT2 <- data.table(x = c('a', 'b', 'dt2'), z = 5:7)
setkey(DT1, x); setkey(DT2, x);
merge(DT1, DT2) # inner join
#=>     x    y     z
#=> 1:  a    1     5
#=> 2:  a    2     5
#=> 3:  b    3     6

Fast Reading
big_df <- data.frame(x = rnorm(1E6), y = rnorm(1E6))
file <- tempfile()
write.table(big_df, file = file, row.names = FALSE, col.names = TRUE, sep = '\t', quote = FALSE)
system.time(fread(file))
#=> user 0.312 system 0.015 elapsed 0.326

-- Week 2

[Reading from MySQL]

- Free and widely used open source database software
- Widely used in internet based applications
- Data are strcutred in: 
  + Databases
  + Tables within databases
  + Fields within tables
- Each row is called a record

Step 1 - MySQL # http://dev.mysql.com/doc/refman/5.7/en/installing.html
Step 2 - On a Mac: install.packages.("RMySQL")

Connecting and listing databases

ucscDb <- dbConnect(MySQL(), user = 'genome', host = 'genome-mysql.cse.ucsc.edu') # establish a connection
result <- dbGetQuery(ucscDb, 'show databases;'); # run a MySQL command
dbDisconnect(ucscDb); # Do this when you are done

Connecting to hg19 and listing tables

hg19 <- dbConnect(MySQL(), user = 'genome', db = 'hg19', host = 'genome-mysql.cse.ucsc.edu')
allTables <- dbListTables(hg19)
length(allTables)

dbListFields(hg19, 'affyU133Plus2')
dbGetQuery(hg19, 'select count(*) from affyU133Plus2')

Read from the table

affyData <- dbReadTable(hg19, 'affyU133Plus2')
head(affyData)

Select a specific subset

query <- dbSendQuery(hg19, 'select * from affyU133Plus2 where misMatches between 1 and 3') # this stores the table remotely; not locally
affyMis <- fetch(query); # afterwards you store it locally using the fetch() command
quantile(affyMis$misMatches) # this shows you we only have data where misMatches is between 1 and 3
affyMisSmall <- fetch(query, n = 10); # Only fetch first 10 rows
dbClearResult(query); # This closes the query, which would otherwise still be active at the server
dim(affyMissSmall) # only 10 rows
dbDisconnect(hg19) # Remember to close the connection

[Reading from HDF5] # Hierarchical Data Format

- Used for storing large data sets
- Supports storing a range of data types
- Hierarchical data format
- groups containing zero or more data sets and metadata
  + Have a group header with group name and list of attributes
  + Have a group symbol table with a list of objects in group
- datasets multi-dimensional array of data elements with metadata
  + Have a header with name, datatype, dataspace and storage layout
  + Have a data array with the data

source('http://bioconductor.org/biocLite.R')
biocLite('rhdf5')

library(rhdf5)
created = h5createFile('example.h5')
created

- This will install packages from Bioconductor, primarily used for genomics but also has good 'big data' packages
- Can be used to interface with hdf5 data sets
- This lecture is modeled very closely on the rhdf5 tutorial that can be found here:
  http://www.bioconductor.org/packages/release/bioc/vignettes/rhdf5/inst/doc/rhdf5.pdf

created <- h5createGroup('example.h5', 'foo') # create group called foo
created <- h5createGroup('example.h5', 'baa')
created <- h5createGroup('example.h5', 'foo/foobaa') # sub-group of foo called foobaa
h5ls('example.h5') # list groups within example.h5

Write to Groups

A <- matrix(1:10, nr = 5, nc = 2)
h5write(A, 'example.h5', 'foo/A') # write A to a group called A, which is a sub of foo
B <- array(seq(0.1, 2.0, by = 0.1) , dim = c(5, 2, 2)) # multi-dimensional array
attr(B, 'scale') <- 'liter'
h5write(B, 'example.h5', 'foo/foobaa/B')
h5ls('example.h5')

Write a Data Set

df <- data.frame(1L:5L, seq(0, 1, length.out = 5),
  c('ab', 'cde', 'fghi', 'a', 's'), stringsAsFactors = FALSE)

h5write(df, 'example.h5', 'df') # write df to a group called df
h5ls('example.h5')

Reading Data

readA <- h5read('example.h5', 'foo/A')
readB <- h5read('example.h5', 'foo/foobaa/B')
readdf <- h5read('example.h5', 'df')
readA # outputs foo/A

[Writing and Reading Chunks]

h5write(c(12, 13, 14), 'example.h5', 'foo/A', index = list(1:3, 1)) # first three rows, first column // index just takes a list that contains dimensions
h5read('example.h5', 'foo/A') # overwrite part of foo/A to 12, 13, 14

[Reading from The Web]

Webscraping
- Programatically extracting data from the HTML code of websites.
  + It can be a great way to get data 
  + Many websites have information you may want to programmatically read
  + In some cases this is against the terms of the website
  + Attempting to read too many pages too quickly can get your IP address blocked

Getting data off webpages - readLines()

con <- url('http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en')
htmlCode = readLines(con)
close(con)
htmlCode
#=> '<!DOCTYPE html><html><head><title>Jeff Leek - Google Scholar Citations</title>' ... etc.

Parsing with XML

library(XML)
url <- 'http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en'
html <- htmlTreeParse(url, useInternalNodes = TRUE)
xpatSapply(html, '//title', xmlValue)
#=> 'Jeff Leek - Google Scholar Citations'
xpatSApply(html, '//td[@id='col-citedby']', xmlValue)

GET from the httr package

library(httr)
html2 <- GET(url)
content2 <- content(html2, as = 'text')
parsedHtml <- htmlParse(content2, asText = TRUE)
xpathSApply(parsedHtml, '//title', xmlValue)

Accessing websites with passwords

pg1 <- GET('http://httpbin.org/basic-auth/user/passwd', authenticate('user', 'passwd'))
names(pg2)
content <- (pg2, as = 'text')

Using Handles

google <- handle('http://google.com') # if you authenticate it here you will not have to do it over and over again
pg1 <- GET(handle = google, path = '/')
pg2 <- GET(handle = google, path = 'search')

[Reading from APIs]

library(httr)
myapp <- oauth_app('twitter', # name for own convenience
                   key = 'yourConsumerKeyHere', secret = 'yourConsumerSecretHere')
sig <- sign_oauth1.0(myapp, # signing in
                    token = 'youTokenHere',
                    token_secret = 'yourTokenSecretHere')
homeTL <- GET('https://api.twitter.com/1.1/statuses/home_timeline.json', sig)

Converting the json object

json1 <- content(homeTL)
json2 <- jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]

# To know which URL to use, look it up in the Twitter documentation

In general look at the documentation

- httr allows GET, POST, PUT, DELETE requests if you are authorized
- You can authenticate with a user name or a password
- Most modern APIs use something like oauth
- httr works well with Facebook, Google, Twitter, Github, etc.

[Reading from other sources]

Interacting more directly with files
- file: open a connection to a text file
- url: open a connection to a url 
- gzfile: open a connection to a .gz file # zip file
- bzfile: open a connection to a .bz2 file # zip file
- ?connections for more information
- Remember to close connections!

foreign package
- loads data from Minitab, S, SAS, SPSS, Stata, Systat
- Basic functions read.foo # foo as placeholder/example, it is the extension for the particular programming language
  + read.arff (Weka)
  + read.dta (Stata)
  + read.mtp (Minitab)
  + read.octave (Octave)
  + read.spss (SPSS)
  + read.xport (SAS)
- See the help page for more details: http://cran.r-project.org/web/packages/foreign/foreign.pdf

Examples of other database packages
- RPostresSQL provides a DBI-compliant database connection from R.
- RODBC provides infterfaces to multiple databases including PostgreQL, MySQL, Microsoft Access and SQLite.
- RMongo and rmongodb provide interfaces to MongoDb.

Reading images
- jpeg
- readbitmap
- png
- EBImage (Bioconductor)

Reading GIS data
- rdgal
- rgeos
- raster

Reading music data
- tuneR
- seewave

-- Week 3

[Subsetting and Sorting]

Subsetting - quick review

set.seed(13435)
X <- data.frame('var1' = sample(1:5), 'var2' = sample(6:10), 'var3' = sample(11:15))
X <- X[sample(1:5),] # rearrange the order of the rows
X$var2[c(1, 3)] <- NA # set some values to NA
X[,1] # open the first column
X[,'var1'] # open var1 column
X[1:2, 'var2'] # out put the first two rows of var2

Logicals ands and ors

X[(X$var1 <= 3 & X$var3 > 11), ] # where var1 is smaller or equal to 3, var 3 is bigger than 11
X[(Xvar1 <= 3 | X$var3 > 15), ] # var1 smaller or equal to 3 or var3 bigger than 15

& and && indicate logical AND and | and || indicate logical OR. 
The shorter form performs elementwise comparisons in much the same way as arithmetic operators. 
The longer form evaluates left to right examining only the first element of each vector. 
Evaluation proceeds only until the result is determined.

Dealing with missing values

X[which(X$var2 > 8), ] # NAs are allowed and ommitted (treated as if FALSE)

Sorting 

sort(X$var1) # sort in increasing order
sort(X$var1, decreasing = TRUE) # sort in decreasing order
sort(X$var2, na.last = TRUE) # puts NA values last

Ordering

X[order(X$var1),1] # order the rows by the value of var1
X[order(X$var1, X$var3),1] # first sort on var1, if there are values that are the same in var1, it will use var3 to sort

Ordering with plyr

library(plyr)
arrange(X,var1)
arrange(X,desc(var1))

Adding rows and colums

X$var4 <- rnorm(5) # var4 didn't exist before
Y <- cbind(X, rnorm(5)) # right column bind --> swap the parameters will bind on the left
Z <- rbind(X, rnorm(5)) # row bind on the bottom of the df, swap the parameters will bind it at the top

[Summarizing data]

if(!file.exists('./data')){dir.create('./data')}
fileUrl <- 'https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD'
download.file(fileUrl, destfile = './data/restaurants.csv', method = 'curl')
restData <- read.csv('./data/restaurants.csv')

Look at a bit of the data 

head(restData, n = 3) # show top 3 rows
tail(restData, n = 3) # show bottom 3 rows
summary(restData) # overall summary
str(restData) # shows structure of df

Quantiles of quantitative variables
quantile(restData$councilDistrict, na.rm = TRUE) # shows quantiles
quantile(restData$councilDistrict, probs = c(0.5, 0.75, 0.9)) # shows 50%, 75% & 90% percentiles

Make table

table(restData$zipCode, useNA = 'ifany') # create a touble (count) / useNA = if-any will take NA's as a class and count them
table(restData$coucilDistrict, restData$zipcode) # creates 2-dimensional table

Check for missing values

sum(is.na(restData$councilDistrict)) # returns 0 if none (is.na returns TRUE -1- if value is NA)
any(is.na(restData$councilDistrict)) # returns FALSE if none
all(restData$zipCode > 0) # returns TRUE if no NAs / all the values have to satisfy the condition

colSums(is.na(restData)) # check for NAs in every column
all(colSums(is.na(restData)) == 0 =) # returns TRUE if all colSums == 0

Values with specific characteristics

table(restData$zipCode %in% c('21212'))
table(restData$zipCode %in% c('21212', '21213'))
restData[restData$zipCode %in% c('21212', '21213'), ] # you can use this to index

Cross tabs

data(UCBAdmissions)
DF <- as.data.frame(UCBAdmissions)
summary(DF)

xt <- xtabs(Freq ~ Gender + Admit, data = DF) # Look at frequency broken down by Gender and Admit (admitted vs. rejected)

Flat tables

warpbreaks$replicate <- rep(1:9, len = 54) # up to 54 values
xt <- xtabs(breaks ~ ., data = warpbreaks) # . denotes "all the other variables"
# Now there are 3 variables: replicate, tension & wool
# (so you get a list of multiple 2-dimensional tables)
ftable(xt) # flattens this in one more compact table again

Size of a data set

fakeData <- rnorm(1e5)
object.size(fakeData) # returns the size in bytes
print(object.size(fakeData), units = 'Mb') # prints the size in Mb

[Creating New Variables]

Why create new variables?
- Often the raw data will not have a value you are looking for 
- You will need to transform the data to get the values you would like 
- Usually you will add those values to the data frames you are working with 
- Common variables to create 
  + Missingness indicators 
  + "Cutting up" quantitative variables # turn into factors 
  + Applying transforms 

Creating sequences

Sometimes you need an index for your data set 

s1 <- seq(1, 10, by 2) # 1 to 10 with steps of 2
s2 <- seq(1, 10, length = 3) # 1 to 10 in 3 variables 
x <- c(1, 3, 8, 25, 100)
seq(along = x) # simply count along x (so 1-5)

Subsetting variables

restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland") # returns T/F column
restData$zipWrong <- ifelse(restData$zipCode < 0, TRUE, FALSE) # like SQL IF()
table(restData$zipWrong, restData$zipCode < 0)

Creating categorical variables

restData <- cut(restData$zipCode, breaks = quantile(restData$zipCode)) # break it on the quantiles
table(rstData$zipGroups)

Easier cutting 

library(Hmisc)
restData <- cut2(restData$zipCode, g = 4) # break into 4 groups
table(restData$zipGroups)

Creating factor variables 

restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf) # factor

Levels of factor variables

yesno <- sample(c('yes', 'no'), size = 10, replace = TRUE)
yesnofac <- factor(yesno, levels = c('yes', 'no'))
relevel(yesnofac, ref = 'yes')
as.numeric(yesnofac)

Using the mutate function 

library(Hmisc)
library(plyr)

restData2 <- mutate(restData, zipGroups = cut2(zipCode, g = 4)) # restData2 is a copy of restData, but with some additional columns/data
table(restData2$zipGroups)

Common transforms

- abs(x) - absolute value 
- sqrt(x) - square root 
- ceiling(x) - ceiling(3.475) is 4
- floor(x) - floor(3.475) is 3
- round(x, digits = n) - round(3.475, digits = 2) is 3.48
- signif(x, digits = n) - signif(3.475, digits = 2) is 3.5
- cos(x), sin(x) etc.
- log(x) - natural logarithm
- log2(x), log10(x) - other common logs 
- exp(x) - exponentiating x

[Reshaping Data]

The goal is tidy data:
1 - Each variable forms a column 
2 - Each observation forms a row 
3 - Each table/file stores data about one kind of observation

Start with reshaping 

library(reshape2)
head(mtcars)

Melting data frames 

mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id = c('carname', 'gear', 'cyl'), measure.vars = c('mpg', 'hp')) # melt the measure values in one column
head(carMelt, n = 3)
tail(carMelt, n = 3)

Casting data frames 

cylData <- dcast(carMelt, cyl ~ variable) # returns the counts per group by default 
cyldata <- dcast(carMelt, cyl ~ variable, mean) #now takes the mean

Averaging values

head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

Another way - split

spIns <- split(InsectSpray$count, InsectSprays$spray) # returns a list, each element of the list contains all the count values (e.g. many)
sprCount <- lapply(spIns, sum) # list apply sum
unlist(sprCount) # transforms the list into a vector
sapply(spIns, sum) # yields same result as above 2 lines

Another way - plyr package

ddply(InsectSprays, .(spray), summarize, sum = sum(count)) # .() replaces ''

Creating a new variable 

spraySums <- ddply(InsectSprays, .(spray), summarize, sum = ave(count, FUN = sum)) # ave() --> group average
dim(SpraySums)

Also see the function:
- acast(): for casting as multi-dimensional arrays 
- arrange(): for faster reordering without using order() commands 
- mutate(): adding new variables

[Managing Data Frames with dplyr]

The data frame is a key data structure in statistics and in R
- There is one observation per row
- Each column represents a variable or measure or characteristic 
- Primary implementation that you will use is the default R implementation
- Other implementations, particularly relational databases systems

dplyr 
- Developed by Hadley Wickham of Rstudio
- An optimized and distilled version of plyr package (also by Hadley)
- Does not proivde any 'new' functionality per se, but greatly simplifies
  existing functionality in R 
- Provides a 'grammar' (in particular, verbs) for data manipulation
- Is very fast, as many key operations are coded in C++

dplyr Verbs 
- select: return a subset of the columns of a dataframe
- filter: extract a subset of rows from a data frame based on logical conditions
- arrange: reorder rows of a data frame 
- rename: rename variables in a data frame 
- mutate: add new variables/columns or transform existing variables
- summarise / summarize: generate summary statistics of different variables in the data frame, possibly within strata 

There is also a handy print method that prevens you from printing a lot of data to the console

dplyr Properties 
- The first argument is a data frame 
- The subsequent argument describes what to do with it, and you can refer
  to columns in the data frame directly without using the $ operator (just use the names)
- The result is a new data frame 
- Data frames must be properly formatted and annotated for this to all be useful

Basic Tools 

library(dplyr)

+ select()

chicago <- readRDS('chicago.rds')
names(chicago) # print variable names
head(select(chicago, city:dptp)) # return me the head of all the columns from city up to dptp
head(select(chicago, -(city:dptp))) # return everything except those columns

# in regular R this would be:

i <- match('city', names(chicago)) # find col index city
j <- match('dptp', names(chicago)) # find col index dptp
head(chicago[, -(i:j)])

+ filter() 

chic.f <- filter(chicago, pm25tmean2 > 30)
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)

+ arrange()

chicago <- arrange(chicago, date) # arrange by date ASC
chicago <- arrange(chicago, desc(date)) # arrange DESC

+ rename()

chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp) # new_name = old_name

+ mutate()

chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE)) # creates new variable: pm25 - its mean
head(select(chicago, pm25, pm25detrend))

+ group_by()

chicago <- mutate(chicago, tempcat = factor(1 * (tmpd > 80), labels = c('cold', 'hot'))) # factor variable, day hot or cold (> 80)
hotcold <- group_by(chicago, tempcat) # grouped into hot & cold

+ summarize()

summarize(hotcold, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
summarize(years, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

== Special Operator: The Pipeline Operator %>% ==

# All functions require to take a df as the first argument
# So you don't need to specify this when using %>%

chicago %>% mutate(month = as.POSIXlt(date)$mon + 1) %>% group_by(month) %>% summarize(pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

Once you learn the dplyr 'grammar' there are a few additional benefits
- dplyr can work with other data frame 'backends'
  + data.table for large fast tables
  + SQL interface for relational databases via the DBI package

[Merging Data]

Peer review data

if(!file.exists('./data')){dir.create('./data')}
fileUrl1 <- 'https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv'
fileUrl2 <- 'https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv'
download.file(fileUrl1, destfile = './data/reviews.csv', method = 'curl')
download.file(fileUrl1, destfile = './data/solutions.csv', method = 'curl')
reviews <- read.csv('./data/reviews.csv')
solutions <- read.csv('/data/solutions.csv')

+ merge() 
- Merges data frames 
- Important parameters: x, y, by, by.x, by.y, by.all 

mergedData <- merge(reviews, solutions, by.x = 'solutions_id', by.y = 'id', all = TRUE) # all = TRUE --> if there is a row with a value in x but not in y, show it but with NA for the missing values: FULL OUTER JOIN

Default - merge all common column names 

intersect(names(solutions), names(reviews)) # which columns are in both data frames 
mergedData2 <- merge(reviews, solutions, all = TRUE) # when you don't tell what to merge on, it will try to merge on all those variables returned by intersect()

Using join in the plyr package 
# Faster, but less full featured - defaults to left join, see help file for more info 

df1 <- data.frame(id = sample(1:10), x = rnorm(10))
df2 <- data.frame(id = sample(1:10), y = rnorm(10))
arrange(join(df1,df2), id)

df1 <- data.frame(id = sample(1:10), x = rnorm(10))
df2 <- data.frame(id = sample(1:10), y = rnorm(10))
df3 <- data.frame(id = sample(1:10), y = rnorm(10))
dfList = list(df1, df2, df3)
join_all(dfList)

# Compute averages of all variables for all activity-subject combinations

averages <- df %>%
  gather(variable, value, -c(activity, subject)) %>%
  group_by(activity, subject, variable) %>%
  summarize(average = mean(value)) %>%
  spread(variable, average)

-- Week 4

[Editing Text Variables]

- tolower() # turn all into lower case
- toupper() # turn all into upper case

Fixing character vectors

strsplit(x, split) 
- Good for automatically splitting variable names 

splitNames = strsplit(names(cameraData), '\\.') # splits on '.' (periods) --> need to escape it with \\ since it is a reserved character
splitNames[[5]] # returns a list (not a vector)

Quick aside: lists 

# 2 named & 1 unnamed components in the list
mylist <- list(letters = c('A' 'b', 'c'), numbers = 1:3, matrix(1:25, ncol = 5))
head(mylist)

mylist[1]
#=> A b c # returns a list element
mylist$letters 
#=> A b c # returns a list element
mylist[[1]] # returns a vector
#=> A b c

sapply(X, FUN)
- Applies a function to each element in a vector list 

splitNames[[6]][1]
#=> Location 

firstElement <- function(x) {x[1]}
sapply(splitNames, firstElement)

sub(pattern, replacement, x)

names(reviews)
sub('_', '', names(reviews)) # replaces the FIRST _ with nothing ('')
testName <- "this_is_a_test"
gsub('_','',testName) # replaces ALL _ with nothing ('')

grep() & grepl() # finding values

grep('Alameda', cameraData$intersection) # returns the indices where the pattern is found
#=> 4 5 36

table(grepl('Alameda', cameraData$intersection)) # returns FALSE/TRUE (which we now tabulate)

cameraData2 <- cameraData[!grepl('Alameda, cameraData$intersection'), ]

grep('Alameda', cameraData$intersection, value = TRUE) # value = TRUE: will return both the indices and the actual values

grep('JeffStreet', cameraData$intersection)
#=> integer(0)
length(grep('JeffStreet', cameraData$intersection))
#=> 0 # when value not found vector length is equal to 0

More useful string functions

library(stringr)
nchar('Jeffrey Leek')
#=> 12
substr('Jeffrey Leek', 1, 7)
#=> Jeffrey
paste('Jeffrey', 'Leek') # defaults by space-separated, use sep = "" argument to change
#=> Jeffrey Leek
paste0('Jeffrey', 'Leek')
#=> JeffreyLeek
str_trim('Jeff    ')
#=> Jeff

Important points about text in data sets

- Names of variables should be
  + All lower case when possible
  + Descriptive (Diagnosis vs Dx)
  + Not duplicated
  + Not have underscores or dots or whitespaces
- Variables with character values 
  + Should usually be made into factor variables (depends on application)
  + Should be descriptive (use TRUE/FALSE instead of 0/1 and Male/Female versus 0/1 or M/F)

[Regular Expressions I & II] # sub, gsub, grep & grepl use these patterns

- Regular expressions can be thought of as a combination of literals and metacharacters
- To draw an analogy with natural language, think of literal text forming the words of this language, 
  and the metacharacters defining its grammar
- Regular expressions have a rich set of metacharacters

Literals
- Simplest pattern consists only of literals (e.g. 'Dog');
  a match occurs if the sequence of literals occurs anywhere in the text being tested.
- What if we only want the word 'Obama'? Or sentences that end in the word 'Clinton', or 'clinton'?
- We need a way to express:
  + Whitespace word boundaries
  + Sets of literals 
  + The beginning and end of a line 
  + Alternatives ('war' or 'peache')
    * Metacharacters to the rescue!

1 - '^' : start of the line # From: ..
2 - '$' : end of the line # end.$ 
3 - '[]' : list a set of characters that is accepted at a given point in the match # [Bb][Uu][Ss][Hh]
    + You can specify a range of letters [a-z] or [a-zA-Z]; notice that the order does not matter # Alphanumerical: [A-Za-z0-9]
    + When used at the beginning of a character class, the '^' is also a metacharacter and
      indicates matching characters NOT in the indicated class: '[^?.]' # lines ending with anything other than a . or ? --> inside lists only ^ is a special character!
4 - '.' : any character 
5 - '|' : OR operator 
    + This does not mean 'pipe' in the context of regex; instead it translates to 'or';
      we can use it to combine two expressions, the subexpressions being called the alternatives 
    + Alternatives can be regex themselves: ^[Gg]ood|[Bbad] # Starting with [Gg]ood, or anywhere [Bbad]
6 - '()' - Subexpressions are often contained in parentheses to constrain the alternatives 
    + ^([Gg]ood|[Bb]ad) # line starts with [Gg]ood or with [Bb]ad
7 - '?' : indicates that the indicated expression is optional 
    + '[Gg]eorge( [Ww]\.)? [Bb]ush' 
8 - '*' : any number 
9 - '+' : at least one 
10 - '{m,n}' : interval quantifiers
    + they let you specify the minimum and maximum number of matches of an expression 
    + '{m, n}' : means at least m but not more than n 
    + '{m}' : means exactly m matches 
    + '{m,}' : means at least m matches (up to infinity)

Parantheses
- In most implementations of regular expressions, the parantheses not only limit the scope 
  of the alternatives divided by a '|', but it can be used to 'remember' test matched by the 
  subexpression enclosed
- We refer to the matched text with '\1', '\2', etc.
- So the expression '+([a-zA-Z]+) +\1 +' will match:
  + time for bed, night night twitter!
  + blah blah blah blah

Star 
- The * is 'greedy' so it always matches the longest possible string that satisfies the regular expression.
- So: '^s(.*)s' matches:
  + sore shoulders, stupid ergonomics
  + The greediness can be turnoed off with the '?', as in: '^s(.*?)s$'
    * sore shoulders
  + After paranthesis '?' means 'optional', with repetition chars they mean 'not greedy' 

Some differences with Python: 6 : '()' & 7 : '?'

Summary
- Regular expressions are used in many different languages, not unique to R 
- Regular expressions are composed of literals and metacharacters that 
  represent sets or classes of characters/words
- Text processing via regular expressions is a very powerful way to 
  extract data from 'unfriendly' sources (not all data comes as a CSV file)
- Used with the functions grep, grepl, sub, gsub and others that involve searching
  for text strings

[Working with Dates]

d1 = date() #=> gives curent date
class(d1) #=> character
d2 = Sys.Date() 
class(d2) #=> Date

Formatting dates
- '%d' : day as number (0-31)
- '%a' : abbreviated weekday
- '%A' : unabbreviated weekday 
- '%m' : month (00-12)
- '%b' : abbreviated month 
- '%B' : unabbreviated month
- '%y' : 2 digit year 
- '%Y' : 4 digit year

format(d2, "%a %b %d")
#=> Sun Jan 12

Creating dates 
x <- c('1jan1960', '2jan1960', '31mar1960', '30jul1960')
z <- as.Date(x, '%d%b%Y')
#=> 1960-01-01 1960-01-02 1960-03-31 1960-07-30
z[1] - z[2]
#=> Time difference of -1 days
as.numeric(z[1]-z[2])
#=> -1

Converting to Julian 
weekdays(d2)
#=> Sunday
months(d2)
#=> January 
julian(d2)
#=> [1] 16082 # number of days since origin
#=> attr(,'origin')
#=> [1] "1970-01-01" # what the origin is

Lubridate
library(lubridate)
ymd('20140108') # year-month-day
mdy() # month-day-year
dmy() # day-month-year

Dealing with times
ymd_hms() # _ hours-minutes-seconds
ymd_hms('2011-08-03 10:15:03', tz = 'Pacific/Auckland') # set timezone 
?Sys.timezone # returns system time zone

x <- dmy(c('1jan2013', '2jan2013', '31mar2013', '30jul2013'))
wkday(x[1]) # lubridate: wkday(), base-R: weekday()
#=> 3
wkday(x[1], label = TRUE)
#=> Tues
#=> Levels: Sun < Mon < Tues < Wed < Thurs < Fri < Sat

