[The Data Scientists Toolbox]

-- Week 1

1 - Define the question
2 - Define the ideal data set
3 - Determine what data you can access
4 - Obtain the data
5 - Clean the data
6 - Exploratory data analysis
7 - Statistical prediction/modeling
8 - Interpret results
9 - Challenge results
10 - Synthesize/write up results
11 - Create reproducible code
12 - Distribute results to other people

help.search("rnorm") # search the help files
args("rnorm") # get the arguments
rnorm # see the code behind the function

Key characteristics of hackers
- Willing to find answers on their own
- Knowledgeable about where to find answers on their own
- Unintimidated by new data types or packages
- Unafraid to say they do not know the answer
- Polite but relentless

Data Analysis synonyms per data type:
- Biostatistics: medical data
- Data Science: for data from web analytics
- Machine Learning: for data in computer science/computer vision
- Natural Language Processing: for data from texts
- Signal processing: for data from electrical signals
- Business analytics: for data on customers
- Econometrics: for economic data
- Statistical process control: for data about industrial processes

Reading Lines of a Text File 
- readLines can be useful for reading in lines of webpages

[R Programming]

## This might take time
con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)
head(x)
[1] "<!DOCTYPE HTML PUBLIC\ ..." # etc.

& and && indicate logical AND and | and || indicate logical OR. 
The shorter form performs elementwise comparisons in much the same 
way as arithmetic operators. The longer form evaluates left to right 
examining only the first element of each vector. Evaluation proceeds 
only until the result is determined. The longer form is appropriate 
for programming control-flow and typically preferred in if clauses.

c(1, 3, 3) > c(1, 1, 1) || c(1, 1, 1) > c(1, 1, 1) 
#=> FALSE
c(1, 3, 3) > c(1, 1, 1) | c(1, 1, 1) > c(1, 1, 1) 
#=> FALSE TRUE TRUE

[Getting and Cleaning Data]

ucscDb <- dbConnect(MySQL(), user = "genome", host = "genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;")
dbDisconnect(ucscDb)
result

Raw versus Processed Data

Raw Data
- The original source of the data
- Often hard to use for data analyses
- Data analysis includes processing
- Raw data may only need to be processed once

Processed data
- Data that is ready for analysis
- Processing can include merging, subsetting, transorming, etc.
- There may be standards for processing
- All steps should be recorded

[Statistical Inference]

Example bootstrap code

B <- 1000
n <- length(gmVol)
resamples <- matrix(sample(gmVol,
							n * B,
							replace = TRUE),
							B, n)
medians <- apply(resamples, 1, median)
sd(medians)
[1] 3.148706
quantile(medians, c(0.025, 0.975))
#=> outputs the 95% conf interval

-- Week 2

[Command Line Interface]

The CLI can help you:
- Navigate folders
- Create files, folders, and programs
- Edit files, folders, and programs
- Run computer programs

Special Directories: 

1 - root
- The directory at the top of the tree is called the root directory
- The root directory contains all other directories
- The name of this directory is represented by a slash: / 

2 - home
- Your home directory is represented by a tilde: ~
- Your home directory usually contains most of your personal files, pictures, music, etc.
- The name of your home directory is usually the name you use to log into your computer.

CLI Commands
- CLI commands follow this recipe: command flags arguments
- command: the CLI command which does a specific task
- flags: options we give to the command to trigger certain behaviors, preceded by a -
- arguments: can be what the command is going to modify, or other options for the command
- Depending on the command, there can be zero or more flags and arguments
- For example, pwd is a command that requires no flags or arguments

- 'clear' --> will clear out the commands in your current CLI window
- 'ls' --> lists files and folders in the current directory
- 'ls -a' --> lists hidden and unhidden files and folders
- 'la -al' --> lists details for hidden and unhidden files and folders
  + Notice that '-a' and '-l' are flags, which can be combined into '-al'

- 'cd ..' --> go one directory above
- 'cd' --> without arguments, goes to home directory (~)
- 'mkdir' --> make directory, takes as argument the name of the directory you are creating
- 'touch test_file' --> creates an empty file (test_file)
- 'cp' --> stands for copy, takes as first argument a file, the second argument to path where it should be placed
- cp can also be used for copying the contents of directories, but you must use the -r flag (recursive)
- the line: 'cp -r Documents More_docs' copies the contents of Documents into More_docs
- 'rm test_file' --> removes test_file
- You can use rm to delete entire directories and their contents by using the -r flag (recursive)
  + Be careful, no way to undo this
- 'mv' --> move files to other directories, takes a file and a path as arguments
- You can also use mv to rename files, simply move new_file to renamed_file 
- 'echo' will print whatever you provide
- 'date' will print todays date

[Git]

What is Version Control?

"Version control is a system that records changes to a file or set of files over time
so that you can recall specific versions later."

- Many of us constantly create something, save it, change it, then save it again
- Version (or revision) control is a means of managing this process in a relaible and efficient way
- Especially important when collaborating with others

What is Git?

"Git is a free and open source distributed version control system designed to
handle everything from small to very large projects with speed and efficiency."

- Created by the same people who developed Linux
- The most popular implementation of version control today
- Everything is stored in local repositories on your computer
- Operated from the CL

Configure Username and Email

- Each commit to a Git repository will be 'tagged' with the username of the person
  who made the commit
- Enter the following commands in Git Bash, one at a time, to set your username and email:

git config --global user.name "Your Name Here"
git config --global user.email "your_email@example.com"
git config --list # to confirm changes

- You can always change these with the same commands

[Introduction to GitHub]

"GitHub is a web-based hosting service 
for software development that uses the Git revision control system."

1 - Create a new repository on GitHub (DataScience)
2 - Create directory with same name in home directory: 'mkdir ~/DataScience'
3 - Go into this directory 'cd ~/DataScience'
4 - Initialize a local Git repository in this directory: 'git init'
5 - Point your local repository at the remote repository you just created on GitHub: 'git remote add origin https://github.com/LoekL/DataScience.git'

Another option is to fork a repository of someone else, which makes a copy of the repository as it is right then and there
and puts this in your GitHub account.
- You then need to make a local copy of the repo on your computer
  + This is called 'cloning'
  + 'git clone https://github.com/youUserNameHere/repNameHere.git'
    * This will clone the repository into your current directory

[Basic Git Commands]

Adding
- Suppose you add new files to a local repository under version control
- You need to let Git know that they need to be tracked
  + 'git add .' --> adds all new files
  + 'git add -u' --> updates tracking for files that changed names or were deleted
  + 'git add -A' --> does both of the previous
- You should do this before committing

Committing
- You have changes you want to commit to be saved as an intermediate version
- You type the command: 'git commit -m "message"'
  + This only updates your local repo, not the remote repo on GitHub

Pushing
- You have saved local commits you would like to update on the remote (GitHub)
  + Type: 'git push'

Branches
- Sometimes you are working on a project with a version being used by many people
- You may not want to edit that version
- So you can create a branch with the command:
  + 'git checkout -b branch_name'
- To see what branch you are on type: 'git branch'
- To switch back to the master branch type: 'git checkout master'

Pull requests
- If you fork someones repo or have multiple branches you will both be working separately
- Sometimes you want to merge in your changes into the other branch/repo
- To do so you need to send a pull request
- This is a feature of GitHub

[Basic Markdown]

Markdown Syntax (.md)

Headings
## This is a secondary heading
### This is a tertiary heading

Unordered lists
* first item in list
* second item in list
* third item in list

[Installing R Packages]

a <- available.packages()
head(rownames(a), 3) # show the names of the first few packages

You can install multiple packages at once: 

install.packages(c("slidify", "ggpot2", "devtools"))

Different way:

- To get the basic installer and basic set of R packages (warning, will install multiple packages):
source("http://bioconductor.org/biocLite.R")
biocLite()

- Place the names of the R packages in a character vector:
biocLite(c("GenomicFeatures", "AnnotationDbi"))

After loading a package, the functions exported by that package will be attached to the top of the search() list (after the workspace)

libary(ggplot2)
search()

[Types of Data Science Questions]

In approximate order of difficulty:

- Descriptive 
  + Goal: Describe a set of data
    * The first kind of data analysis performed
    * Commonly applied to census data
    * The description and interpretation are different steps
    * Descriptions can usually not be generalized without any statistical modelling

- Exploratory
  + Goal: Find relationships you did not know about
    * Exploratory models are good for discovering new connections
    * They are also useful for defining future studies
    * Exploratory analyses are usually not the final say 
    * Exploratory analyses alone should not be used for generalizing/predicting
    * Correlation does not imply causation

- Inferential
  + Goal: use a relatively small sample of data to say something about a bigger population
    * Inference is commonly the goal of statistical models
    * Inference involves estimating both the quantity you care about and you uncertainty about your estimate
    * Inference depends heavily on both the population and the sampling scheme

- Predictive
  + Goal: To use the data on some objects to predict values for another object
    * If X predicts Y it does not mean that X causes Y
    * Accurate prediction depends heavily on measuring the right variables
    * Although there are better and worse prediction models, more data and a simple model work really well
    * Prediction is very hard, especially about the future

- Causal
  + Goal: To find out what happens to one variable when you make another variable change
    * Usually randomized studies are required to identify causation
    * There are approaches to inferring causation in non-randomized studies, but they are complicated and sensitive to assumptions
    * Causal relationships are usually identified as average effects, but may not apply to every individual
    * Causal models are usually the "gold standard" for data analysis

- Mechanistic
  + Goal: Understand the exact changes in variables that lead to changes in other variables for individual objects
    * Incredibly hard to infer, except in simple situations
    * Usually modeled by a deterministic set of equations (physical/engineering science)
    * Generally the random component of the data is measurement error
    * If the equations are known but the parameters are not, they may be inferred with data analysis

[What is data?]

"Data are values of qualitative or quantitative variables, belonging to a set of items."
# Set of items: population, the set of objects you're interested in
# Variables: A measurement or characteristic of an item
# Qualitative: country of origin, sex, treatment
# Quantitative: height, weight, blood pressure

The data is the second most important thing
- The most important thing in data science is the question
- The second most important is the data
- Often the data will limit or enable the questions
- But having data can not save you if you do not have a question

[Experimental Design]

Confounding Variable --> variable which actually explains relationship (age; when looking at literacy & feet size)
# Randomization helps with mitigating confounding variables, since in both treatment groups observations should have both low & high values of confounding variables

Prediction Key Quantities

- Sensitivity # Pr(positive test | disease)
- Specificity # Pr(negative test | no disease)
- Positive Predictive Value # Pr(disease | positive test)
- Negative Predictive Value # Pr(no disease | negative test)
- Accuracy # Pr(correct outcome)

Summary
- Good experiments:
  + Have replication
  + Measure variability # variability within groups has to be small!
  + Generalize to the problem you care about
  + Are transparent
- Prediction is not Inference
  + Both can be important
- Beware of data dredging




















