== Reproducible Research ==

-- Week 1

[What is Reproducible Research About?]

[Reproducible Research: Concepts and Ideas (part 1)]

Replication

- The ultimate standard for stengthening scientific evidence is replication of findings
  and conducting studies with independent:
  + Investigators
  + Data
  + Analytical methods
  + Laboratories
  + Instruments
- Replication is particularly important in studies that can impact
  broad policy or regulatory decisions

What is wrong with Replication?
- Some studies cannot be replicated
  + No time, opportunistic
  + No money
  + Unique (e.g. event in time)
- Reproducible Research: Make analytic data and code available so
  that others may reproduce findings

--> even though you cannot replicate (gold standard), at least you can reproduce (middle standard), nothing would be worse (floor)

Why Do We Need Reproducible Research?
- New technologies increasing data collection throughput; data are more complex and extremely high dimensional
- Existing databases can be merged into new “megadatabases”
- Computing power is greatly increased, allowing more sophisticated analyses
- For every field “X” there is a field “Computational X”

Example: Reproducible Air Pollution and Health Research
- Estimating small (but important) health effects in the presence of much stronger signals
- Results inform substantial policy decisions, affect many stakeholders
  + EPA regulations can cost billions of dollars
- Complex statistical methods are needed and subjected to intense scrutiny

[Reproducible Research: Concepts and Ideas (part 2)]

The IOM Report

In the Discovery/Test Validation stage of omics-based tests:
- Data/metadata used to develop test should be made publicly available
- The computer code and fully specified computational procedures used for development of the candidate omics-based test should be made sustainably available
- “Ideally, the computer code that is released will encompass all of the steps of computational analysis, including all data preprocessing steps, that have been described in this chapter. All aspects of the analysis need to be transparently reported.”

What do We Need?
- Analytic data are available
- Analytic code are available
- Documentation of code and data
- Standard means of distribution

Who are the Players?
- Authors
  + Want to make their research reproducible
  + Want tools for RR to make their lives easier (or at least not much harder)
- Readers
  + Want to reproduce (and perhaps expand upon) interesting findings
  + Want tools for RR to make their lives easier

Challenges:
- Authors must undertake considerable effort to put data/results on the web (may not have resources like a web server)
- Readers must download data/results individually and piece together which data go with which code sections, etc.
- Readers may not have the same resources as authors
- Few tools to help authors/readers (although toolbox is growing!)

In Reality..

- Authors
  + Just put stuff on the web
  + (Infamous) Journal supplementary materials
  + There are some central databases for various fields (e.g. biology, ICPSR)
- Readers
  + Just download the data and (try to) figure it out
  + Piece together the software and run it

[Reproducible Research: Concepts and Ideas (part 3)]

Literate (Statistical) Programming
- An article is a stream of text and code
- Analysis code is divided into text and code “chunks”
- Each code chunk loads data and computes results
- Presentation code formats results (tables, figures, etc.)
- Article text explains what is going on
- Literate programs can be weaved to produce human-readable documents and tangled to produce machine-readable documents

- Literate programming is a general concept that requires
  i. A documentation language (human readable)
  ii. A programming language (machine readable)
- Sweave uses LATEX and R as the documentation and programming languages # Pronounciation: S-weave
- Sweave was developed by Friedrich Leisch (member of the R Core) and is maintained by R core
- Main web site: http://www.statistik.lmu.de/~leisch/Sweave

Sweave Limitations
- Sweave has many limitations
- Focused primarily on LaTeX, a difficult to learn markup language used only by weirdos
- Lacks features like caching, multiple plots per chunk, mixing programming languages and many other technical items
- Not frequently updated or very actively developed

Knitr
- knitr is an alternative (more recent) package
- Brings together many features added on to Sweave to address limitations
- knitr uses R as the programming language (although others are allowed) and variety of documentation languages
  + LaTeX, Markdown, HTML
- knitr was developed by Yihui Xie (while a graduate student in statistics at Iowa State)
- See http://yihui.name/knitr/

Summary
- Reproducible research is important as a minimum standard, particularly for studies that are difficult to replicate
- Infrastructure is needed for creating and distributing reproducible documents, beyond what is currently available
- There is a growing number of tools for creating reproducible documents

[Scripting Your Analysis]

Golden Rule of Reproducibility: Script Everything

[Structure of a Data Analysis (part 1)]

Steps in a data analysis:
# Part 1:
- Define the question
- Define the ideal data set
- Determine what data you can access
- Obtain the data
- Clean the data
# Part 2:
- Exploratory data analysis
- Statistical prediction/modeling
- Interpret results
- Challenge results
- Synthesize/write up results
- Create reproducible code

The key challenge in data analysis:
"Ask yourselves, what problem have you solved, ever, that was worth solving,
where you knew all of the given information in advance? Where you didn’t have
a surplus of information and have to filter it out, or you had insufficient
information and have to go find some?"

Defining a question

# An example
- Start with a general question:
  + Can I automatically detect emails that are SPAM that are not?
- Make it concrete:
  + Can I use quantitative characteristics of the emails to classify them as SPAM/HAM?

Define the ideal data set
- The data set may depend on your goal
  + Descriptive - a whole population
  + Exploratory - a random sample with many variables measured
  + Inferential - the right population, randomly sampled
  + Predictive - a training and test data set from the same population
  + Causal - data from a randomized study
  + Mechanistic - data about all components of the system

Determine what data you can access
- Sometimes you can find data free on the web
- Other times you may need to buy the data
- Be sure to respect the terms of use
- If the data do not exist, you may need to generate it yourself

Obtain the data
- Try to obtain the raw data
- Be sure to reference the source
- Polite emails go a long way
- If you will load the data from an internet source, record the url and time accessed

Clean the data
- Raw data often needs to be processed
- If it is pre-processed, make sure you understand how
- Understand the source of the data (census, sample, convenience sample, etc.)
- May need reformating, subsampling - record these steps
- Determine if the data are good enough - if not, quit or change data

Our cleaned data set:
# If it isn't installed, install the kernlab package with install.packages()
library(kernlab)
data(spam)
str(spam[, 1:5])

[Structure of a Data Analysis (part 2)]

- Subsampling our data set
  + We need to generate a test and training set (prediction)

# If it isn't installed, install the kernlab package
library(kernlab)
data(spam)
# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601,size=1,prob=0.5)
table(trainIndicator)
trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]

Exploratory data analysis
- Look at summaries of the data
- Check for missing data
- Create exploratory plots
- Perform exploratory analyses (e.g. clustering)

# Names
names(trainSpam)

# Head
head(trainSpam)

# Summaries
table(trainSpam$type)

# Plots
plot(trainSpam$capitalAve ~ trainSpam$type) # very skewed distribution, hard to see
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type) # therefore we take the log10 (base 10), however many 0's so we add 1 (+ 1) --> taking log of zero does not make sense

# Relationships between predictors
plot(log10(trainSpam[,1:4]+1))

# Clustering
par(mar=c(0,0,0,0))
hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

# New clustering
hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

Statistical prediction/modeling
- Should be informed by the results of your exploratory analysis
- Exact methods depend on the question of interest
- Transformations/processing should be accounted for when necessary
- Measures of uncertainty should be reported

trainSpam$numType = as.numeric(trainSpam$type)-1
costFunction = function(x,y) sum(x!=(y > 0.5))
cvError = rep(NA,55)
library(boot)
for(i in 1:55){
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula,family="binomial",data=trainSpam)
  cvError[i] = cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

# Get a measure of uncertainty

## Use the best model from the group
predictionModel = glm(numType ~ charDollar,family="binomial",data=trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel,testSpam)
predictedSpam = rep("nonspam",dim(testSpam)[1])

## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification table
table(predictedSpam,testSpam$type)

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)

Interpret results
- Use the appropriate language
  + describes
  + correlates with/associated with
  + leads to/causes
  + predicts
- Give an explanation
- Interpret coefficients
- Interpret measures of uncertainty

Our example
- The fraction of characters that are dollar signs can be used to predict if an email is Spam
- Anything with more than 6.6% dollar signs is classified as Spam
- More dollar signs always means more Spam under our prediction
- Our test set error rate was 22.4%

Challenge results
- Challenge all steps:
  + Question
  + Data source
  + Processing
  + Analysis
  + Conclusions
- Challenge measures of uncertainty
- Challenge choices of terms to include in models
- Think of potential alternative analyses

Synthesize/write-up results
- Lead with the question
- Summarize the analyses into the story
-  Do not include every analysis, include it:
   + If it is needed for the story
   + If it is needed to address a challenge
- Order analyses according to the story, rather than chronologically
- Include "pretty" figures that contribute to the story

In our example
- Lead with the question
  + Can I use quantitative characteristics of the emails to classify them as SPAM/HAM?
- Describe the approach
  + Collected data from UCI -> created training/test sets
  + Explored relationships
  + Choose logistic model on training set by cross validation
  + Applied to test, 78% test set accuracy
- Interpret results
  + Number of dollar signs seems reasonable, e.g. "Make money with Viagra \$ \$ \$ \$!"
- Challenge results
  +  78% isnot that great
  + I could use more variables
  + Why logistic regression?

[Organizing Your Analysis]

Data analysis files
- Data
  + Raw data
  + Processed data
- Figures
  + Exploratory figures
  + Final figures
- R code
  + Raw / unused scripts
  + Final scripts
  + R Markdown files
- Text
  + README files
  + Text of analysis / report

Raw Data
- Should be stored in your analysis folder
- If accessed from the web, include url, description, and date accessed in README

Processed data
- Processed data should be named so it is easy to see which script generated the data.
- The processing script - processed data mapping should occur in the README
- Processed data should be tidy

Exploratory figures
- Figures made during the course of your analysis, not necessarily part of your final report.
- They do not need to be "pretty"

Final Figures
- Usually a small subset of the original figures
- Axes/colors set to make the figure clear
- Possibly multiple panels

Raw scripts
- May be less commented (but comments help you!)
- May be multiple versions
- May include analyses that are later discarded

Final scripts
- Clearly commented
  + Small comments liberally - what, when, why, how
  + Bigger commented blocks for whole sections
- Include processing details
- Only analyses that appear in the final write-up

R markdown files
- R markdown files can be used to generate reproducible reports
- Text and R code are integrated
- Very easy to create in Rstudio

Readme files
- Not necessary if you use R markdown
- Should contain step-by-step instructions for analysis
- Here is an example https://github.com/jtleek/swfdr/blob/master/README

Text of the document
- It should include a title, introduction (motivation), methods (statistics you used), results (including measures of uncertainty), and conclusions (including potential problems)
- It should tell a story
- It should not include every analysis you performed
- References should be included for statistical methods

Further resources
- Information about a non-reproducible study that led to cancer patients being mistreated: The Duke Saga Starter Set
- Reproducible research and Biostatistics
- Managing a statistical analysis project guidelines and best practices
- Project template - a pre-organized set of files for data analysis

-- Week 2

[Coding Standards in R]

1 - Always use text files / text editors
2 - Indent your code
3 - Limit the width of your code (80 columns?)
4 - Limit the length of individual functions

Indenting
- Indenting improves readability
- Fixing line length (80 columns) prevents lots of nesting and very long functions
- Suggested: Indents of 4 spaces at minimum, 8 spaces ideal

[Markdown]

What is Markdown?

"Markdown is a text-to-HTML conversion tool for web writers. Markdown allows you to
write using an easy-to-read, easy-to-write plain text format, then convert it to
structurally valid XHTML (or HTML)."

- John Gruber, creator of Markdown

Markdown Syntax
- Italics: *This text will appear italicized!*
- Bold: **This text will appear bold!**
- Headings:
  + ## This is a secondary heading
  + ### This is a tertiary heading
- Unordered Lists:

- first item in list
- second item in list
- third item in list

- Ordered Lists

1. first item in list
2. second item in list
3. third item in list

- Links:

[Johns Hopkins Bloomberg School of Public Health](http://www.jhsph.edu/)
[Download R](http://www.r-project.org/)
[RStudio](http://www.rstudio.com/)

- Advanced Linking:

I spend so much time reading [R bloggers][1] and [Simply Statistics][2]!
[1]: http://www.r-bloggers.com/   "R bloggers"
[2]: http://simplystatistics.org/ "Simply Statistics"

- Newlines: Newlines require a double space after the end of a line.

Markdown Resources:
- The Offical Markdown Documentation: http://daringfireball.net/projects/markdown/basics
- Githubs Markdown Guide: https://help.github.com/categories/writing-on-github/

[R Markdown]

What is Markdown?
- Created by John Gruber and Aaron Swartz
- A simplified version of "markup" languages
- Allows one to focus on writing as opposed to formatting
- Simple/minimal intuitive formatting elements
- Easily converted to valid HTML (and other formats) using existing tools
- Complete information is available at http://daringfireball.net/projects/markdown/
- Some background information at http://daringfireball.net/2004/03/dive_into_markdown

What is R Markdown?
- R markdown is the integration of R code with markdown
- Allows one to create documents containing "live" R code
- R code is evaluated as part of the processing of the markdown
- Results from R code are inserted into markdown document
- A core tool in literate statistical programming
- R markdown can be converted to standard markdown using the knitr package in R
- Markdown can be converted to HTML using the markdown package in R
- Any basic text editor can be used to create a markdown document; no special editing tools needed
- The R markdown --> markdown --> HTML work flow can be easily managed using R Studio (but not required)
- These slides were written in R markdown and converted to slides using the slidify package

[knitr (part 1)]

Problems, Problems
- Authors must undertake considerable effort to put data/results on the web
- Readers must download data/results individually and piece together
  which data go with which code sections, etc.
- Authors/readers must manually interact with websites
- There is no single document to integrate data analysis with textual
  representations; i.e. data, code, and text are not linked

Literate Statistical Programming
- Original idea comes from Don Knuth
- An article is a stream of text and code
- Analysis code is divided into text and code “chunks”
- Presentation code formats results (tables, figures, etc.)
- Article text explains what is going on
- Literate programs are weaved to produce human-readable documents
  and tangled to produce machine-readable documents

- Literate programming is a general concept. We need
  + A documentation language
  + A programming language
- The original Sweave system developed by Friedrich Leisch used LaTeX and R
- knitr supports a variety of documentation languages

How Do I Make My Work Reproducible?
- Decide to do it (ideally from the start)
- Keep track of things, perhaps with a version control system to track
  snapshots/changes
- Use software whose operation can be coded
- Don’t save output # keep raw file + preprocessing code
- Save data in non-proprietary formats

Literate Programming: Pros
- Text and code all in one place, logical order
- Data, results automatically updated to reflect external changes
- Code is live--automatic “regression test” when building a document

Literate Programming: Cons
- Text and code all in one place; can make documents difficult to read,
  especially if there is a lot of code
- Can substantially slow down processing of documents (although there
  are tools to help)

[knitr (part 2)]

What is knitr?
- An R package written by Yihui Xie (while he was a grad student at Iowa State)
  + Available on CRAN
- Supports RMarkdown, LaTeX, and HTML as documentation languages
- Can export to PDF, HTML
- Built right into RStudio for your convenience

Requirements
- A recent version of R
- A text editor (the one that comes with RStudio is okay)
- Some support packages also available on CRAN
- Some knowledge of Markdown, LaTeX, or HTML
- We will use Markdown here

What is Markdown?
- A simplified version of “markup” languages
- No special editor required
- Simple, intuitive formatting elements
- Complete information available at http://goo.gl/MUt9i5

What is knitr Good For?
- Manuals
- Short/medium-length technical documents
- Tutorials
- Reports (esp. if generated periodically)
- Data preprocessing documents/summaries

What is knitr NOT Good For?
- Very long research articles
- Complex time-consuming computations
- Documents that require precise formatting

[knitr (part 3)]

Using knitr outside of R-studio:

library(knitr)
setwd(<working directory>)
knit2html(“document.Rmd”)
browseURL(“document.html”)

A Few Notes
- knitr will fill a new document with filler text; delete it
- Code chunks begin with '```{r}' and end with '```'
- All R code goes in between these markers
- Code chunks can have names, which is useful when we start making graphics

```{r firstchunk}
## R code goes here
```

- By default, code in a code chunk is echoed, as will the results of the
  computation (if there are results to print)

[knitr (part 4)]

Processing of knitr Documents (what happens under the hood)
- You write the RMarkdown document (.Rmd)
- knitr produces a Markdown document (.md)
- knitr converts the Markdown document into HTML (by default)
  .Rmd -->  .md --> .html
- You should NOT edit (or save) the .md or .html documents until
  you are finished # gets overwritten etc.

{r simulation, echo = FALSE} # do not echo the code, only show results
{r simulation, echo = FALSE, results = 'hide'} # hide the results as well

Inline Text Computations

# My First knitr Document
## Introduction

```{r computetime, echo = FALSE}
time <- format(Sys.time(), '%a %b %d %X %Y')
rand <- rnorm(1)
```

The current time is `r time`. My favorite random number is `r rand`.

```{r scatterplot, fig.height=4} # adjust figure height
x <- rnorm(100); y <- x + rnorm(100, sd = 0.5)
par(mar = c(5, 4, 1, 1), las = 1)
plot(x, y, main = "My simulated data")
```

```{r showtable, results='asis'}
library(xtable)
xt <- xtable(summary(fit))
print(xt, type = 'html')
```

Setting Global Options
- Sometimes we want to set options for every code chunk that are different from the defaults
- For example, we may want to suppress all code echoing and results output
- We have to write some code to set these global options

```{r setoptions, echo = FALSE}
opts_chunk$set(echo = FALSE, results = 'hide') # by default all code chunks are not echoed and results are hidden
```

You can now override the default by for instance stating echo = TRUE.

Some Common Options
- Output
  + results: “asis”, “hide”
  + echo: TRUE, FALSE
- Figures
  + fig.height: numeric
  + fig.width: numeric

Caching Computations
- What if one chunk takes a long time to run?
- All chunks have to be re-computed every time you re-knit the file
- The cache=TRUE can be set on a chunk-by-chunk basis to store resultsz
  of computation
- After the first run, results are loaded from cache # as long as nothing is changed!

Caching Caveats
- If the data or code (or anything external) changes, you need to
  re-run the cached code chunks
- Dependencies are not checked explicitly
- Chunks with significant side effects may not be cacheable

Summary
- Literate statistical programming can be a useful way to put text,
  code, data, output all in one document
- knitr is a powerful tool for integrating code and text in a simple
  document format

-- Week 3

[Communicating Results]

- People are busy, especially managers and leaders
- Results of data analyses are sometimes presented in oral form, but often the first
  cut is presented via email
- It is often useful to breakdown the results of an analysis into different levels of granularity/detail
- Getting responses from busy peope: http://goo.gl/sJDb9V

Hierarchy of Information: Research Paper
1 - Title / Author list
2 - Abstract
3 - Body / Results
4 - Supplementary Materials / the gory details
5 - Code / Data / really gory details

Hierarchy of Information: Email Presentation
- Subject line / Sender info
  + At a minimum; include one
  + Can you summarize the findings in one sentence?
- Email body
  + A brief description of the problem/context; recall what was proposed and executed;
    summarize findings / results; 1-2 paragraphs.
  + If action needs to be taken as a result of this presentation, suggest some options
    and make them as concrete as possible.
  + If questions need to be addressed, try to make them yes/no.
- Attachment(s)
  + R Markdown file
  + knitr report
  + Stay concise, do not spit out pages of code (becuase you used knitr we know it is available)
- Links to Supplementary Materials
  + Code / Software / Data
  + GitHub repository / Project web site

[Reproducible Research Checklist (part 1)]

DO: Start With Good Science
- Garbage in, garbage out
- Coherent, focused question simplifies many problems
- Working with good collaborators reinforces good practices
- Something that is interesting to you will (hopefully) motivate good habits

DONT: Do Things By Hand
- Editing spreadsheets of data to "clean it up"
  + Removing outliers
  + QA / QC
  + Validating
- Editing tables or figures (e.g. rounding, formatting)
- Downloading data from a web site (clicking links in a web browser)
- Moving data around your computer; splitting / reformatting data files
- "We're just going to do this once...."

Things done by hand need to be precisely documented (this is harder than it sounds)

DONT: Point And Click
- Many data processing / statistical analysis packages have graphical user interfaces (GUIs)
- GUIs are convenient / intuitive but the actions you take with a GUI can be difficult for others to reproduce
- Some GUIs produce a log file or script which includes equivalent commands; these can be saved for later examination
- In general, be careful with data analysis software that is highly interactive; ease of use can sometimes lead to non-reproducible analyses
- Other interactive software, such as text editors, are usually fine

[Reproducible Research Checklist (part 2)]

DO: Teach a Computer
- If something needs to be done as part of your analysis / investigation, try to teach your computer to do it (even if you only need to do it once)
- In order to give your computer instructions, you need to write down exactly what you mean to do and how it should be done
- Teaching a computer almost guarantees reproducibilty
- For example, by hand, you can
  1 - Go to the UCI Machine Learning Repository at http://archive.ics.uci.edu/ml/
  2 - Download the Bike Sharing Dataset by clicking on the link to the Data Folder, then clicking on the link to the zip file of dataset, and choosing "Save Linked File As..." and then saving it to a folder on your computer
- Or You can teach your computer to do the same thing using R:

download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/
               Bike-Sharing-Dataset.zip", "ProjectData/Bike-Sharing-Dataset.zip")

- Notice here that:
  + The full URL to the dataset file is specified (no clicking through a series of links)
  + The name of the file saved to your local computer is specified
  + The directory in which the file was saved is specified ("ProjectData")
  + Code can always be executed in R (as long as link is available)

DO: Use Some Version Control
- Slow things down (which is good!)
- Add changes in small chunks (do not just do one massive commit)
- Track / tag snapshots; revert to old versions
- Software like GitHub / BitBucket / SourceForge make it easy to publish results

DO: Keep Track of Your Software Environment
- If you work on a complex project involving many tools / datasets, the software and computing environment can be critical for reproducing your analysis
- Computer architecture: CPU (Intel, AMD, ARM), GPUs,
- Operating system: Windows, Mac OS, Linux / Unix
- Software toolchain: Compilers, interpreters, command shell, programming languages (C, Perl, Python, etc.), database backends, data analysis software
- Supporting software / infrastructure: Libraries, R packages, dependencies
- External dependencies: Web sites, data repositories, remote databases, software repositories
- Version numbers: Ideally, for everything (if available)

sessionInfo() # tells you a lot about your environment

[Reproducible Research Checklist (part 3)]

DONT: Save Output
- Avoid saving data analysis output (tables, figures, summaries, processed data, etc.), except perhaps temporarily for efficiency purposes.
- If a stray output file cannot be easily connected with the means by which it was created, then it is not reproducible.
- Save the data + code that generated the output, rather than the output itself
- Intermediate files are okay as long as there is clear documentation of how they were created

DO: Set Your Seed
- Random number generators generate pseudo-random numbers based on an initial seed (usually a number or set of numbers)
  + In R you can use the set.seed() function to set the seed and to specify the random number generator to use
- Setting the seed allows for the stream of random numbers to be exactly reproducible
- Whenever you generate random numbers for a non-trivial purpose, always set the seed

DO: Think About the Entire Pipeline
- Data analysis is a lengthy process; it is not just tables / figures / reports
- Raw data → processed data → analysis → report
- How you got the end is just as important as the end itself
- The more of the data analysis pipeline you can make reproducible, the better for everyone

Summary: Checklist
- Are we doing good science?
- Was any part of this analysis done by hand?
  + If so, are those parts precisely document?
  + Does the documentation match reality?
- Have we taught a computer to do as much as possible (i.e. coded)?
- Are we using a version control system?
- Have we documented our software environment?
- Have we saved any output that we cannot reconstruct from original data + code?
- How far back in the analysis pipeline can we go before our results are no longer (automatically) reproducible?

[Evidence-Based Data Analysis (part 1)]

Replication and Reproducibility

Replication
- Focuses on the validity of the scientific claim
- "Is this claim true?"
- The ultimate standard for strengthening scientific evidence
- New investigators, data, analytical methods, laboratories, instruments, etc.
- Particularly important in studies that can impact broad policy or regulatory decisions

Reproducibility
- Focuses on the validity of the data analysis
- "Can we trust this analysis?"
- Arguably a minimum standard for any scientific study
- New investigators, same data, same methods
- Important when replication is impossible

Background and Underlying Trends
- Some studies cannot be replicated: No time, No money, Unique/opportunistic
- Technology is increasing data collection throughput; data are more complex and high-dimensional
- Existing databases can be merged to become bigger databases (but data are used off-label)
- Computing power allows more sophisticated analyses, even on "small" data
- For every field "X" there is a "Computational X" # e.g. computational biostatistics

The Result?
- Even basic analyses are difficult to describe
- Heavy computational requirements are thrust upon people without adequate training in statistics and computing
- Errors are more easily introduced into long analysis pipelines
- Knowledge transfer is inhibited # due to complexity, etc.
- Results are difficult to replicate or reproduce
- Complicated analyses cannot be trusted

[Evidence-Based Data Analysis (part 2)]

What Problem Does Reproducibility Solve?

What we get
- Transparency
- Data Availability
- Software / Methods Availability
- Improved Transfer of Knowledge

What we do NOT get
- Validity / Correctness of the analysis

# An analysis can be reproducible and still be wrong
# We want to know “can we trust this analysis?”
# Does requiring reproducibility deter bad analysis?

Problems with Reproducibility

The premise of reproducible research is that with data/code available, people can check each other and the whole system is self-correcting
- Addresses the most “downstream” aspect of the research process – post-publication
- Assumes everyone plays by the same rules and wants to achieve the same goals (i.e. scientific discovery)

[Evidence-Based Data Analysis (part 4)]

Who Reproduces Research?
- For reproducibility to be effective as a means to check validity, someone needs to do something
  + Re-run the analysis; check results match
  + Check the code for bugs/errors
  + Try alternate approaches; check sensitivity
  + The need for someone to do something is inherited from traditional notion of replication
- Who is "someone" and what are their goals?

The Story So Far
- Reproducibility brings transparency (wrt code+data) and increased transfer of knowledge
- A lot of discussion about how to get people to share data
- Key question of "can we trust this analysis?" is not addressed by reproducibility
- Reproducibility addresses potential problems long after they’ve occurred ("downstream")
- Secondary analyses are inevitably coloured by the interests/motivations of others

Evidence-based Data Analysis
- Most data analyses involve stringing together many different tools and methods
- Some methods may be standard for a given field, but others are often applied ad hoc
- We should apply thoroughly studied (via statistical research), mutually agreed upon methods to analyze data whenever possible
- There should be evidence to justify the application of a given method

[Evidence-Based Data Analysis (part 5)]

Evidence-based Data Analysis
- Create analytic pipelines from evidence-based components – standardize it
- A Deterministic Statistical Machine http://goo.gl/Qvlhuv
- Once an evidence-based analytic pipeline is established, we shouldn’t mess with it
- Analysis with a “transparent box”
- Reduce the "researcher degrees of freedom"
- Analogous to a pre-specified clinical trial protocol

Case Study: Estimating Acute Effects of Ambient Air Pollution Exposure
- Acute/short-term effects typically estimated via panel studies or time series studies
- Work originated in late 1970s early 1980s
- Key question: "Are short-term changes in pollution associated with short-term changes in a population health outcome?"
- Studies usually conducted at community level
- Long history of statistical research investigating proper methods of analysis
- Can we encode everything that we have found in statistical/epidemiological research into a single package?
- Time series studies do not have a huge range of variation; typically involves similar types of data and similar questions
- We can create a deterministic statistical machine for this area?

DSM Modules for Time Series Studies of Air Pollution and Health # Deterministic Statistical Machine
1 - Check for outliers, high leverage, overdispersion
2 - Fill in missing data? NO!
3 - Model selection: Estimate degrees of freedom to adjust for unmeasured confounders
    + Other aspects of model not as critical
4 - Multiple lag analysis
5 - Sensitivity analysis wrt
    + Unmeasured confounder adjustment
    + Influential points

Where to Go From Here?
- One DSM is not enough, we need many!
- Different problems warrant different approaches and expertise
- A curated library of machines providing state-of-the art analysis pipelines
- A CRAN/CPAN/CTAN/… for data analysis
- Or a “Cochrane Collaboration” for data analysis

A Curated Library of Data Analysis
- Provide packages that encode data analysis pipelines for given problems, technologies, questions
- Curated by experts knowledgeable in the field
- Documentation/references given supporting each module in the pipeline
- Changes introduced after passing relevant benchmarks/unit tests

Summary
- Reproducible research is important, but does not necessarily solve the critical question of whether a data analysis is trustworthy
- Reproducible research focuses on the most "downstream" aspect of research dissemination
- Evidence-based data analysis would provide standardized, best practices for given scientific areas and questions
- Gives reviewers an important tool without dramatically increasing the burden on them
- More effort should be put into improving the quality of "upstream" aspects of scientific research

-- Week 4

[Caching Computations]

The cacher package for R
- Add-on package for R
- Evaluates code written in files and stores intermediate results in a key-value database
- R expressions are given SHA-1 hash values so that changes can be tracked and code reevaluated if necessary
- “Cacher packages” can be built for distribution
- Others can “clone” an analysis and evaluate subsets of code or inspect data objects

Using cacher as an Author
1 - Parse the R source ﬁle; Create the necessary cache directories and subdirectories
2 - Cycle through each expression in the source ﬁle:
    + If an expression has never been evaluated, evaluate it and store any resulting R objects in the cache database,
    + If a cached result exists, lazy-load the results from the cache database and move to the next expression,
    + If an expression does not create any R objects (i.e., there is nothing to cache), add the expression to the list of expressions where evaluation needs to be forced
    + Write out metadata for this expression to the metadata ﬁle.

- The cachepackage function creates a cacher package storing
  + Source file
  + Cached data objects
  + Metadata
- Package file is zipped and can be distributed
- Readers can unzip the file and immediately investigate its contents via cacher package

library(datasets)
library(stats)

## Load the dataset
data(airquality)

## Fit a linear model
fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
summary(fit)

## Plot some diagnostics
par(mfrow = c(2, 2))
plot(fit)

> library(cacher)
> clonecache(id = "092dcc7dda4b93e42f23e038a60e1d44dbec7b3f")
> clonecache(id = “092d”)  ## Same as above
created cache directory '.cache'

> showfiles()
[1] "top20.R"
> sourcefile("top20.R")

Cloning an Analysis
- Local directories created
- Source code files and metadata are downloaded
- Data objects are not downloaded by default
- References to data objects are loaded and corresponding data can be lazy-loaded on demand

Examining Code

# Just some simply example analysis:

> code()
source file: top20.R
1  cities <- readLines("citylist.txt")
2  classes <- readLines("colClasses.txt")
3  vars <- c("date", "dow", "death",
4  data <- lapply(cities, function(city) {
5  names(data) <- cities
6  estimates <- sapply(data, function(city) {
7  effect <- weighted.mean(estimates[1,])
8  stderr <- sqrt(1/sum(1/estimates[2,])

> graphcode()

Tracing Code Backwards

> objectcode(“data”)
source file: top20.R
1  cities <- readLines("citylist.txt")
2  classes <- readLines("colClasses.txt")
3  vars <- c("date", "dow", "death", "tmpd", "rmtmpd", "dptp",
             "rmdptp", "l1pm10tmean")
4  data <- lapply(cities, function(city) {
           filename <- file.path("data", paste(city, "csv",
                                 sep = "."))
           d0 <- read.csv(filename, colClasses = classes,
                          nrow = 5200)
           d0[, vars]
   })
5  names(data) <- cities

Running Code
- The runcode function executes code in the source file
- By default, expressions that results in an object being created are not run and the resulting objects is lazy-loaded into the workspace
- Expressions not resulting in objects are evaluated # when it cannot be cached, it is N/A

Checking Code and Objects
- The checkcode function evaluates all expressions from scratch (no lazy-loading)
- Results of evaluation are checked against stored results to see if the results are the same as what the author calculated
  + Setting RNG seeds is critical for this to work # RNG - Random Number Generator
- The integrity of data objects can be verified with the checkobjects function to check for possible corruption of data (i.e. in transit)

Inspecting Data Objects

> loadcache()

> ls()
[1] "cities"    "classes"   "data"      "effect"
[5] "estimates" "stderr"    "vars"

> cities
/ transferring cache db file b8fd490bcf1d48cd06...
 [1] "la"   "ny"   "chic" "dlft" "hous" "phoe"
 [7] "staa" "sand" "miam" "det"  "seat" "sanb"
[13] "sanj" "minn" "rive" "phil" "atla" "oakl"
[19] "denv" "clev"

cacher Summary
- The cacher package can be used by authors to create cache packages from data analyses for distribution
- Readers can use the cacher package to inspect others’ data analyses by examining cached computations
- cacher is mindful of readers’ resources and efficiently loads only those data objects that are needed

[Case Study: Air Pollution]
- What Causes PM to be Toxic?
- PM is composed of many different chemical elements
- Some components of PM may be more harmful than others
- Some sources of PM may be more dangerous than others
- Identifying harmful chemical constituents may lead us to strategies for controlling sources of PM

NMMAPS
- The National Morbidity, Mortality, and Air Pollution Study (NMMAPS) was a national study of the short-term health effects of ambient air pollution
- Focused primarily on particulate matter ($PM_{10}$) and ozone ($O_3$)
- Health outcomes included mortality from all causes and hospitalizations for cardiovascular and respiratory diseases
- Key publications
  + http://www.ncbi.nlm.nih.gov/pubmed/11098531
  + http://www.ncbi.nlm.nih.gov/pubmed/11354823
- Funded by the Health Effects Institute
  + Roger Peng currently serves on the Health Effects Institute Health Review Committee

NMMAPS and Reproducibility
- Data made available at the Internet-based Health and Air Pollution Surveillance System (http://www.ihapss.jhsph.edu)
- Research results and software also available at iHAPSS
- Many studies (over 67 published) have been conducted based on the public data http://www.ncbi.nlm.nih.gov/pubmed/22475833
- Has served as an important test bed for methodological development

What Causes Particulate Matter to be Toxic?
- Lippmann et al. found strong evidence that Ni modified the short-term effect of $PM_{10}$ across 60 US communities
- No other PM chemical constituent seemed to have the same modifying effect
- To simple to be true?

# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1665439/

A Reanalysis of the Lippmann et al. Study
- Reexamine the data from NMMAPS and link with PM chemical constituent data
- Are the findings sensitive to levels of Nickel in New York City?

# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2137127/

What Have We Learned?
- New York does have very high levels of nickel and vanadium, much higher than any other US community
- There is evidence of a positive relationship between Ni concentrations and $PM_{10}$ risk
- The strength of this relationship is highly sensitive to the observations from New York City
- Most of the information in the data is derived from just 3 observations

Lessons Learned
- Reproducibility of NMMAPS allowed for a secondary analysis (and linking with PM chemical constituent data) investigating a novel hypothesis (Lippmann et al.)
- Reproducibility also allowed for a critique of that new analysis and some additional new analysis (Dominici et al.)
- Original hypothesis not necessarily invalidated, but evidence not as strong as originally suggested (more work should be done)
- Reproducibility allows for the scientific discussion to occur in a timely and informed manner
- This is how science works