[Exploratory Data Analysis]

## Week 1

[Principles of Analytic Graphics] - Edward Tufte

1 - Show comparisons
+ Evidence for a hypothesis is always relative to another competive hypothesis
+ Always ask "compared to what?"

2 - Show causality, mechanism, explanation, systematic structure
+ What is your causal framework for thinking about a question?

3 - Show multivariate Data
+ Multivariate = more than 2 variables
+ The real world is multivariate
+ Need to "escape flatland"

4 - Integration of Evidence
+ Completely integrate words, numbers, images, diagrams
- Data graphics should make use of many modes of data representation
- Do not let the tool drive the analysis

5 - Describe and document the evidence with appropriate labels, scales, sources, etc.
- A data graphic should tell a complete story that is credible

6 - Content is King
+ Analytical presentations ultimately stand or fall depending on the quality, relevance and
integrity of their content

[Exploratory Graphs]

Why do we use graphs in data analysis?

- To understand data properties
- To find patterns in data
- To suggest modeling strategies
- To 'debug' analyses
- To communicate results

Characteristic of Exploratory Graphs

- They are made quickly
- A large number are made
- The goal is for personal understanding
- Axes/legends are generally cleaned up (later)
- Color/size are primarily used for information

pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)

Simple Summaries of Data

## One Dimension Data

1 - Five-number summary: 	summary(pollution$pm25)
2 - Boxplots: 				boxplot(pollution$pm25, col = "blue") '+' abline(h = 12)
3 - Histograms: 			hist(pollution$pm25, col = "green", breaks = 100) '+' rug(pollution$pm25) + abline(v = median(pollution$pm25), col = "magenta", lwd = 4)
4 - Barplot:				barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")
5 - Density plot 			 

## Two Dimension Data

- Multiple/overlayed 1-D plots (Lattice/ggplot2)
- Scatterplots
- Smooth scatterplots

## > 2 Dimension

- Overlayed/multiple 2-D plots; coplots
- Use color, size, shape to add dimensions
- Spinning plots
- Actual 3-D plots (not that useful)

1 - Multiple Boxplots:	boxplot(pm25 ~ region, data = pollution, col = "red") 
2 - Multiple Histograms:
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) 
# mfrow - a vector of length 2, where the first argument specifies the number of rows and the second the number of columns of plots
# mar – A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

3 - Scatterplot
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

Multiple Scatterplots

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

Summary

- Exploratory plots are "quick and dirty"
- Let you summarize the data (usually graphically) and highly any broad features
- Explore basic questions and hypotheses (and perhaps rule them out)
- Suggest modeling strategies for the "next step"

[Plotting Systems in R]

The Base Plotting System
+ "Artist's palette" model
+ Start with a blank canvas and build up from there
+ Start with plot function (or similar)
+ Use annotation functions to add/modify (text, lines, points, axis)
+ Convenient, mirrors how we think of building plots and analyzing data
- Can not go back once plot has started (i.e. to adjust margins); need to plan in advance
- Difficult to 'translate' to others once a new plot has been created (no graphical 'language')
- Plot is just a series of R commands

library(datasets)
data(cars)
with(cars, plot(speed, dist))

The Lattice System
+ Plots are created with a single functionc all (xyplot, bwplot, etc.)
+ Most useful for conditioning types of plots: looking at how X changes with Y across levels of Z
+ Things like margins/spacing set automatically because entire plot is specified at once
+ Good for putting many plots on a screen
- Sometimes awkward to specify an entire plot in a single function call
- Annotation in plot is not especially intuitive
- Use of panel functions and subscripts difficult to wield and requires intense preparation
- Cannot "add" to the plot once it is created

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

The ggplot2 System
+ Splits the difference between base and lattice in a number of ways
+ Automatically deals with spacings, text, titles but also allows you to annotate by "adding" to a plot
+ Superficial similarity to lattice but generally easier/more intuitive to use
+ Default mode makes many choices for you (but you can still customize to your hearts desire)

library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

Summary

- Base: "artist's palette" model
- Lattice: Entire plot specified by one function; conditioning
- ggplot2: Mixes elements of Base and Lattice

[Base Plotting System]

The core plotting and graphics engine in R is encapsulated in the following packages:
- Graphics: contains plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
- grDevices: contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.

The lattice plotting system is implemented using the following packages:
- Lattice: contains code for producing Trellis graphics, which are independent of the "base" graphics system; includes functions like xyplot, bwplot, levelplot.
- Grid: implements a different graphing system independent of the "base" system; the lattice package builds on top of grid; we seldom call fucntions from the grid package directly.

The Process of Making a Plot
- Where will the plot be made? On the screen? In a file?
- How will the plot be used?
  + Is the plot for viewing temporarily on the screen?
  + Will it be presented in a web browser?
  + Will it eventually end up in a paper that might be printed?
  + Are you using it in a presentation?
- Is there a large amount of data going into the plot? Or is it just a few points?
- Do you need to be able to dynamically resize the graphics (vector format)?

Base Graphics
- There are two phases to creating a base plot
  + Initializing a new plot
  + Annotating (adding to) an existing plot
- Calling plot(x, y) or hist(x) will launch a graphics device (if one is not already open) and draw a new plot on the device
- If the arguments to plot are not of some speciall class, then the default method for plot is called;
this function has many arguments, letting you set the title, x axis label, y axis label, etc.
- The base graphics system has many parameters that can be set and tweaked; these parameters are documented in ?par; it would not hurt to try to memorize this help page!

# with(data, expression)
# example applying a t-test to a data frame mydata 
# with(mydata, t.test(y ~ group))

# by(data, factorlist, function)
# example obtain variable means separately for
# each level of byvar in data frame mydata 
# by(mydata, mydatat$byvar, mean(x))

library(datasets)
with(airquality, plot(Wind, Ozone))

library(datasets)
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

Some Important Base Graphics Parameters
- 'f': the plotting symbol (default is open circle)
- 'lty': the line type (default is solid line), can be dashed, dotted etc.
- 'lwd': the line width, specified as an integer multiple
- 'col': the plotting color, specified as a number, string, or hex code; the colors() function gives you a vector of colors by name.
- 'xlab': character string for the x-axis label
- 'ylab': character string for the y-axis label 

The par() function is used to specify global graphics parameters that affect all plots in an R session.
These parameters can be overridden when specified as arguments to specific plotting functions.

- 'las': the orientation of the axis labels on the plot
- 'bg': the background color
- 'mar': the margin size
- 'oma': the outer margin size (default is 0 for all sides)
- 'mfrow': number of plots per row, column (plots are filled row-wise)
- 'mfcol': number of plots per row, column (plots are filled column-wise)

Default values for global graphics parameters:

par("lty")
## [1] "solid"
par("col")
## [1] "black"
par("pch")
## [1] 1
par("bg")
## [1] "transparent"
par("mar")
## [1] 5.1 4.1 4.1 2.1 --> it goes bottom, left, top, right
par("mfrow")
## [1] 1 1 --> single plot (1 row, 1 column)

Base Plotting functions
- 'plot': make a scatterplot, or other type of plot depending on the class of the object being plotted
  + 'lines': add lines to a plot, given a vector of x values and a corresponding vector of y values (or a 2-column matrix); this function just connects the dots
  + 'points': add points to a plot
  + 'text': add text labels to a plot using specified x, y coordinates (within plot)
  + 'title': add annotations to x, y axis labels, title subtitle, outer margin (outside plot)
  + 'mtext': add arbitrary text tot he margins (inner or outer) of the plot
  + 'axis': adding axis ticks/labels

library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City") ## Add a title

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue")) # you do overplot existing points here (see below type = "n" to avoid this)

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n")) # type = "n" --> means "plot nothing"
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

Multiple Base Plots

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) # 1 row, 3 columns
with(airquality, {
	plot(Wind, Ozone, main = "Ozone and Wind")
	plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
	plot(Temp, Ozone, main = "Ozone and Temperature")
	mtext("Ozone and Weather in New York City", outer = TRUE)
	})

Summary 

- Plots in the base plotting system are created by calling successive R functions to "build up" a plot
- Plotting occurs in two stages:
  + Creation of a plot
  + Annotation of a plot (adding lines, points, text, legends)
- The base plotting system is very flexible and offers a high degree of control over plotting

[Base Plotting Demonstration]

- example(points)
- pch = 21-25 --> fill is specified with bg, border colour is specified with col
- legend("topleft", legend = "Data", pch = 20)
  + bottomleft
  + topleft
  + topright
  + bottomright
- fit <- lm(y ~ x) '+' abline(fit, lwd = 3, col = "blue")

g <- gl(2, 50, labels = c("Male", "Female"))
plot(x, y, type = "n")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue", pch = 19)

[Graphics Devices in R]

What is a Graphics Device?

- A graphics device is something where you can make a plot appear
  + A window on your computer (screen device)
  + A PDF file (file device)
  + A PNG or JPEG file (file device)
  + A scalable vector graphics (SVG) file (file device)
- When you make a plot in R, it has to be "sent" to a specific graphics device
- The most common place for a plot to be "sent" is the screen device
  + On a Mac the screen device is launched with quartz()
  + On Windows the screen device is launched with windows()
  + On Unix/Linux the screen device is launched with x11()

- When making a plot, you need to consider how the plot will be used to determine what device the plot should be sent to.
  + The list of devices is found in ?Devices; there are also devices created by users on CRAN
- For quick visualizations and exploratory analysis, usually you want to use the screen device
  + Functions like plot in base, xyplot in lattice, or qplot in ggplot2 will default to sending a plot to the screen device
  + On a given platform (Mac, Windows, Unix/Linux) there is only one screen device
- For plots that may be printed out or be incorporated into a document (e.g. papers/reports, slide presentations), usually a file device is more appropriate
  + There are many different file devices to choose from
- NOTE: Not all graphics devices are available on all platforms (i.e. you cannot launch the windows() on a Mac)

How Does a Plot Get Created?

First Approach: # Commonly used when using screen device
1 - Call a plotting function like plot, xyplot, or qplot
2 - The plot appears on the screen device
3 - Annotate the plot if necessary
4 - Enjoy

library(datasets)
with(faithful, plot(eruptions, waiting)) ## Make plot appear on screen device
title(main = "Old Faithful Geyser data") ## Annotate with a title

Second Approach: # Commonly used for file devices
1 - Explicitly launch a graphics device
2 - Calling a plotting function to make a plot (Note: if you are using a file device, no plot will appear on the screen)
3 - Annotate plot if necessary
4 - Explicitly close graphics device with dev.off() (this is very important!)

pdf(file = "myplot.pdf") ## Open PDF device; create 'myplot.pdf' in my working directory
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data") ## Annotate plot; still nothing on screen
dev.off() ## Close the PDF file device
## Now you can view the file 'myplot.pdf' on your computer

There are two basic types of file devices: vector and bitmap devices

Vector formats:
- pdf: useful for line-type graphics, resizes well, usually protable, not efficient if a plot has many objects/points
- svg: XML-based scalable vector graphics, supports animation and interactivity, potentially useful for web-based plots
- win.metafile: Windows metafile format (only on windows)
- postscript: older format, also resizes well, usually portable, can be used to create encapsulated postscript files; Windows systems often do not have a postscript viewer

Bitmap formats:
- png: bitmapped formated, good for line drawings or images with solid colors, uses lossless compression (like the old GIF format)
most web browsers can read this format natively, good for plotting many many many points, does not resize well
- jpeg: good for photographs or natural scenes, uses lossy compression, good for plotting many many points, does not resize well, can be read by almost
any computer and any web browser, not great for line drawings
- tiff: creates bitmap files in the TIFF format; supports lossless compression
- bmp: a native Windows bitmapped format

Multiple Open Graphics Devices

- It is possible to open mulitiple graphics devices (screen, file, or both), for example when viewing multiple plots at once # open a new one on a Mac with the quartz() function
- Plotting can only occur on one graphics device at a time
- The currently active graphics device can be found by calling dev.cur()
- Every open graphics device is assigned an integer >= 2
- You can change the active graphics device with dev.set(<integer>) where <integer> is the number associated with the graphics device you want to switch to

Copying Plots

Copying a plot to another device can be useful because some plots require a lot of code and it can be a pain to type all that in again for a different device.

- dev.copy(): copy a plot from one device to another
- dev.copy2pdf: specifically copy a plot to a PDF file

NOTE: Copying a plot is not an exact operation, so the result may not be identical to the original

library(datasets)
with(faithful, plot(eruptions, waiting)) ## Create plot on screen device
title(main = "Old Faithful Geyser data") ## Add a main title
dev.copy(png, file = "geyserplot.png") ## Copy my plot to a PNG file
dev.off() ## Don't forget to close the PNG device

Summary

- Plots must be created on a graphics device
- The default graphics device is almost always the screen device, which is most useful for exploratory analysis
- File devices are useful for creating plots that can be included in other documents or sent to other people
- For file devices there are vector and bitmap formats:
  + Vector formats are good for line drawings and plots with solid colors using a modest number of points
  + Bitmap formats are good for plots with a large number of points, natural scenes or web-based plots

## Week 2

[Lattice Plotting System]

The Lattice Plotting System

- The lattice plotting system is implemented using the following packages:
  + lattice: contains code for producing Trellis graphics, which are independent of the 'base' graphics system; includes functions like xyplot, bwplot, levelplot
  + grid: implements a different graphing system independent of the 'base' system; the lattice package builds on top of grid
    * We seldom call functions from the grid package directly
  + The lattice plotting system does not have a 'two-phase' aspect with separate plotting and annotation like in base plotting
  + All plotting/annotation is done at once with a single function call

Lattice Functions
- 'xyplot': this is the main function for creating scatterplots
- 'bwplot': box-and-whiskers plots ("boxplots")
- 'histogram': Histograms
- 'stripplot': like a boxplot but with actual points
- 'dotplot': plot dots on "violin strings"
- 'splom': scatterplot matrix; like pairs() in  base plotting system
- 'levelplot', 'contourplot': for plotting 'image' data

Lattice Functions

Lattice functions generally take a formula for their first argument, usually of the form:

xyplot(y ~ x | f * g, data)

- We use the formula notation here, hence the ~
- On the left of the ~ is the y-axis variable, on the right is the x-axis variable
- f and g are conditioning variables - they are optional
  + the * indicates an interaction between two variables
  + "I want to look at every scatterplot of x/y for every f/g combination"
- The second argument is the data frame or list from which the variables in the formula should be looked up
  + If no data frame or list is passed, then the parent frame is used
- If no other arguments are passed, there are defaults that can be used

Simple Lattice Plot

library(lattice)
library(datasets)
## Simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)

library(lattice)
library(datasets)
## Convert 'Month' to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))

Lattice Behavior

Lattice functions behave differently from base graphics functions in one critical way. 
- Base graphics functions plot data directly to the graphics device (screen, PDF file, etc.)
- Lattice graphics functions return an object of class trellis
- The print methods (of the trellis object) for lattice functions actually do the work of plotting the data on a graphics device
- Lattice functions return "plot objects" that can, in principle, be stored (but it is usually better to just save the code + data)
- On the command line, trellis objects are auto-printed so that it appears the function is plotting the data

p <- xyplot(Ozone ~ Wind, data = airquality) ## Nothing happens!
print(p) ## Plot appears!
xyplot(Ozone ~ Wind, data = airquality) ## Auto-printing feature of R!

Lattice Panel Functions
- Lattice functions have a panel function which controls what happens inside each panel of the plot
- The lattice package comes with default panel functions, but you can supply your own if you want to customize what happens in each panel
- Panel functions receive the x/y coordinates of the data points in their panel (along with any optional arguments)

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(x ~ y | f, layout = c(2, 1)) ## Plot with 2 panels

## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) { ## ... == any other arguments 
	panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot' -- to make the points, axis, labels, etc. appear
	panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
})

## Another custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) { 
	panel.xyplot(x, y, ...) ## First call default panel function
	panel.abline(h = median(y), lty = 2) ## Overlay a simple linear regression line
})

Summary

- Lattice plots are constructed with a single function call to a core lattice function (e.g. xyplot)
- Aspects like margins and spacings are automatically handled and defaults are usually sufficient
- The lattice system is ideal for creating conditioning plots where you examine the same kind of plot under many different conditions
- Panel functions can be specified/customized to modify what is plotted in each of the plot panels

[ggplot2]

What is ggplot2?
- An implementation of the Grammar of Graphics by Leland Wilkinson
- Written by Hadley Wickham (while he was a graduate student at Iowa State)
- A 'third' graphics system for R (along with base and lattice)
- Available from CRAN via install.packages()
- Website: http://ggplot2.org (better documentation)

- Grammar of graphics represents and abstraction of graphic ideas/objects
- Think 'verb', 'noun', 'adjective' for graphics
- Allows for a 'theory' of graphics on which to build new graphics and graphic objects
- "Shorten the distance from mind to page"

"In brief, the grammar tells us that a statistical graphic is a mapping from
data to aesthetic attributes (colour, shape, size) of geometric objects (points,
lines, bars). The plot may also contain statistical transformations of the data
and is drawn on a specific coordinate system."

The Basics: qplot()

- Works much like the plot function in base graphics system
- Looks for data in data frame, similar to lattice, or in the parent environment
- Plots are made up of aesthetics (size, shape, color) and geoms (points, lines)
- Factors are important for indicating subsets of the data (if they are to have different properties); they should be labeled
- The qplot() hides what goes on underneath, which is okay for most operations
- ggplot() is the core function and very flexible for doing things qplot() cannot do

library(ggplot2)
qplot(displ, hwy, data = mpg, color = drv)

Adding a geom

qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

Histograms

qplot(hwy, data = mpg, fill = drv)

Facets

qplot(displ, hwy, data = mpg, facets = . ~ drv) ## . == nothing, so 3 columns, you can replace '.' with another factor
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2) ## . == nothing so 3 rows [ROWS ~ COLUMNS]

Histogram of eNO

qplot(log(eno), data = maacs, fill = mopos)

Density Smooth 

qplot(log(eno), data = maacs, geom = "density")
qplot(log(eno), data = maacs, geom = "density", color = mopos)

qplot(log(pm25), log(eno), data = maacs, color = mopos)
qplot(log(pm25), log(eno), data = maacs, shape = mopos)

qplot(log(pm25), log(eno), data = maacs, color = mopos, geom = c("point", "smooth"), method = "lm")

Scatterplots: eNO vs. PM25 

qplot(log(pm25), log(eno), data = maacs, geom = c("point", "smooth"), method = "lm", facets = . ~ mopos)

Summary of qplot()

- The qpot() function is the analog to plot() but with many built-in features
- Syntax somewhere in between base/lattice
- Produces very nice graphics, essentially publication ready (if you like the design)
- Difficult to go against the grain/customize (do not bother, use full ggplot2 power in that case)

Basic Components of a ggplot2 Plot
- A data frame
- aesthetic mappings: how data are mapped to color, size
- geoms: geometric objects like points, lines, shapes
- facets: for conditional plots
- stats: statistical transformations like binning, quantiles, smoothing
- scales: what scale an aesthetic map uses (exmple: male = red, female = blue)
- coordinate system

Building Plots with ggplot2

- When building plots in ggplot2 (rather than using qplot) the 'artists palette' model may be the closest analogy
- Plots are built up in layers
  + Plot the data
  + Overlay a summary
  + Metadata and annotation

Basic Plot

qplot(logpm25, NocturnalSympt, data = maacs, facets = . ~ bmicat, geom = c("point", "smooth"), method = "lm")

Building Up in layers

g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ bmicat)

Annotation
- Labels: xlab(), ylab(), labs(), ggtitle()
- Each of the 'geom' functions has options to modify
- For things that only make sense globally, use theme()
  + Example: theme(legend.position = "none")
- Two standard appearance themes are included
  + theme_gray(): The default theme (gray background)
  + theme_bw(): More stark/plain

Modifying Aesthetics

geom_point(color = "steelblue", size = 4, alpha = 1/2) ## color is a constant value
geom_point(aes(color = bmicat), size = 4, alpha = 1/2) ## color is a data variable, here I map color to bmicat using aes

Modifying Labels

labs(title = "MAACS Cohort")
labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")

Customizing the Smooth

geom_smooth(size = 4, linetype = 3, method = "lm", se = False) ## se == confidence interval

Changing the Theme

theme_bw(base_family = "Times") ## default font is now Times

A Note about Axis Limits

testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50, 2] <- 100 ## outlier | testdat[rows, columns]

plot(testdat$x, testdat$y, type = "l", ylim = c(-3, 3))

g <- ggplot(testdat, aes(x = x, y = y)) + geom_line()
+ ylim(-3, 3) ## will subset the data and only include points that fall within range --> exclude the outlier!
+ coord_cartesian(ylim = c(-3, 3)) ## outlier included

More Complex Example
- How does the relationship between PM2.5 and nocturnal symptoms vary by BMI and NO2?
- Unlike our previous BMI variable, NO2 is continuous
- We need to make NO2 categorical so we can condition on it in the plotting 
  + Use the cut() function for this

Making NO2 Tertiles (3 segments)

## Calculate the deciles of the data
cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length = 4), na.rm = TRUE)

## Cut the data at the deciles and create a new factor variable
maacs$no2dec <- cut(maacs$logno2_new, cutpoints)

## See the levels of the newly created factor variable
levels(maacs$no2dec)

x <- 1:100
cutpoints <- quantile(x, seq(0, 1, length = 4), ra.rm = TRUE)
y <- cut(x, cutpoints)

## Setup ggplot with data frame
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
+ geom_point(alpha = 1/3)
+ facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4) ## instead of facet_grid()
+ geom_smooth(method = "lm", se = FALSE, col = "steelblue")
+ theme_bw(base_family = "Avenir", base_size = 10)
+ labs(x = expression("log " * PM[2.5]))
+ labs(y = "Nocturnal Symptoms")
+ labs(title = "MAACS Cohort")

Summary

- ggplot2 is very powerful and flexible if you learn the 'grammar' and the various elements that can be tuned/modified
- Many more types of plots can be made; explore and mess around with the package (references mentioned in Part 1 are useful)

[Miscellaneous]

+ Simpsons Paradox

[Assignment 1 - Peer Homework Insights]

dim(df) # what are the table dimensions
attach(df) # no longer have to do df$Data but can just type Data
subset <- Date == "1/2/2007" | Date == "2/2/2007"
newData <- data[subset, ]
rownames(newData) <- 1:nrow(newData) # re-setting the rownames (rownumbers)

file <- "E:/Julius/Studies/Data Science Specialization/Exploratory Data Analysis/Week1Project/household_power_consumption.txt"
#check if file exists
folder <- "E:/Julius/Studies/Data Science Specialization/Exploratory Data Analysis/Week1Project/ExData_Plotting1"
if ((file.exists(file)) & (dir.exists(folder))) {
setwd(folder)
}
firstRows <- read.table(file, header = TRUE, nrows = 5, sep = ";", dec = ".")
colCl <- sapply(firstRows, class)
library(sqldf)
sData <- read.csv.sql(file, sql = "SELECT * FROM file WHERE DATE = '1/2/2007' OR Date = '2/2/2007'", header = TRUE, sep = ";", colClasses = colCl)
closeAllConnections()

dev.off(which = dev.cur())

[Hierarchical Clustering]

Can we find things that are close together?

Clustering organizes things that are close into groups.
- How do we define close?
- How do we group things?
- How do we visualize the grouping?
- How do we interpret the grouping?

Hierarchical Clustering
- An agglomerative approach
  + Find closest two things
  + Put them together
  + Find next closest
- Requires
  + A defined distance
  + A merging approach
- Produces
  + A tree showing how close things are to each other (called dendrogram)

How do we define close?
- Most important step
  + Garbage in --> garbage out
- Distance or similarity
  + Continuous - Euclidian distance (Pythagoras if you have two points)
  + Continuous - correlation similarity
  + Binary - Manhattan distance (wiki: Taxicab_geometry)
- Pick a distance/similarity that makes sense for your problem

[Example]

(1) - Produce Plot

set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

(2) - Calculate Distance: dist(), important parameters: x, method

df <- data.frame(x = x, y = y)
dist(df) 
# produces a distance matrix, calculates the pair-wise distance between all points
# Defaults to euclidian distance, there are other options

(3) - Clustering

1 - Points 5 & 6 are closest together
2 - We then merge these two points, they locations are combined to form a new center location
3 - Next two closest points are 10 & 11, we do the same there

df <- data.frame(x = x, y = y)
distxy <- dist(df)
hClustering <- hclust(distxy)
plot(hClustering) # Produces Dendogram

(4) - Cutting the Dendogram/Tree

- Now we must decide where do we want to cut
- We cut on the Y axis (Height == Distance)
- Cutting it on 2.0 would result in 2 clusters, right now it is arbitrary where to cut
- After cutting you get the cluster assignment, i.e. you specify the clusters

(5) - Prettier Dendrograms

myplclust <- function(hclust, lab = hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)), hang = 0.1 ...) {
	## Modification of plclust for plotting hclust objects *in color*! Copyright
	## Eva KF Chan 2009 Arguments:
	# hclust: hclust object
	# lab: a character vector of labels of the leaves of the tree
	# lab.col: colour for the labels
	# NA: default device foreground colour
	# hang: as in hclust & plclust
	# Side effect: a display of hierarchical cluster with coloured leaf labels
	y <- rep(hclust$height, 2)
	x <- as.numeric(hclust$merge)
	y <- y[which(x < 0)]
	x <- x[which(x < 0)]
	x <- abs(x)
	y <- y[order(x)]
	x <- x[order(x)]
	plot(hclust, labels = FALSE, hang = hang, ...)
	text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
		col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}
 
df <- data.frame(x = x, y = y)
distxy <- dist(df)
hClustering <- hclust(distxy) # hclust --> h == hierarchical
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

# http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=79

- Merging Points:
  + Average Linkage: simply the average of their X/Y coordinates as new location (of all cluster points)
  + Complete Linkage: take X/Y coordinates of the two farthest points (biggest distance) as new locations

- heatmap()

df <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(df)[sample(1:12),]
heatmap(dataMatrix)

Notes and Further Resources

- Gives an idea of the relationships between variables/observations
- The picture may be unstable
  + Change a few points
  + Have different missing values
  + Pick a different distance
  + Change the merging strategy
  + Change the scale of points for one variable
- But it is deterministic
- Choosing where to cut is not always obvious
- Should be primarily used for exploration
- https://www.youtube.com/watch?v=PArRvqLUP6o


- idea of transposing your matrix, clustering will go over rows, to do it across columns, transpose it first!

df <- data.frame(x = 1:5, y = 1:5, z = 1:5)
transposed_df <- t(df)
index <- sample(ncol(transposed_df), 50)
dist <- dist(transposed_df[,index]) # distance is calculated over rows, you want to calculate it across variables, so you first need to transpose the matrix
h <- hclust(dist)
plot(h, labels = tab[index, 3], cex = 0.85)

cutree(h, h = 100) # (object, h = where to cut) --> this will generate our clusters

d <- dist(t(df))
mds <- cmdscale(d) # classical multiple dimension scale plot
cols <- as.factor(tissue)
plot(mds, col = as.numeric(cols)) # every tissue will become a different (next) number
legend("topleft", levels(cols, col = seq(along = levels(cols))))

##

set.seed(1)
df <- data.frame(x = sample(1:10), y = sample(1:10), z = sample(1:10))
distance <- dist(df) # dd <- as.dist(1-cor(t(df))) correlation --> looks at correlation patterns, not absolute distances
h <- hclust(distance)
plot(h)
cols <- as.factor(c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3))
mds <- cmdscale(distance)
plot(mds, col = cols)
legend("topleft", pch = 1, col = as.numeric(cols), legend = cols)


- Inner Join: merge(df1, df2, by = "CustomerId")
- Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
- Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
- Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
- Cross join: merge(x = df1, y = df2, by = NULL)

[K-Means Clustering]

- A partitioning approach
  + Fix a number of clusters
  + Get 'centroids' of each cluster
  + Assign things to closest centroid
  + Recalculate centroids
- Requires
  + A defined distance metric
  + A number of clusters
  + An initial guess as to cluster centroids
- Produces
  + Final estimate of cluster centroids
  + An assignment of each point to clusters

set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

kmeans()
- Important parameters: x, centers, iter.max, nstart
- dataFrame <- data.frame(x, y)
- kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj) # gives you a list of all elements in the list (kmeansObj)
kmeansObj$cluster
kmeansObj$centers # etc.

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

- Heatmaps

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[ , nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[ , order(kmeansObj$cluster)], yaxt = "n")

- K-Means requries a number of clusters
  + Pick by eye/intuition
  + Pick by cross validation/information theory, etc.
  + Determining the number of clusters
- K-Mseans is not deterministic
  + Different number of clusters
  + Different number of iterations

[Dimension Reduction]

Principal Component Analysis (PCA) and Singular Value Decomposition (SVD)

set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[ , nrow(dataMatrix):1]) # you want columns over the x-axis --> rows, columns & x, y; columns normally goes over y, hence you need to transpose it

par(mar = rep(0.2, 4))
heatmap(dataMatrix) # uses hclust over rows & cols

# No real pattern to be found above.
# What if we add a pattern?

set.seed(678910)
for (in in 1:40) {
	# flip a coin
	coinFlip <- rbinom(1, size = 1, prob = 0.5)
	# if coin is heads add a common pattern to that row
	if (coinFlip) {
		dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
	}
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[ , nrow(dataMatrix):1])
heatmap(dataMatrix)

# Patterns in rows and columns

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[ , nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

# Related Problems

- You have multivariate variables X1, ..., Xn so X1 = (X11, ..., X1m)
  + [PCA] Find a new set of multivariate variables that are uncorrelated (i.e. independent, if they measure the same thing they are redundant) that are uncorrelated and explain as much variance as possible.
  + [SVD] If you put all the variables together in one matrix, find the best matrix created with fewer variables (lower rank) that explains the original data.
- The first goal is statistical, and the second goal is data compression.

# Related solutions - PCA/SVD

[SVD]

- If X is a matrix with each variable in a column and each observation in a row then the SVD is a "matrix decomposition"
  + X = UDV^T (original matrix is decomposed in 3 separete matrices: U, D & V)
  + where the columns of U are orthogonal [statistically independent] (left singular vectors), the V columns are orthogonal (right singular vectors) and D is a diagonal matrix (singular values)

[PCA]

- The principal components are equal are to the right singular values if you first scale (subtract the mean, divide by standard deviation; same with standard deviaton, correlation, etc.) the variables

# Components of the SVD - 'u' and 'v'

sv1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[ , nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(sdv1$v[, 1], xlab = "column", ylab = "First right singular vector", pch = 19)

# Components of the SVD - Variance explained ('d')
# Singular Value: represents the % of the total variation in your dataset that is explained by that particular component
# Components are ordered DESC 

plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Proportionate of variance explained", pch = 19) # scaled

# Relationship to principal components
# Right SV == Columns | Left SV == Rows

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0,1))

# Components of the SVD - Variance explained ('d')

constantMatrix <- dataMatrixOrdered * 0
for (i in 1:dim(dataMatrixOrdered)[1]) {
	constantMatrix[i,] <- rep(c(0, 1), each = 5)
}
svd1 <- svd(constantMatrix)
par(mfrow = c(1, 3))
image(t(constantMatrix)[ , nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular Value", pch = 19) # one value is really high, rest all 0
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Proportion of variance explained", pch = 19) # 1 SV explains 100%

# There is only one pattern in this matrix (first 5 columns are 0's, next 5 are 1's)
# Even though there are multiple variables/observations (columns), there really is only 1 dimension

# What if we add a second pattern?

set.seed(678910)
for (i in 1:40) {
	# flip a coin
	coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
	coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
	# if coin is heads add a common pattern to that row
	if (coinFlip1) {
		dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
	}
	if (coinFlip2) {
		dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
	}
}

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

# Look at the patterns

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[ , nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1") # coinFlip1
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2") # coinFlip2

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[ , nrow(dataMatrixOrdered):1])
# Should show first pattern with variance explained of svd2$d[1]^2/sum(svd2$d)
plot(svd2$v[ , 1], pch = 19, xlab = "Column", ylab = "First right singular vector") # Right = Column
# Should show second pattern svd2$d[2] with variance explained of svd2$d[2]^2/sum(svd2$d)
plot(svd2$v[ , 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")
# Though they are a bit confounded, patterns are mixed

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular Value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of Variance Explained", pch = 19)

# Missing Values

dataMatrix2 <- dataMatrixOrdered
## Randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2)) # Doesn't work!

# Solution: Imputing {impute}
library(impute) # Available from http://bioconductor.org
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data # takes a missing value and replaces it with the nearest best value
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1, 2))
plot(svd1$v[ , 1], pch = 19)
plot(svd2$v[ , 1], pch = 19)

# Face Example

load("data/face.rda")
image(t(faceData)[ , nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular Vector", ylab = "Variance Explained")

sv1 <- svd(scale(faceData))
## Note that %*% is matrix multiplication

# Here svd1$d[1] is a constant
approx1 <- svd1$u[ , 1] %*% t(svd1$v[ , 1]) * svd1$d[1]

# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[ , 1:5] %*% diag(svd1$d[1:5]) * t(svd1$v[ , 1:5])
approx5 <- svd1$u[ , 1:10] %*% diag(svd1$d[1:10]) * t(svd1$v[ , 1:10])

par(mfrow = c(1, 4))
image(t(approx1)[ , nrow(approx1):1], main = "(a)")
image(t(approx5)[ , nrow(approx1):1], main = "(b)")
image(t(approx10)[ , nrow(approx1):1], main = "(c)")
image(t(faceData)[ , nrow(faceData):1], main = "(c)") ## Original data

Notes and Further Resources

- Scale matters
- PCs/SVs may mix real patterns
- Can be computationally intensive
- Advanced data analysis from an elementary point of view [URL]
- Elements of statistical learning
- Alternatives
  + Factor Analysis
  + Independent component analysis
  + Latent semantic analysis

[Plotting and Color in R]

- The default color schemes for most plots are horrendous
- Recently there have been developments to improve the handling/specification of colors in plots/graphs etc.
- These are functions in R and in external packages that are very handy

Color Utilities in R

- The grDevices package has two functions
  + colorRamp
  + colorRampPalette
- These functions take palettes of color and help to interpolate between the colors
- The function colors() lists the names of colors you can use in any plotting function

Color Palette Utilities in R
- colorRamp: Take a palette of colors and return a function that takes values between 0 and 1,
indicating the extremes of the color palette (e.g. see the 'gray' function)
- colorRampPalette: Take a palette of colors and return a function that takes integer arguments and 
returns a vector of colors interpolating the palette (like heat.colors or topo.colors)

pal <- colorRamp(c("red", "blue"))
pal(0) #=> red
pal(1) #=> blue
pal(0.5) #=> mix of red & blue
pal(seq(0, 1, len = 10))

pal <- colorRampPalette(c("red", "yellow"))
pal(10) 

#RRGGBB
#FF0000

RColorBrewer Package
- One package on CRAN that contains interesting/useful color palettes
- There are 3 types of palettes
  + Sequential # used for ordered, numerical/continuous data (low to high)
  + Diverging # data that diverges or deviates from the mean
  + Qualitative # not ordered (factors, categorical)
- Palette information can be used in conjunction with the colorRamp() and colorRampPalette()

RColorBrewer and colorRampPalette

library(RColorBrewer)
cols <- brewer.pal(3, "BuGn") # for a list check ?brewer.pal (help-page)
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

The smoothScatter function

x <- rnorm(10000)
z <- rnorm(10000)

smoothScatter(x, y)

Some Other Plotting Notes

- The rgb function can be used to produce any color via red, green, blue proportions
- Color transparency can be added via the alpha parameter to rgb
- The colorspace package can be used for a different control over colors

plot(x, y, pch = 19)
plot(x, y, pch = 19, col = rgb(0, 0, 0, 0.2)) # rgb(red, green, blue, alpha) -- alpha = transparency

Summary

- Careful use of colors in plots/maps/etc. can make it easier for the reader to get what you are trying to say (why make it harder?)
- The RColorBrewer package is an R package that provides color palettes for sequential, categorical and diverging data
- The colorRamp and colorRampPalette functions can be used in conjunction with color palettes to connect data to colors
- Transparency can sometimes be used to clarify plots with many points

[Clustering Case Study]

Plotting average acceleration for first subject

par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1[1]))
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1[2]))
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)

source("myplclust.R") # some visualisation function
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

Plotting max acceleration for the first subject

par(mfrow = c(1, 2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])

Clustering based on maximum acceleration

source("myplclust.R") # some visualisation function
distanceMatrix <- dist(sub1[, 10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

Singular Value Decomposition

svd1 = svd(scale(sub1[, -c(562, 563)]))
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)

Find maximum contributor

plot(svd1$v[, 2], pch = 19)

maxContrib <- which.max(svd1$v[, 2])
distanceMatrix <- dist(sub1[, c(10:12), maxContrib])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

K-Means clustering (nstart = 1, first try)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 10) # the last two colums only contain qualitative data, hence they're exluded
table(kClust$cluster, sub1$activity)

## By looking at the values of the cluster centers you can get a sense of what you can use
## To predict that activity!

# Cluster 1 Variable Centers (Laying)
plot(kClust$center[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")
# Cluster 4 Variable Centers (Walking)
plot(kClust$center[4, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")

##

- Swirl SVD
  + scale() - subtract the column mean from every element and divide the result by the column standard deviation
  + svd(mat) # mat is a simple matrix
  + mat == matu %*% diag %*% t(matv)
  + x <- svd(scale(mat)) --> x$v == prcomp(scale(mat)) # v == right singular values
    - PCA of a scaled matrix yields the V matrix (right singular vectors) of the same scaled matrix.
    - LEFT = rowMeans
    - RIGHT = colmeans 
  + Cannot deal with missing data --> impute.knn uses the k nearest neighbors to calculate a values to use in place of the missing data. You may want to specify an integer k which indicates how many
    neighbors you want to average to create this replacement value. The bioconductor package (http://bioconductor.org) has an impute package which you can use
    to fill in missing data. One specific function in it is impute.knn.
    - a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1]) # because svd1$d[1] is a constant
    - a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2]) # now all are matrices (you turn the two values into a diag matrix with diag())

-- Air Pollution Case Study

'less' example.txt --> quickly view content of example.txt (before knowing how to read it)
grep ^RC example.txt --> do any of the lines start with 'RC'

- pm0 <- read.table(example.txt)
  + comment.char = "#" --> ignore the lines that start with "#"
  + na.strings = "" --> missing values are indicated with a blank string (not NA, NULL, etc.)

cnames <- readLines("example.txt", 1) # only read the first line
cnames <- strsplit(cnames, "|", fixed = TRUE) # returns a list
names(pm0) <- make.names(cnames[[1]]) # make.names() removes white space and adds points ('.') in between instead; it turns them into valid names

x0 <- pm0$Sample.Value
mean(is.na(x0)) #=> 0.1126 --> 11.26% is missing

- Cntrl + L --> cleans the console!

boxplot(x0, x1)
boxplot(log10(x0), log10(x1)) # to fix the scales (each step is 10x bigger (log10))
                              # negative values can't be converted --> produces NaN

negative <- x1 < 0 
sum(negative, na.rm = TRUE) # x number of values are negative
mean(negative, na.rm = TRUE) # x% of total is negative

dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
hist(dates, "month") # historgram by month
hist(dates[negative], "month") # which month do the negative values occur

site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site0 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")

str(site0) #=> chr [1:33] --> so there are 33 state.code/county.code combinations in here
both <- intersect(site0, site1) # which of those combinations exist in both sets

# How many measurements do we have of the meters that are in both time periods? We'd like to have many.

pm0$County.Site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$County.Site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

cnt0 <- subset(pm0, State.Code == 36 & County.Site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & County.Site %in% both)

sapply(split(cnt0, cnt0$County.Site), nrow) # split the df by County.Site and count the rows for each of the monitors
sapply(split(cnt1, cnt0$County.Site), nrow) # split the df by County.Site and count the rows for each of the monitors

pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

dates1 <- pm1sub$Date
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub) 

dates0 <- pm1sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

par(mfrow = c(1,2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 2)
abline(h = median(x0sub, na.rm = TRUE))
plot(dates1, x1sub, pch = 20)
abline(h = median(x2sub, na.rm = TRUE))

# We need to standardize the y-range of the plots

rng <- range(x0sub, x1sub, na.rm = TRUE)

plot(dates0, x0sub, pch = 2, ylim = rng)
abline(h = median(x0sub, na.rm = TRUE))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x2sub, na.rm = TRUE))

# Exploring change at the state level

mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)

mrg <- merge(d0, d1, by = "state")
dim(mrg) # 53, 3

par(mfrow = c(1, 1))
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[, 3]))) # we now use points() to only add points to the existing plot; no xlim needed

segments(rep(1999, 52), mrg[ ,2], rep(2012, 52), mrg[ ,3])


[Further Resources]

- R Graph Gallery
- R Bloggers