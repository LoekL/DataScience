== Developing Data Products ==

-- Week 1

-- Shiny --

[Shiny 1 - Introduction to Shiny]

What is Shiny?
- Shiny is a platform for creating interactive R programs embedded into a web page
- Suppose that you create a prediction algorithm, with shiny you can very easily create web input form that calls R and thus your prediction algorithm and displays the results
- Using Shiny, the time to create simple, yet powerful, web-based interactive data products in R is minimized
  + However, it lacks the flexibility of full featured (and more complex) solutions
- Shiny is made by the fine folks at R Studio

Some mild prerequisites
- Shiny does not really require it, but as with all web programming, a little knowledge of html, css and js is very helpful
  + html gives a web page structure and sectioning as well as markup instructions
  + css gives the style
  + js for interactivity
- There are too many tutorials online to count for getting basic proficiency in these topics
- Shiny uses bootstrap (no relation to the statistics bootstrap) style, which (to me) seems to look nice and renders well on mobile platforms # http://getbootstrap.com/

What else is out there?
- Creating any solution requiring fairly deep knowledge of web client/server programming
- OpenCPU by Jerome Ooms, is a really neat project providing an API for calling R from web documents
  + And he even hosts an OpenCPU server, but you can create your own

Context
- You created a novel prediction algorithm to predict risk for developing diabetes
  + You are hoping patients and caregivers will be able to enter their data and, if needed, take preventative measures
- You want to create a web site so that users can input the relevant predictors and obtain their prediction
- Your prediction algorithm (ok, so you are not going to be saving the world with this one):

diabetesRisk <- function(glucose) glucose / 200

  + link for a real prediction score # http://www.ncbi.nlm.nih.gov/pubmed/12610029

Getting started
1 - Make sure you have the latest release of R installed
2 - If on windows, make sure that you have Rtools installed

install.packages("shiny")
library(shiny)

3 - Great tutorial at http://shiny.rstudio.com/tutorial/
4 - Basically, this lecture is walking through that tutorial offering some of our insights
5 - Note, some of the proposed interactive plotting uses of Shiny could be handled by the very simple manipulate function rstudio manipulate
6 - Also, rCharts is will be covered in a different lecture

A Shiny project
- A shiny project is a directory containing at least two parts
  + One named ui.R (for user interface) controls how it looks
  + One named server.R that controls what it does

# ui.R

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Data science FTW!"),
  sidebarPanel(
    h3('Sidebar text') # h3 = third level HTML heading
  ),
  mainPanel(
      h3('Main Panel text')
  )
))

# server.R

library(shiny)
shinyServer(
  function(input, output) { # example function returns NULL
  }
)

To run it
- In R, change to the directories with these files and type runApp()
- Or put the path to the directory as an argument
- It should open an browser window with the app running

[Shiny 2 - Basic HTML and Getting Input]

R functions for HTML markup

# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Illustrating markup"), # comma's between things!
  sidebarPanel(
      h1('Sidebar panel'),
      h1('H1 text'),
      h2('H2 Text'),
      h3('H3 Text'),
      h4('H4 Text')

  ),
  mainPanel(
      h3('Main Panel text'),
      code('some code'), # code-block
      p('some ordinary text')
  )
))

Illustrating inputs ui.R

shinyUI(pageWithSidebar(
  headerPanel("Illustrating inputs"),
  sidebarPanel(
    numericInput('id1', 'Numeric input, labeled id1', 0, min = 0, max = 10, step = 1),
    checkboxGroupInput("id2", "Checkbox", # checkbox labeled as id2
                   c("Value 1" = "1",
                     "Value 2" = "2",
                     "Value 3" = "3")),
    dateInput("date", "Date:") # date input labeled as date (firt arg)
  ),
  mainPanel(

  )
))

Part of ui.R

mainPanel(
    h3('Illustrating outputs'),
    h4('You entered'),
    verbatimTextOutput("oid1"),
    h4('You entered'),
    verbatimTextOutput("oid2"),
    h4('You entered'),
    verbatimTextOutput("odate")
  )

--> Where do oid1, oid2 & odate come from?

# server.R

shinyServer(
  function(input, output) {
    output$oid1 <- renderPrint({input$id1}) # renderPrint --> output is formatted as printed text
    output$oid2 <- renderPrint({input$id2})
    output$odate <- renderPrint({input$date})
  }
)

[Shiny 3 - Created a Very Basic Prediction Function]

Lets build our prediction function:

# ui.R

shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Diabetes prediction"),

    sidebarPanel(
      numericInput('glucose', 'Glucose mg/dl', 90, min = 50, max = 200, step = 5), # 90 = default value
      submitButton('Submit') # Add a submit button
    ),
    mainPanel(
        h3('Results of prediction'),
        h4('You entered'),
        verbatimTextOutput("inputValue"),
        h4('Which resulted in a prediction of '),
        verbatimTextOutput("prediction")
    )
  )
)

# server.R

# You can use the source() function to read in a different script if the function is very long, etc.
diabetesRisk <- function(glucose) glucose / 200

shinyServer(
  function(input, output) {
    output$inputValue <- renderPrint({input$glucose})
    output$prediction <- renderPrint({diabetesRisk(input$glucose)}) # renderPrint --> so you print it
  }
)

[Shiny 4 - Working with Images]

Image example
- Lets build an example with an image
- How about we create a histogram of data
- Put a slider on so that the user has to guess the mean

# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Example plot"),
  sidebarPanel(
    sliderInput('mu', 'Guess at the mean',value = 70, min = 62, max = 74, step = 0.05,)
  ),
  mainPanel(
    plotOutput('newHist') # Here you plot the output
  )
))

# server.R

library(UsingR)
data(galton)

shinyServer(
  function(input, output) {
    output$newHist <- renderPlot({ # render a plot instead of text (renderPrint)
      hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
      mu <- input$mu
      lines(c(mu, mu), c(0, 200),col="red",lwd=5)
      mse <- mean((galton$child - mu)^2)
      text(63, 150, paste("mu = ", mu))
      text(63, 140, paste("MSE = ", round(mse, 2)))
      })

  }
)

[Shiny 5 - Discussion]

Tighter control over style
- All of the style elements are handled through ui.R
- Instead, you can create a www directory and then an index.html file in that directory
  + This link goes through the html needed # http://rstudio.github.io/shiny/tutorial/#html-ui
  + You just have to have specific js libraries and appropriately name ids and classes. This is beyond the scope of this class
  + For students with a lot of experience in html, js, css it would be a breeze and probably easier and more flexible than the R html controls in ui.R

Other things Shiny can do
- Allow users to upload or download files
- Have tabbed main panels
- Have editable data tables
- Have a dynamic UI
- User defined inputs and outputs
- Put a submit button so that Shiny only executes complex code after user hits submit

Distributing a Shiny app
- The quickest way is to send (or put on github or gist or dropbox or whatever) someone the app directory and they can then call runApp
- You could create an R package and create a wrapper that calls runApp
  + Of course, these solutions only work if the user knows R
- Another option is to run a shiny server
  + Requires setting up a (Shiny server)[http://www.rstudio.com/shiny/server/]
    * Probably easiest if you use one of the virtual machines where they already have Shiny servers running well (for example, on AWS)
  + Setting up a Shiny server is beyond the scope of this class as it involves some amount of linux server administration
  + Groups are creating a Shiny hosting services that will presumably eventually be a fee for service or freemium service
  + BTW, don not put system calls in your code (this is one of the first things many of us do for fun, but it introduces security concerns)

[ShinyApps.io]

library(shinyapps)
# go to directory with files
runApp() # to make sure it runs locally
deployApp() # deploys on Shiny server (make sure you've set up your account accordingly)

-- More advanced Shiny (Shinier) --

[More Advanced Shiny Discussion, Reactivity]

Shiny Revisited
- In the last lecture, we covered basic creation of Shiny applications
- If you tried it and are like most, you had an easy time with ui.R but a harder time with server.R
- In this lecture, we cover some more of the details of shiny
- Since writing the last lecture, a more detailed tutorial has been created that is worth checking out (http://shiny.rstudio.com/tutorial/)

Details
- Code that you put before shinyServer in the server.R function gets called once when you do runApp()
- Code inside the unnamed function of shinyServer(function(input, output){ but not in a reactive statement will run once for every new user (or page refresh)
- Code in reactive functions of shinyServer get run repeatedly as needed when new values are entered (reactive functions are those like render*)

Experiment (code in the slidify document)

# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Hello Shiny!"),
  sidebarPanel(
      textInput(inputId="text1", label = "Input Text1"),
      textInput(inputId="text2", label = "Input Text2")
  ),
  mainPanel(
      p('Output text1'),
      textOutput('text1'),
      p('Output text2'),
      textOutput('text2'),
      p('Output text3'),
      textOutput('text3'),
      p('Outside text'),
      textOutput('text4'),
      p('Inside text, but non-reactive'),
      textOutput('text5')
  )
))

# server.R

Set x <<- 0 before running:

library(shiny)
x <<- x + 1 # use <<- to make sure things are assigned outside all environments (globally)
y <<- 0

shinyServer(
  function(input, output) {
    y <<- y + 1
    output$text1 <- renderText({input$text1})
    output$text2 <- renderText({input$text2})
    output$text3 <- renderText({as.numeric(input$text1)+1})
    output$text4 <- renderText(y)
    output$text5 <- renderText(x)
  }
)

Try it:
- type runApp()
- Notice hitting refresh increments y but entering values in the textbox does not
- Notice x is always 1
- Watch how it updated text1 and text2 as needed.
- Does not add 1 to text1 every time a new text2 is input.
- Important try runApp(display.mode='showcase') # handy to see what shiny is actually executing in the back-end

[More Advanced Shiny, the Reactive Function]

Reactive expressions
- Sometimes to speed up your app, you want reactive operations
  (those operations that depend on widget input values) to be performed
  outside of a render statement
- For example, you want to do some code that gets reused in several render
  statements and do not want to recalculate it for each
- The reactive function is made for this purpose

## Example
# server.R

shinyServer(
  function(input, output) {
    x <- reactive({as.numeric(input$text1)+100}) # we now only have to do this once
    output$text1 <- renderText({x()                          })
    output$text2 <- renderText({x() + as.numeric(input$text2)})
  }
)

# As opposed to:

shinyServer(
  function(input, output) {
    output$text1 <- renderText({as.numeric(input$text1)+100  })
    output$text2 <- renderText({as.numeric(input$text1)+100 + # here we have to increment by 100 twice
        as.numeric(input$text2)})
  }
)

Discussion
- Do runApp(display.mode='showcase')
- (While inconsequential) the second example has to add 100 twice every time text1 is updated for the second set of code
- Also note the somewhat odd syntax for reactive variables
  + you call the variable like a function: x()

[More Advanced Shiny, Conditional Execution of Reactive Statements]

Non-reactive reactivity (what?)
- Sometimes you do not want shiny to immediately perform reactive calculations from widget inputs
- In other words, you want something like a submit button

# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Hello Shiny!"),
  sidebarPanel(
      textInput(inputId="text1", label = "Input Text1"),
      textInput(inputId="text2", label = "Input Text2"),
      actionButton("goButton", "Go!")
  ),
  mainPanel(
      p('Output text1'),
      textOutput('text1'),
      p('Output text2'),
      textOutput('text2'),
      p('Output text3'),
      textOutput('text3')
  )
))

# Server.R

shinyServer(
  function(input, output) {
    output$text1 <- renderText({input$text1})
    output$text2 <- renderText({input$text2})
    output$text3 <- renderText({
        input$goButton
        # isolate statement will hold execution of the statement until the button is pressed
        isolate(paste(input$text1, input$text2))
    })
  }
)

Try it out:
- Notice it does not display output text3 until the go button is pressed
- input$goButton (or whatever you named it) gets increased by one for every time pushed
- So, when in reactive code (such as render or reactive)
  you can use conditional statements like below to only execute code on
  the first button press or to not execute code until the first or subsequent button press

if (input$goButton == 1){ Conditional statements }

## Example
# Here is some replaced code from our previous server.R

output$text3 <- renderText({
    if (input$goButton == 0) "You have not pressed the button"
    else if (input$goButton == 1) "you pressed it once"
    else "OK quit pressing it"
})

[More Advanced Shiny, Odds and Ends]

More on layouts
- The sidebar layout with a main panel is the easiest
- Using shinyUI(fluidpage( is much more flexible and allows tighter access to the bootstrap styles
- Examples here (http://shiny.rstudio.com/articles/layout-guide.html)
- fluidRow statements create rows and then the column function from within it can create columns
- Tabsets, navlists and navbars can be created for more complex apps

Directly using html
- For more complex layouts, direct use of html is preferred (http://shiny.rstudio.com/articles/html-ui.html)
- Also, if you know web development well, you might find using R to create web layouts kind of annoying
- Create a directory called www in the same directory with server.R
- Have an index.html page in that directory
- Your named input variables will be passed to server.R <input type="number" name="n" value="500" min="1" max="1000" />
- Your server.R output will have class definitions of the form shiny- <pre id="summary" class="shiny-text-output"></pre>

Debugging techniques for Shiny
- Debugging shiny apps can be tricky
- We saw that runApp(displayMode = 'showcase') highlights execution while a shiny app runs
- Using cat in your code displays output to stdout (so R console)
- The browser() function can interupt execution and can be called conditionally (http://shiny.rstudio.com/articles/debugging.html)

-- Manipulate --

[Manipulate]

Manipulate
- Suppose that you want to create a quick interactive graphic
  + You have to do it now
  + The intended users also use Rstudio
- manipulate is a really cool solution that is often all you need to quickly make interactive graphics

Documentation
- Manipulate is well documented at the Rstudio web site here: http://www.rstudio.com/ide/docs/advanced/manipulate
- From there, try this

library(manipulate)
manipulate(plot(1:x), x = slider(1, 100))

- You can create a slider, checkbox, or picker (drop down) and have more than one

# Example from the regression class

library(manipulate)
myHist <- function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

-- Interactive Graphics --

[rCharts Introduction]

rCharts
- rCharts is a way to create interactive javascript visualizations using R
- So:
  + You do not have to learn complex tools, like D3
  + You simply work in R learning a minimal amount of new syntax
- rCharts was written by Ramnath Vaidyanathan (friend of the Data Science Series),
  who also wrote slidify, the framework we use for all of the lectures in the class
- This lecture is basically going through (http://ramnathv.github.io/rCharts/)

# Example | nvD3 run

require(rCharts)
haireye = as.data.frame(HairEyeColor)
n1 <- nPlot(Freq ~ Hair, group = 'Eye', type = 'multiBarChart',
  data = subset(haireye, Sex == 'Male')
)
n1$save('fig/n1.html', cdn = TRUE) # name it n1.html
cat('<iframe src="fig/n1.html" width=100%, height=600></iframe>')

[rCharts more Examples]

Slidify interactive
- The above was an example of embedding an rChart in a slidify document
  + In the YAML yaml ext_widgets : {rCharts: ["libraries/nvd3"]}
- Or, if you use more than one library
- YAML example yaml ext_widgets : {rCharts: ["libraries/highcharts", "libraries/nvd3", "libraries/morris"]}

Viewing the plot
- The object n1 contains the plot
  + In RStudio, typing n1 brings up the plot in the RStudio viewer (or you can just not assign it to an object)
- Do n1$ then hit TAB to see the various functions contained in the object
  + n1$html() prints out the html for the plot
- I do n1$save(filename) then bring the code back into slidify document
  + This is recommended for slidify, but if you are just looking at the plot, it is unnecessary

## Deconstructing another example
# Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
r1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point') #  '| Species' --> conditioned/grouped by Species
r1$save('fig/r1.html', cdn = TRUE)
cat('<iframe src="fig/r1.html" width=100%, height=600></iframe>') # this only embeds it into slidify, if you aren't using it, this step is unnecessary

# Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
r2 <- rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')
r2$save('fig/r2.html', cdn = TRUE)
cat('<iframe src="fig/r2.html" width=100%, height=600></iframe>')

## How to get the js/html or publish an rChart
# Now you can add whatever you would like

r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")
r1$print("chart1") # print out the js
r1$save('myPlot.html') #save as html file
r1$publish('myPlot', host = 'gist') # save to gist, rjson package required
r1$publish('myPlot', host = 'rpubs') # save to rpubs

rCharts has links to several libraries
- We will do some examples
- Note Ramnath mentions that io2012 and polychart have conflicting js
  + They seem to work for me with that theme, but I get errors if I load the polychart library
  + If debugging with io and polychart, factor that in

# morris example run
data(economics, package = "ggplot2")
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1$save('fig/m1.html', cdn = TRUE)
cat('<iframe src="fig/m1.html" width=100%, height=600></iframe>')

# xCharts run
require(reshape2)
uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c("category", "year")
x1 <- xPlot(value ~ year, group = "category", data = uspexp, type = "line-dotted")
x1$save('fig/x1.html', cdn = TRUE)
cat('<iframe src="fig/x1.html" width=100%, height=600></iframe>')

[rCharts Mapping and Discussion]

# Leaflet run
map3 <- Leaflet$new()
map3$setView(c(51.505, -0.09), zoom = 13)
map3$marker(c(51.5, -0.09), bindPopup = "<p> Hi. I am a popup </p>")
map3$marker(c(51.495, -0.083), bindPopup = "<p> Hi. I am another popup </p>")
map3$save('fig/map3.html', cdn = TRUE)
cat('<iframe src="fig/map3.html" width=100%, height=600></iframe>')

# Rickshaw run
usp = reshape2::melt(USPersonalExpenditure)
# get the decades into a date Rickshaw likes
usp$Var2 <- as.numeric(as.POSIXct(paste0(usp$Var2, "-01-01")))
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = "Var1", data = usp, type = "area", width = 560)
# add a helpful slider this easily; other features TRUE as a default
p4$set(slider = TRUE)
p4$save('fig/p4.html', cdn = TRUE)
cat('<iframe src="fig/p4.html" width=100%, height=600></iframe>')

# highchart run
h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line",
    "bubble", "scatter"), group = "Clap", size = "Age")
h1$save('fig/h1.html', cdn = TRUE)
cat('<iframe src="fig/h1.html" width=100%, height=600></iframe>')

rCharts summarized
- rCharts makes creating interactive javascript visualizations in R ridiculously easy
- However, non-trivial customization is going to require knowledge of javascript
- If what you want is not too big of a deviation from the rCharts examples, then it is awesome
  + Otherwise, it is challenging to extend without fairly deep knowledge of the JS libraries that it is calling
- rCharts is under fairly rapid development

[GoogleVis] # https://developers.google.com/chart/interactive/docs/gallery

Basic idea
- The R function creates an HTML page
- The HTML page calls Google Charts
- The result is an interactive HTML graphic

# Example
suppressPackageStartupMessages(library(googleVis)) # suppressPackageStartupMessages just ignores a startup message
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=600, height=400))
print(M,"chart")

Charts in googleVis # "gvis + ChartType"
- Motion charts: gvisMotionChart
- Interactive maps: gvisGeoChart
- Interactive tables: gvisTable
- Line charts: gvisLineChart
- Bar charts: gvisColumnChart
- Tree maps: gvisTreeMap

# http://cran.r-project.org/web/packages/googleVis/googleVis.pdf

# Plots on maps
G <- gvisGeoChart(Exports, locationvar="Country", # locationvar can be a set of longitude & langitudes or (country) names
                  colorvar="Profit",options=list(width=600, height=400))
print(G,"chart") # or plot(G)

# Specifying a region
G2 <- gvisGeoChart(Exports, locationvar="Country",
                  colorvar="Profit",options=list(width=600, height=400,region="150")) # region = 150 --> Europe
print(G2,"chart")

Finding parameters to set under options: https://developers.google.com/chart/interactive/docs/gallery/geochart

# Setting more options
df <- data.frame(label=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
        options=list(title="Hello World", legend="bottom",
                titleTextStyle="{color:'red', fontSize:18}",
                vAxis="{gridlines:{color:'red', count:3}}",
                hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                series="[{color:'green', targetAxisIndex: 0},
                         {color: 'blue',targetAxisIndex:1}]",
                vAxes="[{title:'Value 1 (%)', format:'##,######%'},
                                  {title:'Value 2 (\U00A3)'}]",
                curveType="function", width=500, height=300
                ))

print(Line,"chart")

# https://github.com/mages/Introduction_to_googleVis/blob/gh-pages/index.Rmd

# Combining multiple plots together
G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

print(GTM,"chart")

# Seeing the HTML code
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=600, height=400))
print(M)

Things you can do with Google Vis
- The visualizations can be embedded in websites with HTML code
- Dynamic visualizations can be built with Shiny, Rook, and R.rsp
- Embed them in R markdown based documents
  + Set results="asis" in the chunk options
  + Can be used with knitr and slidify

For more info:
- type: demo(googleVis)
- http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis.pdf
- http://cran.r-project.org/web/packages/googleVis/googleVis.pdf
- https://developers.google.com/chart/interactive/docs/gallery
- https://developers.google.com/chart/interactive/faq

[Plotly]

load("courseraData.rda")

## Make sure that you've followed the first few set up steps
## https://plot.ly/ggplot2/getting-started/
## Particularly set_credentials_file(username=FILL IN, api_key=FILL IN)
library(plotly)

library(ggplot2)
## First do a bar plot in ggplot
g <- ggplot(myData, aes(y = enrollment, x = class, fill = offering))
g <- g + geom_bar(stat = "identity")
g

## Let's try to get it into plot.ly
py <- plotly()
out <- py$ggplotly(g)
out$response$url

-- Week 2

-- Slidify --

[Slidify Intro]

What is Slidify?
- Slidify was created by Ramnath Vaidyanathan in order to streamline the process of creating and publishing R driven presentations.
- Slidify is an amalgamation of other technologies including knitr, Markdown, and several javascript libaries for HTML5 presentations.
- Slidify is infinitely extendable and customizable, yet it is easy to use!
- Slidify allows embedded code chunks and mathematical formulas which keeps your presentation reproducable.
- Slidify presentations are just HTML files, so you can view them with any web browser and share them easily on Github, Dropbox, or your own website.

Getting Slidify
0 - Fire up RStudio!
1 - First, make sure you have devtools installed and loaded

install.packages("devtools")
library(devtools)

2 - Second, install Slidify

install_github('slidify', 'ramnathv')
install_github('slidifyLibraries', 'ramnathv')

3 - Third, load Slidify

library(slidify)

Getting started with Slidify
- Set the working directory to where you want to create your Slidify project

setwd("~/sample/project/")

- Create your project and give your project a name (My project is named "first_deck")

author("first_deck")

Getting started with Slidify
- author("first_deck") causes the following to happen:
  1 - A directory with the name of your project is created inside of your current directory.
  2 - Inside of this directory an assets directory and a file called "index.Rmd" is created.
  3 - The assets directory is populated with the following empty folders: css, img, js, and layouts.
  4 - The newly created index.Rmd R Markdown file will open up in RStudio.
- Any custom css, images, or javascript you want to use should respecively be put into the newly created css, img, and js folders.

[Slidify Working It Out]

slidify('index.Rmd')
library(knitr)
browseURL('index.html')

[Slidify Customization]

Getting to know index.Rmd : YAML # Yet Another Markdown Language
- index.Rmd is the R Markdown document which you will use to compose the conent of your presentation.
- The first part of an index.Rmd file is a bit of YAML code which will look like this:

---
title       :
subtitle    :
author      :
job         :
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      #
widgets     : []            # {mathjax [rendering mathematical formulas], quiz [adding quiz questions], bootstrap [style]}
mode        : selfcontained # {standalone, draft} - if you have access to the internet or not (pre-load + save libraries if not, etc.)
---

Getting to know index.Rmd : YAML
- You can edit your YAML to include the title, subtitle, author, and job of the author, including what slide framework you wish to use, which code highlighter you wish to use, and any widgets you want to include.
- Other fields you can include in your YAML: a logo to appear in your title slide under logo, the path to your assets folder and the paths to any other folders you may be using under url, and the specific theme for your code highlighter of choice under hitheme.

---
logo        : my_logo.png
url:
  assets: ../assets
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : zenburn       # code highlighter theme
---

- Remember that ../ signifies the parent directory.

Getting to know index.Rmd : YAML

The YAML for the presentation you are currently viewing looks like this:

---
title       : Slidify
subtitle    : Data meets presentation
author      : Jeffrey Leek, Assistant Professor of Biostatistics
job         : Johns Hopkins Bloomberg School of Public Health
logo        : bloomberg_shield.png
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      #
url:
  lib: ../../libraries
  assets: ../../assets
widgets     : [mathjax]     # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
---

Getting to know index.Rmd : Making Slides
- Your first two slides are made for you under the YAML:

## Read-And-Delete

1. Edit YAML front matter
2. Write using R Markdown
3. Use an empty line followed by three dashes to separate slides!

- - - .class #id

## Slide 2

- Whatever you put after ## will be the title of the slide.
- --- marks the end of the slide.
- .class #id are CSS attributes you can use to customize the slide.
- Whatever you put between ## and --- is up to you! As long as it is valid R Markdown or HTML.

[Slidify More Details]

Getting to know index.Rmd : Making Slides
- To compile your presentation make sure the working directory contains your index.Rmd file and enter the following command:

slidify("index.Rmd")

- An HTML flie should appear in your current directory, open it with your favorite web browser and enjoy your Slidify deck!

browseURL("index.html")

Publishing to Github
- First, log in to GitHub and create a new empty repository.
- Use the following command, but replace user with your username and repo with the name of your new repository (both arguments are strings).

publish_github(user, repo)

HTML5 Deck Frameworks
- The following frameworks are compatible with Slidify for making your presentations:
  + io2012
  + html5slides
  + deck.js
  + dzslides
  + landslide
  + Slidy

Mathjax
- You can include $\LaTeX$ math formatting as follows
- Edit your YAML

widgets     : [mathjax]

- Enter inline math code with $x^2$ $x^2$
- Enter centered code with $$\frac{-b \pm \sqrt{b^2 - 4 a c}}{2a}$$ $$\frac{-b \pm \sqrt{b^2 - 4 a c}}{2a}$$

HTML
- Just include html in the Rmd file and it will get kept as html when it is slidified
- Especially useful for stuff like images or tables where you need finer control of the html options
- Also, remember you can edit the final html slide
  + This is not the best solution (since why do mostly slidify, a reproducible format if you are going to break that reproducibility at the last step?)
  + But, sometimes useful in a pinch (like if you are frantically preparing course slides at the last minute)
- Similarly, you can incorporate JS, or anything else you can do in a web page

Adding interactive elements to slidify
- You can add interactive elements to slidify
  + Quiz questions
  + interactive Rcharts plots
  + Shiny apps
- Of course, you could do this directly with html/js
- More easily, the dev version of slidify has this built in
- See http://slidify.github.io/dcmeetup/demos/interactive/
  + The following example was taken from there

Rmd syntax

## Question 1

What is 1 + 1?

1
_2_
3
4
*** .hint This is a hint

*** .explanation This is an explanation

[Slidify Reminder about Knitting R]

'''{r, echo=TRUE, results='hide'}
- Shows code, but not the results
'''

[Very Quick Introduction to gh-pages]
touch Readme.md
git init
git add * # adds everything
git commit -m "first commit"
git remote add origin git@github.com:bcaffo/testPres.git
git push -u origin master

# Make the HTML viewable in github as HTML
git branch gh-pages
git checkout gh-pages
git push origin gh-pages
touch .nojekyll
git add .nojekyll
git commit -a -m "added a .nojekyll file"
git push origin gh-pages

bcaffo.github.io/testPres/testPres.html#/

- This is an example for Rpres files.
- The publish() command does all of this for slidify, though you can also follow this process.

-- Rstudio Presenter --

[RStudio Presenter 1 - Introduction and Getting Started]

RStudio Presentation
- RStudio created a presentation authoring tool within their development environment.
- If you are familiar with slidify, you will also be familiar with this tool
  + Code is authored in a generalized markdown format that allows for code chunks
  + The output is an html5 presentation
  + The file index for the presenter file is .Rpres, which gets converted to an .md file and then to an html file if desired
  + There is a preview tool in RStudio and GUIs for publishing to Rpubs or viewing/creating an html file

Authoring content
- This is a fairly complete guide
  + http://www.rstudio.com/ide/docs/presentations/overview
- Quick start is
  + file then New File then R Presentation
  + (alt-f then f then p if you want key strokes)
  + Use basically the same R markdown format for authoring as slidify/knitr
    * Single quotes for inline code
    * Tripple qutoes for block code
    * Same options for code evaluation, caching, hiding etcetera

[RStudio Presenter 2 - Authoring Details]

Compiling and tools
- R Studio auto formats and runs the code when you save the document
- Mathjax JS library is loaded by default so that $x^2$ yields $x^2$
- Slide navigation button on the preview; clicking on the notepad icon takes you to that slide in the deck
- Clicking on more yields options for
  + Clearning the knitr cache
  + Viewing in a browser (creates a temporay html file in AppData/local/temp for me)
  + Create a html file to save where you want
- A refresh button
- A zoom button that brings up a full window

Visuals
transition: linear
- R Studio has made it easy to get some cool html5 effects,
  like cube transitions with simple options in YAML-like code after
  the first slide such as transition: rotate
- You can specify it in a slide-by-slide basis

Here is the option "linear":

transition: linear

- Just put transition: linear right after the slide creation (three equal signs or more in a row)
- Transition options
  + http://www.rstudio.com/ide/docs/presentations/slide_transitions_and_navigation

Hierarchical organization:

type: section

- If you want a hierarchical organization structure, just add a type: typename option after the slide
- This changes the default appearance
  + http://www.rstudio.com/ide/docs/presentations/slide_transitions_and_navigation
- This is of type section

Here is a subsection:

type: subsection # basically a different slide theme than section (higher level)

Two columns:

- Do whatever for column one
- Then put *** on a line by itself with blank lines before and after

***

- Then do whatever for column two

Changing the slide font:

font-import: http://fonts.googleapis.com/css?family=Risque font-family: 'Risque'

- Add a font-family: fontname option after the slide
  + http://www.rstudio.com/ide/docs/presentations/customizing_fonts_and_appearance
- Specified in the same way as css font families
  + http://www.w3schools.com/cssref/css_websafe_fonts.asp
- Use font-import: url to import fonts
- Important caveats
  + Fonts must be present on the system that you are presenting on, or it will go to a fallback font
  + You have to be connected to the internet to use an imported font (so do not rely on this for offline presentations)
- This is the Risque
  + http://fonts.googleapis.com/css?family=Risque

Really changing things
- If you know html5 and CSS well, then you can basically change whatever you want
- A css file with the same names as your presentation will be autoimported
- You can use css: file.css to import a css file
- You have to create named classes and then use class: classname to get slide-specific style control from your css
  + (Or you can apply then within a <span>)
- Ultimately, you have an html file, that you can edit as you wish
  + This should be viewed as a last resort, as the whole point is to have reproducible presentations, but may be the easiest way to get the exact style control you want for a final product

[RStudio Presenter 3 - Discussion and Comparison with Slidify]

Slidify versus R Studio Presenter

Slidify
- Flexible control from the R MD file
- Under rapid ongoing development
- Large user base
- Lots and lots of styles and options
- Steeper learning curve
- More command-line oriented
- R Studio Presenter

***

R Studio Presenter
- Embedded in R Studio
- More GUI oriented
- Very easy to get started
- Smaller set of easy styles and options
- Default styles look very nice
- Ultimately as flexible as slidify with a little CSS and HTML knowledge

-- Week 3

-- R Packages --

[R Packages - Part 1]

What is an R Package?
- A mechanism for extending the basic functionality of R
- A collection of R functions, or other (data) objects
- Organized in a systematic fashion to provide a minimal amount of consistency
- Written by users/developers everywhere

Where are These R Packages?
- Primarily available from CRAN and Bioconductor
- Also available from GitHub, Bitbucket, Gitorious, etc. (and elsewhere)
- Packages from CRAN/Bioconductor can be installed with install.packages()
- Packages from GitHub can be installed using install_github() from the devtools package
  + You do not have to put a package on a central repository, but doing so makes it easier for others to install your package.

What is the Point?
- "Why not just make some code available?"
- Documentation / vignettes
- Centralized resources like CRAN
- Minimal standards for reliability and robustness
- Maintainability / extension
- Interface definition / clear API
- Users know that it will at least load properly

Package Development Process
- Write some code in an R script file (.R)
- Want to make code available to others
- Incorporate R script file into R package structure
- Write documentation for user functions
- Include some other material (examples, demos, datasets, tutorials)
- Package it up!

Package Development Process
- Submit package to CRAN or Bioconductor
- Push source code repository to GitHub or other source code sharing web site
- People find all kinds of problems with your code
  + Scenario 1: They tell you about those problems and expect you to fix it
  + Scenario 2: They fix the problem for you and show you the changes
- You incorporate the changes and release a new version

[R Packages - Part 2]

R Package Essentials
- An R package is started by creating a directory with the name of the R package
- A DESCRIPTION file which has info about the package
- R code! (in the R/ sub-directory)
- Documentation (in the man/ sub-directory)
- NAMESPACE (optional, but do it)
- Full requirements in Writing R Extensions

The DESCRIPTION File
- Package: Name of package (e.g. library(name))
- Title: Full name of package
- Description: Longer description of package in one sentence (usually)
- Version: Version number (usually M.m-p format)
- Author, Authors@R: Name of the original author(s)
- Maintainer: Name + email of person who fixes problems
- License: License for the source code

The DESCRIPTION File

These fields are optional but commonly used

- Depends: R packages that your package depends on
- Suggests: Optional R packages that users may want to have installed
- Date: Release date in YYYY-MM-DD format
- URL: Package home page
- Other fields can be added

DESCRIPTION File: gpclib # example

Package: gpclib
Title: General Polygon Clipping Library for R
Description: General polygon clipping routines for R based on Alan Murtas C library
Version: 1.5-5
Author: Roger D. Peng rpeng@jhsph.edu with contributions from Duncan Murdoch and Barry Rowlingson; GPC library by Alan Murta
Maintainer: Roger D. Peng rpeng@jhsph.edu
License: file LICENSE
Depends: R (>= 2.14.0), methods
Imports: graphics
Date: 2013-04-01
URL: http://www.cs.man.ac.uk/~toby/gpc/, http://github.com/rdpeng/gpclib

R Code
- Copy R code into the R/ sub-directory
- There can be any number of files in this directory
- Usually separate out files into logical groups
- Code for all functions should be included here and not anywhere else in the package

The NAMESPACE File
# Export
- Used to indicate which functions are exported
- Exported functions can be called by the user and are considered the public API
- Non-exported functions cannot be called directly by the user (but the code can be viewed)
- Hides implementation details from users and makes a cleaner package interface
# Import
- You can also indicate what functions you import from other packages
- This allows for your package to use other packages without making other packages visible to the user
- Importing a function loads the package but does not attach it to the search list

Key directives

- export("<function>") # make a function available for use to a user using your package
- import("<package>") # import a package
- importFrom("<package>", "<function>") # Import a specific function from a package

# Also important

- exportClasses("<class>")
- exportMethods("<generic>")

NAMESPACE File: mvtsplot package # example

export("mvtsplot") # this function you export
importFrom(graphics, "Axis") # only import the Axis function
import(splines) # import splines in its entirety (package)

NAMESPACE File: gpclib package # second example

export("read.polyfile", "write.polyfile")

importFrom(graphics, plot)

exportClasses("gpc.poly", "gpc.poly.nohole")

exportMethods("show", "get.bbox", "plot", "intersect”, "union”, "setdiff",
              "[", "append.poly", "scale.poly", "area.poly", "get.pts",
              "coerce", "tristrip", "triangulate")

Documentation
- Documentation files (.Rd) placed in man/ sub-directory
- Written in a specific markup language
- Required for every exported function
  + Another reason to limit exported functions
- You can document other things like concepts, package overview

Help File Example: line Function

# name, alias, title & description

\name{line}
\alias{line}
\alias{residuals.tukeyline}
\title{Robust Line Fitting}
\description{
  Fit a line robustly as recommended in \emph{Exploratory Data Analysis}.
}

# usage & arguments

\usage{
line(x, y)
}
\arguments{
  \item{x, y}{the arguments can be any way of specifying x-y pairs.  See
    \code{\link{xy.coords}}.}
}

# details & value

\details{
  Cases with missing values are omitted.

  Long vectors are not supported.
}
\value{
  An object of class \code{"tukeyline"}.

  Methods are available for the generic functions \code{coef},
  \code{residuals}, \code{fitted}, and \code{print}.
}

# references

\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis},
  Reading Massachusetts: Addison-Wesley.
}

Building and Checking
- R CMD build is a command-line program that creates a package archive file (.tar.gz)
- R CMD check runs a battery of tests on the package
- You can run R CMD build or R CMD check from the command-line using a terminal or command-shell application
- You can also run them from R using the system() function

# R-checks from within the R-console:

system("R CMD build newpackage")
system("R CMD check newpackage")

Checking
- R CMD check runs a battery of tests:
  + Documentation exists
  + Code can be loaded, no major coding problems or errors
  + Run examples in documentation
  + Check docs match code
  + All tests must pass to put package on CRAN

Getting Started
- The package.skeleton() function in the utils package creates a "skeleton" R package
- Directory structure (R/, man/), DESCRIPTION file, NAMESPACE file, documentation files
- If there are functions visible in your workspace, it writes R code files to the R/ directory
- Documentation stubs are created in man/
- You need to fill in the rest!

Summary
- R packages provide a systematic way to make R code available to others
- Standards ensure that packages have a minimal amount of documentation and robustness
- Obtained from CRAN, Bioconductor, Github, etc.

Summary - Workflow
- Create a new directory with R/ and man/ sub-directories (or just use package.skeleton())
- Write a DESCRIPTION file
- Copy R code into the R/ sub-directory
- Write documentation files in man/ sub-directory
- Write a NAMESPACE file with exports/imports
- Build and check

[Building R Packages Demo]

%*% == matrix multiplication

- The below uses Roxygen
  + Click on Build (only available when creating a package)
  + Go to Build Tools
  + Use Roxygen to generate:
    * Rd Files
    * Namespace file
    * Build & reload

#' Building a Model with Top Ten Features
#' @param x a n * p matrix of n observations
#' @return a vector of coefficiencts from the final fitted model with top 10 features
#' @author Roger Peng
#' @details this function yak yak yak
#' @seealso \code{lm}
#' @export so it exports the functions
#' @importFrom stats lm

Comments like those automatically create all the documentation (Namespace files) for your functions!

library(help = topten) # to view package info
topten # view written code
?topten # documentation file shown
?predictten # documentation file second function shown

Click 'check' to run rcommand check

[R Classes and Methods - Part 1]

Classes and Methods
- A system for doing object oriented programming
- R was originally quite interesting because it is both interactive and has a system for object orientation.
  + Other languages which support OOP (C++, Java, Lisp, Python, Perl) generally speaking are not interactive languages
- In R much of the code for supporting classes/methods is written by John Chambers himself (the creator of the original S language) and documented in the book Programming with Data: A Guide to the S Language
- A natural extension of Chambers’ idea of allowing someone to cross the user −→ programmer spectrum
- Object oriented programming is a bit different in R than it is in most languages — even if you are familiar with the idea, you may want to pay attention to the details

Two styles of classes and methods
- S3 classes/methods
  + Included with version 3 of the S language.
  + Informal, a little kludgey
  + Sometimes called old-style classes/methods
- S4 classes/methods
  + more formal and rigorous
  + Included with S-PLUS 6 and R 1.4.0 (December 2001)
  + Also called new-style classes/methods

Two worlds living side by side
- For now (and the forseeable future), S3 classes/methods and S4 classes/methods are separate systems (but they can be mixed to some degree).
- Each system can be used fairly independently of the other.
- Developers of new projects (you!) are encouraged to use the S4 style classes/methods.
  + Used extensively in the Bioconductor project
- But many developers still use S3 classes/methods because they are “quick and dirty” (and easier).
- In this lecture we will focus primarily on S4 classes/methods
- The code for implementing S4 classes/methods in R is in the methods package, which is usually loaded by default (but you can load it with library(methods) if for some reason it is not loaded)

Object Oriented Programming in R
- A class is a description of an thing. A class can be defined using setClass() in the methods package.
- An object is an instance of a class. Objects can be created using new().
- A method is a function that only operates on a certain class of objects.
- A generic function is an R function which dispatches methods. A generic function typically encapsulates a “generic” concept (e.g. plot, mean, predict, ...)
  + The generic function does not actually do any computation.
- A method is the implementation of a generic function for an object of a particular class.

Things to look up
- The help files for the ‘methods’ package are extensive — do read them as they are the primary documentation
- You may want to start with ?Classes and ?Methods
- Check out ?setClass, ?setMethod, and ?setGeneric
- Some of it gets technical, but try your best for now—it will make sense in the future as you keep using it.
- Most of the documentation in the methods package is oriented towards developers/programmers as these are the primary people using classes/methods

Classes

All objects in R have a class which can be determined by the class function

class(1) # numeric
class(TRUE) # logical
class(rnorm(100)) # numeric
class(NA) # logical
class("foo") # character

Data classes go beyond the atomic classes

x <- rnorm(100)
y <- x + rnorm(100)
fit <- lm(y ~ x)  ## linear regression model
class(fit) # lm

Generics/Methods in R
- S4 and S3 style generic functions look different but conceptually, they are the same (they play the same role).
- When you program you can write new methods for an existing generic OR create your own generics and associated methods.
- Of course, if a data type does not exist in R that matches your needs, you can always define a new class along with generics/methods that go with it

An S3 generic function (in the ‘base’ package)

The mean and print functions are generic

mean
print

# They find an appropriate method for any data type that is passed to them

S3 methods

The mean generic function has a number of methods associated with it.
methods("mean") #lists all methods

An S4 generic function

The show function is from the methods package and is the S4 equivalent of print
show

# The show function is usually not called directly (much like print) because objects are auto-printed.

S4 methods

showMethods("show") # lists all the methods of show

Generic/method mechanism

The first argument of a generic function is an object of a particular class (there may be other arguments)
1 - The generic function checks the class of the object.
2 - A search is done to see if there is an appropriate method for that class.
3 - If there exists a method for that class, then that method is called on the object and we’re done.
4 - If a method for that class does not exist, a search is done to see if there is a default method for the generic. If a default exists, then the default method is called.
5 - If a default method doesn’t exist, then an error is thrown.

Examining Code for Methods
- You cannot just print the code for a method like other functions because the code for the method is usually hidden.
- If you want to see the code for an S3 method, you can use the function getS3method.
- The call is getS3method(<generic>, <class>)
- For S4 methods you can use the function getMethod
- The call is getMethod(<generic>, <signature>) (more details later)

[R classes and Methods - Part 2]

S3 Class/Method: Example 1

What’s happening here?

set.seed(2)
x <- rnorm(100)
mean(x)

1 - The class of x is “numeric”
2 - But there is no mean method for “numeric” objects!
3 - So we call the default function for mean.

head(getS3method("mean", "default"), 10) # first 10 lines (head)
tail(getS3method("mean", "default"), 10 # last 10 lines (tail)

S3 Class/Method: Example 2

What happens here?

set.seed(3)
df <- data.frame(x = rnorm(100), y = 1:100)
sapply(df, mean)

1 - The class of df is "data.frame"; each column can be an object of a different class
2 - We sapply over the columns and call the mean function
3 - In each column, mean checks the class of the object and dispatches the appropriate method.
4 - We have a numeric column and an integer column; mean calls the default method for both

Calling Methods Directly
- Some S3 methods are visible to the user (i.e. mean.default),
- Never call methods directly
- Use the generic function and let the method be dispatched automatically.
- With S4 methods you cannot call them directly at all

S3 Class/Method: Example 3

The plot function is generic and its behavior depends on the object being plotted.

set.seed(10)
x <- rnorm(100)
plot(x)

For time series objects, plot connects the dots

set.seed(10)
x <- rnorm(100)
x <- as.ts(x) ## Convert to a time series object
plot(x)

# Because the class changed, it now uses a different method to plot, hence the result is different

Write your own methods!
- If you write new methods for new classes, you’ll probably end up writing methods for the following generics:
  + print/show
  + summary
  + plot
- There are two ways that you can extend the R system via classes/methods
  + Write a method for a new class but for an existing generic function (i.e. like print)
  + Write new generic functions and new methods for those generics

S4 Classes
- Why would you want to create a new class?
  + To represent new types of data (e.g. gene expression, space-time, hierarchical, sparse matrices)
  + New concepts/ideas that haven’t been thought of yet (e.g. a fitted point process model, mixed-effects model, a sparse matrix)
  + To abstract/hide implementation details from the user I say things are “new” meaning that R does not know about them (not that they are new to the statistical community).

S4 Class/Method: Creating a New Class
- A new class can be defined using the setClass function
  + At a minimum you need to specify the name of the class
  + You can also specify data elements that are called slots
  + You can then define methods for the class with the setMethod function Information about a class definition can be obtained with the showClass function

S4 Class/Method: Polygon Class

- Creating new classes/methods is usually not something done at the console; you likely want to save the code in a separate file

library(methods)
setClass("polygon",
         representation(x = "numeric",
                        y = "numeric"))

- The slots for this class are xand y
- The slots for an S4 object can be accessed with the @ operator.

- A plot method can be created with the setMethod function.
  + For setMethod you need to specify a generic function (plot), and a signature.
  + A signature is a character vector indicating the classes of objects that are accepted by the method.
  + In this case, the plot method will take one type of object, a polygon object.

- Creating a plot method with setMethod.

setMethod("plot", "polygon",
          function(x, y, ...) {
                  plot(x@x, x@y, type = "n", ...) # you call plot within the new method for polygons for plot (because it knows how the handle numerics)
                  xp <- c(x@x, x@x[1]) # with x@x you fetch the x slot of the polygon object called x in this function
                  yp <- c(x@y, x@y[1]) # with x@y you fetch the y slot of the polygon object called x in this function
                  lines(xp, yp)
      })

- Notice that the slots of the polygon (the x- and y-coordinates) are accessed with the @ operator.
- After calling setMethod the new plot method will be added to the list of methods for plot.

library(methods)
showMethods("plot")

- Notice that the signature for class polygon is listed. The method for ANY is the default method and it is what is called when now other signature matches

p <- new("polygon", x = c(1, 2, 3, 4), y = c(1, 2, 3, 1)) # create a polygon object
plot(p) # which the generic function plot() now knows how to handle

Summary
- Developing classes and associated methods is a powerful way to extend the functionality of R
- Classes define new data types
- Methods extend generic functions to specificy the behavior of generic functions on new classes
- As new data types and concepts are created, classes/methods provide a way for you to develop an intuitive interface to those data/concepts for users

Where to Look, Places to Start
- The best way to learn this stuff is to look at examples
- There are quite a few examples on CRAN which use S4 classes/methods. You can usually tell if they use S4 classes/methods if the methods package is listed in the Depends: field
- Bioconductor (http://www.bioconductor.org) — a rich resource, even if you know nothing about bioinformatics
- Some packages on CRAN (as far as I know) — SparseM, gpclib, flexmix, its, lme4, orientlib, filehash
- The stats4 package (comes with R) has a bunch of classes/methods for doing maximum likelihood analysis. ￼￼￼￼￼

Quiz
1 -  Description file - vignette - unit tests?
2 - mean
3 - getMethod()
4 - param, return, export, examples, createMean(x)



