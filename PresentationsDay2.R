########################################
# CompBio Presentations Day 2          #
# Marathon Day                         #
# Nov. 28 2018                         #
########################################

# Interactive Dashboards with Shiny -------------------------------------------
library(shiny)

# Define the user interface
ui <- fluidPage(#This function changes display to fit various devices
  titlePanel("This is a test"),
  sidebarLayout(position = "right", 
                sidebarPanel(h1("This is a header"), 
                             selectInput(inputId = "X", label = "X", names(trees))),
                mainPanel(h1 = "header")
  )
)

#Define the server
server <- function(input, output){
  selectrData <- reactive( #reactive function allows users to manipulate part of app
    trees[,input$x]
  )
}

#Run the app
shinyApp(ui = ui, server = server)

#shiny also works with plotly, so graphs in the app can have interactive points

# Displaying/annotating phylo trees with ggtree -------------------------------
library(ggtree)

tree <- rtree(n = 13, tip.label = LETTERS[seq(1:13)])

ggtree(tree)

#Can also add nodes and annotations associated with them
ggtree(tr = tree, layout = "fan")+ #changing layout changes appearance
  geom_cladelabel(node = 21, label = "a label", offset = 0.5)
  
# Comparing data frames with diffdf -------------------------------------------
library(diffdf)
library(tidyverse)

x <- presidential

#changing data
y <- x
y[10,1] <- "Gore"
y[10,4] <- "Democratic"

diffdf(x,y)

#omitting data
y <- head(x, nrow(x) - 2)

diffdf(x,y)

#differently sorted objects
y <- arrange(x, name)

diffdf(x,y) #this freaks out

diffdf(x,y,keys = "start") #keys argument is how first matrix is sorted
#using keys argument automatically sorts data by specified key

#store rows with differences
y <- x
y[10,1] <- "Gore"
y[10,4] <- "Democratic"

xydiff <- diffdf(x,y) #this is the right output, but it isn't useful

#This gives you a useful output
wrongrow <- diffdf_issuerows(y, xydiff)
wrongrow 
rightrow <- diffdf_issuerows(x, xydiff)
rightrow

#replace wrong data with right
y[10,] <- rightrow

# 3d graphics with plot 3d ----------------------------------------------------
library(plot3D)
library(plot3Drgl)
library(car)

data(airquality)
head(airquality)

x <- airquality$Temp
y <- airquality$Wind
z <- airquality$Day

#basic 3d scatter plot
scatter3D(x,y,z)

#modify plot appearance
scatter3D(x,y,z, bty = "u", cex = 0.5, col = ramp.col(c("lightgray","black")), 
          col.grid = "black",legend = F)
#bty changes the axes
#setting bty to u makes box user-defined

#confidence intervals
myCI <- list(x = matrix(nrow = length(z), data = rep(0.1, 2*length(z))))
scatter3D(x,y,z, bty = "b2", CI = myCI)           

#regression plane
fit <- lm(z~x+y)
grid.lines <- 25
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)

xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), nrow = grid.lines, 
                 ncol = grid.lines)

fitpoints <- predict(fit)

scatter3D(x,y,z, bty = "b2", surf = list(x= x.pred, y = y.pred, z = z.pred,
                                         facets = NA, fit = fitpoints))

plotrgl()

scatter3d(x,y,z, surface = F)
