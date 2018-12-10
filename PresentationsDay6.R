###################################
# Presentations Day 6             #
# Dec 6 2018                      #
###################################

# CARET: Classification And REgression Training -----------------------------------
library(caret)
library(patchwork)
library(tidyverse)

set.seed(1235)

#Load data
load("wine.rda")
colnames(wine) <- make.names(colnames(wine))

str(wine)

wine.long <- gather(wine, attr, val, -class)

#Fitting a model...
#Prepare data
#Partition into training and testing set, preserve proportion of classes
part <- createDataPartition(wine$class, p = 0.8, list = F)

train <- wine[part, ]
test <- wine[-part, ]
#After making these, you want to check the proportions of each class again

#train the model
rpart.model.wine <- train(class ~. , data = train, method = "rpart")

#make predictions
rpart.wine.predict <- predict(rpart.model.wine, newdata = test)
head(rpart.wine.predict)

#Build a better model by changing the defaults: do some preprocessing
#Standardize the data
prepross.values <- preProcess(train, method = c("center", "scale"))
train <- predict(prepross.values, train)
test <- predict(prepross.values, test)
head(train)

#Specify training parameters
modelLookup("nnet")
param.grid <- expand.grid(decay = c(0.8, 0.2, 0.05), size = c(1,3,5))
resample <- trainControl(method = "LGOCV")
net.model.wine <- train(class~., data = train, method = "nnet",
                        tuneGrid = param.grid, trControl = resample, maxit = 200)

net.wine.predict <- predict(net.model.wine, newdata = test)

#A better way to view predictions
confusionMatrix(net.wine.predict, test$class)

# Figuring out which variables matter
imp <- varImp(net.model.wine)

imp.df <- data.frame("attr" = rownames(imp[[1]]),imp = imp[[1]][,1])

ggplot(wine.long, aes(x = class, y = scale(val), col = class))+
  geom_boxplot()+
  facet_wrap(.~attr, scale = "free")

# Visualizing genomic data using Sushi -----------------------------------------
library(Sushi)
Suschi_data <- data(package = "Sushi")
data(list = Suschi_data$results[,3])

head(Sushi_DNaseI.bedgraph)

chrom <- "chr11"
chrostart <- 1650000
crhomend <- 2350000

plotBedgraph(Sushi_DNaseI.bedgraph, chrom, chromstart = chrostart, 
             chromend = crhomend)
