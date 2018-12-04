########################################
# CompBio Presentations Day 1          #
# 27 November 2018                     #
########################################

#PCA using FactoMineR and factoextra -----------------------------------------------

#load packages and data
library(FactoMineR)
library(factoextra)

data("iris")
head(iris)

iris2 <- iris[,1:4]

#Run the PCA
iris.pca <- PCA(iris2, scale.unit = T, graph = F)

#Look at eigenvalues
iris.pca$eig

#scree plot for visualization
fviz_screeplot(iris.pca, ncp = 4)
#NCP is the number of PCs to show

#make a factor map
plot.PCA(iris.pca, axes = c(1,2), choix = "var")

#biplot
fviz_pca(iris.pca)
#That's a mess

#Clean it up with a scale showing relative contributions
fviz_pca_var(iris.pca, col.var = "contrib")+
  scale_color_gradient2(low = "blue", mid = "steelblue", high = "red", 
                        midpoint = 25)+
  theme_bw()

#individual data points without labels
fviz_pca_ind(iris.pca, label = "none")

#add some color to make clusters more informative
fviz_pca_ind(iris.pca, label = "none", habillage = iris$Species, addEllipses = T,
             ellipse.level = 0.95)

fviz_pca_biplot(iris.pca, geom.ind = "point", fill.ind = iris$Species, 
                col.ind = "black", addEllipses = T, col.var = "contrib")

#Creating web-based graphs with plotly----------------------------------
#load packages and data
library(plotly)

data(mpg)

#Make the graph
p <- plot_ly(data = mpg, x = ~displ, y = ~cty)

#plotly also interacts with ggplot with ggplotly
p2 <- ggplot(data = mpg, aes(x = displ, y = cty, color = manufacturer))+
  geom_point()

ggplotly(p2)

#work with more data
tx <- txhousing
head(txhousing)

allCities <- txhousing %>%
  group_by(city) %>%
  plot_ly(x = ~date, y = ~median)

#3d plots
plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length, 
        type = "scatter3d", mode = "markers", size = ~Petal.Width, color = ~Species)

#Animations
df <- data.frame(x = c(1:5, 4:1), y = c(1:5, 4:1), f = (1:9))

p <- plot_ly(data = df, x = ~x, y = ~y, frame = ~f, type = "scatter", 
             transition = ~f, mode = "markers", showleged = T)
