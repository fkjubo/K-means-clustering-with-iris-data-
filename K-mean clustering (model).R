# Importing Irish data

data <- read.csv("iris.csv", T, ",")

# changing the column name

names(data)<- c("Id", "SepalL", "SepalW", "PetalL", "PetalW", "Species")

str(data)
summary(data)

# deleting first column

data <- data[,-1]

# installing packages for visualize and understand the data

library(ggplot2)
library(dplyr)

Iris_setosa <- data %>%
  filter(Species == "Iris-setosa") %>%
  ggplot(aes(x= SepalL, y= SepalW)) +
  geom_point()

Iris_versicolor <- data %>%
  filter(Species == "Iris-versicolor") %>%
  ggplot(aes(x= SepalL, y= SepalW)) +
  geom_point()

Iris_virginica <- data %>%
  filter(Species == "Iris-virginica") %>%
  ggplot(aes(x= SepalL, y= SepalW)) +
  geom_point()

ggplot(Iris_setosa, aes(x= PetalL, y= PetalW)) +
  geom_point()

ggplot(Iris_versicolor, aes(x= PetalL, y= PetalW)) +
  geom_point()

ggplot(Iris_virginica, aes(x= PetalL, y= PetalW)) +
  geom_point()

# normalizing data

data.N<- data[,-5]
data.mean <- apply(data.N, 2, mean)
data.sd <- apply(data.N, 2, sd)

data.N <- scale(data.N, data.mean, data.sd)

# calculating Euclidean distance

distance <- dist(data.N)

print(distance, digits = 3)

# Scree plot for selecting optimal K

SP <- (nrow(data.N)-1)*sum(apply(data.N, 2, var))
for(i in 2:20) SP[i]<- sum(kmeans(data.N, centers = i)$withinss)
plot(1:20, SP, type = "b", xlab = "Number of clusters", ylab = "Within Group")

#K-means

model <- kmeans(data.N, 3)

# visulazing data

plot(SepalL~SepalW, data.N, col= model$cluster)
plot(PetalL~PetalW, data.N, col= model$cluster)
