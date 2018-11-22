fitnessData<- read.csv("~/Fitness/Portfolio Project/Workout-Visualizations/FitNotes_Export_2018_10_12_15_16_59.csv")
library(networkD3)

fitnessData
fitnessData$Date = as.Date(fitnessData$Date, format = "%m/%d/%Y")
fitnessData

fitnessData2018 = fitnessData[fitnessData$Date > "2017-12-31",]

totalReps = data.frame(aggregate(fitnessData2018$Reps, by = list(Exercise = fitnessData2018$Exercise), FUN = sum))

totalReps

bodyCategories = data.frame(unique(fitnessData2018$Category))

totalReps$ID = 0:(nrow(totalReps)-1)

nodes = data.frame("name" = totalReps$Category)

links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from. 
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

###############################################################
## This Stuff Works
###############################################################
nodes = data.frame("name" =
                     c("Node A", # Node 0
                       "Node B", # Node 1
                       "Node C", # Node 2
                       "Node D"))# Node 3
links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from.
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

