## Functions
# totalDataDF need to be instantiated DFs when this is called
volumeBuilder <- function(totalDataDF, nextIndexID, muscle) {
  
  totalDataDF = totalDataDF[totalDataDF$Category == muscle,]
  newData = data.frame(aggregate(totalDataDF$Weight..lbs., by = list(Exercise = totalDataDF$Exercise), FUN = sum))
  
  newData$Category = muscle
  newData$fromID = nextIndexID:(nrow(newData) - 1 + nextIndexID)
  newData$toID = max(newData$fromID) + 1
  
  return(newData)
}

nodesBuilder <- function(totalDataDF, nodes, muscle) {
  
  totalDataDF = totalDataDF[totalDataDF$Category == muscle,]
  newData = data.frame(aggregate(totalDataDF$Weight..lbs., by = list(Exercise = totalDataDF$Exercise), FUN = sum))
  
  nodes = rbind(nodes, data.frame("name" = newData$Exercise), data.frame("name" = muscle))
  return(nodes)
  
}

## Read in Data
fitnessData<- read.csv("~/Fitness/Portfolio Project/Workout-Visualizations/FitNotes_Export_Latest.csv")
library(networkD3)

fitnessData$Date = as.Date(fitnessData$Date, format = "%m/%d/%Y")

data2018 = fitnessData[fitnessData$Date > "2017-12-31",]

totalVolume = data.frame(Exercise = factor(), x = numeric(), Category = character(), fromID = integer(), toID = numeric())
totalVolume = rbind(totalVolume, volumeBuilder(data2018, 0, "Biceps"))
##
totalVolume = rbind(totalVolume, volumeBuilder(data2018, max(totalVolume$toID)+1, "Triceps"))
##

nodes = data.frame(name = factor())
nodes = nodesBuilder(data2018, nodes, "Biceps")
##
nodes = nodesBuilder(data2018, nodes, "Triceps")
##

links = as.data.frame(matrix(c(totalVolume$fromID, totalVolume$toID, totalVolume$x), ncol = 3))

names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

###############################################################
## This Stuff Works
##
## Saving as a reference for using sankeyNetwork()
###############################################################
# nodes = data.frame("name" =
#                      c("Node A", # Node 0
#                        "Node B", # Node 1
#                        "Node C", # Node 2
#                        "Node D"))# Node 3
# links = as.data.frame(matrix(c(
#   0, 1, 10, # Each row represents a link. The first number
#   0, 2, 20, # represents the node being conntected from.
#   1, 3, 30, # the second number represents the node connected to.
#   2, 3, 40),# The third number is the value of the node
#   byrow = TRUE, ncol = 3))
# names(links) = c("source", "target", "value")
# sankeyNetwork(Links = links, Nodes = nodes,
#               Source = "source", Target = "target",
#               Value = "value", NodeID = "name",
#               fontSize= 12, nodeWidth = 30)

