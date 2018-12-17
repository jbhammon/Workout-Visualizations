fitnessData<- read.csv("~/Fitness/Portfolio Project/Workout-Visualizations/FitNotes_Export_Latest.csv")
library(networkD3)

fitnessData$Date = as.Date(fitnessData$Date, format = "%m/%d/%Y")
fitnessData

data2018 = fitnessData[fitnessData$Date > "2017-12-31",]

nextCategory = data2018[data2018$Category == "Biceps",]
totalVolume = data.frame(aggregate(nextCategory$Weight..lbs., by = list(Exercise = nextCategory$Exercise), FUN = sum))

totalVolume$fromID = 0:(nrow(totalVolume)-1)
totalVolume$toID = max(totalVolume$fromID) + 1

nodes = rbind(data.frame("name" = totalVolume$Exercise), data.frame("name" = "Biceps"))

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

