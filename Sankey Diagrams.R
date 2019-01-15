## Functions

# This function takes the total data being manipulated and next category to be pulled out and
# summarized, and returns the summarized data and the appropriate toID/fromID values to let it
# be included in the Sankey Diagram. It's then expected the df that's returned will be tacked onto
# the other categories that have been summarized with rbind()

# totalDataDF - df for which all the data is being plotted, in this case it's all the data from 2018
# nextIndexID - the next index value to use for new ribbons being added to the Sankey Diagram
# muscle      - string for the next muscle or body area being summarized and returned
volumeBuilder <- function(totalDataDF, nextIndexID, muscle) {
  
  totalDataDF = totalDataDF[totalDataDF$Category == muscle,]
  newData = data.frame(aggregate(totalDataDF$Weight..lbs., by = list(Exercise = totalDataDF$Exercise), FUN = sum))
  
  newData$Category = muscle
  newData$fromID = nextIndexID:(nrow(newData) - 1 + nextIndexID)
  newData$toID = max(newData$fromID) + 1
  
  return(newData)
}

# This function pulls out the actual text of the exercise names to be added to the
# nodes df, so that they can be displayed in the Sankey diagram

# totalDataDF - df for which all the data is being plotted, in this case it's all the data from 2018
# nodes       - df that's holding all the names indexed so far
# muscle      - string for the next muscle or body area being summarized and returned
nodesBuilder <- function(totalDataDF, nodes, muscle) {
  
  totalDataDF = totalDataDF[totalDataDF$Category == muscle,]
  newData = data.frame(aggregate(totalDataDF$Weight..lbs., by = list(Exercise = totalDataDF$Exercise), FUN = sum))
  
  nodes = rbind(nodes, data.frame("name" = newData$Exercise), data.frame("name" = muscle))
  return(nodes)
  
}

library(networkD3)

## Read in and prep the data
fitnessData<- read.csv("~/Fitness/Portfolio Project/Workout-Visualizations/FitNotes_Export_Latest.csv")
fitnessData$Date = as.Date(fitnessData$Date, format = "%m/%d/%Y")
data2018 = fitnessData[fitnessData$Date > "2017-12-31",]
data2018 = data2018[data2018$Weight..lbs. > 0,]

# Create column for reps * weight
# Sum up that new column for each exercise
# Pare down to Exercise, Category, and Sum

# Initialize DFs to hold data as categories are added
totalVolume = data.frame(Exercise = factor(), x = numeric(), Category = character(), fromID = integer(), toID = numeric())
nodes = data.frame(name = factor())

#indexReference = matrix(c(0, "biceps", 1, "triceps", 2, "shoulders", 3, "back", 4, "chest", 5, "abs", 6, "quads", 7, "calfs", 8, "arms", 9, "legs", 10, "upper body", 11, "total"), byrow=TRUE, ncol=2)

# Adding data for each category
totalVolume = rbind(totalVolume, volumeBuilder(data2018, 0, "Biceps"))
totalVolume = rbind(totalVolume, volumeBuilder(data2018, max(totalVolume$toID)+1, "Triceps"))
totalVolume = rbind(totalVolume, volumeBuilder(data2018, max(totalVolume$toID)+1, "Shoulders"))
totalVolume = rbind(totalVolume, volumeBuilder(data2018, max(totalVolume$toID)+1, "Back"))
totalVolume = rbind(totalVolume, volumeBuilder(data2018, max(totalVolume$toID)+1, "Chest"))
totalVolume = rbind(totalVolume, volumeBuilder(data2018, max(totalVolume$toID)+1, "Legs"))

# Add names for each category
nodes = nodesBuilder(data2018, nodes, "Biceps")
nodes = nodesBuilder(data2018, nodes, "Triceps")
nodes = nodesBuilder(data2018, nodes, "Shoulders")
nodes = nodesBuilder(data2018, nodes, "Back")
nodes = nodesBuilder(data2018, nodes, "Chest")
nodes = nodesBuilder(data2018, nodes, "Legs")

###############################################################
## Creating and saving the Sankey Diagram
links = as.data.frame(matrix(c(totalVolume$fromID, totalVolume$toID, totalVolume$x), ncol = 3))

names(links) = c("source", "target", "value")
myNetwork = sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)
saveNetwork(myNetwork, file = 'SankeyDiagram.html', selfcontained = FALSE)
###############################################################

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

