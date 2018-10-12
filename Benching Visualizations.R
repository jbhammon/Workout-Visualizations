benchingVol <- read.csv("~/Fitness/Portfolio Project Data/Benching Volumes by Week.csv")
benchingVol$Percent.of.Max <- as.factor(benchingVol$Percent.of.Max)
library(ggplot2)

myPlot <- ggplot(data=benchingVol, aes(x=Week, y=Total.weight, group=Percent.of.Max)) + geom_line(aes(color = Percent.of.Max)) + geom_point(aes(color = Percent.of.Max)) + theme_minimal()  + theme(legend.position="bottom")

myPlot <- myPlot + ggtitle("Trending My Total Weight Benched by Workout") + xlab("Date") + ylab("Total Weight Benched that Day")

myPlot