workoutExport <- read.csv("~/Fitness/Portfolio Project/Workout-Visualizations/FitNotes_Export_2018_11_21_12_37_10.csv")
library(ggplot2)

benchReps = workoutExport[workoutExport$Exercise == "Flat Barbell Bench Press",c(1,4,5)]
percentMax70 = benchReps[seq(1, nrow(benchReps), 9), ]
numWeeks = nrow(percentMax70)
percentMax70 = rbind(percentMax70, benchReps[seq(2, nrow(benchReps), 9), ])
percentMax70 = rbind(percentMax70, benchReps[seq(3, nrow(benchReps), 9), ])
percentMax70$weekNum = c(seq(1:numWeeks), seq(1:numWeeks), seq(1:numWeeks))
percentMax70$percentile = rep(1, each=nrow(percentMax70))
percentMax70$totalWeight = percentMax70$Weight..lbs. * percentMax70$Reps
percentMax70 = aggregate(percentMax70$totalWeight, by=list(Week=percentMax70$weekNum), FUN=sum)
percentMax70$percentile = rep(1, each=nrow(percentMax70))

percentMax75 = benchReps[seq(4, nrow(benchReps), 9), ]
numWeeks = nrow(percentMax75)
percentMax75 = rbind(percentMax75, benchReps[seq(5, nrow(benchReps), 9), ])
percentMax75 = rbind(percentMax75, benchReps[seq(6, nrow(benchReps), 9), ])
percentMax75$weekNum = c(seq(1:numWeeks), seq(1:numWeeks), seq(1:numWeeks))
percentMax75$percentile = rep(2, each=nrow(percentMax75))
percentMax75$totalWeight = percentMax75$Weight..lbs. * percentMax75$Reps
percentMax75 = aggregate(percentMax75$totalWeight, by=list(Week=percentMax75$weekNum), FUN=sum)
percentMax75$percentile = rep(2, each=nrow(percentMax75))

percentMax80 = benchReps[seq(7, nrow(benchReps), 9), ]
numWeeks = nrow(percentMax80)
percentMax80 = rbind(percentMax80, benchReps[seq(8, nrow(benchReps), 9), ])
percentMax80 = rbind(percentMax80, benchReps[seq(9, nrow(benchReps), 9), ])
percentMax80$weekNum = c(seq(1:numWeeks), seq(1:numWeeks), seq(1:numWeeks))
percentMax80$totalWeight = percentMax80$Weight..lbs. * percentMax80$Reps
percentMax80 = aggregate(percentMax80$totalWeight, by=list(Week=percentMax80$weekNum), FUN=sum)
percentMax80$percentile = rep(3, each=nrow(percentMax80))


benchReps = rbind(percentMax70, percentMax75)
benchReps = rbind(benchReps, percentMax80)
benchReps$percentile = as.factor(benchReps$percentile)

myPlot <- ggplot(data=benchReps, aes(x=Week, y=x, group=percentile)) + geom_line(aes(color = percentile)) + geom_point(aes(color = percentile)) + theme_minimal()  + theme(legend.position="bottom")

myPlot <- myPlot + ggtitle("Tracking my Bench Press Progress") + xlab("Week Number") + ylab("Pounds Benched")

myPlot <- myPlot + scale_x_discrete(limits=c(seq(1:max(benchReps$Week))))

myPlot <- myPlot + scale_color_hue(labels = c("70%", "75%", "80%")) 

myPlot