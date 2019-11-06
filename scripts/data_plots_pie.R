# Read CSV into R
electionData <- read.csv(file="F:/SI/survey/si-election-2019-survey/responses/responses_preprocessed_2.csv", header=TRUE, sep=",")
#head(electionData)
mytable <- table(electionData$Voted_Candidate)

#plot pie chart for the voted candidates
pct <- round(mytable/sum(mytable)*100,2)
lbls <- paste(names(mytable),"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c('red', 'yellow', 'blue', 'pink','brown','green','violet')
pie(mytable, main="Voted Candidates",labels = lbls, col=colors)



