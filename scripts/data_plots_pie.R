# Read CSV into R
electionData <- read.csv(file="F:/SI/survey/si-election-2019-survey/responses/responses_preprocessed_2.csv", header=TRUE, sep=",")
#head(electionData)
winnertable <- table(electionData$Voted_Candidate)

#plot pie chart for the voted candidates
pct <- round(winnertable/sum(winnertable)*100,2)
lbls <- paste(names(winnertable),"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c('red', 'yellow', 'blue', 'pink','brown','green','violet')
pie(winnertable, main="Voted Candidates",labels = lbls, col=colors)


partytable <- table(electionData$Winning_Party_Guess)

#plot pie chart for the opinion on the most likely party to win the Presidential Election
pct2 <- round(partytable/sum(partytable)*100,2)
lbls2 <- paste(names(partytable), pct2) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels
colors2 <- c('yellow','red', 'green', 'pink','blue','brown','violet')
pie(partytable, main="Opinion on the Most Likely Party to Win the Presidential Election",labels = lbls2, col=colors2)






