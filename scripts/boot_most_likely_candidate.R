library(boot)

# Read CSV into R
electionData <- read.csv(file="D:/Nuwani/MSc/Stat/si-election-2019-survey/responses/responses_preprocessed_2.csv", header=TRUE, sep=",")
#filter Sinhala Buddhist Votes
#votedCandidate = electionData[electionData$Race == 'Sinhalese',]
#votedCandidate = votedCandidate[votedCandidate$Religion == 'Buddhist',]

mytable <- table(electionData$Winning_Party_Guess)

#plot pie chart for the winning party guess
pct <- round(mytable/sum(mytable)*100,2)
lbls <- paste(names(mytable),"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c('red', 'yellow', 'blue', 'pink','brown')
pie(mytable, main="Most Likely Party to Win",labels = lbls, col=colors)

#calculate the proportitions for each candidate
total_count = nrow(electionData)
gotabaya_count  = nrow(electionData[electionData$Winning_Party_Guess == 'Sri Lanka Podujana Peramuna (SLPP)',])
gotabaya_s = c(rep(1, gotabaya_count), rep(0, total_count-gotabaya_count)) 

sajith_count  = nrow(electionData[electionData$Winning_Party_Guess == 'New Democratic Front (NDF)',])
sajith_s = c(rep(1, sajith_count), rep(0, total_count-sajith_count)) 

anura_count  = nrow(electionData[electionData$Winning_Party_Guess == 'National People\'s Party (NPP)',])
anura_s = c(rep(1, anura_count), rep(0, total_count-anura_count)) 

mahesh_count  = nrow(electionData[electionData$Winning_Party_Guess == 'National People\'s Movement (NPM)',])
mahesh_s = c(rep(1, mahesh_count), rep(0, total_count-mahesh_count)) 

other_count  = nrow(electionData[electionData$Winning_Party_Guess == 'Other',])
other_s = c(rep(1, other_count), rep(0, total_count-other_count))

mean_stat <- function(data, indices){
  dt<-data[indices]
  return (mean(dt))
}

gotabaya_votes_b = boot(gotabaya_s, mean_stat, R=1000)
plot(gotabaya_votes_b)
print("BOOTSTRAP CONFIDENCE INTERVAL - GOTABAYA - SLPP")
boot.ci(gotabaya_votes_b, conf=0.95, type="bca")

sajith_votes_b = boot(sajith_s, mean_stat, R=1000)
plot(sajith_votes_b)
print("BOOTSTRAP CONFIDENCE INTERVAL - SAJITH - NDF")
boot.ci(sajith_votes_b, conf=0.95, type="bca")

anura_votes_b = boot(anura_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - ANURA - NPP")
boot.ci(anura_votes_b, conf=0.95, type="bca")

mahesh_votes_b = boot(mahesh_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - Mahesh - NPM")
boot.ci(mahesh_votes_b, conf=0.95, type="bca")

other_votes_b = boot(other_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - OTHER")
boot.ci(other_votes_b, conf=0.95, type="bca")

