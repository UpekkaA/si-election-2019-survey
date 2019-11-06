library(boot)

# Read CSV into R
electionData <- read.csv(file="D:/Nuwani/MSc/Stat/si-election-2019-survey/responses/responses_preprocessed_2.csv", header=TRUE, sep=",")
#filter Sinhala Buddhist Votes
votedCandidate = electionData[electionData$Race == 'Sinhalese',]
votedCandidate = votedCandidate[votedCandidate$Religion == 'Buddhist',]

mytable <- table(votedCandidate$Voted_Candidate)

#plot pie chart for the voted candidates of Sinhala Buddhist Responses
pct <- round(mytable/sum(mytable)*100,2)
lbls <- paste(names(mytable),"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c('red', 'yellow', 'blue', 'pink','brown','green','violet')
pie(mytable, main="Voted Candidates of Sinhala Buddhist Responses",labels = lbls, col=colors)

#calculate the proportitions for each candidate
total_count = nrow(votedCandidate)
gotabaya_count  = nrow(votedCandidate[votedCandidate$Voted_Candidate == 'Gotabhaya Rajapaksha (SLPP)',])
gotabaya_s = c(rep(1, gotabaya_count), rep(0, total_count-gotabaya_count)) 

sajith_count  = nrow(votedCandidate[votedCandidate$Voted_Candidate == 'Sajith Premadasa (NDF)',])
sajith_s = c(rep(1, sajith_count), rep(0, total_count-sajith_count)) 

anura_count  = nrow(votedCandidate[votedCandidate$Voted_Candidate == 'Anura Kumara Dissanayake (NPP)',])
anura_s = c(rep(1, anura_count), rep(0, total_count-anura_count)) 

mahesh_count  = nrow(votedCandidate[votedCandidate$Voted_Candidate == 'General Mahesh Senanayake (NPM)',])
mahesh_s = c(rep(1, mahesh_count), rep(0, total_count-mahesh_count)) 

other_count  = nrow(votedCandidate[votedCandidate$Voted_Candidate == 'Other',])
other_s = c(rep(1, other_count), rep(0, total_count-other_count))

mean_stat <- function(data, indices){
  dt<-data[indices]
  return (mean(dt))
}

gotabaya_votes_b = boot(gotabaya_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - GOTABAYA")
boot.ci(gotabaya_votes_b, conf=0.95, type="bca")

sajith_votes_b = boot(sajith_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - SAJITH")
boot.ci(sajith_votes_b, conf=0.95, type="bca")

anura_votes_b = boot(anura_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - ANURA")
boot.ci(anura_votes_b, conf=0.95, type="bca")

mahesh_votes_b = boot(mahesh_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - Mahesh")
boot.ci(mahesh_votes_b, conf=0.95, type="bca")

other_votes_b = boot(other_s, mean_stat, R=1000)
print("BOOTSTRAP CONFIDENCE INTERVAL - OTHER")
boot.ci(other_votes_b, conf=0.95, type="bca")

