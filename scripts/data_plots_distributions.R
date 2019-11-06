# library
library(ggplot2)

# load dataset
electionData <- read.csv(file="F:/SI/survey/si-election-2019-survey/responses/responses_preprocessed_2.csv", header=TRUE, sep=",")
candidate <- table(electionData$Voted_Candidate)

# Stacked
#ggplot(electionData, aes(fill=candidate, y=candidate, x=education)) + 
  #geom_bar(position="stack", stat="identity")

# Age Distribution
ages <- table(electionData$Age)
barplot(ages, main="Age Distribution", col=c("darkblue"),
        xlab="Age",ylab="Frequency")

# Education Distribution
education <- table(electionData$Highest_Educational_Qualification)
barplot(education, main="Education Distribution", col=c("darkblue"),
        xlab="Education",ylab="Frequency", cex.names=0.8 , las=2)

# Religion Distribution
Religion<- table(electionData$Religion)
barplot(Religion, main="Religion Distribution", col=c("darkblue"),
        xlab="Religion",ylab="Frequency", cex.names=0.8)

# Race Distribution
Race <- table(electionData$Race)
barplot(Race, main="Race Distribution", col=c("darkblue"),
        xlab="Race",ylab="Frequency", cex.names=0.8)

# Occupation Distribution
Occupation <- table(electionData$Occupation)
barplot(Occupation, main="Occupation Distribution", col=c("darkblue"),
        xlab="Occupation",ylab="Frequency", cex.names=0.8 , las=2)

# Distrct Distribution
District<- table(electionData$District)
barplot(District, main="District Distribution", col=c("darkblue"),
        xlab="District",ylab="Frequency", cex.names=0.8 , las=2)