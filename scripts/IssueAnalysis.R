# Read CSV into R
electionData <- read.csv(file="../responses/responses_preprocessed_2.csv", header=TRUE, sep=",")
#head(electionData)
issue_data <- electionData$Most_Important_Issue_to_be_Addressed

issue = vector()
for (i in 1:length(issue_data)) {
  lapply(issue_data[i], tolower)
  issue <- c(issue, unlist(strsplit(as.character(lapply(issue_data[i], tolower)), ", ", fixed=TRUE)))
  }

issue_table = table(issue)
#plot bar chart for the most importatnt issue
par(mar=c(3, 15, 3, 1))
barplot(issue_table, main="Issue Distribution", col=c("darkblue"),
        xlab="Frequency", las=1, horiz=TRUE)
