# Classify the training set
# Should normally be done manually


# LOAD

# Load sentences that should be classified
sentences <- read.table("train.csv", sep = ";",
					header = TRUE,
					stringsAsFactors = FALSE)

# Load lists/trainOld.csv
sentencesOld <- read.table("lists/trainOld.csv", sep = ";",
					header = TRUE,
					stringsAsFactors = FALSE)
					

# CLASSIFY

for (i in 1:nrow(sentences)){
	label <- sentencesOld$label[which(sentencesOld$sent == sentences$sent[i])]
	if (length(label) == 1)	sentences$label[i] <- label
	else					sentences$label[i] <- ""
}



# DATA WRITING

if (writeSwitch){
	write.csv2(sentences, file="train.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)
}