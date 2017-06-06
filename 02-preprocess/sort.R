# Load files with results of preprocess.R
# Make a list of unque sentences, ranked by frequency

# INPUT

# More switches
writeSwitch <- TRUE
plotSwitch  <- FALSE

# File with results of preprocess.R
F1 = "preprocessed.csv"



# DATA LOADING

M <- read.csv(F1, header = TRUE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)



# PROCESSING

# Make table
Mtable <- as.data.frame(table(M$preprocessed))

# Sort table by frequency
Mtable <- Mtable[order(Mtable$Freq, decreasing = T), ]

# Format: label | sent | frequency
# label column is empty, must be filled in partially, manually by user
Mtable <- cbind(rep("", dim(Mtable)[1]), as.data.frame(Mtable))
colnames(Mtable) <- c("label", "sentence", "frequency")



# DATA WRITING

write.csv2(Mtable, file="train.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)



# PLOTTING

if (plotSwitch){

	# Normal plot
	pdf(file = "Zipf.pdf")
	plot(Mtable$frequency, xlab = "number", ylab = "frequency")
	dev.off()

	# Log-log plot
	x<-1:dim(Mtable)[1]
	pdf(file = "loglogZipf.pdf")
	plot(log(x), log(Mtable$frequency), xlab = "log(number)", ylab = "log(frequency)")
	dev.off()
}




