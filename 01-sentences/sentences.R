# Split texts in sentences

library(stringr)



# INPUT

# Files with protocols
F1 = c("../colorectalProtocols.csv")

# File with abbreviations
fabbrev = "abbreviations.csv"

# Output file
fileoutsent <- "sentences.csv"



# DATA LOADING

# Load protocols
text0 <- c()
for (f in 1:length(F1)){
	text0 <- rbind(text0, read.csv(F1[f], header = TRUE, sep = ";", encoding = "UTF-8", stringsAsFactors = F))
}

# Keep non-empty only
keep <- which(!(grepl("^\\s*$", text0$content)))
text0 <- text0[keep, ]

# Load abbreviations
abbrev <- read.csv(fabbrev, header = TRUE, sep = ";", encoding = "ANSI", stringsAsFactors = F)[ ,1]





# PROCESSING

# Replace single linebreak "\n" by space, and replace multiple linebreaks by a single one
text0$content <- gsub(text0$content, pattern = "\n(?!\n)", replacement = " ", perl = T)

# Regex to match: space - letters, slashes, round brackets - period - space 
reg <- " [[[:alpha:]]|/|(|)]*\\. "

# List of words after which we split, for later check
words <- c()

# Insert linebreaks at specific positions
for (p in 1:length(text0$reference)){

	# Find candidates for endwords and their positions
	endwords <- str_extract_all(text0$content[p], reg)[[1]]
	endpos <- str_locate_all(text0$content[p], reg)[[1]]

	# Make a dataframe
	endwp <- as.data.frame(cbind(endwords, endpos))

	# Remove brackets from candidates
	endwp$endwords <- gsub(endwp$endwords, pattern = "(\\(|\\))", replacement = "")

	# Remove leading and trailing spaces from candidates
	endwp$endwords <- gsub(endwp$endwords, pattern = "^\\s+|\\s+$", replacement = "")
		
	# Remove candidates that are in list of abbreviations
	keep <- which(!(endwp$endwords %in% abbrev))
	endwp <- endwp[keep, ]
	
	# Remove candidates that are just one character + dot
	# keep <- which(!(grepl("^[[:alnum:]]\\.$", endwp$endwords)))
	keep <- which(!(grepl("^\\(?[[:alnum:]]\\)?\\.$", endwp$endwords)))
	endwp <- endwp[keep, ]
	
	# Add to list of end words
	words <- c(words, endwp$endwords)

	# Check if endwords not empty
	if (dim(endwp)[1] > 0){
		# Add "\n" to remaining endwords in protocol.  Backwards!
		for (i in dim(endwp)[1]:1){
			n <- as.numeric(as.character(endwp$end[i]))
			text0$content[p] <- paste(substr(text0$content[p], 1, n-1), "\n", substr(text0$content[p], n, nchar(text0$content[p])), sep = "")
		}
	}
}

# Replace multiple linebreaks that are separated only by whitespace, by a single linebreak
text0$content <- gsub(text0$content, pattern = "\n(\\s|\n)*\n", replacement = "\n")
text0$content <- gsub(text0$content, pattern = "\n(\\s)+", replacement = "\n")





# DATA FRAMING

# Estimated number of sentences per protocol
nsent <- 50	

# Estimated number of rows needed
nr <- dim(text0)[1]*nsent

# String with 20 R's
dummyref <- paste0(replicate(20, "R"), collapse = "")

# String with 500 A's
dummysent <- paste0(replicate(500, "A"), collapse = "")

# Pre-allocate matrix with dummyref and dummysent
M <- cbind(matrix(dummyref, nrow = nr), rep(0, nr), matrix(dummysent, nrow = nr))



# Fill matrix
i <- 1

for (p in 1:length(text0$reference)){

	# Split in sentences
	sent_ <- strsplit(text0$content[p], "\n")[[1]]
	
	# Remove leading and trailing spaces from sentences
	sent_ <- gsub(sent_, pattern = "^\\s+|\\s+$", replacement = "")
	
	# Keep only non-empty sentences
	keep <- which(sent_ != "")
	sent_ <- sent_[keep]
	
	# Sentence numbers
	sentNum_ <- 1:length(keep)
	
	# Fill into matrix
	refs_ <- rep(text0$reference[p], length(sent_))
	M[i:(i+length(sent_)-1),1] <- refs_
	M[i:(i+length(sent_)-1),2] <- sentNum_
	M[i:(i+length(sent_)-1),3] <- sent_
	
	# New line number
	i <- i + length(sent_)
	
}

# Keep only filled part of matrix
M <- M[1:(i-1), ]

# Name columns
colnames(M) <- c("reference", "sentenceNumber", "sentence")


 


# DATA WRITING

# Write M to file
write.csv2(as.data.frame(M), file = fileoutsent, fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)

# Write words to file
words <- sort(unique(words))
write(words, file="words.txt", append = FALSE)

