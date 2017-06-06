# Machine learning on type sentences


library(class)
library(stringr)


# INPUT

# Input file
F1 <- "../02-preprocess/train.csv"

# Parameters for learning algorithm
maxn <- 1
neighbours <- 3

# Functions
source("functions.R")




# DATA LOADING

x <- read.csv(file = F1, header = TRUE, stringsAsFactors = F, sep = ";")
colnames(x) <- c("label", "sentence", "frequency")




# TRAINING

# Training data have a value for x$label, hence not "" 
train <- x[which(x$label != ""), ]

# Number of training sentences
ntrain = nrow(train)

# Make n-grams
ngrams = c()
for (k in 1:ntrain){
	ngrams <- c(ngrams, allNGrams(train$sent[k], maxn))
}
ngrams <- unique(ngrams)

# Count n-grams for every training sentence
# Dataframe with each line a sentence
#			columns = n-gram counts
train.ngram <- data.frame(matrix(nrow = 0, ncol = length(ngrams)))
colnames(train.ngram) <- ngrams

# Count and add
for (j in 1:ntrain){
	train.ngram[nrow(train.ngram) + 1, ] <- countNGrams(train$sent[j], ngrams)
}



# LEARNING

# Learning data have no value for x$label ("")
learn <- x[which(x$label == ""), ]

# Number of learning sentences
nlearn = nrow(learn)

# Learning
learnLabel <- c()
prob <- c()

for (i in 1:nlearn){

	# Learning sentence
	sent_ <- learn$sent[i]

	# Count ngrams for learning sentence
	sent_.ngram <- countNGrams(sent_, ngrams)
	

	temp <- knn(train = train.ngram, test = sent_.ngram, cl = train$label, k = neighbours, prob = T)
	learnLabel <- c(learnLabel, toString(temp[1]))
	prob <- c(prob, attr(temp, "prob"))
}

# Add learnLabel to learn dataframe
learn$label[1:length(learnLabel)] <- learnLabel




# DATA WRITING

# Only those with a value for label
write.csv2(learn[learn$label != "", ], file="learned.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)




