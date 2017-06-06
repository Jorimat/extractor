# Machine learning on type sentences
# Test on training data with leave-one-out


# Start time
# t0 <- Sys.time()

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

# Training data have a value for x$class, hence not "" 
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



# TEST OF LEARNING ALGORITHM
# Leave-one-out

# Learning
learnLabel <- c()
prob <- c()

for (i in 1:ntrain){
	# Indices of test training set (all sentences except i)
	if (i == 1){
		train_ <- 2:ntrain
	} else if (i > 1 && i<ntrain){
		train_ <- c(1:(i-1), (i+1):ntrain)
	} else if (i == ntrain){
		train_ <- 1:(ntrain-1)
	}
	
	# Machine learning with k nearest neighbours
	temp <- knn(train = train.ngram[train_, ], test = train.ngram[i, ], cl = train$label[train_], k = 3, prob = T)
	learnLabel <- c(learnLabel, toString(temp[1]))
	prob <- c(prob, attr(temp, "prob"))
}




# EVALUATION

# Dataframe with known and learned labels and probabilities
eval <- as.data.frame(t(rbind(train$label, learnLabel, prob)))
colnames(eval)[1] = "label"

# Print evalutation to screen
cat("maxn =", maxn, ", k =", neighbours, "\n")
cat(ntrain, "training sentences\n")
score <- length(which(eval$label == eval$learnLabel))/ntrain
cat("Score: ", score, "\n")

# Confusion matrix
label <- sort(unique(train$label))

confusion <- matrix(nrow=length(label), ncol = length(label))
rownames(confusion) <- label
colnames(confusion) <- label

for (i in 1:length(label)){
	for (j in 1:length(label)){
		confusion[i,j] <- length(intersect(which(eval$label == label[i]), which(eval$learnLabel == label[j])))
	}
}
cat("Confusion matrix:\n")
print(confusion)


