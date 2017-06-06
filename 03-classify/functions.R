# Function that makes n-grams out of a sentence
allNGrams <- function(sentence_, maxn_)
{
	ngrams = c()

	# Split into words
	words <- strsplit(sentence_, " ")[[1]]
	nw <- length(words)
	
	# 1-grams = words
	ngrams <- c(ngrams, words)

	# higher n-grams
	if (maxn_ > 1){
		nj <- min(maxn_ -1, length(words)-1)	
		for (j in 1:nj){
			for (i in 1:(nw-j)){
				ngrams <- c(ngrams, paste(words[i:(i+j)], collapse = " "))
			}
		}
	}
	
	# Return
	ngrams
}


# Function that counts n-grams of a sentence
# Input: list with n-grams, sentence
# Output: list with counts
countNGrams <- function(sentence_, ngram_)
{
	count_ <- c()
	for (i in 1:length(ngram_)){
		pat <- paste0("\\b", ngram_[i], "\\b", collapse = "")
		count_ <- c(count_, str_count(sentence_, pat))
	}
	count_
}


# Normalization function
normalize <- function(x) {
	num <- x - min(x)
	denom <- max(x) - min(x)
	if (denom > 0){
		return (num/denom)
	} else {
		return (x)
	}
}