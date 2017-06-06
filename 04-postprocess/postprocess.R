# Postprocessing: calculate number of protocols with mutation(s)

# INPUT

# File with all preprocessed sentences and protocol numbers
Frefsent <- "../02-preprocess/preprocessed.csv"

# File with manually classified sentences
Ftrain <- "../02-preprocess/train.csv"

# File with sentences classified by learning algorithm
Flearned <- "../03-classify/learned.csv"

# Output file
fileout <- "results.txt"






# DATA LOADING

# Sentences with protocol number
zin <- read.csv(Frefsent, header = TRUE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)

# Order sentences on ref
zin <- zin[order(zin$ref), ]

# Unique sentences with learned label
unsent <- read.csv(Ftrain, header = TRUE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)

# Keep only lines with a value for x$label
unsent <- unsent[which(unsent$label != ""), ]

# Add learned data
unsent <- rbind(unsent, read.csv(Flearned, header = TRUE, encoding = "UTF-8", sep = ";", stringsAsFactors = F))






# PROCESSING
# t1 <- Sys.time()
# writeLines("Processing...")

# Empty dataframe with five columns: one with string, four with logicals
#df <- data.frame(ref = character(), P = logical(), N = logical(), A = logical(), O = logical(), Conclusion = character())

# Number of rows needed
nr <- length(unique(zin$ref))

# String with 20 R's
dummyref <- paste0(replicate(20, "R"), collapse = "")

# Pre-allocate matrix with two columns of dummyref and five columns of booleans
M <- cbind(rep(dummyref, nr), matrix(FALSE, nrow=nr, ncol=5))
colnames(M) <- c("ref", "P", "N", "A", "O", "Conclusion")
M <- as.data.frame(M, stringsAsFactors = F)

# Coerce to logicals
for (c in 2:6){
	M[ , c] <- as.logical(M[ , c]) 
}

# Loop:
noMatch = c()
i <- 1
j <- 1

while (i < dim(zin)[1]){
# while (i < 1000){
	# Make reference local
	ref_ <- zin$ref[i]
	
	# Write ref_ to M
	M$ref[j] <- ref_

	# Loop while ref is the same	
	while (zin$ref[i] == ref_){
		# Search label in unsent
		label_ <- unsent$label[which(zin$preprocessed[i]==unsent[,2])]
		
		# Check that there is only one matching label
		if (length(label_) != 1){
			noMatch = c(noMatch, i)
		} else {
		
		# Add boolean values for P, N etc. in df
		if (label_ == "P"){
			M$P[j] <- TRUE}
		if (label_ == "N"){
			M$N[j] <- TRUE}
		if (label_ == "A"){
			M$A[j] <- TRUE}
		if (label_ == "O"){
			M$O[j] <- TRUE}
		}
				
		i <- i + 1
	}
	j <- j + 1
}




# Draw conclusions for each protocol based on boolean values for P, N etc. and put in M
for (j in 1:dim(M)[1]){
# for (j in 1:1000){
	if (M$P[j] && M$N[j]){
		M$Conclusion[j] = "Contradiction"}
	if (M$P[j ] && !(M$N[j ])){
		M$Conclusion[j] = "P"}
	if (!(M$P[j ]) && M$N[j ]){
		M$Conclusion[j] = "N"}
	if (!(M$P[j]) && !(M$N[j]) && M$A[j]){
		M$Conclusion[j] = "A"}	
	if (!(M$P[j ]) && !(M$N[j ]) && !(M$A[j ]) && M$O[j ]){
		M$Conclusion[j] = "O"}		
	if (!(M$P[j ]) && !(M$N[j ]) && !(M$A[j ]) && !(M$O[j ])){
		M$Conclusion[j] = "No classified sentences"}		
}







# STATISTICS


# Numbers
nP = length(which(M$Conclusion == "P"))
nN = length(which(M$Conclusion == "N"))
nA = length(which(M$Conclusion == "A"))
nO = length(which(M$Conclusion == "O"))
nC = length(which(M$Conclusion == "Contradiction"))
nU = length(which(M$Conclusion == "No classified sentences"))

# Proportions
if(file.exists(fileout)) file.remove(fileout)

cat("\n")
write.table("\n", fileout, append = T, quote = F, row.names = F, col.names = F)
text_ <- paste(round(100*(nP+nN)/dim(M)[1]), "% of the texts show results of a K-RAS test.")
writeLines(text_)
write.table(text_, fileout, append = T, quote = F, row.names = F, col.names = F)
text_ <- paste("\t", round(100*nP/(nP+nN)), "% of the results is positive.")
writeLines(text_)
write.table(text_, fileout, append = T, quote = F, row.names = F, col.names = F)
text_ <- paste("\t", round(100*nN/(nP+nN)), "% of the results is negative.")
writeLines(text_)
write.table(text_, fileout, append = T, quote = F, row.names = F, col.names = F)

text_ <- paste(round(100*nA/nrow(M)), "% of the texts state that a KRAS test was requested or performed, but do not contain the result.")
writeLines(text_)
write.table(text_, fileout, append = T, quote = F, row.names = F, col.names = F)

text_ <- paste(round(100*nC/nrow(M)), "% of the texts contain contradictive test results.")
writeLines(text_)
write.table(text_, fileout, append = T, quote = F, row.names = F, col.names = F)




# WRITE M TO FILE

write.table(M, file = "M.csv", quote = TRUE, sep = ";",
            eol = "\n", na = "", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
			
			














