# Select and preprocess sentences

library(stringr)



# INPUT

# Switches
preprocess = T
screendumptz = F
screendumpclusters = F
writetofile = T

# File with protocol sentences
F1 = "../01-sentences/sentences.csv"

# Keyword for filtering
keyword = "k-?ras"

# File with last names
Flastnames = "Lists/familienamen.txt"

# File with first names
Ffirstnames = "Lists/voornamen.txt"

# File with titles
Ftitles = "Lists/titels.txt"

# File with places
Fplace = "Lists/plaatsen.txt"

# File with institutions
Finst = "Lists/instellingen.txt"

# File with zip codes
Fzipcode = "Lists/postcodes.txt"


# Start time
t0 <- Sys.time()
t1 <- Sys.time()


# Functions
firstpreprocess <- function(column){
	# Special characters -> normal
	column <- gsub(column, pattern = "(à|â)",     replacement = "a")
	column <- gsub(column, pattern = "(é|è|ê|ë)", replacement = "e")
	column <- gsub(column, pattern = "(î|ï)",     replacement = "i")
	column <- gsub(column, pattern = "(ô|ö)",     replacement = "o")
	column <- gsub(column, pattern = "(ù|û|ü)",   replacement = "u")
	column <- gsub(column, pattern = "ç",         replacement = "c")
	column <- gsub(column, pattern = "(“|”)",  	  replacement = "")

	# Sint etc. -> "St"
	column <- gsub(column, pattern = "([Ss]int|SINT|[Ss]ainte?|SAINTE?)", replacement = "St")

	# Multiple spaces -> single
	column <- gsub(column, pattern = "( ){2,}" , replacement = " ")

	# Output
	column
}
	
	
groupPat <- function(pat0, ng)
{
	# pat0: original list of patterns
	# ng: number of original patterns in one group
	# pat1: new, grouped pattern

	# Length of original pattern
	np0 <- length(pat0)
	
	# Length of pattern to make
	np1 <- floor(np0/ng)
	
	if (np1 > 0){
		# Allocate memory
		pat1 <- vector(mode = "list", length = np1)
		
		# Make new pattern
		for (i in 1:np1-1){
			pat1[i] <- paste0("\\b", pat0[(i*ng+1):((i+1)*ng)], "\\b", collapse = "|")
		}
		pat1[np1] <- paste0("\\b", pat0[(np1*ng+1):np0], "\\b", collapse = "|")	
	} else {
		pat1 <- paste0("\\b", pat0, "\\b", collapse = "|")
	}
	
	# Output
	pat1	
}





# DATA LOADING AND FILTERING



# Load sentences from F1
M <- read.csv(F1, header = TRUE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)

# Rename columns
colnames(M) = c("ref", "sentNum", "original")

# Keep only sentences with keyword
keep <- which(grepl(keyword, M$original, ignore.case = TRUE))
M <- M[keep, ]

# Remove sentences with "___", i.e. fill-out forms
exclude <- which(grepl("___", M$original))
if (length(exclude > 0))	M <- M[-exclude, ]

# Number of sentences with keyword
lstart = length(keep)

# Transform to data frame
M <- as.data.frame(M)

# Add column for preprocessed sentences (memory allocation)
M <- cbind(M, data.frame(preprocessed = M$original))



# PREPROCESSING

	
# Load files with named entities
lastname <- read.csv(Flastnames, header = FALSE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)[[1]]
firstname <- read.csv(Ffirstnames, header = FALSE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)[[1]]
titel <- read.csv(Ftitles, header = FALSE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)[[1]]
place <- read.csv(Fplace, header = FALSE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)[[1]]
inst <- read.csv(Finst, header = FALSE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)[[1]]
zipcode <- read.csv(Fzipcode, header = FALSE, encoding = "UTF-8", sep = ";", stringsAsFactors = F)[[1]]

# Remove punctuation and tab from sentences
# Not from named entities, because these are sometimes regex
M$preprocessed <- gsub(M$preprocessed, pattern = "[[:punct:]]|\\t", replacement = " ")

# First preprocessing of sentences and named entities
M$preprocessed <- firstpreprocess(M$preprocessed)
place          <- firstpreprocess(place)
lastname       <- firstpreprocess(lastname)
firstname      <- firstpreprocess(firstname)
inst           <- firstpreprocess(inst)
zipcode        <- firstpreprocess(zipcode)
titel		   <- firstpreprocess(titel)


	
	
# Named entity replacement

# Institutions
for (i in 1:length(inst)){
	M$preprocessed <- gsub(M$preprocessed, pattern = paste0("\\b", inst[i], "\\b", sep = ""), replacement = "INST")
	M$preprocessed <- gsub(M$preprocessed, pattern = paste0("\\b", toupper(inst[i]), "\\b", sep = ""), replacement = "INST")
}





# Places

pat <- groupPat(place, 100)
for (i in 1:length(pat)){
	M$preprocessed <- gsub(M$preprocessed, pattern = pat[i], replacement = "PLACE")
}

pat <- groupPat(toupper(place), 100)
for (i in 1:length(pat)){
	M$preprocessed <- gsub(M$preprocessed, pattern = pat[i], replacement = "PLACE")
}

# Point of compass + PLACE  -> PLACE
pat <- "[Nn]oord|[Zz]uid]|[Oo]ost|[Ww]est) ?PLACE"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "PLACE")
pat <- "PLACE (du|de l)? ?([Nn]ord|[Ss]ud|[Oo]uest|[Es]st)"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "PLACE")






# Last names

pat <- groupPat(lastname, 100)
for (i in 1:length(pat)){
	M$preprocessed <- gsub(M$preprocessed, pattern = pat[i], replacement = "LASTNAME")
}

pat <- groupPat(toupper(lastname), 100)
for (i in 1:length(pat)){
	M$preprocessed <- gsub(M$preprocessed, pattern = pat[i], replacement = "LASTNAME")
}	





# First names in combination with LASTNAME
pat <- groupPat(firstname, 100)
for (i in 1:length(pat)){
	pat_ <- paste0("(", pat[i], ") ?(?= LASTNAME)")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
	pat_ <- paste0("(?<= LASTNAME ) ?(", pat[i], ")")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
}



# Same with first names in upper case
pat <- groupPat(toupper(firstname), 100)
for (i in 1:length(pat)){
	pat_ <- paste0("(", pat[i], ") ?(?= LASTNAME)")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
	pat_ <- paste0("(?<= LASTNAME ) ?(", pat[i], ")")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
}



# First names in combination with FIRSTNAME
pat <- groupPat(firstname, 100)
for (i in 1:length(pat)){
	pat_ <- paste0("(", pat[i], ") ?(?= FIRSTNAME)")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
	pat_ <- paste0("(?<= FIRSTNAME ) ?(", pat[i], ")")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
}		

# Same with first names in upper case
pat <- groupPat(toupper(firstname), 100)
for (i in 1:length(pat)){
	pat_ <- paste0("(", pat[i], ") ?(?= FIRSTNAME)")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
	pat_ <- paste0("(?<= FIRSTNAME ) ?(", pat[i], ")")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " FIRSTNAME ", perl = T)
}




# Titel + evt uppercase + (FIRSTNAME|LASTNAME)
pat <- groupPat(titel, 100)
for (i in 1:length(pat)){
	pat_ <- paste0("(", pat[i], ") ?(?=(FIRSTNAME|LASTNAME))")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " TITLE ", perl = T)
	
	# pat_ <- paste0("(", pat[i], ") ?[A-Z] ?(?=(FIRSTNAME|LASTNAME))")
	pat_ <- paste0("(", pat[i], ") ?[A-Z ]{3} ?(?=(FIRSTNAME|LASTNAME))")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " TITLE INITIAL ", perl = T)

}

# Idem in upper case
pat <- groupPat(toupper(titel), 100)
for (i in 1:length(pat)){
	pat_ <- paste0("(", pat[i], ") ?(?=(FIRSTNAME|LASTNAME))")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " TITLE ", perl = T)
					
	# pat_ <- paste0("(", pat[i], ") ?[A-Z] ?(?=(FIRSTNAME|LASTNAME))")
	pat_ <- paste0("(", pat[i], ") ?[A-Z ]{3} ?(?=(FIRSTNAME|LASTNAME))")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat_, replacement = " TITLE INITITIAL ", perl = T)
}




# SPECIAL CASES
	
# Special case: Title + Van|Op + (PLACE|LASTNAME)
for (i in 1:length(titel)){
	pat <- paste("\\b", titel[i], "\\b (van|Van|VAN|op|Op|OP) ([Dd]e[rn] )?(PLACE|LASTNAME)", sep = "");
	M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "TITLE LASTNAME", perl = T)	
}

# Special case: Title + upper case + Van (PLACE|LASTNAME)
for (i in 1:length(titel)){
	pat <- paste("\\b", titel[i], "\\b", " [A-Z ]{4} ", "(van|Van|VAN|op|Op|OP) ([Dd]e[rn] )?(PLACE|LASTNAME)", sep = "");
	M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "TITLE INITIAL LASTNAME", perl = T)	
}

# Special case: FIRSTNAME + Van + (PLACE|LASTNAME)
for (j in 1:length(firstname)){
	pat <- paste("\\b", firstname[j], "\\b (van|Van|VAN|op|Op|OP) ([Dd]e[rn] )?(PLACE|LASTNAME)", sep = "");
	M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "FIRSTNAME LASTNAME", perl = T)
}



# DATE, HOUR, ZIP CODE

# Date
pat <- "\\b(le )?([0-2]?[0-9]|30|31) (0?[1-9]|10|11|12) (20)?(0[0-9]|1[0-6])\\b"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- "\\b(le )?([0-2]?[0-9]|30|31) (januari|februari|maart|april|mei|juni|juli|augustus|september|oktober|november|december)( (20)?(0[0-9]|1[0-6]))?\\b"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- "\\b(le )?([0-2]?[0-9]|30|31) (jan|febr?|mrt|apr|mei|jun|jul|aug|sept?|okt|nov|dec)( (20)?(0[0-9]|1[0-6]))?\\b"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- "\\b(le )?([0-2]?[0-9]|30|31) (janvier|fevrier|mars|avril|mai|juin|juillet|aout|septembre|octobre|novembre|decembre)( (20)?(0[0-9]|1[0-6]))?\\b"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")
	
pat <- "\\b(le )?([0-2]?[0-9]|30|31) (janv?|fevr?|mar|avr|mai|juin|juill?|aout|sept?|oct|nov|dec)( (20)?(0[0-9]|1[0-6]))?\\b"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- paste0("\\b(le )?([0-2]?[0-9]|30|31) (", toupper("januari|februari|maart|april|mei|juni|juli|augustus|september|oktober|november|december"), ")( (20)?(0[0-9]|1[0-6]))?\\b")
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- paste0("\\b(le )?([0-2]?[0-9]|30|31) (", toupper("jan|febr?|mrt|apr|mei|jun|jul|aug|sept?|okt|nov|dec"), ")( (20)?(0[0-9]|1[0-6]))?\\b")
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- paste0("\\b(le )?([0-2]?[0-9]|30|31) (", toupper("janvier|fevrier|mars|avril|mai|juin|juillet|aout|septembre|octobre|novembre|decembre"), ")( (20)?(0[0-9]|1[0-6]))?\\b")
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")

pat <- paste0("\\b(le )?([0-2]?[0-9]|30|31) (", toupper("janv?|fevr?|mar|avr|mai|juin|juill?|aout|sept?|oct|nov|dec"), ")( (20)?(0[0-9]|1[0-6]))?\\b")
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "DATE")	


# Hour e.g. 16u15, 21h30
pat <- "\\b(((0|1)?[0-9])|2[0-3])(u|h|U|H)[0-5][0-9]\\b"
M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "HOUR", perl = T)


# Zip code + PLACE -> ZIPCODE PLACE
for (i in 1:length(zipcode)){
	pat <- paste0("\\b", zipcode[i], "\\b PLACE")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = "ZIPCODE PLACE", perl = T)	
}




# GROUPING of named entities

# Every combination of TITLE, FIRSTNAME, INITIAL with LASTNAME -> PERSON
M$preprocessed <- gsub(M$preprocessed, pattern = "\\b(TITLE|FIRSTNAME|INITIAL| )*LASTNAME ?(FIRSTNAME| )*", replacement = " PERSON ")

# article + INST + preposition + INST|PLACE -> INST
M$preprocessed <- gsub(M$preprocessed, pattern = "\\b(de|het|le|la|les|l| )?INST(INST|PLACE|d|de|des|du|van|pour|voor|in|het|a|au|a l|te| )*(INST|PLACE)", replacement = " INST ")

# PERSON + preposition + INST|PLACE -> PERSON
M$preprocessed <- gsub(M$preprocessed, pattern = "\\bPERSON(d|de|des|du|van|a|te|in|het| )*(INST|PLACE)", replacement = " PERSON ")

# PERSON + conjunction + PERSON  ->  PERSON
M$preprocessed <- gsub(M$preprocessed, pattern = "\\bPERSON(PERSON|en|et| )*PERSON", replacement = " PERSON ")

# PERSON + [Ss]t PERSON -> PERSON
M$preprocessed <- gsub(M$preprocessed, pattern = "\\bPERSON [Ss]t PERSON", replacement = " PERSON ")

# [Dd]r ([Vv]an )?([Dd]e[rn]? )?PERSON -> PERSON
M$preprocessed <- gsub(M$preprocessed, pattern = "\\b[Dd]r ([A-Z] )?([Vv]an )?([Dd]e[rn]? )?PERSON\\b", replacement = "PERSON")

# [Dd]r ([Vv]an )?([Dd]e[rn]? )?PLACE -> PERSON
M$preprocessed <- gsub(M$preprocessed, pattern = "\\b[Dd]r ([A-Z] )?([Vv]an )?([Dd]e[rn]? )?PLACE\\b", replacement = "PERSON")	


# Lower case
M$preprocessed <- lapply(M$preprocessed, tolower)

# Code words back to upper case
# ! Confusion possible !
for (w in c("firstname", "lastname", "place", "inst", "person", "date", "postcode", "street", "hour")){
	pat = paste0("\\b",w,"\\b", collapse = "")
	M$preprocessed <- gsub(M$preprocessed, pattern = pat, replacement = toupper(w))
}

# Transform special words
M$preprocessed <- gsub(M$preprocessed, pattern = "k ras", replacement = "kras")

# Trim extra whitespaces
M$preprocessed <- gsub(M$preprocessed, pattern = "\\s+", replacement = " ")

# Trim leading and trailing spaces
M$preprocessed <- lapply(M$preprocessed, str_trim)

# Coerce to data frame
M <- data.frame(lapply(M, as.character), stringsAsFactors=F)







# DATA WRITING

# Write to files
write.csv2(M[, c("ref", "sentNum", "original")], file="original.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)
write.csv2(M[, c("ref", "sentNum", "preprocessed")], file="preprocessed.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)





