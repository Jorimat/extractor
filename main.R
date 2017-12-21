setwd("01-sentences")
source("sentences.R")

setwd("../02-preprocess")
source("preprocess.R")
source("sort.R")
source("manual-classification.R")

setwd("../03-classify")
source("classify.R")
source("classify-test.R")

setwd("../04-postprocess")
source("postprocess.R")

setwd("..")
