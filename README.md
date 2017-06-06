# extractor
Extract information using text mining techniques

GENERALITIES

In this tutorial, you learn to extract information from texts.  More specifically, the result of the KRAS mutation test is extracted from pathological reports.  The tutorial demonstrates a simplified version of the code I used for my presentation at the GRELL 2017 Ascension Meeting on May 25, 2017, in Brussels.  The simplifications were made as a compromise between readability and applicability to more various real-world problems.

For privacy reasons, the original pathological reports cannot be shared.  I created a new, artificial corpus of texts, based on the 1860 novel "Max Havelaar" by Multatuli, and a list of typical sentences containing the keyword KRAS.  These sentences are in Dutch and French, as was the case for the original pathological reports.  The result is a strange mixture of sentences from "Max Havelaar" and pathological reports, but it does a fine job in demonstrating the code.  The texts and their reference number are in the file colorectalProtocols.csv.

The final goal of the code is to decide for each pathological report to which of the following classes it belongs:
- P: a positive test result (KRAS gene is mutated)
- N: a negative test result (KRAS gene is not mutated)
- A: the report states that a test was requested or performed, but shows no result
- O: there is no relevant information about KRAS in the report
- Contradiction: there is at least one positive and one negative result in the report

The code works fine for texts where the desired information is contained in only a few sentences, and these sentences are identifiable by the presence of one or more keywords.  If this is not the case, serious adaptions are necessary, or an overall different approach.

The code is organized in a few subfolders.  In each subfolder, there are one or more R-scripts that write output to files in the same folder.  This is done to allow the user to check the results of each step, and for me to make it easier to explain what is done at every step.

Additionally, you can also have a look at the GRELL abstract and presentation in the folder doc/presentation.

Too simply run everything, open an R terminal, go to the directory containing this file and run 
> source("main.R")


	
	
PREREQUISITES

I used R version 3.3.2 (2016-10-31) 32 bit, but more recent versions will probably also work.  You need the following packages:
- class
- stringr	
	
	
	
CODE EXPLAINED


01-sentences/sentences.R

A relatively simple piece of code that cuts the texts in the second column of the file colorectalProtocols.csv into sentences.  Cutting is done at interpunctions, but the code also takes care not to cut after abbreviations.  It uses as list of common abbreviations in Dutch and French.  If you want to use this code for your own work, you should add abbreviations to the file 01-sentences/abbreviations.csv.  The code also outputs endwords to the file words.txt.  If this file contains a word where the sentence should not split, add this to abbreviations.csv, and run again.


02-preprocess/preprocess.R

This is often the most time-consuming and critical part of text mining.  It is perhaps not as sexy as classification algorithms, but choices made here usually have more influence on the final result.  It is basically a chain of find-and-replaces, based on lists of words.  The main goal is to make the sentences more uniform, without losing relevant information.  Before preprocessing, a selection of the sentences with the keyword "k-?ras" (regular expression) is made.  If you want to use this code for your own work, you should change the keyword in preprocess.R and add named entities to the files in the directory 02-preprocess/Lists.  For many protocols, and long lists of named entities, this code has significant running times.


02-preprocess/sort.R

This script makes a unique list of the preprocessed sentences, and sorts them according to the frequency of occurrence.  If plotSwitch is set to TRUE, it also makes graphs of the sentence frequency, demonstrating that the law of Zipf is also valid for protocol sentences (https://en.wikipedia.org/wiki/Zipf%27s_law).


02-preprocess/manual-classification.R

Normally, after this step, the most common sentences should be classified in the file train.csv.  You do this by adding the label P, N, A or O in the first column, using your favorite spreadsheet or text editor.  Since this is a boring and time-consuming job, and since few people speak both Dutch and French, you can also run manual-classification.R (works only for this tutorial).


03-classify/classify.R

Now that we have around 500 manually classified sentences, and some more unclassified sentences, we are ready to do machine learning.  We use the bag-of-words feature abstraction and the k-nearest-neighbours (KNN) classifier.  I tried different feature extractors and classifiers, but these simple ones were the best.  The purpose of this tutorial is not the explanation of the algorithms, but you can find some theory here:
http://en.wikipedia.org/wiki/Document-term_matrix
https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm
You can change the parameters maxn (the maximal number of words in one feature) and neighbours (the number of neighbours for the KNN algorithm).


03-classify/classify-test.R

This script tests the validity of classify.R, using the leave-one-out method.  Please make sure to use the same values for maxn and neighbours.
Caveat: in machine learning, usually there is a train set, a development set and a validation set.  We do not follow this convention here, since we do not really develop new machine learning algorithms.  We merely apply an existing one.
The score is simply the ratio of the number of correctly classified sentences over the total number of sentences in the train set.  Further, the confusion matrix is displayed.  Additional validation parameters (precision, recall, F-measure, etc) can be added by the user.
This script is not necessary for main.R to work, but you should definitely test the performance at some point.  


04-postprocess/postprocess.R

This final script brings the classified sentences together for each report and draws a conclusion.  It also provides some statistics, ideal for presentations.



CLOSURE

I encourage everyone to improve this code and share it with others thorugh Github.  Suggestions are also most welcome.

If you use this code for your work, please refer to us.



Joris Mattheijssens
Belgian Cancer Registry
June 5, 2017
