# load the library
library(stylo)

# To proceed in this part you need your texts arranged in two folders named:
# 'primary_set' (= training set) and 'secondary_set' (= test set)
# Go to the folder that contains them:
setwd(".........")

# start classification
classify()

# open 'final_results.txt' and see how good was the accuracy of the classification
# and what, if anything, got misclassfied

# now let's try running a serial experiment with a very simple type of cross-validation
# perform the classification:
results = classify(cv.folds = 10)

# get the classification accuracy:
results$cross.validation.summary

###################################################################################################################################################################################################################
# And let's try to make the cross-validation step by step 
# and see how well classify() can manage in telling the author
# of every text in our corpus

library(stylo)

#######
# Option 1: leave-one-out cross validation

# loading the corpus
texts = load.corpus.and.parse(files = "all", corpus.dir = "corpus")

# getting a genral frequency list
freq.list = make.frequency.list(texts, head = 1000)
# preparing the document-term matrix:
word.frequencies = make.table.of.frequencies(corpus = texts, features = freq.list)

# now the main procedure takes place:
results  = crossv(training.set = word.frequencies, cv.mode = "leaveoneout",
                  classification.method = "delta")

# see what's inside:
summary(results)

# e.g., check which texts were misattributed to which authors
results$misclassified

# or get the number of correct classifications:
results$y
sum(results$y, na.rm = TRUE)

# or see how Emily Bronte's books were classified:
results$expected
results$expected == 'EBronte'
results$predicted[results$expected == 'EBronte']

#######
# Option 2: leave-one-out cross validation


##################################################################################################################################################
# Now with rolling classification - you need two folders - 'reference_set' (= training set), 
# which holds the training data, and 'test_set' (= test set), where you put the text you want to examine

library(stylo)

# just run the fist experiment
rolling.classify(classification.method = "svm")
# let's also try to save the graph
rolling.classify(classification.method = "svm", write.png.file = TRUE)
# In fact there is a number of parameters you can control, go ahead and try changing some of them, you can find all options in CRAN documentation: https://cran.r-project.org/web/packages/stylo/stylo.pdf or by checking
help(rolling.classify) 

results = rolling.classify(write.png.file = TRUE, classification.method = "nsc", 
                 mfw=50, training.set.sampling = "normal.sampling", 
                 slice.size = 5000, slice.overlap = 4500) 



# If you want to mark specific parts in the text, 
# just put the word "xmilestone" (without "") in the place of interest.
# These locations are later stored in:
results$milestone.points

###
# How to find point at which the classification changes
results$classification.results
classes = unique(results$classification.results)
changes = diff(results$classification.results==classes[1])
# This gives you placement of the last slice before a change
results$classification.results[abs(changes)==1]


########################################################################################
# prepare the text tokenization
tokenized.texts = load.corpus.and.parse(files = "all", corpus.dir = "corpus", markup.type = "plain", language = "Other", encoding = "UTF-8")
# make features
features = make.frequency.list(tokenized.texts, head = 2000)
# make table of frequencies
data = make.table.of.frequencies(tokenized.texts, features, relative = TRUE)

# do the imposters
imposters(reference.set = data[-c(1),], test = data[1,], distance="wurzburg") 

# you can also compare it with cross-validation results
crossv(training.set = data, cv.mode = "leaveoneout", classification.method = "svm") 
