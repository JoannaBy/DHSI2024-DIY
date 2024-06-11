library(stylo)

setwd("C:/OneDrive - Uniwersytet Jagielloński/Naukowe/DigitalHum/DHSI/Small_collection/")

results = stylo()

# Look into the stylo_config.txt file to see
# what arguments you can set in the stylo(argument = value) function  


# No graphical user interface now!
# Copy-paste a couple of parameters from stylo_config.txt
results = stylo(gui = FALSE, mfw.min = 100, mfw.max = 200,
                ngram.size = 4, mfw.list.cutoff = 500)

# Print out a sample of the n-gram frequency table
results$table.with.all.freqs

# Choose the first book and n-grams from from 1st to 10th most important
results$table.with.all.freqs[1,1:10]
# As above, but call the book by its name
results$table.with.all.freqs["Richardson_Pamela",1:10]
# plot(results$table.with.all.freqs[1,1:1000])

# Save the names of n-grams into a variable
ngrams = colnames(results$table.with.all.freqs)
# Find ngrams, whose frequency differs between two books by more than a threshold
select = abs(results$table.with.all.freqs["Richardson_Pamela",1:200]
             -results$table.with.all.freqs["Dickens_David",1:200])>0.01
ngrams[select]
