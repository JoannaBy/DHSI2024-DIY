library(stylo)

# Load data already included in the 'stylo' package
data(galbraith)

# See what's inside
galbraith
# or just the names of the texts
rownames(galbraith)

# Run stylo
stylo(frequencies = galbraith)
