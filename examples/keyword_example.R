
# Sean Palicki 
# 14, Sep, 2022 
# Keyword Discovery

# Working directory set by R project...otherwise set manually
# setwd()
getwd()

#devtools::install.github('rKeywords')
library(rKeywords)

# Load Required Packages
require(dplyr, quietly = TRUE)
require(quanteda, quietly = TRUE)
require(stringr, quietly = TRUE)

# Initialize SpaCy and set Python path 
require(spacyr)
spacyr::spacy_initialize(
  model = "en_core_web_lg", # Must have SpaCy setup with loaded models
  python_executable = "/usr/local/bin/python3" # Set to your local Python path
)
options(warn=-1)

# Load training data - Quanteda formatted corpus
rawCorp <- readRDS("data/uk_eng_corp_sample.rds") # Use your quanteda formatted sample corpus
gc()

# Input starting search string
rawString <- "Immigrant* OR migrant* OR asylum seeker* OR refugee* OR undocumented OR guest worker* OR international student* OR emigrant*"

# Download GloVe model. Automated example, can also be downloaded from url directly
if (!file.exists("models/glove.6B.300d.txt")) {
  temp <- tempfile()
  url <- "https://nlp.stanford.edu/data/glove.6B.zip"
  options(timeout=4300) 
  download.file(paste0(url), temp)
  R.utils::gunzip(temp, "models/glove.6B.300d.txt")
}

# Input pre-trained word embedding - GloVe
modelPath <- 'models/glove.6B.300d.txt'

# Convert to correct format
seedWords <- convert_bool(rawString)

# Generate new keywords
keywordsNew <- automate_keywords(seedWords = seedWords, corpus = rawCorp, modelPath = modelPath, nCandidates = 200)

# Export keywords as search string
queryNew <- create_query(keywordsNew, n = 40, type = "regex")

# Purge memory
rm(rawCorp)
rm(vectors)
gc()

# Close SpaCy
spacyr::spacy_finalize()
