# rKeywords
R package for generating keywords using English text.

devtools::install.github('seankellyhp/rKeywords')

## Set up R environment 
### Requires Python conda installation for SpaCy NLP package 

### Load rKeywords Packages
library(rKeywords)

### Load other required packages 
require(dplyr, quietly = TRUE)
require(quanteda, quietly = TRUE)
require(stringr, quietly = TRUE)

### Initialize SpaCy and set Python dependencies 
require(spacyr)

### Install SpaCy fresh from R using spacyr helpers
spacy_install()
spacy_initialize()

### Download GloVe model. Automated example, can also be downloaded from url directly
if (!file.exists("models/glove.6B.300d.txt")) {
  temp <- tempfile()
  url <- "https://nlp.stanford.edu/data/glove.6B.zip"
  options(timeout=4300) 
  download.file(paste0(url), temp)
  R.utils::gunzip(temp, "models/glove.6B.300d.txt")
}

### Working directory set by R project...otherwise set manually to a project folder
getwd()
options(warn=-1)

## Get Keywords 
### Load training data - Quanteda formatted corpus
rawCorp <- readRDS("data/uk_eng_corp_sample.rds") # Use your quanteda formatted sample corpus
gc()

### Input pre-trained word embedding - GloVe
modelPath <- 'models/glove.6B.300d.txt'

### Input starting search string
rawString <- "Immigrant* OR migrant* OR asylum seeker* OR refugee* OR undocumented OR guest worker* OR international student* OR emigrant*"

### Convert to correct format
seedWords <- convert_bool(rawString)

### Generate new keywords
keywordsNew <- automate_keywords(seedWords = seedWords, corpus = rawCorp, modelPath = modelPath, nCandidates = 200)

## Format Search Query and Export Keywords
### Export keywords as search string
queryNew <- create_query(keywordsNew, n = 40, type = "regex")

###Purge memory
rm(rawCorp)
rm(vectors)
gc()

### Close SpaCy
spacyr::spacy_finalize()
