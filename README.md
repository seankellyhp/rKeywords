# Getting Started Guide

This package is a work in progress tool for expanding search queries by adding domain 
relevant keywords from a sample text. The primary function "automate_keywords()" also 
provides meta-data about keywords using word counts, parts-of-speech, and named entity tagging to assist with 
keyword selection. 

The package currently only supports English language query expansion. Additional 
languages will be supported in later iterations.

## Install rKeywords in R from GitHub

```
devtools::install_github('seankellyhp/rKeywords')
```


## Install and configure Spacyr using the following guide. 
### See https://spacyr.quanteda.io/ for miniconda and spacyr installation 

```
library("spacyr")
spacy_install() 
spacy_download_langmodel("en_core_web_lg") # Optional
spacy_initialize()
spacy_initialize(save_profile = TRUE) # Optional

```

----
## Automatically discover new keywords using query expansion  

```
require(rKeywords, quietly = TRUE )
require(dplyr, quietly = TRUE)
require(quanteda, quietly = TRUE)
require(stringr, quietly = TRUE)
```

### Input starting search string

```
rawString <- "Immigrant* OR migrant* OR asylum seeker* OR visa*"
seedWords <- convert_bool(rawString)
```

### Load sample corpus

Should be a sample of domain relevant text data in quanteda corpus format. 

```
rawCorp <- readRDS("data/uk_eng_corp_sample.rds")
```

### Path to pre-trained GloVe word embedding model

```
modelPath <- 'models/glove.6B.300d.txt'
```

### Expand keywords 

```
keywordsNew <- automate_keywords(seedWords = seedWords, 
corpus = rawCorp, 
modelPath = modelPath, 
nCandidates = 200)

queryNew <- create_query(keywordsNew, n = 40, type = "regex")
print(queryNew)
```


