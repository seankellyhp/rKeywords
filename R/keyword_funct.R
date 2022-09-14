
# Sean Palicki
# 19, Jul, 2022
# Keyword Discovery

# Requires magrittr, dplyr, quanteda, spacyr, tibble
print("Janus requires spacyR, quanteda, quanteda.textstats, stringr, vroom, tibble, dplyr, oolong, and reticulate. An installation of SpaCy and associated language model are also required using the python programming language, see https://spacy.io/usage and https://cran.r-project.org/web/packages/spacyr/vignettes/using_spacyr.html.")

# Clean and Split Corpus
clean_corpus <- function(seedWords, corpus, minSentenceChar = 3) {

  #' Clean training text corpus
  #' @param seedWords starting search string keywords.
  #' @param corpus quanteda formatted text corpus.
  #' @param minSentenceChar minimum number of words contained in a sentence.
  #' @return sentence level quanteda corpus.

  # If corpus not in quanteda format, then convert, else, continue

  # If seedWords not in list, then format, else continue
  print("Cleaning Corpus")

  seedRegex <- tolower(paste(lapply(seedWords, function(x) gsub(" ", "[ -_]", x)), collapse = "|"))

  # Filter using seed words
  findSeed <- stringr::str_detect(quanteda::texts(rawCorp), stringr::regex(seedRegex, ignore_case = TRUE))
  shortCorp <- quanteda::corpus_subset(rawCorp, findSeed)

  # Split sentences
  sentence_tokens <- spacyr::spacy_tokenize(shortCorp, what = "sentence",
                                            output = "data.frame")
  names(sentence_tokens) <- c("orig_doc", "sentence")

  # Sentence Clean
  sentLength <- lapply(sentence_tokens$sentence, stringr::str_length)
  sentClean <- which(sentLength >= minSentenceChar)
  sentenceCleanDB <- sentence_tokens[sentClean,]

  # Convert to Corpus
  sentence_corp <- quanteda::corpus(sentenceCleanDB, text_field = "sentence")

  return(sentence_corp)
}

# Tag NER, POS, Nounphrase

tag_corpus <- function(tidyCorp) {

  #' Parse and tag corpus with entities and parts of speech
  #' @param tidyCorp corpus returned by clean_corpus
  #' @return parsed and lemmatised text with entity and pos tags
  #'
  # Parse text with all
  print("Tagging Corpus")
  parsedtxt <- spacyr::spacy_parse(tidyCorp,
                                   pos = TRUE,
                                   tag = TRUE,
                                   lemma = TRUE,
                                   entity = TRUE,
                                   nounphrase = TRUE)


  # Prep Entities
  entityListCons <- spacyr::entity_consolidate(parsedtxt, concatenator = "_")

  # Prep Nounphrases
  nounListCons <- spacyr::nounphrase_consolidate(parsedtxt, concatenator = "_")

  nounphraseOnly <- nounListCons %>%
    dplyr::filter(pos == "nounphrase") %>%
    dplyr::mutate(hasCon = ifelse(grepl("_", .$token), 1, 0)) %>%
    dplyr::filter(hasCon == 1)

  # Join Tags
  allPOSEntNounphrase <- dplyr::bind_rows(entityListCons, nounphraseOnly) %>%
    select(-hasCon)

  return(allPOSEntNounphrase)
}


filter_corpus <- function(tagCorp, keepList) {

  #' Filter parts of speech, punctuation, and stopwords from corpus.
  #' @param tagCorp cleaned and tagged corpus
  #' @param keepList list of POS types to keep
  #' @return tokenized and cleaned corpus with POS choices.
  #'
  # Load Required Packages
  require(quanteda.textstats, quietly = TRUE)

  # Replace Token with Lemma
  parsedTextCl <- tagCorp %>%
    dplyr::select(-token) %>%
    dplyr::rename("token" = lemma) %>%
    dplyr::filter(pos %in% keepList)

  # Clean Corpus
  clText <- parsedTextCl %>%
    quanteda::as.tokens() %>%
    quanteda::tokens(corp_toks,
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
    ) %>%
    quanteda::tokens_remove(quanteda::stopwords()) %>%
    quanteda::tokens_tolower()

  allWords <- unique(unlist(clText))

  return(allWords)

}

get_vocabulary <- function(parsedText, minTermFrequency = 2) {

  #' Get filteres list of unique corpus domain relevant words.
  #' @param parsedText fully cleaned corpus with tagging and filtering.
  #' @param minTermFrequency minimum number of times a word appears in a text to be kept.
  #' @return unique list of words in corpus.

  #### Master entities

  parsedText$entity_type[is.na(parsedText$entity_type) | parsedText$entity_type == ""] <- "X"
  parsedText$word <- stringr::str_trim(tolower(parsedText$lemma), side = c("both"))

  ### Parts of Speech
  parsedTmp <- parsedText %>%
    dplyr::group_by(word, entity_type) %>%
    dplyr::summarise(Count = dplyr::n(), pos = pos) %>%
    dplyr::distinct(word, entity_type, Count, pos, .keep_all = TRUE) %>%
    dplyr::ungroup()

  parsedTmp <- parsedTmp %>%
    dplyr::group_by(word) %>%
    dplyr::mutate(Percentage=round(Count/sum(Count)*100,2), pos = pos) %>%
    dplyr::slice_max(order_by = Percentage, n = 1) %>%
    dplyr::distinct(word, .keep_all = TRUE)

  ###

  parsedTmp <- parsedTmp %>%
    dplyr::group_by(word, entity_type) %>%
    dplyr::summarise(TotCount = sum(Count), Count = dplyr::n(), Percentage = Percentage, pos = pos) %>%
    dplyr::ungroup()

  parsedTmp <- parsedTmp %>%
    dplyr::group_by(word) %>%
    dplyr::slice_max(order_by = Count, n = 1) %>%
    dplyr::distinct(word, .keep_all = TRUE)

  parsedTextFin <- parsedTmp %>%
    dplyr::select(!(c(Count))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(TotCount >= minTermFrequency)

  return(parsedTextFin)

}

# Helper functions
# L2 Norm
norm_L2 <- function(x){

  #' L2 normalization
  #' @param x vector
  #' @return L2 norm vector
  #'
  newNorm <- sqrt(sum(x^2))
  return(newNorm)

}

clean_vocabulary <- function(wordList) {

  #' Further clean word list using rules.
  #' @param wordList domain relevant vacabulary.
  #' @return cleaned vocabulary.
  #'
  splitWords <- unique(unlist((stringr::str_split(wordList, pattern = "_"))))
  splitWords <- splitWords[!splitWords %in% quanteda::stopwords()]
  splitWords <- splitWords[!splitWords %in% as.character(as.list(1:100))]
  splitWords <- splitWords[!stringr::str_detect(splitWords, '[[:punct:]]')]
  return(splitWords)

}

## Embeddings

load_glove = function(filename, corpus, n=5000, minTermFrequency = 2) {

  #' Filter domain relevant vocabulary from pre-trained GloVe embeddings.
  #' @param filename path to pre-trained GloVe word embedding text file.
  #' @param corpus cleaned text corpus
  #' @param n total top words to keep from cleaned text corpus
  #' @param minTermFrequency minimum occurances of word in training text
  #' @return list of embeddings relevant to domain vocabulary.

  print("Discovering Keywords")

  # Load Required Packages
  require(vroom, quietly = TRUE)

  filename = path.expand(filename)
  if (!file.exists(filename)) stop(paste0("File ", filename, " does not exist"))

  vocabulary <- get_vocabulary(corpus, minTermFrequency = minTermFrequency)  %>%
    dplyr::arrange(desc(TotCount)) %>%
    dplyr::slice_head(n=n) %>%
    dplyr::select(word,
                  "total_count" = TotCount,
                  "part_of_speech" = pos,
                  entity_type)

  vocabSplit <- clean_vocabulary(vocabulary$word)
  gc()

  gl_model <- vroom::vroom(filename, delim = " ", vroom::locale(encoding = "UTF-8"), quote = "Z",
                           skip_empty_rows = TRUE, skip = 1) %>% # Add skip = 1 to skip row label
    dplyr::filter(X1 %in% vocabSplit)

  gc()
  gl_model <- gl_model %>%
    dplyr::mutate(across(2:ncol(gl_model), ~ scale(., center = FALSE, scale = norm_L2(.))))

  colnames(gl_model) = c('word',paste('dim',1:(ncol(gl_model)-1),sep = '_'))

  model_results <- list("metadata" = vocabulary, "embeddings" = gl_model)

  return(model_results)

}

multi_word_avg <- function(vocabulary, vectors) {

  #' Average embeddings for multi-part words
  #' @param vocabulary words
  #' @param vectors embeddings
  #' @return average embedding for multi-part words.

  # Split seed words
  dictionarySplit <- stringr::str_split(vocabulary, pattern = "[- _]")
  cleanWords <- clean_vocabulary(vocabulary)

  # Keep only seed words
  wordVec <- vectors$embeddings %>%
    dplyr::filter(word %in% tolower(as.character((unique(unlist(dictionarySplit))))))

  cleanVec = wordVec[FALSE,]

  for (i in 1:length(dictionarySplit)) {

    wordInd <- tolower(unlist(dictionarySplit[[i]]))
    wordFull <- paste(wordInd, collapse = "_")

    rawVec <- wordVec %>%
      dplyr::filter(word %in% wordInd)

    if(nrow(rawVec) > 1) {

      tmpVec <- rawVec[,-1] %>%
        dplyr::summarise(dplyr::across(.cols = everything(), mean)) %>%
        dplyr::mutate(word = wordFull) %>%
        dplyr::relocate(word)

    } else {

      tmpVec <- rawVec[,-1] %>%
        dplyr::mutate(word = wordFull) %>%
        dplyr::relocate(word)

    }

    cleanVec <- rbind(cleanVec, tmpVec)

  }

  cleanVec <- dplyr::distinct(cleanVec, dplyr::across(contains("dim_")), .keep_all = TRUE)

}


get_centroid <- function(dictionary, vectors) {

  #' Get average embedding for a list of words based on centroid.
  #' @param dictionary list of domain relevant words
  #' @param vectors word embeddings
  #' @return normalized centroids for word vectors

  cleanVec <- multi_word_avg(dictionary, vectors)

  # Average overall seed words
  centroid <- colMeans(cleanVec[,-1])
  centroid <- centroid / norm_L2(centroid)
  return(centroid)

}

get_keywords = function(dictionary, vectors) {

  #' Get word recommendations most similar to starting search string.
  #' @param dictionary list of domain relevant words
  #' @param vectors word embeddings
  #' @return cosine similarity

  print("Ranking Keywords")

  # Load Required Packages
  require(tibble, quietly = TRUE)

  # Get Centroid
  centroid <- get_centroid(dictionary, vectors)

  # Subset Multi-Word Phrases # This can be done going into the function # saves dictionary
  multiWord <- vectors$metadata$word[stringr::str_detect(vectors$metadata$word, '[-_]')]

  multWordVec <- multi_word_avg(multiWord, vectors)

  # Convert to Matrix
  metadata <- vectors$metadata
  vectors <- tibble::column_to_rownames(vectors$embeddings, 'word')
  vectorsExpand <- tibble::column_to_rownames(multWordVec, 'word')
  vectors <- dplyr::bind_rows(vectors, vectorsExpand)  %>%
    distinct(dplyr::across(contains("dim_")), .keep_all = TRUE)

  vectors <- data.matrix(vectors)
  rm(vectorsExpand)

  # Multiply Matrices
  similarities <- (vectors %*% centroid)[,1] # Matrix Mult
  similarities <- tibble::tibble(word=names(similarities), similarity=similarities)  %>%
    dplyr::arrange(-similarity) %>%
    dplyr::right_join(metadata, by = 'word')

}


# Helper Functions

export_query <- function(results, type = c("boolean", "regex")) {

  #' Format search query
  #' @param results list of keywords
  #' @param type boolean or regex style query
  #' @return cleaned search query

  if (tolower(type) == "boolean") {
    query <- paste(gsub("_", " ", results), collapse = "' OR '")
    query <- paste0("'", query, "'")

  } else if (tolower(type) == "regex") {
    query <- paste(gsub("_", "[ ]", results), collapse = "|")
    query <- paste0("'", query, "'")

  }

  print(query)
  return(query)
}

create_query <- function(results, n = 25, type = c("boolean", "regex")) {

  #' Export keyword results to formatted query
  #' @param results list of keywords
  #' @param n number of keywords in new query
  #' @param type boolean or regex style query
  #' @return paste friendly search strings

  results <- dplyr::slice_max(results, n = n, order_by = weighted_similarity)$word
  query <- export_query(results, type = type)
  return(query)
}


make_bool <- function(regexQ) {

  #' Convert Regex to Boolean
  #' @param regexQ Regex style query
  #' @return Boolean style query

  inclOR <- gsub("\\|", " OR ", tolower(regexQ))
  inclOR <- gsub("\\[", "", inclOR)
  inclOR <- gsub("\\]", "", inclOR)
  inclAND <- gsub("\\)\\(\\?\\:\\.\\+\\)\\(", ") AND (", inclOR)
  return(inclAND)
}

convert_bool <- function(string, regexPattern = " or | and | doc |\\*") {

  #' Convert Regex or Boolean query to string
  #' @param string regex or boolean style query
  #' @return list of keywords

  temp <- gsub(pattern = regexPattern, replacement = "   ", x = string, ignore.case = TRUE)
  temp <- strsplit(tolower(temp), split = "   ")
  temp <- lapply(temp, function(x){x[!x ==""]})
  temp <- unlist(temp)
  return(temp)
}

# Automated 'super' function
automate_keywords = function(seedWords, corpus, modelPath, minSentenceChar = 3, minTermFrequency = 2, nVectors=5000,
                             nCandidates = 200, weightSimilarity = 2, keepPOS = c("NOUN", "ADJ", "X", "PROPN", "ENTITY", "nounphrase", "PRON")) {

  #' Automated super function
  #' @description One function to clean, process, and generate new keywords
  #' @param seedWords starting seed words
  #' @param corpus domain relevant training text corpus
  #' @param modelPath path to pre-trained word2vec embedding text

  #' @return list of keywords

  # Load Required Packages
  require(dplyr, quietly = TRUE)
  require(quanteda, quietly = TRUE)
  require(stringr, quietly = TRUE)

  # Prepare Seed Words
  seedWords <- tolower(seedWords)

  # Preprocess Training Data
  tagCorp <- tag_corpus(clean_corpus(seedWords, corpus, minSentenceChar = minSentenceChar))

  # Manage Memory
  rm(corpus)
  gc()

  # Filter Vocabulary using POS Tagging
  wordList <- filter_corpus(tagCorp, keepPOS)

  tagCorpCl <- tagCorp %>%
    filter(tolower(lemma) %in% wordList)

  # Manage Memory
  rm(tagCorp)
  rm(wordList)

  gc()

  # Load Word Embeddings and Discover Keywords

  # Load Word Vectors
  vectorsGlove <- load_glove(modelPath, tagCorpCl, n=nVectors, minTermFrequency = minTermFrequency)
  gc()

  # Find words
  keywords <- get_keywords(seedWords, vectorsGlove) %>%
    dplyr::arrange(desc(unname(similarity))) %>%
    dplyr::filter(similarity >= 0) %>%
    dplyr::mutate(total_countL2 = scale(total_count, center = FALSE, scale = norm_L2(total_count)),
                  similarityL2 = scale(similarity, center = FALSE, scale = norm_L2(similarity)),
                  weighted_similarity = (total_countL2 + (weightSimilarity * similarityL2))/2) %>%
    dplyr::select(-c(total_countL2, similarityL2)) %>%
    tibble::tibble() %>%
    dplyr::arrange(desc(weighted_similarity))  %>%
    dplyr::relocate(weighted_similarity, .after = word) %>%
    dplyr::slice_max(n=nCandidates, order_by = weighted_similarity)

  attributes(keywords$similarity)$names <- NULL
  attributes(keywords$weighted_similarity)$dimnames <- NULL
  attributes(keywords$weighted_similarity) <- NULL

  # Manage Memory
  rm(vectorsGlove, tagCorpCl)
  gc()

  return(keywords)

}
