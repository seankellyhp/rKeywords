% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyword_funct.R
\name{filter_corpus}
\alias{filter_corpus}
\title{Filter parts of speech, punctuation, and stopwords from corpus.}
\usage{
filter_corpus(tagCorp, keepList)
}
\arguments{
\item{tagCorp}{cleaned and tagged corpus}

\item{keepList}{list of POS types to keep}
}
\value{
tokenized and cleaned corpus with POS choices.
}
\description{
Filter parts of speech, punctuation, and stopwords from corpus.
}
