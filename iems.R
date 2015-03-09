# ----init----

# tm package is used for corpus creation, processing, document term matrix, tf-idf...
library(tm)

# RWeka package is used to create a custom tokenizer
library(RWeka)

# stringr package is used for string manipulation
library(stringr)

# openNLP is used for sentence segmentation, NER, and POS tagging
library(openNLP)

# initiate sentence annotator
sentencer <- Maxent_Sent_Token_Annotator(language = "en")

# initiate word token annotator
worder <- Maxent_Word_Token_Annotator()

# initiate POS tagging annotator
tagger <- Maxent_POS_Tag_Annotator()

# loading corpus
files <- DirSource("~/Documents/R/IEMS395 HW4/BI")
corpus <- Corpus(files,
                 readerControl=list(language="en"))

# ----functions----

# tokenizing functions to select all tokens of size between [min] and [max] words
Tokenizer <- function(doc,min,max) {
  
  NGramTokenizer(doc, 
                 Weka_control(min = min, max = max))
  
}

# creating document term matrix
CreateDTM <- function(corp,min,max) {
  
  dtm <- DocumentTermMatrix(corp, 
                            control = list(
                              tokenize = Tokenizer(doc=corp,min=min,max=max)))
  
  return(dtm)
  
}

# processing a question: punctuation, stop words
# ProcessQuestion <- function(question) {
#   
#   question <- Corpus(VectorSource(question))
#   question <- tm_map(question,
#                      removePunctuation)
#   question <- tm_map(question,
#                      removeWords,
#                      stopwords('english'))
#   question <- as.character(sapply(question, 
#                                   `[`, "content"))
#   
#   return(question)
#   
# }

# identifying keywords to look for in the document term matrix
FindKeywords <- function(words) {
  
  words <- as.String(words)
  wordsAnn <- annotate(words, list(sentencer, worder))
  pos <- annotate(words, tagger, wordsAnn)
  
  pos <- subset(pos, type == "word")
  tags <- sapply(pos$features, `[[`, "POS")
  words <- words[pos]
  
  keywords <- words[tags == "NNP" | tags == "CD"]
  
  return(keywords)
  
}

# identifying documents containing input keywords
FindDocuments <- function(keywords) {
  
  terms <- sapply(keywords, 
                  function(x) colnames(masterTFxIDF)[grepl(tolower(x),
                                                           colnames(masterTFxIDF))])
  terms <- c(unlist(terms))
  
  mat <- masterTFxIDF[apply(masterTFxIDF[,tolower(terms)], 1, 
                            function(x) mean(x)>0),
                      tolower(terms)]
  docs <- sort(apply(mat, 1, function(x) mean(x)), decreasing=TRUE)
  docNames <- names(docs)
  
  searchDocs <- corpus[names(corpus) %in% docNames]
  
  return(searchDocs)
  
}

# identifying all paragraphs from documents containing keywords
FindParagraphs <- function(keywords,searchDocs) {
  
  allParagraphs <- vector();
  
  for (i in 1:length(searchDocs)) {
    
    df <- data.frame(sapply(keywords, 
                            grepl, 
                            searchDocs[[i]]))
    
    paragraphs <- searchDocs[[i]]$content[
      apply(df, 1, function(x) all(as.numeric(x)>0))]
    
    if (length(paragraphs) > 0) {
      
      allParagraphs <- c(allParagraphs, paragraphs)
      
    }
    
  }
  
  return(allParagraphs)
  
}

# breaking paragraphs down into sentences
FindSentences <- function(paragraph) {
  
  paragraph <- as.String(paragraph)
  locations <- annotate(paragraph,sentencer)
  sentences <- paragraph[locations]
  
  sentences <- sentences[nchar(sentences) > 3]
  
  return(sentences)
  
}

# ranking sentences using tf-idf
RankSentences <- function(sentences,keywords) {
  
  senCorp <- Corpus(VectorSource(sentences))
  dtm <- DocumentTermMatrix(senCorp)
  tfidf <- weightTfIdf(dtm, normalize=TRUE)
  
  mat <- tfidf[apply(tfidf[,tolower(keywords)], 1, 
                            function(x) all(x>0) && mean(x)>0),
                      tolower(keywords)]
  mat <- as.matrix(mat)
  
  scores <- sort(apply(mat, 1, function(x) mean(x)), decreasing=TRUE)
  sentences <- sentences[as.numeric(names(scores))]
  
  return(list("sentences" = sentences, "scores" = scores))
  
}

# finding the CEO name
FindCEONames <- function(sentence) {
  
  locations <- gregexpr("CEO [A-Z][a-z]* [A-Z][a-z]*",sentence)
  names <- regmatches(sentence,locations)
  
  if (length(names[[1]]) > 0) {
    
    names <- substr(names,5,nchar(names))
    
  }
  
  return(names)
  
}

# ----main----

# creating "master" DTM and TF-IDF matrices for all tokens in corpus
masterDTM <- DocumentTermMatrix(corpus)
masterTFxIDF <- weightTfIdf(masterDTM, 
                            normalize=TRUE) 
masterTFxIDF <- as.matrix(masterTFxIDF)

question <- readline("Enter a question: ")
keywords <- FindKeywords(question)
searchDocs <- FindDocuments(keywords)
paragraphs <- FindParagraphs(keywords,searchDocs)
sentences <- unlist(lapply(paragraphs, FindSentences))
ranks <- RankSentences(sentences,keywords)
sentences <- ranks$sentences
scores <- ranks$scores

# if q1: which companies went bankrupt in month _ of year _?


# if q2: who is the CEO of company _? 
names <- unlist(lapply(sentences, FindCEONames))
finalName <- names(which.max(table(names)))
print(finalName)

# if q3: what factors affect GDP? what percentage changes are associated with them?
