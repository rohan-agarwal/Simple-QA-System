# ----init----

# tm package is used for corpus creation, processing, dtm, tf-idf...
library(tm)

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

# note: need to install openNLPmodels.en package for NER tagging
#install.packages("openNLPmodels.en", 
#                 repos = "http://datacube.wu.ac.at/", type = "source")

# initiate company tagging annotator using NER
compTagger <- Maxent_Entity_Annotator(kind="organization")

# loading corpus
pwd <- paste(getwd(),"/BI",sep="")
files <- DirSource(pwd)
corpus <- Corpus(files,
                 readerControl=list(language="en"))

# ----functions----

# identifying keywords to look for in the document term matrix
FindKeywords <- function(words) {
  
  words <- as.String(words)
  wordsAnn <- annotate(words, list(sentencer, worder))
  pos <- annotate(words, tagger, wordsAnn)
  
  pos <- subset(pos, type == "word")
  tags <- sapply(pos$features, `[[`, "POS")
  words <- words[pos]
  
  #rule-based keyword extraction
  if ("Who" %in% words) {
    keywords <- words[tags == "NNP"]
  }
  if ("Which" %in% words) {
    keywords <- words[tags == "NNP" | tags == "JJ" | tags == "CD"]
  }
  if ("affects" %in% words) {
    keywords <- c(words[tags == "NNP"],"factor")
  }
  if ("associated" %in% words) {
    keywords <- c(words[11:(length(words)-1)],"GDP")
  }

  return(keywords)
  
}

# identifying documents containing input keywords
FindDocuments <- function(keywords) {
  
  terms <- sapply(keywords, 
                  function(x) colnames(masterTFxIDF)[
                    grepl(tolower(x),
                          colnames(masterTFxIDF))])
  terms <- c(unlist(terms))
  terms <- unique(terms)
  
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
  
  sentences <- as.character(sentences)
  sentences <- sentences[nchar(sentences) > 3]
  return(sentences)
  
}

# ranking sentences using tf-idf
RankSentences <- function(sentences,keywords) {
  
  senCorp <- Corpus(VectorSource(sentences))
  dtm <- DocumentTermMatrix(senCorp)
  tfidf <- weightTfIdf(dtm, normalize=TRUE)
  
  terms <- sapply(keywords, 
                  function(x) colnames(tfidf)[grepl(tolower(x),
                                                    colnames(tfidf))])
  terms <- c(unlist(terms))
  terms <- unique(terms)
  
  mat <- tfidf[apply(tfidf[,tolower(terms)], 1, 
                            function(x) mean(x)>0),
                      tolower(terms)]
  mat <- as.matrix(mat)
  
  scores <- sort(apply(mat, 1, function(x) mean(x)), decreasing=TRUE)
  sentences <- sentences[as.numeric(names(scores))]
  
  return(list("sentences" = sentences, "scores" = scores))
  
}

# extracting relevant names
GetNames <- function(ranks) {
  
  if ("CEO" %in% keywords) {
    
    func <- function(sentence) {
      
      locations <- gregexpr("[A-Z][a-z]+ [A-Z]+[a-z]+",sentence)
      names <- regmatches(sentence,locations)
      return(names)
      
    }
  
  }
  
  if ("bankrupt" %in% keywords) {
    
    func <- function(sentence) {
      
      sentence <- as.String(sentence)
      a2 <- annotate(sentence, list(sentencer, worder))
      
      if (as.numeric(length(compTagger(sentence,a2)) > 0)) {
        names <- sentence[compTagger(sentence, a2)]
        return(names)
      }
      
    }
    
  }
  
  if ("GDP" %in% keywords) {
    
    df <- data.frame(sapply(keywords, 
                            grepl, 
                            ranks$sentences))
    
    words <- ranks$sentences[apply(df, 1, function(x) all(as.numeric(x)>0))]
    words <- words[grepl("%",words) | grepl("percent",words)]
    return(words)
  }
  
  allNames <- lapply(ranks$sentences, func)
  
  return(list("names" = allNames, 
              "scores" = ranks$scores))
  
}

# ranking the names based on tf-idf scores
RankNames <- function(names,question) {
  
  if (grepl("What affects GDP",question)) {
    topNames <- names
  }
  
  else if (grepl("drop or increase",question)) {
    
    topNames <- vector();
    
    for (i in 1:length(names)) {
      
      words <- as.String(names[i])
      wordsAnn <- annotate(words, list(sentencer, worder))
      pos <- annotate(words, tagger, wordsAnn)
      
      pos <- subset(pos, type == "word")
      tags <- sapply(pos$features, `[[`, "POS")
      words <- words[pos]
      
      for (i in 1:length(words)) {
        if (tags[i] == "CD" && words[i+1] == "%") {
          topNames <- c(topNames,paste(words[i],words[i+1],sep=""))
        }
      }
    
    }
    
  }
  
  else {
    uniqueNames <- unique(unlist(names$names))
    nameScore <- rep(0,length(uniqueNames))
    
    nameScore <- sapply(uniqueNames, 
                        function(x) 
                          sum(as.numeric(names$scores[grepl(x,names$names)])))
    
    topNames <- sort(nameScore,decreasing=TRUE)[1:5]
  }

  return(topNames)
  
}

# ----main----

# creating "master" DTM and TF-IDF matrices for all tokens in corpus
masterDTM <- DocumentTermMatrix(corpus)
masterTFxIDF <- weightTfIdf(masterDTM, 
                            normalize=TRUE) 
#masterTFxIDF <- as.matrix(masterTFxIDF)

main <- function() {
  
  question <- readline("Enter a question: ")
  print("Generating keywords..."); flush.console()
  keywords <<- FindKeywords(question)
  print("Finding relevant documents..."); flush.console()
  searchDocs <- FindDocuments(keywords)
  print("Finding relevant paragraphs..."); flush.console()
  paragraphs <- FindParagraphs(keywords,searchDocs)
  print("Finding relevant sentences..."); flush.console()
  sentences <- unlist(lapply(paragraphs, FindSentences))
  print("Ranking sentences...")
  ranks <- RankSentences(sentences,keywords)
  print("Extracting names...")
  names <- GetNames(ranks)
  print("Getting final answer...")
  finalNames <- RankNames(names,question)
  
  if (grepl("What affects GDP",question)) {
    print("Below are related sentences indicating the factors that affect GDP:");flush.console()
  }
  
  else if (grepl("drop or increase",question)) {
    print("Below are all related percentages:");flush.console()
  }
  
  else {
    print("Top 5 results with scores:");flush.console()
  }
  
  return(finalNames)

}