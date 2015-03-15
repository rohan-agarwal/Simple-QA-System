# ----INSTRUCTIONS----
# 1) Run ALL of the code once - select everything and run it in console
# 2) Execute the main() function in order to ask a question

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
  # annotating words from sentence
  wordsAnn <- annotate(words, list(sentencer, worder))
  # identifying POS tags for each word
  pos <- annotate(words, tagger, wordsAnn)
  
  # formatting
  pos <- subset(pos, type == "word")
  tags <- sapply(pos$features, `[[`, "POS")
  words <- words[pos]
  
  # rule-based keyword extraction
  if ("Who" %in% words) {
    keywords <- words[tags == "NNP"]
  }
  if ("Which" %in% words) {
    keywords <- words[tags == "NNP" | tags == "JJ" | tags == "CD"]
  }
  if ("affects" %in% words) {
    keywords <- c(words[tags == "NNP"])
  }
  if ("associated" %in% words) {
    # selecting last words of the sentence (related factors)
    # and the word "GDP"
    keywords <- c(words[11:(length(words)-1)],"GDP")
  }

  return(keywords)
  
}

# identifying documents containing input keywords
FindDocuments <- function(keywords) {
  
  # using keywords to search for a new set of terms in the list of unigrams
  # if any unigram contains a keywords it is used
  # i.e. "bankruptcy" & bankrupt
  terms <- sapply(keywords, 
                  function(x) colnames(masterTFxIDF)[
                    grepl(tolower(x),
                          colnames(masterTFxIDF))])
  terms <- c(unlist(terms))
  terms <- unique(terms)
  
  # selecting the TF-IDF matrix with only the above calculated terms
  mat <- masterTFxIDF[apply(masterTFxIDF[,tolower(terms)], 1, 
                            function(x) mean(x)>0),
                      tolower(terms)]
  docs <- sort(apply(mat, 1, function(x) mean(x)), decreasing=TRUE)
  docNames <- names(docs)
  
  # returning documents containing at least 1 keyword
  searchDocs <- corpus[names(corpus) %in% docNames]
  
  return(searchDocs)
  
}

# identifying all paragraphs from documents containing keywords
FindParagraphs <- function(keywords,searchDocs) {
  
  # creating a vector to store all paragraphs
  allParagraphs <- vector();
  
  for (i in 1:length(searchDocs)) {
    
    # data frame of paragraphs that contain all keywords
    df <- data.frame(sapply(keywords, 
                            grepl, 
                            searchDocs[[i]]))
    
    # paragraphs that contain all keywords
    # using all keywords instead of at least one in order to prune irrelevant data
    paragraphs <- searchDocs[[i]]$content[
      apply(df, 1, function(x) all(as.numeric(x)>0))]
    
    if (length(paragraphs) > 0) {
      
      # adding selected paragraphs to vector of all paragraphs
      allParagraphs <- c(allParagraphs, paragraphs)
      
    }
    
  }
  
  return(allParagraphs)
  
}

# breaking paragraphs down into sentences
FindSentences <- function(paragraph) {
  
  # annotating sentences in paragraphs
  paragraph <- as.String(paragraph)
  locations <- annotate(paragraph,sentencer)
  sentences <- paragraph[locations]
  
  # selecting sentences above a character threshold
  # to avoid artefacts of bad formatting
  sentences <- as.character(sentences)
  sentences <- sentences[nchar(sentences) > 3]
  return(sentences)
  
}

# ranking sentences using tf-idf
RankSentences <- function(sentences,keywords) {
  
  # creating a new corpus out of the sentences
  senCorp <- Corpus(VectorSource(sentences))
  # creating a document term matrix
  dtm <- DocumentTermMatrix(senCorp)
  # calculating tf-idf values
  tfidf <- weightTfIdf(dtm, normalize=TRUE)
  
  # expanding list of keywords using grep to search for all related words
  terms <- sapply(keywords, 
                  function(x) colnames(tfidf)[grepl(tolower(x),
                                                    colnames(tfidf))])
  terms <- c(unlist(terms))
  terms <- unique(terms)
  
  # extracting sentences that contain at least one keywords
  mat <- tfidf[apply(tfidf[,tolower(terms)], 1, 
                            function(x) mean(x)>0),
                      tolower(terms)]
  mat <- as.matrix(mat)
  
  # returning scores and sentences in descending order as a list
  scores <- sort(apply(mat, 1, function(x) mean(x)), decreasing=TRUE)
  sentences <- sentences[as.numeric(names(scores))]
  
  return(list("sentences" = sentences, "scores" = scores))
  
}

# extracting relevant names
GetNames <- function(ranks) {
  
  if ("CEO" %in% keywords) {
    
    func <- function(sentence) {
      
      # using regular expressions to extract the names from every sentence
      locations <- gregexpr("[A-Z][a-z]+ [A-Z]+[a-z]+",sentence)
      names <- regmatches(sentence,locations)
      return(names)
      
    }
  
  }
  
  if ("bankrupt" %in% keywords) {
    
    func <- function(sentence) {
      
      # using a NER annotator from the openNLP package
      sentence <- as.String(sentence)
      a2 <- annotate(sentence, list(sentencer, worder))
      
      # formatting
      if (as.numeric(length(compTagger(sentence,a2)) > 0)) {
        names <- sentence[compTagger(sentence, a2)]
        return(names)
      }
      
    }
    
  }
  
  if ("GDP" %in% keywords) {
    
    # selecting sentences that only contain keywords
    df <- data.frame(sapply(keywords, 
                            grepl, 
                            ranks$sentences))
    
    # selecting sentences that only contain percentages
    words <- ranks$sentences[apply(df, 1, function(x) all(as.numeric(x)>0))]
    words <- words[grepl("%",words) | grepl("percent",words)]
    # note that in the case of GDP-related questions the output is different
    # just the pruned sentences containing keywords and percentages are returned
    return(words)
  }
  
  # note that this part only occurs if the sentence is about CEO/bankruptcy
  # extracting names for all sentences
  allNames <- lapply(ranks$sentences, func)
  
  #returning names and associated tf-idf scores for sentences 
  return(list("names" = allNames, 
              "scores" = ranks$scores))
  
}

# ranking the names based on tf-idf scores
RankNames <- function(names,question) {
  
  if (grepl("What affects GDP",question)) {
    
    # selecting sentences that contain "factor" or "component"
    topNames <- names[grepl("factor",names) | grepl("component",names)]
  }
  
  else if (grepl("drop or increase",question)) {
    
    # establishing vector of all names (in this case percentages)
    topNames <- vector();
    
    for (i in 1:length(names)) {
      
      # sentence annotating
      words <- as.String(names[i])
      wordsAnn <- annotate(words, list(sentencer, worder))
      # POS tagging
      pos <- annotate(words, tagger, wordsAnn)
      
      # formatting
      pos <- subset(pos, type == "word")
      tags <- sapply(pos$features, `[[`, "POS")
      words <- words[pos]
      
      for (i in 1:length(words)-1) {
        # selecting digits followed by a percent sign
        if (tags[i] == "CD" && words[i+1] == "%") {
          topNames <- c(topNames,paste(words[i],words[i+1],sep=""))
        }
        # selecting anything followed by the word percent
        if (words[i+1] == "percent") {
          topNames <- c(topNames,paste(words[i],words[i+1],sep=""))
        }
      }
    
    }
    
  }
  
  else {
    # formatting
    uniqueNames <- unique(unlist(names$names))
    nameScore <- rep(0,length(uniqueNames))
    
    # tallying up scores for unique names
    nameScore <- sapply(uniqueNames, 
                        function(x) 
                          sum(as.numeric(names$scores[grepl(x,names$names)])))
    
    # returning the answers for the top five scores
    topNames <- sort(nameScore,decreasing=TRUE)[1:5]
  }

  return(topNames)
  
}

# ----main----

# creating "master" DTM and TF-IDF matrices for all tokens in corpus
masterDTM <- DocumentTermMatrix(corpus)
masterTFxIDF <- weightTfIdf(masterDTM, 
                            normalize=TRUE) 

#main function
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