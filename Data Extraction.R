####Information Extraction

# Load package
pacman::p_load(SnowballC, tm, reshape2, slam, stringr, ggplot2, dplyr)
#Clear previous and load data
rm(list = ls())

######################## Functions ########################
visualizeBarPlot <- function(ftm.df, colorBars = "grey40", titleBarPlot = ""){
  ggplot(ftm.df[1:50,], aes(x = reorder(term,freq), y = freq/1000)) +
    geom_bar(stat = "identity", fill=colorBars) +
    xlab("Terms") + ylab("Frequency (* 1000)")+
    ggtitle(paste(titleBarPlot, "(Top 50)"))  + coord_flip()
  
}

getTermsFrequency <- function(corpus.tdm){
  all.terms <- findFreqTerms(corpus.tdm)
  freq = tm_term_score(x = corpus.tdm, terms = all.terms, FUN = slam::row_sums)
  terms <- names(freq); names(freq) <- NULL
  corpora.allTermsFrequency <- data.frame(term = terms, freq = freq)
  corpora.allTermsFrequency[order(corpora.allTermsFrequency$freq, decreasing = T), ]
}

######################## Load Data ########################
# Check directory
cname <- file.path("~", "Documents", "Zealous", "R", "Zealous", "Cases_Word2")   
cname   
dir(cname)

# Load surnames
getwd()
surnames <- read.csv("Surnames.csv", header = T)
lastnames <- surnames[,1]

#Preprocessing
revs <- Corpus(DirSource(cname))   

for (j in length(revs)){
  revs[[j]] <- gsub("driving under the influence", "dui", revs[[j]])
  revs[[j]] <- gsub("driving while under the influence", "dui", revs[[j]])
}

revs <- tm_map(revs, tolower) # turn document to lowercase
revs <- tm_map(revs, removeWords, stopwords("english"))
revs <- tm_map(revs, removeWords, stopwords())# delete a, the, and etc
#revs <- tm_map(revs, removeWords, lastnames) # delete lastnames 
revs <- tm_map(revs, removeNumbers) # delete numbers 
revs <- tm_map(revs, removePunctuation) # delete punctuation
revs <- tm_map(revs, stripWhitespace) # delete whitespace
# revs <- tm_map(revs, stemDocument) # Delete common word endings (e.g., "ing", "es")  
 

dtm <- DocumentTermMatrix(revs)
smaller <- as.matrix(DocumentTermMatrix(revs, control=list(bounds = list(global = c(10,Inf)))))

tdm <- TermDocumentMatrix(revs)   
findFreqTerms(dtm, 100)
freq <- as.matrix(dtm)
freq <- smaller
freq <- 1 + log(freq)
freq[is.infinite(freq)] <- 0

idf <- log( ncol(freq) / ( 1 + rowSums(freq != 0) ) ) 
idf <- diag(idf)
tf_idf <- crossprod(freq, idf)
colnames(tf_idf) <- rownames(freq)
tf_idf <- tf_idf / sqrt( rowSums( tf_idf^2 ) )
tf_idf <- t(tf_idf)

###### if(control is selected)
freq <- data.frame(freq)
control_data <- subset(freq, select=c(control, controlled, key, ignition, remanded, affirmed, reversed))
control_data$sum <- rowSums(control_data)
dim(control_data[which(control_data$sum > 0),]) #167 11

if(FALSE){
  vapply(freq, function(x) length(unique(x)) > 1, logical(1L))
  freq[vapply(freq, function(x) length(unique(x)) > 1, logical(1L))]
  
  frequent_terms_df <- getTermsFrequency(tdm)
  visualizeBarPlot(frequent_terms_df)
  
  if(FALSE){
    tf <- freq
    idf <- as.matrix(log(nrow(freq)/colSums(freq)))
    tfidf <- freq
    coldata <- data.frame()
    for(word in names(idf)){
      idf <- log(nrow(freq[,word]/colSums(freq[,word])))
      tfidf[,word] <- tf[,word] * idf[word]
    }
  }
  
  findAssocs(dtm, c("appellant"), corlimit=0.8) # specifying a correlation limit of 'corlimit'
}

################ Direction of case ######################
getwd()
setwd("/Users/bigphd/Documents/Zealous/R/Zealous/Cases_Word2")
folder <- "/Users/bigphd/Documents/Zealous/R/Zealous/Cases_Word2"
temp <- list.files(pattern="*.doc")
myfiles <- lapply(temp, FUN = readLines)
keylist = list()
for (i in 1:length(myfiles)) {
  #i= 40
  # i = 2
  myfiles[i] <- str_c(myfiles[i],collapse='"')    
  myfiles[i] <- str_replace_all(myfiles[i], "[[:punct:]]", "")
  #Turn string into dataframe to search for keywords
  split_data <- stringr::str_split(myfiles[i], pattern = " ")
  split_data <- as.data.frame(split_data)
  colnames(split_data) <- "word"
  loc <- grep("Appellant|Defendant", split_data$word)[1]
#  loc <- grep("Appellant", split_data)
#  loc <- grep("defendantappellant", split_data)
  dat <-split_data[(loc-2):(loc+2),]
  list <- substr(dat, 1, 25)
  keylist[[i]] <- list
}

big_data <- do.call(rbind, keylist)
big_data <- as.data.frame(big_data)

extracted <- cbind(control_data, big_data)
is_control <- function(nodeID, data){
  #switch input depending what it is
  #is numeric change to "control"
  data <- extracted
  control <- as.data.frame(rowSums(data[,c(1:4)]))
  colnames(control)[1] <- "inControl"
  data <- cbind(data, control)

}