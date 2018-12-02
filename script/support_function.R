#function correction
library(readxl)

list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}





clean_text <- function(x, lowercase=TRUE)
{
  # x: character string
  
  # lower case
  if (lowercase)
    x = tolower(x)
  # remove numbers
  x = gsub("\n", "", x,fixed = T)
  x = gsub("\r", "", x,fixed = T)
  x = gsub("\t", "", x,fixed = T)
  x = gsub("[[:digit:]]", "", x)
  # remove punctuation symbols
  
    x = gsub("[[:punct:]]", "", x)
  # remove extra white spaces
  
    x = gsub("[ \t]{2,}", " ", x)
    x = gsub("^\\s+|\\s+$", "", x)
  
  # return
  x
}


clean_nlp <- function(docs)
{
  docs <- sapply(docs, tolower)
  
  rm_stopwords <- function(x)
  {
    x <- strsplit(x, " ")[[1]]
    x <- x[!(x %in% stopwords(kind = "en"))]
    x <- x[!(x == "")]
    x <- paste(x, collapse = " ")
  }
  
  docs <- sapply(docs, rm_stopwords)
  
  #stem words - remove the -ing or -ed endings to keep root of word
  docs <- stemDocument(docs)
  
  # remove numbers
  docs <- removeNumbers(docs)
  
  return(docs)
}
