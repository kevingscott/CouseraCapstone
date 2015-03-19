library(tm)
library(RWeka)
library(plyr)

# Sets the default number of threads to use
options(mc.cores=1)


lines <- readLines("~/SwiftKey_Data/final/en_US/en_US.twitter.txt")

custom_token <- function(these_lines){
  #endline_regex <- "[\.\?\!]\s"
  remove_non_words <- "[^[:alpha:] ']"
  
  clean_lines <- gsub(remove_non_words,'',tolower(these_lines))
  tokens <- scan_tokenizer(clean_lines)
  
  return(tokens)
}

doc.vec <- VectorSource(lines[1:10000])
doc.corpus <- Corpus(doc.vec)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

tdm <- TermDocumentMatrix(doc.corpus, control = list(tokenize = BigramTokenizer))

map <- new.env(hash=T, parent=emptyenv())

addGram <- function(row){
  word <- row[1]
  freq <- as.numeric(row[2])
  
  if(!exists(word, envir = map))
    map[[word]] <- 0
  
  map[[word]] <- map[[word]]  + freq
}

twitter_file_name <- "~/SwiftKey_Data/final/en_US/en_US.twitter.txt"
blogs_file_name <- "~/SwiftKey_Data/final/en_US/en_US.blogs.txt"
news_file_name <- "~/SwiftKey_Data/final/en_US/en_US.news.txt"

addFileToWordCount <- function(filename)
{
  done <- FALSE
  buffer_size <- 10000
  rows_proc <- 0
  
  con <- file(filename, 'r')
  while(!done)
  {
    lines <- readLines(con,buffer_size)
    
    tokens <- custom_token(lines)
    
    counts <- count(tokens)
    counts$x <- as.character(counts$x)
    
    apply(counts,1,addGram)
    
    rows_proc <- rows_proc + length(lines)
    
    print(rows_proc)
    
    if(length(lines) != buffer_size)
      done <- TRUE
  }
}



addCountsToHashMap <- function(some_lines)
{
  grams <- BigramTokenizer(some_lines)
  
  counts <- count(grams)
  counts$x <- as.character(counts$x)
  
  apply(counts,1,addGram)
}

environmentToDataFrame <- function(map)
{
  keys <- ls(map)
  df <- data.frame(x= keys, count = integer(length(keys)))
  
  #this sucks, there has to be a more elegeant way
  row <- 0
  for (v in ls(map)) {
    df[row,2] <- map[[v]]
    row <- row + 1
    print(row)
  }
  return(df)
}

