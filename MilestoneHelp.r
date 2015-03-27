library(tm)
library(RWeka)
library(plyr)

twitter_file_name <- "C:\\Data\\final\\en_US\\en_US.twitter.txt"
blogs_file_name <- "C:\\Data\\final\\en_US\\en_US.blogs.txt"
news_file_name <- "C:\\Data\\final\\en_US\\en_US.news.txt"


getMetaData <- function(filename)
{
  lines <- readLines(filename)
  
  rowNumbers = length(lines)
  
  buffer_size = 10000
  rows_processed <- 0
  
  counts <- c(0,0)
  
  while(rows_processed < rowNumbers){
    
    tokens <- scan_tokenizer(lines[rows_processed+1:rows_processed+1+buffer_size])
    
    totalTokens <- length(tokens)
    
    rows_processed <- rows_processed + buffer_size
    
    counts[1] <- counts[1]+ totalTokens
    
    print(rows_processed)
  }
  
  return(c(rowNumbers,counts[1],counts[2]))
}


wordCount <- read.csv("~/wordCount.txt")

words_needed_for_percent = data.frame(words=integer(100),percentage=seq(1:100))

current_count <- 0
current_total_words <- 0
current_percentage <- 1
for(count in wordCount$freq){
  
  current_total_words <- current_total_words + count
  current_count <- current_count + 1
  
  while((current_total_words * 100 / total_words) >= current_percentage){
    words_needed_for_percent$words[current_percentage] = current_count
    current_percentage <- current_percentage + 1
  }
}

plot(words_needed_for_percent)