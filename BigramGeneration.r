done <- FALSE
buffer_size <- 100000
rows_proc <- 0

global_counts <- data.frame(word = character(),prev_word = character(), freq = integer())

bigram_token <- function(these_lines){
  #endline_regex <- "[\.\?\!]\s"
  remove_non_words <- "[^[:alpha:] ']"
  
  clean_lines <- gsub(remove_non_words,'',tolower(these_lines))
  tokens <- BigramTokenizer(clean_lines)
  
  return(tokens)
}

addFileToWordCount <- function(filename,count_df)
{
  con <- file(filename, 'r')
  while(!done)
  {
    lines <- readLines(con,buffer_size)
    
    tokens <- bigram_token(lines)
    
    counts <- count(tokens)
    counts$x <- as.character(counts$x)
    
    count_df <- rbind(count_df, counts)
    
    rows_proc <- rows_proc + length(lines)
    
    print(rows_proc)
    
    if(length(lines) != buffer_size)
      done <- TRUE
  }
  
  return(count_df)
}

convertToSeperateCOlumns <- function(df){
  
}


global_counts <-addFileToWordCount(twitter_file_name,global_counts)
sum_twitter_counts <- ddply(global_counts,'x',summarize,freq=sum(freq))
write.csv(sum_twitter_counts,file="sum_twitter_counts")
sum_twitter_counts <- NULL

global_counts <- data.frame(word = character(),prev_word = character(), freq = integer())
global_counts <-addFileToWordCount(blogs_file_name,global_counts)
sum_blogs_counts <- ddply(global_counts,'x',summarize,freq=sum(freq))
write.csv(sum_blogs_counts,file="sum_blogs_counts")
sum_blogs_count <- NULL

global_counts <- data.frame(word = character(),prev_word = character(), freq = integer())
global_counts <-addFileToWordCount(news_file_name,global_counts)
sum_news_counts <- ddply(global_counts,'x',summarize,freq=sum(freq))
write.csv(sum_news_counts,file="sum_newsr_counts")
sum_news_counts <- NULL

sum_newsr_counts <- read.csv("~/sum_newsr_counts", stringsAsFactors=FALSE)
sum_twitter_counts <- read.csv("~/sum_twitter_counts", stringsAsFactors=FALSE)

twitter_news_counts <- rbind(sum_twitter_counts,sum_newsr_counts)

sum_twitter_news_counts <- ddply(twitter_news_counts,'x',summarize,freq=sum(freq))
write.csv(sum_twitter_news_counts,file="sum_twitter_news_counts")


sum_twitter_news_counts <- read.csv("~/sum_twitter_news_counts", stringsAsFactors=FALSE)
sum_blogs_counts <- read.csv("~/sum_blogs_counts", stringsAsFactors=FALSE)

all_counts <- rbind(sum_twitter_news_counts,sum_blogs_counts)

all_counts <- read.csv("~/all_counts_smaller", stringsAsFactors=FALSE)

library(data.table)
all_counts = data.table(all_counts[,2:3])
all_counts_sum <- all_counts[,sum(freq),by=x]

write.csv(all_counts_sum,"all_counts_sum")

split_by_space <- function(row){
  return(stri_split_regex(all_counts_sum$x,'[ ]')[1])
}

read_con <- file('all_counts_sum', 'r')
all_counts_sum <- read.csv(read_con)

done <- FALSE
buffer_size <- 100000
rows <- 1

write_con <- file('words_total','w')
while(!done)
{
  dt <- data.table(all_counts_sum[rows:(rows+buffer_size),])
  dt <- dt[,c("first_word","second_word"):=data.table(str_split_fixed(x," ",2))]
  write.csv(dt[,3:5,with=FALSE],write_con)
  rows <- rows + buffer_size
  print(rows)
  
  if(rows >= nrow(all_counts_sum))
    done <- TRUE
}