done <- FALSE
buffer_size <- 100000
rows_proc <- 0

global_counts <- data.frame(word = character(),prev_word = character(), freq = integer())

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

trigram_token <- function(these_lines){
  #endline_regex <- "[\.\?\!]\s"
  remove_non_words <- "[^[:alpha:] ']"
  
  clean_lines <- gsub(remove_non_words,'',tolower(these_lines))
  tokens <- TrigramTokenizer(clean_lines)
  
  return(tokens)
}
sample_rate <- .15

addFileToWordCount <- function(filename,count_df)
{
  con <- file(filename, 'r')
  while(!done)
  {
    lines <- readLines(con,buffer_size)
    
    lines <- lines[sample(1:buffer_size,floor(sample_rate*buffer_size))]
    
    tokens <- trigram_token(lines)
    
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
write.csv(sum_twitter_counts,file="sum_twitter_counts_tri")
sum_twitter_counts <- NULL

global_counts <- data.frame(word = character(),prev_word = character(), freq = integer())
global_counts <-addFileToWordCount(blogs_file_name,global_counts)
sum_blogs_counts <- ddply(global_counts,'x',summarize,freq=sum(freq))
write.csv(sum_blogs_counts,file="sum_blogs_counts_tri")
sum_blogs_count <- NULL

global_counts <- data.frame(word = character(),prev_word = character(), freq = integer())
global_counts <-addFileToWordCount(news_file_name,global_counts)
sum_news_counts <- ddply(global_counts,'x',summarize,freq=sum(freq))
write.csv(sum_news_counts,file="sum_newsr_counts_tro")
sum_news_counts <- NULL
