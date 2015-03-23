done <- FALSE
buffer_size <- 100000
rows_proc <- 0

global_counts <- data.frame(x = character(), freq = integer())

counts_list <- list()

addFileToWordCount <- function(filename,count_df)
{
  con <- file(filename, 'r')
  while(!done)
  {
    lines <- readLines(con,buffer_size)
    
    tokens <- custom_token(lines)
    
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

global_counts <-addFileToWordCount(twitter_file_name,global_counts)
global_counts <-addFileToWordCount(blogs_file_name,global_counts)
global_counts <-addFileToWordCount(news_file_name,global_counts)

sum_counts <- ddply(global_counts,'x',summarize,freq=sum(freq))

total_words <- sum(sum_counts$freq)
sum_counts_ordered <- sum_counts[order(sum_counts$freq,decreasing=TRUE),]

words_needed_for_percent = data.frame(words=integer(100),percentage=seq(1:100))

current_count <- 0
current_total_words <- 0
current_percentage <- 1
for(count in sum_counts_ordered$freq){
  
  current_total_words <- current_total_words + count
  current_count <- current_count + 1
  
  while((current_total_words * 100 / total_words) >= current_percentage){
    words_needed_for_percent$words[current_percentage] = current_count
    current_percentage <- current_percentage + 1
  }
  
  print(current_percentage)
  print(current_count)
}

