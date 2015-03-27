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
    
    if(length(lines) != buffer_size)
      done <- TRUE
    
    lines <- lines[sample(1:buffer_size,floor(sample_rate*buffer_size))]
    
    tokens <- trigram_token(lines)
    
    counts <- count(tokens)
    counts$x <- as.character(counts$x)
    
    count_df <- rbind(count_df, counts)
    
    rows_proc <- rows_proc + length(lines)
    
    print(rows_proc)
    
   
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
write.csv(sum_news_counts,file="sum_newsr_counts_tri")
sum_news_counts <- NULL

sum_newsr_counts <- read.csv("~/sum_newsr_counts_tri", stringsAsFactors=FALSE)
sum_twitter_counts <- read.csv("~/sum_twitter_counts_tri", stringsAsFactors=FALSE)
sum_blogs_counts <- read.csv("~/sum_blogs_counts_tri", stringsAsFactors=FALSE)

twitter_news_counts <- rbind(sum_twitter_counts,sum_newsr_counts)
all_counts <- rbind(twitter_news_counts,sum_blogs_counts)
all_counts = data.table(all_counts[,2:3])
all_counts_sum <- all_counts[,sum(freq),by=x]

write.csv(all_counts ,file="allr_counts_tri")

all_counts_tri <- read.csv("~/allr_counts_tri", stringsAsFactors=FALSE)

done <- FALSE
buffer_size <- 100000
rows <- 1

write_con <- file('tri_final','w')
while(!done)
{
  dt <- data.table(all_counts_tri[rows:(rows+buffer_size),])
  dt <- dt[,c("first_word","second_word","third_word"):=data.table(str_split_fixed(x," ",3))]
  write.csv(dt[,3:6,with=FALSE],write_con)
  rows <- rows + buffer_size
  print(rows)
  
  if(rows >= nrow(all_counts_tri))
    done <- TRUE
}

all_counts_final <- read.csv("~/tri_final", stringsAsFactors=FALSE)
all_counts_final$bigram <- paste(all_counts_final$first_word, all_counts_final$second_word)

all_counts_final <- data.table(all_counts_final)
setkey(all_counts_final,bigram)
all_counts_final$freq <- as.numeric(all_counts_final$freq)
total_bigram_counts <- all_counts_final[,sum(freq),by=bigram]
setkey(total_bigram_counts,bigram)


setPercent <- function(row){
  
  total <- total_bigram_counts[row[1],]
  
  if(is.na(total$count) == TRUE){
    return(0)
  }
  
  #print(row)
  #print(total)
  
  return (as.numeric(row[2])/total$count)
}

total_bigram_counts$probability <- apply(all_counts_final[,c(6,2),with=FALSE],1,setPercent)
