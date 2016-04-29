require(quanteda)
require(dplyr)
require(plyr)
require(data.table)
sampling_text <- function(filename1, filename2, filename3, n){
  con1 <- file(filename1, "rb") # read as binary data
  content <- readLines(con1, encoding = "UTF-8", warn = TRUE, skipNul = TRUE)
  num_of_lines = length(content)
  sampleSize = ceiling(num_of_lines * 0.2)
  set.seed(n)
  random_lines = sample(seq(1, num_of_lines), sampleSize, replace = FALSE)
  test_lines <- random_lines[1:100]
  training_lines <- random_lines[101:sampleSize]
  train_text = content[training_lines]
  con2 = file(filename2, "w")
  writeLines(train_text, con2)
  
  test_text = content[test_lines]
  con3 = file(filename3, "w")
  writeLines(test_text, con3)
  close(con1)
  close(con2)
  close(con3)
}
sampling_text("./Capstone/corpus/en_US.twitter.txt", "./Capstone/corpus/txt/twitter_train.txt","./Capstone/corpus/txt/twitter_test.txt", 1234)
sampling_text("./Capstone/corpus/en_US.news.txt", "./Capstone/corpus/txt/news_train.txt", "./Capstone/corpus/txt/news_test.txt", 2345)
sampling_text("./Capstone/corpus/en_US.blogs.txt", "./Capstone/corpus/txt/blogs_train.txt", "./Capstone/corpus/txt/blogs_test.txt", 1236)
cleaningText <- function(filename1, filename2){
  con1 <- file(filename1, "rb") # read as binary data
  content <- readLines(con1, encoding = "UTF-8", warn = TRUE, skipNul = TRUE)
  close(con1)
  content <- toLower(content)
  content <- gsub("/|@|\\|", " ", content)
  content <- gsub("-", " ", content)
  content <- gsub("/", " ", content)
  content <- gsub("<>", "\\'", content)
  content <- gsub("\\. |\\.$"," ", content)
  content <- gsub("\\? |\\?$"," ", content)
  content <- gsub("\\! |\\!$"," ", content)
  content <- gsub("<85>"," ", content)
  content <- gsub("<92>","'", content)
  content <- gsub("\\&", " and ", content)
  content <- gsub("[^[:alnum:][:space:]\'<>]", " ", content)
  content <- gsub(" www(.+) ", " ", content)
  content <- gsub(" [b-hj-z] "," ", content)
  con2 = file(filename2, "w")
  writeLines(content, con2)
  close(con2)
}

cleaningText("./Capstone/corpus/txt/twitter_train.txt", "./Capstone/corpus/MLEModel20/twitter_train.txt")
cleaningText("./Capstone/corpus/txt/news_train.txt", "./Capstone/corpus/MLEModel20/news_train.txt")
cleaningText("./Capstone/corpus/txt/blogs_train.txt", "./Capstone/corpus/MLEModel20/blogs_train.txt")

cleaningText("./Capstone/corpus/txt/twitter_test.txt", "./Capstone/corpus/MLEModel20_test/twitter_test.txt")
cleaningText("./Capstone/corpus/txt/news_test.txt", "./Capstone/corpus/MLEModel20_test/news_test.txt")
cleaningText("./Capstone/corpus/txt/blogs_test.txt", "./Capstone/corpus/MLEModel20_test/blogs_test.txt")
myCorpus <- corpus(textfile("./Capstone/corpus/MLEModel20/*.txt"))

badWords <- read.csv("./Capstone/corpus/Terms-to-Block.csv", stringsAsFactors = FALSE, skip = 4)
badWords <- badWords[, 2]

tokens <- quanteda::tokenize(toLower(myCorpus), removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE)
tokens <- removeFeatures(tokens, badWords)
toks1 <- quanteda::ngrams(tokens, n = 1, concatenator = " ")
unigram_dt <- data.table(as.data.frame(table(unlist(toks1))))
names(unigram_dt) = c("unigram", "uni_freq")
unigram_dt <- setorder(unigram_dt, -uni_freq)
rm(toks1)

# remove the unigrams those start with numbers
uni_final <- subset(unigram_dt, !(grepl("^[0-9]", unigram)), unigram:uni_freq)
rm(unigram_dt)
uni_final <- setorder(uni_final, -uni_freq)
# add a column that indexes the unigram
uni_final$word1_int <- 1:242246
# create a dictionary with all unigrams, for searching the final queries
uni_dict <- 1:242246

# calculate the probabilities for unigrams
uni_final <- uni_final[, uni_prob := uni_freq/sum(uni_freq)][]

names(uni_final) <- c("word1",  "uni_freq", "word1_int", "uni_prob")

# create a unigram datatable for merging
names(uni_dict) <- uni_final$word1
word_dt <- data.table(word1_int = 1:242246, word1 = names(uni_dict))
# stop words
sw <- stopwords("english")
sw_int <- uni_dict[sw]

has_to_remove <- names(uni_dict)[nchar(names(uni_dict)) == 2]
has_to_remove <- has_to_remove[-c(22, 23)] # words "us" and "go" and not in stopwords
has_to_remove <- c(has_to_remove, "wtju", "adhd")
has_to_remove_int <- uni_dict[has_to_remove]

toks2 <- quanteda::ngrams(tokens, n = 2, concatenator = " ")
bigram_dt <- data.table(as.data.frame(table(unlist(toks2))))
names(bigram_dt) = c("bigram", "bi_freq")
rm(toks2)
bigram_dt <- subset(bigram_dt, !(grepl("^[0-9]", bigram)), bigram:bi_freq)
bigram_dt <- setorder(bigram_dt, -bi_freq)
bigram_dt <- bigram_dt[, c("word1", "word2") := tstrsplit(bigram_dt$bigram, " ", fixed = TRUE)][]

bigram_merge <- merge(bigram_dt, uni_final, by = "word1", sort = FALSE)
names(word_dt) <- c("word2_int", "word2")
bigram_merge <- merge(bigram_merge, word_dt, by = "word2", sort = FALSE)
bigram_merge <- bigram_merge[, "bi_prob" := bi_freq/uni_freq][]
bigram_merge <- bigram_merge[, "bigram_int" := as.numeric(paste0(word1_int, word2_int))][]

# for merging the bigrams with the trigrams subset the bigram_merge with bi_freq, bigram_int
bigram_for_merge <- subset(bigram_merge, select = c(bigram, bi_freq, bigram_int))

bigram_dt <- subset(bigram_merge, select = c(word1_int, word2_int, bi_prob))

# write the csv file of bigrams with all the words including stop words
write.csv(bigram_merge, "./Capstone/corpus/MLEModel_20/bigram_full.csv")

rm(bigram_merge)

# creating final bigram table for model removing stopwords and misspelled two letter words from word2_int
bigram_final <- subset(bigram_dt, !(word2_int %in% sw_int), select = c(word1_int,word2_int, bi_prob))
bigram_final <- subset(bigram_final, !(word2_int %in% has_to_remove_int), select = c(word1_int,word2_int, bi_prob))

write.csv(bigram_final, "./Capstone/corpus/MLEModel_20/bigram_final_for_model.csv")

names(bigram_final) <- c("unigram_int", "word", "prob")
bigram_dt <- na.omit(setorder(setkey(bigram_final, unigram_int), unigram_int, -prob)[, .SD[1:5], by=unigram_int], 
                          cols = "word")

write.csv(bigram_dt, "./Capstone/corpus/MLEModel_20/bigram_top5_for_model.csv")
# trigram 

toks3 <- quanteda::ngrams(tokens, n = 3, concatenator = " ")
trigram_dt <- data.table(as.data.frame(table(unlist(toks3))))
names(trigram_dt) = c("trigram", "tri_freq")
rm(toks3)

trigram_dt <- subset(trigram_dt, !(grepl("^[0-9]", trigram)), trigram:tri_freq)
trigram_dt <- setorder(trigram_dt, -tri_freq)

trigram_dt <- trigram_dt[, c("word1", "word2", "word3") := tstrsplit(trigram_dt$trigram, " ", fixed = TRUE)][]

trigram_dt <- trigram_dt[, "bigram" := paste(word1, word2)][]
trigram_dt <- subset(trigram_dt, select = -c(word1, word2))
names(word_dt) <- c("word3", "word3_int")
trigram_merge <- merge(trigram_dt, bigram_for_merge, by = "bigram", sort = FALSE)
trigram_merge <- merge(trigram_merge, word_dt, by = "word3", sort = FALSE)
trigram_merge <- trigram_merge[, "tri_prob_MLE" := tri_freq/bi_freq][]
trigram_merge <- trigram_merge[, "trigram_int" := as.numeric(paste0(bigram_int, word3_int))][]
write.csv(trigram_merge, "./Capstone/corpus/MLEModel_20/trigram_full.csv")
trigram_for_merge <- subset(trigram_merge, select = c(trigram, tri_freq, trigram_int))
trigram_dt <- subset(trigram_merge, select = c(bigram_int, word3_int, tri_prob))

rm(trigram_merge)
# creating final bigram table for model removing stopwords and misspelled two letter words from word2_int
trigram_final <- subset(trigram_dt, !(word3_int %in% sw_int), select = c(bigram_int, word3_int, tri_prob))
trigram_final <- subset(trigram_final, !(word3_int %in% has_to_remove_int), select = c(bigram_int, word3_int, tri_prob))

write.csv(trigram_final, "./Capstone/corpus/MLEModel_20/trigram_final_for_model.csv")


names(trigram_final) <- c("bigram_int", "word", "prob")
trigram_dt <- na.omit(setorder(setkey(trigram_final, bigram_int), bigram_int, -prob)[, .SD[1:5], by=bigram_int], 
                     cols = "word")
write.csv(trigram_dt, "./Capstone/corpus/MLEModel_20/trigram_top5_for_model.csv")

write.csv(uni_final, "./Capstone/corpus/MLEModel_20/unigram_for_model.csv")


trigramModel <- function(s){
  started.at=proc.time()
  query_corpus <- corpus(s)
  query_tokens <- quanteda::tokenize(toLower(query_corpus), ngrams = 1, removeNumbers=TRUE, removePunct=TRUE, 
                                     removeTwitter = TRUE, removeSeparators = TRUE)
  
  l = length(query_tokens[[1]])
  
  if (l >= 2){
    query_tokens <- tail(query_tokens[[1]], 2)
    if(!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- uni_dict[query_tokens]
      query <- as.numeric(paste0(query[1],query[2]))
      matches <- subset(trigram_dt, bigram_int == query, select = word)[[1]]
      if (!FALSE %in% (query_tokens[2] %chin% names(uni_dict)) && length(matches) <3){
        query <- as.numeric(paste0(uni_dict[[query_tokens[2]]]))
        matches <- c(matches, subset(bigram_dt, unigram_int == query, select = word)[[1]])
      }
    }
    else{
      matches <- unigram_dt$word[1:3]
    }
  }
  else if(l == 1){
    query_tokens <- tail(query_tokens[[1]], 1)
    if (!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- as.numeric(paste0(uni_dict[[query_tokens]]))
      matches <- subset(bigram_dt, unigram_int == query, select = word)[[1]]
    }
    else{
      matches <- unigram_dt$word[1:3]
    }
  }
  if(length(matches)> 0){
    nextWords <- names(uni_dict[matches])
  }
  else {
    nextWords <- "Please Enter the Text"
  }
  print(cat("Finished in",timetaken(started.at),"\n"))
  return (nextWords)
}

