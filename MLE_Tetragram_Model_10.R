unigram_dt <- fread("./Capstone/corpus/MLEModel/unigram_clean.csv", sep = ",", select = c(2,3,4), stringsAsFactors = FALSE)
bigram_dt <- fread("./Capstone/corpus/MLEModel/bigram_clean.csv", sep = ",", select = c(2:8), stringsAsFactors = FALSE)
trigram_dt <- fread("./Capstone/corpus/MLEModel/trigram_clean.csv", sep = ",", stringsAsFactors = FALSE)
tetragram_dt <- fread("./Capstone/corpus/MLEModel/tetragram_clean.csv", sep = ",", stringsAsFactors = FALSE)

# remove the unigrams those start with numbers
uni_final <- subset(unigram_dt, !(grepl("^[0-9]", unigram)), unigram:uni_freq)
rm(unigram_dt)
# add a column that indexes the unigram
uni_final$word1 <- seq(1:167990)
# create a dictionary with all unigrams, for searching the final queries
uni_dict <- seq(1:167990)
names(uni_dict) <- uni_final$unigram
# calculate the probabilities for unigrams
uni_final <- uni_final[, uni_prob := uni_freq/sum(uni_freq)][]
# subset the bigram to have only bigram, bi_freq and bi_prob
bigram_dt_1 <- subset(bigram_dt, select = c(bigram, bi_freq, bi_prob))
# split the bigram in to two columns
bigram_dt_1 <- bigram_dt_1[, c("unigram", "unigram2") := tstrsplit(bigram_dt_1$bigram, " ", fixed = TRUE)][]
# merge the unigram_final and bigram_dt_1 and remove the NA values
bigram_dt_2 <- na.omit(merge(bigram_dt_1, uni_final, by = "unigram", sort = FALSE), cols = "word1")

# create a data table with names word2 and unigram2 from uni_dict and repeat the merging
word2_dt <- data.table(word2 = 1:167990, unigram2 = names(uni_dict))

bigram_dt_2 <- na.omit(merge(bigram_dt_2, word2_dt, by = "unigram2", sort = FALSE), cols = "word2")

# making a unique integer for each bigram by pasting word1 and word2
bigram_dt_2 <- bigram_dt_2[, bigram_int := as.numeric(paste0(word1, word2))][]

bigram_dt_final <- subset(bigram_dt_2, select = c(bigram_int, word1, word2, bi_freq, bi_prob, uni_freq, uni_prob))
rm(bigram_dt, bigram_dt_2, bigram_dt_1)
# remove unigram from uni_final
uni_final <- subset(uni_final, select = uni_freq:uni_prob)
write.csv(uni_final, "./Capstone/corpus/IndexedTables/indexed_uni.csv")
write.csv(bigram_dt_final, "./Capstone/corpus/IndexedTables/indexed_bigram.csv")

# read the trigram file
trigram_dt <- fread("./Capstone/corpus/MLEModel/trigram_clean.csv", sep = ",", stringsAsFactors = FALSE)
# subset the trigram
trigram_dt <- subset(trigram_dt, select = trigram:tri_prob)

# split the bigram in to two columns
trigram_dt_1 <- trigram_dt[, c("unigram1", "unigram2", "unigram3") := tstrsplit(trigram_dt$trigram, " ", fixed = TRUE)][]

# merge with three unigram data tables
names(word2_dt) <- c("word3", "unigram3")
trigram_dt_2 <- na.omit(merge(trigram_dt_1, word3_dt, by = "unigram3", sort = FALSE), cols = "word3")
names(word1_dt) <- c("word1", "unigram")
trigram_dt_2 <- na.omit(merge(trigram_dt_2, word1_dt, by = "unigram", sort = FALSE), cols = "word1")
names(word2_dt) <- c("word2", "unigram2")
trigram_dt_2 <- na.omit(merge(trigram_dt_2, word2_dt, by = "unigram2", sort = FALSE), cols = "word2")
trigram_dt_2 <- trigram_dt_2[, bigram_int := as.numeric(paste0(word1, word2))][]
trigram_dt_2 <- trigram_dt_2[, trigram_int := as.numeric(paste0(word1, word2, word3))][]
# remove the unigrams, trigram, word1 and word2
trigram_dt_final <- subset(trigram_dt_2, select = c(tri_freq, bi_freq, tri_prob, word3, bigram_int, trigram_int))
write.csv(trigram_dt_final, "./Capstone/corpus/IndexedTables/indexed_trigram.csv")

trigram_dt_without_sw <- subset(trigram_dt_final, !(word3 %in% sw_int), select = c(tri_freq, bi_freq, tri_prob, word3, bigram_int, trigram_int))

rm(traigram_dt_final, trigram_dt_2, trigram_dt_1, word2_dt, word3_dt)
rm(trigram_dt)

# read tetragarm
tetragram_dt <- fread("./Capstone/corpus/MLEModel/tetragram_clean.csv", sep = ",", stringsAsFactors = FALSE)
tetragram_dt <- subset(tetragram_dt, select = c(tetragram, tetra_freq, tetra_prob))

# split the bigram in to two columns
tetragram_dt_1 <- tetragram_dt[, c("unigram1", "unigram2", "unigram3", "unigram4") := tstrsplit(tetragram_dt$tetragram, " ", fixed = TRUE)][]

# merge with three unigram data tables
names(word1_dt) <- c("word1", "unigram1")
tetragram_dt_1 <- na.omit(merge(tetragram_dt_1, word1_dt, by = "unigram1", sort = FALSE), cols = "word1")
names(word1_dt) <- c("word2", "unigram2")
tetragram_dt_1 <- na.omit(merge(tetragram_dt_1, word1_dt, by = "unigram2", sort = FALSE), cols = "word2")

names(word1_dt) <- c("word3", "unigram3")
tetragram_dt_1 <- na.omit(merge(tetragram_dt_1, word1_dt, by = "unigram3", sort = FALSE), cols = "word3")

names(word1_dt) <- c("word4", "unigram4")
tetragram_dt_1 <- na.omit(merge(tetragram_dt_1, word1_dt, by = "unigram4", sort = FALSE), cols = "word4")

tetragram_dt_1 <- tetragram_dt_1[, trigram_int := as.numeric(paste0(word1, word2, word3))][]
tetragram_dt_1 <- tetragram_dt_1[, tetragram_int := as.numeric(paste0(word1, word2, word3, word4))][]

tetragram_dt_final <- subset(tetragram_dt_1, select = c(tetra_freq, tetra_prob, word4, trigram_int, tetragram_int))

tetragram_dt_without_sw <- subset(tetragram_dt_final, !(word4 %in% sw_int), select = c(tetra_freq, tetra_prob, word4, trigram_int, tetragram_int))


write.csv(tetragram_dt_final, "./Capstone/corpus/IndexedTables/indexed_tetragram.csv")

rm(tetragram_dt, tetragram_dt_1)


# MLE Model:

# trim the data tables:

## Avoiding the stopwords as prediction

bigram_dt <- subset(bigram_dt_final, select = c(word1, word2, bi_prob))
names(bigram_dt) <- c("unigram_int", "word", "prob")
bigram_dt <- subset(bigram_dt, !(word %in% sw_int), select = unigram_int:prob)
trigram_dt <- subset(trigram_dt_final, select = c(tri_prob, word3, bigram_int))
names(trigram_dt) <- c("prob", "word", "bigram_int")
trigram_dt <- subset(trigram_dt, !(word %in% sw_int), select = prob:bigram_int)
tetragram_dt <- subset(tetragram_dt_final, select = c(tetra_prob, word4, trigram_int))
names(tetragram_dt) <- c("prob", "word", "trigram_int")
tetragram_dt <- subset(tetragram_dt, !(word %in% sw_int), select = prob:trigram_int)

has_to_remove <- names(uni_dict)[nchar(names(uni_dict)) == 2]
has_to_remove <- has_to_remove[-c(22, 23)] # words "us" and "go" and not in stopwords
has_to_remove <- c(twoLetter_words$word, "wtju", "adhd")
has_to_remove_int <- uni_dict[has_to_remove]


bigram_dt <- subset(bigram_dt, !(word %in% has_to_remove_int), select = unigram_int:prob)
trigram_dt <- subset(trigram_dt, !(word %in% has_to_remove_int), select = prob:bigram_int)
tetragram_dt <- subset(tetragram_dt, !(word %in% has_to_remove_int), select = prob:trigram_int)



## Keep only top 5 entries for each (n-1)gram in the ngram table

bigram_top5_dt <- na.omit(setorder(setkey(bigram_dt, unigram_int), unigram_int, -prob)[, .SD[1:5], by=unigram_int], 
                          cols = "word")
trigram_top5_dt <- na.omit(setorder(setkey(trigram_dt, bigram_int), bigram_int, -prob)[, .SD[1:5], by=bigram_int], cols = "word")
tetragram_top5_dt <- na.omit(setorder(setkey(tetragram_dt, trigram_int), trigram_int, -prob)[, .SD[1:5], by=trigram_int], cols = "word")

write.csv(tetragram_top5_dt, "./Capstone/corpus/IndexedTables/tetragram_top5.csv")
write.csv(trigram_top5_dt, "./Capstone/corpus/IndexedTables/trigram_top5.csv")
write.csv(bigram_top5_dt, "./Capstone/corpus/IndexedTables/bigram_top5.csv")

bigram_dt <- bigram_top5_dt
trigram_dt <- trigram_top5_dt
tetragram_dt <- tetragram_top5_dt
rm(bigram_top5_dt, trigram_top5_dt, tetragram_top5_dt)
names(uni_final) <- c("uni_freq", "word", "prob")
unigram_dt <- subset(uni_final, !(word %in% sw_int), select = uni_freq:prob) 

# Interpoation:
bigram_dt <- bigram_dt[, prob_smooth := prob *0.2]
trigram_dt <- trigram_dt[, prob_smppth := prob * 0.3]
tetragram_dt <- tetragram_top5_dt[, ]


# MLE Model via Stupid Backoff
tetragramModel_2 <- function(s){
  started.at=proc.time()
  query_corpus <- corpus(s)
  query_tokens <- quanteda::tokenize(toLower(query_corpus), ngrams = 1, removeNumbers=TRUE, removePunct=TRUE, 
                                     removeTwitter = TRUE, removeSeparators = TRUE)
  
  l = length(query_tokens[[1]])
  if (l >= 3){
    query_tokens <- tail(query_tokens[[1]], 3)
    if(!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- uni_dict[query_tokens]
      query <- as.numeric(paste0(query[1],query[2], query[3]))
      matches <- subset(tetragram_dt, trigram_int == query, select = word)[[1]]
      if (!FALSE %in% (query_tokens[2:3] %chin% names(uni_dict)) && length(matches) < 3){
        query <- uni_dict[query_tokens[2:3]]
        query <- as.numeric(paste0(query[1],query[2]))
        matches <- c(matches, subset(trigram_dt, bigram_int == query, select = word)[[1]])
        if (!FALSE %in% (query_tokens[3] %chin% names(uni_dict)) && length(matches) < 3){
          query <- as.numeric(paste0(uni_dict[[query_tokens[3]]]))
          matches <- c(matches, subset(bigram_dt, unigram_int == query, select = word)[[1]])
        }
      }
    }
    else{
      matches <- unigram_dt$word[3]
    }
  }
  else if (l == 2){
    query_tokens <- tail(query_tokens[[1]], 2)
    if(!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- uni_dict[query_tokens]
      query <- as.numeric(paste0(query[1],query[2]))
      matches <- subset(trigram_dt, bigram_int == query, select = word)[[1]]
      if (!FALSE %in% (query_tokens[2] %chin% names(uni_dict)) && length(matches) < 3){
        query <- as.numeric(paste0(uni_dict[[query_tokens[2]]]))
        matches <- c(matches, subset(bigram_dt, unigram_int == query, select = word)[[1]])
      }
    }
    else{
      matches <- unigram_dt$word[2]
    }
  }
  else if(l == 1){
    query_tokens <- tail(query_tokens[[1]], 1)
    if (!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- as.numeric(paste0(uni_dict[[query_tokens]]))
      matches <- subset(bigram_dt, unigram_int == query, select = word)[[1]]
    }
    else{
      matches <- uni_dict[2]
    }
  }
  if(length(matches)> 0){
    nextWords <- names(uni_dict[matches])
    #nextWords <- nextWords[2]
  }
  else {
    nextWords <- "Please Enter the Text"
  }
  print(cat("Finished in",timetaken(started.at),"\n"))
  return (nextWords)
}



tetragramModel_2 <- function(s){
  started.at=proc.time()
  query_corpus <- corpus(s)
  query_tokens <- quanteda::tokenize(toLower(query_corpus), ngrams = 1, removeNumbers=TRUE, removePunct=TRUE, 
                                     removeTwitter = TRUE, removeSeparators = TRUE)
  
  l = length(query_tokens[[1]])
  if (l >= 3){
    query_tokens <- tail(query_tokens[[1]], 3)
    if(!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- uni_dict[query_tokens]
      query <- as.numeric(paste0(query[1],query[2], query[3]))
      matches <- subset(tetragram_dt, trigram_int == query, select = c(word, smooth_prob))
      query <- uni_dict[query_tokens[2:3]]
      query <- as.numeric(paste0(query[1],query[2]))
      matches <- rbind(matches, subset(trigram_dt, bigram_int == query, select = c(word, smooth_prob)))
      query <- as.numeric(paste0(uni_dict[[query_tokens[3]]]))
      matches <- rbind(matches, subset(bigram_dt, unigram_int == query, select = c(word, smooth_prob)))
    }
    else if (!FALSE %in% (query_tokens[2:3] %chin% names(uni_dict))){
      query <- uni_dict[query_tokens[2:3]]
      query <- as.numeric(paste0(query[1],query[2]))
      matches <- subset(trigram_dt, bigram_int == query, select = c(word, prob))
      query <- as.numeric(paste0(uni_dict[[query_tokens[3]]]))
      matches <- rbind(matches, subset(bigram_dt, unigram_int == query, select = c(word, smooth_prob)))
    }
    else if (!FALSE %in% (query_tokens[3] %chin% names(uni_dict))){
      query <- as.numeric(paste0(uni_dict[[query_tokens[3]]]))
      matches <- subset(bigram_dt, unigram_int == query, select = c(word, smooth_prob))
    }
  }
  else if (l == 2){
    query_tokens <- tail(query_tokens[[1]], 2)
    if(!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- uni_dict[query_tokens]
      query <- as.numeric(paste0(query[1],query[2]))
      matches <- subset(trigram_dt, bigram_int == query, select = c(word, smooth_prob))
      query <- as.numeric(paste0(uni_dict[[query_tokens[2]]]))
      matches <- rbind(matches, subset(bigram_dt, unigram_int == query, select = c(word, smooth_prob)))
    }
    else if (!FALSE %in% (query_tokens[2] %chin% names(uni_dict))){
      query <- as.numeric(paste0(uni_dict[[query_tokens[2]]]))
      matches <- subset(bigram_dt, unigram_int == query, select = c(word, smooth_prob))
    }
  }
  else if(l == 1){
    query_tokens <- tail(query_tokens[[1]], 1)
    if (!FALSE %in% (query_tokens %chin% names(uni_dict))){
      query <- as.numeric(paste0(uni_dict[[query_tokens]]))
      matches <- subset(bigram_dt, unigram_int == query, select = c(word, smooth_prob))
    }
  }
  else {
    nextWords <- "Please Enter the Text"
  }
  matches <- setorder(setkey(matches, word), -smooth_prob)[, .SD[1], by=word]
  nextWords <- matches$word
  if(length(nextWords)>=5){
    nextWords <- nextWords[1:5]
  }
  print(cat("Finished in",timetaken(started.at),"\n"))
  return (names(uni_dict[nextWords]))
}


