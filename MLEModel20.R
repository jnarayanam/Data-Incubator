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
word_dt <- data.table(word2_int = 1:242246, word2 = names(uni_dict))
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
bigram_merge <- bigram_merge[, "smooth_prob" := (0.75 * bi_prob) + (0.25 * uni_prob)][]
bigram_merge <- subset(bigram_merge, !(word2 %in% sw), select = unigram:bi_prob)
bigram_merge <- merge(bigram_merge, word_dt, by = "word2", sort = FALSE)
write.csv(bigram_merge, "./Capstone/corpus/MLEModel20/bigram_full.csv")
bigram_merge <- subset(bigram_merge, !(word2_int %in% sw_int), select = word2:word2_int)
bigram_merge <- subset(bigram_merge, !(word2_int %in% has_to_remove_int), select = word2:word2_int)

bigram_dt <- subset(bigram_merge, select = c(bi_freq, word1_int, word2_int,bi_prob, uni_prob))
bigram_dt <- bigram_dt[, "bigram_int" := as.numeric(paste0(word1_int, word2_int))][]
bigram_merge <- bigram_merge[, "bigram_int" := as.numeric(paste0(word1_int, word2_int))][]
write.csv(bigram_merge, "./Capstone/corpus/MLEModel20/bigram_clean.csv")
rm(bigram_merge)

toks3 <- quanteda::ngrams(tokens, n = 3, concatenator = " ")
trigram_dt <- data.table(as.data.frame(table(unlist(toks3))))
names(trigram_dt) = c("trigram", "tri_freq")
rm(toks3)

trigram_dt <- subset(trigram_dt, !(grepl("^[0-9]", trigram)), trigram:tri_freq)
trigram_dt <- trigram_dt[, c("word1", "word2", "word3") := tstrsplit(trigram_dt$trigram, " ", fixed = TRUE)][]

names(word_dt) <- c("word3_int", "word3")

trigram_merge <- merge(trigram_dt, word_dt, by = "word3", sort = FALSE)

names(word_dt) <- c("word1_int", "word1")

trigram_merge <- merge(trigram_merge, word_dt, by = "word1", sort = FALSE)

names(word_dt) <- c("word2_int", "word2")

trigram_merge <- merge(trigram_merge, word_dt, by = "word2", sort = FALSE)



trigram_merge <- trigram_merge[, "bigram_int" := as.numeric(paste0(word1_int, word2_int))][]
trigram_merge <- trigram_merge[, "trigram_int" := as.numeric(paste0(word1_int, word2_int, word3_int))][]
write.csv(trigram_merge, "./Capstone/corpus/MLEModel20/trigram_full.csv")
trigram_merge_1 <- subset(trigram_merge, select = tri_freq:bigram_int)
rm(trigram_dt)

bigram_for_merge <- subset(bigram_dt, select = c(bi_freq, bi_prob, uni_prob, bigram_int))

trigram_merge <- subset(trigram_merge_1, !(word3_int %in% sw_int), select = tri_freq:bigram_int)
trigram_merge <- subset(trigram_merge, !(word3_int %in% has_to_remove_int), select = tri_freq:bigram_int)

trigram_merge_2 <- merge(trigram_merge, bigram_for_merge, by = "bigram_int", sort = FALSE)

trigram_merge_2 <- trigram_merge_2[, "tri_prob" := tri_freq/bi_freq][]

trigram_merge_2 <- trigram_merge_2[, "smooth_prob" := (0.58 *tri_prob) + (0.28 *bi_prob) + (0.14 * uni_prob)][]

trigram_merge_2 <- trigram_merge_2[, "trigram_int" := as.numeric(paste0(word1_int, word2_int, word3_int))][]

write.csv(trigram_merge_2, "./Capstone/corpus/MLEModel20/trigram_clean.csv")

rm(trigram_merge, trigram_merge_1, bigram_for_merge)

toks4 <- quanteda::ngrams(tokens, n = 4, concatenator = " ")
tetragram_dt <- data.table(as.data.frame(table(unlist(toks4))))
names(tetragram_dt) = c("tetragram", "tetra_freq")
rm(toks4)

tetragram_dt <- subset(tetragram_dt, !(grepl("^[0-9]", tetragram)), tetragram:tetra_freq)
tetragram_dt <- tetragram_dt[, c("word1", "word2", "word3", "word4") := tstrsplit(tetragram_dt$tetragram, " ", fixed = TRUE)][]
names(word_dt) <- c("word4_int", "word4")

tetragram_merge <- merge(tetragram_dt, word_dt, by = "word4", sort = FALSE)

tetragram_merge_1 <- subset(tetragram_merge_1, !(word1_int %in% has_to_remove_int), select = c(tetra_freq, word4_int, word3_int, word2_int, word1_int))

tetragram_merge_1 <- subset(tetragram_merge_1, !(word1_int %in% sw_int), select = c(tetra_freq, word4_int, word3_int, word2_int, word1_int))

names(word_dt) <- c("word1_int", "word1")

tetragram_merge_1 <- merge(tetragram_merge_1, word_dt, by = "word1", sort = FALSE)

tetragram_merge_1 <- tetragram_merge_1[, "bigram_int" := as.numeric(paste0(word1_int, word2_int))][]
tetragram_merge_1 <- tetragram_merge_1[, "trigram_int" := as.numeric(paste0(word1_int, word2_int, word3_int))][]
write.csv(tetragram_merge_1, "./Capstone/corpus/MLEModel20/tetragram_full.csv")

tetragram_merge_part_1 <- tetragram_merge_1[1:300000,]
tetragram_merge_part_2 <- tetragram_merge_1[300001:600000,]
tetragram_merge_part_3 <- tetragram_merge_1[600001:900000,]
tetragram_merge_part_4 <- tetragram_merge_1[900001:1218223,]
trigram_for_merge <- subset(trigram_merge_2, select = c(tri_freq, trigram_int, bi_prob, uni_prob, tri_prob))
tetragram_merge_3 <- merge(tetragram_merge_part_1, trigram_for_merge, by = "trigram_int", sort = FALSE)
tetragram_merge_4 <- merge(tetragram_merge_part_2, trigram_for_merge, by = "trigram_int", sort = FALSE)
tetragram_merge_5 <- merge(tetragram_merge_part_3, trigram_for_merge, by = "trigram_int", sort = FALSE)
tetragram_merge_6 <- merge(tetragram_merge_part_4, trigram_for_merge, by = "trigram_int", sort = FALSE)

tetragram_final <- rbind(tetragram_merge_3, tetragram_merge_4, tetragram_merge_5, tetragram_merge_6)

write.csv(tetragram_final, "./Capstone/corpus/MLEModel20/tetragram_full_index.csv")
rm(tetragram_dt, tetragram_merge_6, tetragram_merge_5, tetragram_merge_4, tetragram_merge_3, tetragram_merge_1)
rm(tetragram_merge_part_4, tetragram_merge_part_3, tetragram_merge_part_2, tetragram_merge_part_1)

tetragram_final <- subset(tetragram_final, select = c(trigram_int, word4_int, tetra_freq, tri_freq,tri_prob, bi_prob, uni_prob))
tetragram_final <- tetragram_final[, "tetra_prob" := tetra_freq/tri_freq]
tetragram_final <- tetragram_final[, "smooth_prob" := (0.4 * tetra_prob) + (0.3 *tri_prob) + (0.2 *bi_prob) + (0.1 * uni_prob)]

write.csv(tetragram_final, "./Capstone/corpus/MLEModel20/tetragram_full_prob.csv")

tetragram_dt <- subset(tetragram_final, select= c(trigram_int, word4_int, tetra_prob))
trigram_dt <- subset(trigram_merge_2, select= c(bigram_int, word3_int, tri_prob))
bigram_final <- bigram_dt
bigram_dt <- subset(bigram_final, select = c(word1_int, word2_int, bi_prob))
names(bigram_dt) <- c("unigram_int", "word", "prob")
names(trigram_dt) <- c("bigram_int", "word", "prob")
names(tetragram_dt) <- c("trigram_int", "word", "prob")
bigram_dt <- setorder(bigram_dt, -prob)
trigram_dt <- setorder(trigram_dt, -prob)
tetragram_dt <- setorder(tetragram_dt, -prob)

bigram_top5_dt <- na.omit(setorder(setkey(bigram_dt, unigram_int), unigram_int, -prob)[, .SD[1:5], by=unigram_int], 
                          cols = "word")
trigram_top5_dt <- na.omit(setorder(setkey(trigram_dt, bigram_int), bigram_int, -prob)[, .SD[1:5], by=bigram_int], cols = "word")
tetragram_top5_dt <- na.omit(setorder(setkey(tetragram_dt, trigram_int), trigram_int, -prob)[, .SD[1:5], by=trigram_int], cols = "word")

write.csv(bigram_top5_dt, "./Capstone/corpus/MLEModel20/bigram_top5_for_MLE.csv")
write.csv(trigram_top5_dt, "./Capstone/corpus/MLEModel20/trigram_top5_for_MLE.csv")
write.csv(tetragram_top5_dt, "./Capstone/corpus/MLEModel20/tetragram_top5_for_MLE.csv")

bigram_dt <- bigram_top5_dt
trigram_dt <- trigram_top5_dt
tetragram_dt <- tetragram_top5_dt
