unigram_dt <- fread("./Capstone/corpus/MLEModel/unigram_clean.csv", sep = ",", select = c(2,3,4), stringsAsFactors = FALSE)
bigram_dt <- fread("./Capstone/corpus/MLEModel_20/bigram_full.csv", sep = ",",stringsAsFactors = FALSE)
trigram_dt <- fread("./Capstone/corpus/MLEModel/trigram_clean.csv", sep = ",", stringsAsFactors = FALSE)

uni_for_merge <- subset(uni_final, select = c(word1, uni_freq, word1_int))
write.csv(uni_final, "./Capstone/corpus/MKNModel/unigram_full.csv")

bigram_merge <- bigram_merge[, word2_freq := .N, by = word2]
bigram_merge <- bigram_merge[, word1_freq := .N, by = word1]  # .N means Number of items or elements

bigram_merge <- merge(bigram_merge, uni_for_merge, by = "word1", sort = FALSE)
bigram_merge <- na.omit(bigram_merge, by = "uni_freq")

bigram_merge <- merge(bigram_merge, word_dt, by = "word2", sort = FALSE)


N1 = nrow(subset(bigram_merge, bi_freq == 1, select = bi_freq))  # Number of words that has frequency 1
N2 = nrow(subset(bigram_merge, bi_freq == 2, select = bi_freq))  # Number of words that has frequency 2
Y = N1/(N1 + 2 *N2)
D1 <- 1-(2 * Y * N2/N1)

bigram_merge <- bigram_merge[, prob_MLE := bi_freq/uni_freq][]
bigram_merge <- bigram_merge[, prob_MKN := (bi_freq - D1)/uni_freq + (D1/uni_freq)*word1_freq *word2_freq/sum(bi_freq)][]

bigram_merge <- subset(bigram_merge, select = c(word1, word2, bigram, bi_freq, word1_int, word2_int, uni_freq, prob_MLE, prob_MKN))

write.csv(bigram_merge, "./Capstone/corpus/MKNModel/bigram_full.csv")

uni_for_merge_2 <- subset(uni_final, select = c(word1, uni_prob))


bigram_merge <- merge(bigram_merge, uni_for_merge_2, by = "word1", sort = FALSE)

bigram_merge <- bigram_merge[, smooth_MLE := 0.75 * prob_MLE + 0.25 * uni_prob]

bigram_merge <- bigram_merge[, smooth_MKN := 0.75 * prob_MKN + 0.25 * uni_prob]

write.csv(bigram_merge, "./Capstone/corpus/MKNModel/bigram_full.csv")

bigram_merge <- bigram_merge[, "bigram_int" := as.numeric(paste0(word1_int, word2_int))][]

# for merging the bigrams with the trigrams subset the bigram_merge with bi_freq, bigram_int
bigram_for_merge <- subset(bigram_merge, select = c(bigram, bi_freq, bigram_int, prob_MLE, uni_freq, uni_prob))

bigram_MLE <- subset(bigram_merge, select = c(word1_int, word2_int, prob_MLE, smooth_MLE))

# creating final bigram set for model

has_to_remove <- names(uni_dict)[nchar(names(uni_dict)) == 2]
has_to_remove <- has_to_remove[-c(22, 23)] # words "us" and "go" and not in stopwords
has_to_remove <- c(has_to_remove, "wtju", "adhd")
has_to_remove_int <- uni_dict[has_to_remove]
sw <- stopwords("english")
sw_int <- uni_dict[sw]

#MLE Model:
bigram_final <- subset(bigram_merge, !(word2_int %in% sw_int), select = c(word1_int,word2_int, prob_MLE, smooth_MLE))
bigram_final <- subset(bigram_final, !(word2_int %in% has_to_remove_int), select = c(word1_int,word2_int, prob_MLE, smooth_MLE))

names(bigram_final) <- c("unigram_int", "word", "prob_MLE", "smooth_MLE")
bigram_MLE <- subset(bigram_final, select = c(unigram_int, word, prob_MLE))

bigram_dt_MLE <- na.omit(setorder(setkey(bigram_MLE, unigram_int), unigram_int, -prob_MLE)[, .SD[1:5], by=unigram_int], cols = "word")

write.csv(bigram_dt_MLE, "./Capstone/corpus/MKNModel/bigram_MLE_Final.csv")


bigram_smooth_MLE <- subset(bigram_final, select = c(unigram_int, word, smooth_MLE))

bigram_smooth_dt <- na.omit(setorder(setkey(bigram_smooth_MLE, unigram_int), unigram_int, -smooth_MLE)[, .SD[1:3], by=unigram_int], cols = "word")


write.csv(bigram_smooth_dt, "./Capstone/corpus/MKNModel/bigram_MLESmooth_Final.csv")

# MKN Model:

bigram_final <- subset(bigram_merge, !(word2_int %in% sw_int), select = c(word1_int,word2_int, prob_MKN, smooth_MKN))
bigram_final <- subset(bigram_final, !(word2_int %in% has_to_remove_int), select = c(word1_int,word2_int, prob_MKN, smooth_MKN))

names(bigram_final) <- c("unigram_int", "word", "prob_MKN", "smooth_MKN")
bigram_MKN <- subset(bigram_final, select = c(unigram_int, word, prob_MKN))
bigram_smooth_MKN <- subset(bigram_final, select = c(unigram_int, word, smooth_MKN))


bigram_MKN_dt <- na.omit(setorder(setkey(bigram_MKN, unigram_int), unigram_int, -prob_MKN)[, .SD[1:5], by=unigram_int], cols = "word")

bigram_smoothMKN_dt <- na.omit(setorder(setkey(bigram_smooth_MKN, unigram_int), unigram_int, -smooth_MKN)[, .SD[1:5], by=unigram_int], cols = "word")

write.csv(bigram_MKN_dt, "./Capstone/corpus/MKNModel/bigram_MKN_Final.csv")

write.csv(bigram_smoothMKN_dt, "./Capstone/corpus/MKNModel/bigram_MKNSmooth_Final.csv")


# Trigram

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
write.csv(trigram_merge, "./Capstone/corpus/MKNModel/trigram_full.csv")
names(trigram_merge)[7] <- "bi_prob_MLE"
trigram_for_merge <- subset(trigram_merge, select = c(trigram_int, trigram, tri_freq, tri_prob_MLE, bi_prob_MLE, uni_prob, bi_freq, uni_freq))
trigram_dt <- subset(trigram_merge, select = c(bigram_int, word3_int, tri_prob_MLE, bi_prob_MLE, uni_prob))
trigram_dt <- trigram_dt[, "smooth_prob" := 0.58 * tri_prob_MLE + 0.28 * bi_prob_MLE + 0.14 * uni_prob][]

# trimming trigram for MLE Model

trigram_MLE <- subset(trigram_dt, !(word3_int %in% sw_int), select = c(word3_int,bigram_int, tri_prob_MLE))
trigram_MLE <- subset(trigram_MLE, !(word3_int %in% has_to_remove_int), select = c(bigram_int,word3_int, tri_prob_MLE))

write.csv(trigram_MLE, "./Capstone/corpus/MKNModel/trigram_MLE_full.csv")
names(trigram_MLE) <- c("bigram_int", "word", "prob_MLE")
trigram_dt_MLE <- na.omit(setorder(setkey(trigram_MLE, bigram_int), bigram_int, -prob_MLE)[, .SD[1:3], by=bigram_int], cols = "word")

write.csv(trigram_dt_MLE, "./Capstone/corpus/MKNModel/trigram_MLE_Final.csv")

trigram_smooth_MLE <- subset(trigram_dt, !(word3_int %in% sw_int), select = c(word3_int,bigram_int, smooth_prob))
trigram_smooth_MLE <- subset(trigram_smooth_MLE, !(word3_int %in% has_to_remove_int), select = c(bigram_int,word3_int, smooth_prob))

write.csv(trigram_smooth_MLE, "./Capstone/corpus/MKNModel/trigram_smooth_MLE_full.csv")
names(trigram_smooth_MLE) <- c("bigram_int", "word", "smooth_MLE")
trigram_smooth_MLE <- na.omit(setorder(setkey(trigram_smooth_MLE, bigram_int), bigram_int, -smooth_MLE)[, .SD[1:3], by=bigram_int], cols = "word")

write.csv(trigram_smooth_MLE, "./Capstone/corpus/MKNModel/trigram_smooth_MLE_Final.csv")

# MKN Smoothing

N_tri <- tabulate(trigram_merge$tri_freq)
D <- N_tri[1]/ (N_tri[1] +2*N_tri[2])
N_bi <- sum(bigram_for_merge$bi_freq)

# counting number of times each w2w3 sequence appeared in trigram data
trigram_merge <- trigram_merge[, "N_W1W2" := .N, by = bigram]
names(trigram_merge)[13] <- "w1w2_freq"
trigram_merge <- trigram_merge[, "N_W3" := .N, by = word3]  # .N means Number of items or elements
trigram_sub <- subset(trigram_merge, select = trigram)
rm(trigram_dt, trigram_MLE)

trigram_sub <- trigram_sub[, c("word1", "word2", "word3") := tstrsplit(trigram_sub$trigram, " ", fixed = TRUE)][]

trigram_sub <- trigram_sub[, "bigram" := paste(word2, word3)][]
trigram_sub <- subset(trigram_sub, select = c(trigram, bigram, word3, word2))
trigram_sub <- merge(trigram_sub, bigram_for_merge, by = "bigram", sort = FALSE)
trigram_sub <- trigram_sub[, w3_freq := .N, by = word3]
trigram_sub <- trigram_sub[, w2_freq := .N, by = word2]
names(trigram_sub) <- c("w2w3bigram", "trigram", "word3", "wrod2","w2w3_freq", "w2w3_int", "w2w3_prob", "w2_freq", "w2_prob", "N_W3", "N_W2")
trigram_sub <- subset(trigram_sub, select = c("trigram", "w2w3_freq", "w2w3_int", "w2_freq", "N_W3", "N_W2"))
sum_bigram_freq <- sum(bigram_for_merge$bi_freq)
trigram_sub <- trigram_sub[, "MKN_part2" := ((w2w3_freq - D1)/w2_freq +(D1/w2_freq* N_W2 * N_W3/sum_bigram_freq))]
trigram_sub_1 <- subset(trigram_sub, select = c(trigram, MKN_part2))
trigram_sub_2 <- subset(trigram_merge, select = c("tri_freq", "bi_freq", "w1w2_freq"))
names(trigram_sub_2) <- c("tri_freq", "w1w2_freq", "N_W1W2")
trigram_sub_2 <- trigram_sub_2[, "MKN_part1" := (tri_freq-D)/w1w2_freq + D/w1w2_freq * N_W1W2]


trigram_merge <- trigram_merge[, "MKN_part1" := trigram_sub_2$MKN_part1][]
trigram_sub_3 <- subset(trigram_merge, select = c(trigram, MKN_part1))
trigram_sub_3 <- merge(trigram_sub_3, trigram_sub_1, by = "trigram", sort = FALSE)
trigram_sub_3 <- trigram_sub_3[, "prob_MKN" := MKN_part1 * MKN_part2][]
trigram_sub_3 <- subset(trigram_sub_3, select = c(trigram, prob_MKN))
rm(trigram_sub, trigram_sub_1, trigram_sub_2)
trigram_merge <- merge(trigram_merge, trigram_sub_3, by = "trigram", sort = FALSE)
write.csv(trigram_merge, "./Capstone/corpus/MKNModel/trigram_MKN_Full.csv")
trigram_for_MKN <- subset(trigram_merge, select = c("bigram_int", "word3_int", "prob_MKN"))
write.csv(trigram_for_MKN, "./Capstone/corpus/MKNModel/trigram_MKN_Full_for_Model.csv")
trigram_for_smoothing <- subset(trigram_merge, select = c("bigram", "bigram_int", "word3_int", "prob_MKN"))
rm(trigram_merge, trigram_sub_3)
#trimming

trigram_for_MKN <- subset(trigram_for_MKN, !(word3_int %in% sw_int), select = c(word3_int,bigram_int, prob_MKN))
trigram_for_MKN <- subset(trigram_for_MKN, !(word3_int %in% has_to_remove_int), select = c(bigram_int,word3_int, prob_MKN))
names(trigram_for_MKN) <- c("bigram_int", "word", "prob_MKN")
trigram_for_MKN <- na.omit(setorder(setkey(trigram_for_MKN, bigram_int), bigram_int, -prob_MKN)[, .SD[1:3], by=bigram_int], cols = "word")
write.csv(trigram_for_MKN, "./Capstone/corpus/MKNModel/trigram_MKN_final_for_Model.csv")

trigram_merge <- fread("./Capstone/corpus/MKNModel/trigram_full.csv", stringsAsFactors = FALSE)
