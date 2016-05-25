library(tm)
library(SnowballC)

textfile = './final/en_US/pg1001.txt'
if (!file.exists(textfile)) {
    download.file("http://www.gutenberg.org/ebooks/100.txt.utf-8", destfile = textfile)
}

shakespeare = readLines(textfile)
shakespeare = shakespeare[-(1:173)]
shakespeare = shakespeare[-(124195:length(shakespeare))]
shakespeare = paste(shakespeare, collapse = " ")
shakespeare = strsplit(shakespeare, "<<[^>]*>>")[[1]]
dramatis.personae = grep("Dramatis Personae", shakespeare, ignore.case = TRUE)
shakespeare = shakespeare[-dramatis.personae]

docs = Corpus(VectorSource(shakespeare))

docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removeWords, stopwords("en"))

docs = tm_map(docs, stemDocument)

docs = tm_map(docs, stripWhitespace)


dtm = DocumentTermMatrix(docs)


freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = TRUE)
top100 = freq[ord[1:10]]

findFreqTerms(dtm, 2000)

findAssocs(dtm, "thought", 0.7)


dtmr = DocumentTermMatrix(docs, control = list(wordLength=c(3,20),
                          bounds = list(global = c(3,182))))
fr2 = colSums(as.matrix(dtmr))
order_fr2 = order(fr2, decreasing=TRUE)
top100_2 = fr2[order_fr2[1:100]]
