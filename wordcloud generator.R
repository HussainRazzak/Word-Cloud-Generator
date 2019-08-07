
# Install these packages if you do not have them
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# file_location - where the text file is located
generate_word_cloud <- function(file_location)
{

  # prompt user to choose file from windows explorer  
  file <- file.choose()
  # get name of file which will be used to name the image of the wordcloud
  name_of_text_file <- tools::file_path_sans_ext(basename(file))

  text <- readLines(file)
  # load data as a corpus
  docs <- Corpus(VectorSource(text))
  
  #inspect(docs)
  
  # manipulate text document to remove spaces and special characters
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  # get number of rows from 
  nrows = dtm[["nrow"]]
  m <- as.matrix(dtm[1:nrows,])
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  set.seed(1234)

  file_name = "apple"
  file_location = paste(file_location,name_of_text_file,".png",sep = "")
  
  # save wordcloud as an image
  png(filename=file_location)
    wordcloud(words = d$word, freq = d$freq, min.freq = 3,
              max.words=500, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  dev.off()  
  
}

file_location = "C:/Users/razzakh/Desktop/OneDrive - William Paterson University/MS_ABA/github/"
generate_word_cloud(file_location)




