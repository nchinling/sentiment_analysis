#Text Analysis
library(quanteda)
library(readtext)


#Data Managemenent
library(stringr)

#Data Visualization
library(ggplot2)
library(gridExtra)

#Install ggrepl
#install.package("ggrepl")



#Set working directory

#Sentiment analysis (also known as opinion mining or emotion AI) refers to the use of natural language processing, 
#text analysis, computational linguistics, and biometrics to systematically identify, extract, quantify, and study affective states 
#and subjective information. 


#Task 1: Load in Data and Turn into a corpus object using quanteda and then 
#Data Management -- Pull out some data from our corpus that we can use for plotting 
#and put into a dataset and put into a dataframe


speeches <- readtext("*.txt")  #astrix means take everything
speeches


speeches_corpus <- corpus(speeches) # Corpus collection of documents
speeches_corpus

as.character(speeches_corpus[4]) # HILLARY CLINTON

typeof(speeches_corpus)

summary(speeches_corpus) # overview information Unique works, tokens words or words, number of sentences


nms <- names(speeches_corpus) # names
nms

Dates <- as.numeric(gsub('[^[:digit:]]','', nms)) #gsub regular expressions
Dates

Party <- str_sub(nms, start= -5, -5) # fifth from end
Party

PNames  <- str_sub(nms, end = -12)
PNames


#Task 2: Let's tokenize the data and apply sentiment dictionary 

speech_tokens <- tokens(speeches_corpus) #turn them into individual words and punctuation
speech_tokens #As we can see here, we now have a list of words

#We proceed to calculate sentiment scores for these messages using the Lexicoder Sentiment Dictionary (LSD2015), 
#which was automatically loaded with quanteda. Positive and Negative language

#We might want to check whether the documents are using positive and negative words first. We can do this using tokens Lookup

toks_lsd <- tokens_lookup(speech_tokens, dictionary =  data_dictionary_LSD2015)
head(toks_lsd)

#Turn this into a document feature matrix DFM 

speeches_sentiment <- dfm(speech_tokens, dictionary = data_dictionary_LSD2015[1:2]) #only taking negative and positive
speeches_sentiment 

#turn into dataframe
sentiment <- convert(speeches_sentiment, "data.frame") 
sentiment 


#Maybe we do not want the counts but rather the share of positive and negative words. 
#We can do this mannually or also using "dfm_weight"

?dfm_weight

dfm.sentiment.prop <- dfm_weight(speeches_sentiment, scheme = "prop") # Can also do as a proportion 
dfm.sentiment.prop <- convert(dfm.sentiment.prop, "data.frame") 
dfm.sentiment.prop


dfm.sentiment.prop 

#Task 3: Let's get plotting! 

#First, lets make some data


info_summary <- data.frame(Dates = Dates, Sentiment.Pos = sentiment$positive, 
                          Sentiment.Neg = sentiment$negative, 
                          Party = Party, 
                          PNames = PNames, 
                          prop_pos = dfm.sentiment.prop$positive, 
                          prop_neg = dfm.sentiment.prop$negative)

head(info_summary) # Now we have our dataset


#

sp <- ggplot(info_summary, aes(x=Dates, y=prop_neg, color=Party)) +
  geom_point() +
  theme_minimal()
sp

#Change the colours so that they make more sense

sp <- sp + scale_color_manual(values=c("blue", "red"))  #But we see that the Dems are red and the Rep are blue, which is the opposite of what we would think
sp #mostly positive
# maybe we want to add the names of the People so we can see who is who

#Install ggrepl
#install.package("ggrepl")

sp <- sp + geom_label_repel(
  label=info_summary$PNames,
  position = position_dodge(width = 1.0))
sp

sp <- sp + theme(legend.position = "none") 
sp

sp <- sp + labs(x = "Year", y = "Proportion Negative Sentiment")
sp

#Task 4 Targeted analysis
# Which candidates use more positive words near words about workers and jobs?
#You can use tokens_select() with window argument to perform more targeted sentiment analysis.

?tokens_keep

#window integer of length 1 or 2; the size of the window of tokens adjacent to pattern that will be selected. 

# get relevant keywords and phrases from dictionary (in this case I used list of synonyms for worker)

workers <- c("employ**", "peopl*", "work*", "job*", "assistant", "cog", "flunk", "subordinate", "underling", "yes-man", 
             "drudge", "grub", "hack", "labor*","toiler", 
             "nine-to-five*", "wage*", "associate", "colleague", "coworker", 
             "temp*")

# only keep tokens specified above and their context of ±10 tokens
toks_workers <- tokens_keep(speech_tokens, pattern = phrase(workers), window = 10) #
toks_workers

toks_workers <- tokens_lookup(toks_workers, dictionary = data_dictionary_LSD2015[1:2])
toks_workers 

#We can see that some have 0 like Dewey 

# create a document document-feature matrix and group it by weeks in 2016

dfmat_work <- dfm(toks_workers)
dfmat_work 
dfmat_work_prop <- dfm_weight(dfmat_work, scheme = "prop") # Can also do as a proportion 
dfmat_work_prop

workers_sentiment <- convert(dfmat_work_prop, "data.frame") 
info_summary$work_positive <- workers_sentiment$positive #Add to our dataset from above


#Task 5: Now lets plot

pf <- ggplot(data=info_summary, aes(x=Dates, y=prop_pos, fill= Party)) +
  geom_bar(stat = "identity", position=position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x = NULL, y = "Proporition") +
  theme_minimal()

pf

#Compared with sentiment around work/employment words

f <- ggplot(data=info_summary, aes(x=Dates, y=work_positive, fill= Party)) +
  geom_bar(stat = "identity",position=position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x = NULL, y = "Proporition") +
  theme_minimal()

f

# Plot together 


grid.arrange(pf, f, ncol=2)


#References 

#Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng, Stefan Müller, and Akitaka Matsuo. (2018) “quanteda: An R package for the quantitative analysis of textual data”. Journal of Open Source Software. 3(30), 774. https://doi.org/10.21105/joss.00774.

