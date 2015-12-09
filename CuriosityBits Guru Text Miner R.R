### -- INSTALL REQUIRED R LIBRARIES -- ###

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("openNLP")
#install.packages("NLP")
#install.packages("rJava")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

### -- LOAD REQUIRED LIBRARIES -- ###

require(tm)
require(SnowballC)
require(wordcloud)
require(RSQLite)
require(DBI)
require(ggplot2)
require(semnet)
require(NLP)
require(openNLP)
require(rJava)
require(grid)
### -- LOAD DATA -- ###
# FOR .CSV FILES
db_csv <- read.csv("file:///C:/Users/xxx/xxxx.csv", header = TRUE) 

# FOR .TXT FILES
fileName <- "file:///C:/Users/XXXX.txt"
conn <- file(fileName,open="r")
db_txt <-readLines(conn) 

#FOR SQLITE DATABASE
db_sqlite <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/xxx.sqlite") 
selected_db <- dbGetQuery(db_sqlite,"SELECT content, retweeted_status,retweet_count, query  FROM mention") 
selected_db_nonRT <- selected_db[selected_db$retweeted_status != "THIS IS A RETWEET --> DOUBLE-CHECK JSON", ] #SELECT NON-RTs
selected_db_rted <- selected_db[selected_db$retweet_count > 1, ] #SELECT RETWEETED TWEETS
dbDisconnect(db_sqlite)

### --- FILTER DATA --- ### 
DB <- selected_db_nonRT[selected_db_nonRT$query == "" 
                         | selected_db_nonRT$query =="" ,]  


## -- APPLY ADVANCED SELECTION CRITERIA -- ##
# ONLY LOOK AT TWEETS STARTING WITH @CANDIDATES' HANDLES
selection_criteria_1 <- grep(pattern = "^@xxxx", x=DB$content, ignore.case = TRUE) 
selection_criteria_2 <- grep(pattern = "^.@xxxx", x=DB$content, ignore.case = TRUE)

# REMOVE UNWANTED TWEETS
remove_criteria_1 <- grep(pattern = "^@xxx said", x=DB$content, ignore.case = TRUE)
remove_criteria_2 <- grep(pattern = "^@xxx says", x=DB$content, ignore.case = TRUE)

# APPLY CRITERIA TO THE DATA
DB <- DB[c(selection_criteria_1, selection_criteria_2),]
DB <- DB[-c(remove_criteria_1, remove_criteria_2), ]

### -- TEXT PREPARATION -- ### 

# OPTION 1: PASTE EVERYTHING TOGETHER INTO ONE SINGLE DOCUMENT
DB_corpus <- paste(DB$content, collapse=" ") #HERE, content is the name of the column where tweets are stored in the SQLite database
DB_corpus <- as.String(combined_DB)

# OPTION 2: TREAT EACH ROW IN THE DATABASE AS A DOCUMENT TO CREATE A DOCUMENT-TREE MATRIX
DB_corpus <- DB$content

#OPTION 3: PLUG IN FRESHLY COLLECTED DATA FROM R

# PART OF SPEECH TAGGING 
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
POS <- NLP::annotate(GOP_corpus, list(DB_corpus, word_token_annotator))

#CREATE CORPUS 
DB_corpus <- VectorSource(DB_corpus)
DB_corpus <- Corpus(DB_corpus)

# CONDUCT TEXT TRANSFORMATION
DB_corpus <- tm_map(DB_corpus, content_transformer(tolower))
DB_corpus <- tm_map(DB_corpus, removePunctuation)
DB_corpus <- tm_map(DB_corpus,stripWhitespace)
DB_corpus <- tm_map(DB_corpus,stemDocument)
DB_corpus <- tm_map(DB_corpus,removeWords,stopwords("english"))
DB_corpus <- tm_map(DB_corpus,removeWords,c("via", "twitter", "retweet", " "))

# CREATE DOCUMENT-TERM MATRIX 
DB_dtm <- DocumentTermMatrix(DB_corpus) # sparse matrix

### -- WORD FREQUENCY PLOT -- ##
DB_frequency <- colSums(DB_dtm)
DB_frequency <- sort(DB_frequency, decreasing=TRUE)
DB_frequency[1:5]

DB_wf = data.frame(term=names(DB_frequency),occurrences=DB_frequency)
DB_wf <- DB_wf[order(-DB_wf$occurrences), ] 
DB_wf <- DB_wf[1:20,]

p_DB <- ggplot(DB_wf)
p_DB <- p_DB + geom_bar(stat="identity", aes(reorder(term, -occurrences), occurrences), fill = "#EE5C42", color = "#EE5C42")
p_DB <- p_DB + theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw() +
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(color="#F0F0F0")) +
  theme(panel.grid.major=element_line(color="#D0D0D0",size=.75)) +
  theme(axis.ticks=element_blank()) +
  theme(legend.position="none") +
  ggtitle("THE 20 MOST FREQUENT WORDS") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,color="#535353",size=16)) +
  ylab("FREQUENCY") +
  xlab("KEYWORDS") +
  theme(axis.text.x=element_text(size=12,color="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,color="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,color="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,color="#535353",face="bold",vjust=-.5)) +
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
p_DB
ggsave(filename="DB_freq.png",dpi=600)

### -- CREATE WORDCLOUD -- ###
DB_words <- names(DB_frequency)
wordcloud(DB_words[1:50], DB_frequency[1:50], colors=brewer.pal(6,"Dark2"))

### -- PRODUCE SEMANTIC NETWORK -- ###
DB_smn <- coOccurenceNetwork(DB_dtm)
V(DB_smn)$name 
DB_smn_degree <- igraph::degree(DB_smn)
DB_smn_Sorted <-sort.int(DB_smn_degree,decreasing=TRUE,index.return=FALSE)
DB_smn_top <- DB_smn_Sorted[1:100]
DB_smn_top_name <- names(DB_smn_top)
DB_network <- induced.subgraph(DB_smn, DB_smn_top_name, impl = "copy_and_delete")
summary(DB_network)

### -- VISUALIZE THE SEMANTIC NETWORK -- ##
glay = layout.fruchterman.reingold(DB_network)
par(bg="gray15", mar=c(1,1,1,1))
plot(DB_network, layout=glay,
     vertex.color="#A80533",
     vertex.frame.color = "#A80533",
     vertex.size=V(DB_network)$freq*0.00005,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color= hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=V(DB_network)$freq*0.003,
     edge.width = 0.1,
     edge.lty = "solid",
     edge.curved = TRUE,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.color= "#A80533")
title("WORD NETWORK",
      cex.main=1, col.main="gray95")

### -- LOOK AT TARGET WORDS -- ###
DB_target_words <- c("isis")
DB_egonet <- neighborhood(DB_smn, order = 1, nodes = DB_target_words, mode = "all",mindist = 0)
DB_egonet_target <- induced.subgraph(DB_smn, DB_egonet[[1]], impl = "create_from_scratch")
DB_egonet_smn_degree <- igraph::degree(DB_egonet_target)
V(DB_egonet_target)$size <- degree(DB_egonet_target)
DB_egonet_smn_Sorted <- sort.int(DB_egonet_smn_degree,decreasing=TRUE,index.return=FALSE)
DB_egonet_smn_top <- DB_egonet_smn_Sorted[1:50]
DB_egonet_target_top <- induced.subgraph(DB_egonet_target, names(DB_egonet_smn_top), impl = "create_from_scratch")

### -- VISUALIZE THE TARGET WORD'S EGO-NETWORK -- ###
glay = layout.fruchterman.reingold(DB_egonet_target_top)
par(bg="gray15", mar=c(1,1,1,1))
plot(DB_egonet_target_top, layout=glay,
     vertex.color="#A80533",
     vertex.frame.color = "#A80533",
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color= hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex= V(DB_egonet_target_top)$freq*0.005,
     edge.width = 0.1,
     edge.lty = "solid",
     edge.curved = TRUE,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.color= "#A80533")
title("THE TARGET WORD NETWORK",
      cex.main=1, col.main="gray95")
