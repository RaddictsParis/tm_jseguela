
####################################
###  Clustering de textes avec R ###
####################################

###
###  author: Julie Seguela, Data Scientist chez Keyrus
###  date: 29 janvier 2015
###


###
### Les textes
###


donnees <- read.csv2("data/lsa_abstracts.csv",  header = T, 
                     stringsAsFactors = F)

donnees[1, c("Title", "Year", "Journal")]
donnees[1, "Abstract"]


###
### Découpage en phrases
###


require(tm)
require(openNLP)
sa <- Maxent_Sent_Token_Annotator(language="en", model=NULL)

vsubstr <- Vectorize(substr, vectorize.args=c("start", "stop"))

phrases <- data.frame()
for (i in 1:nrow(donnees)){
  
  annot <- annotate(donnees$Abstract[i], sa)
  df_temp <- data.frame(Title = donnees$Title[i], 
                        Sentence = vsubstr(donnees$Abstract[i], annot$start, annot$end),
                        stringsAsFactors = F)
  phrases <- rbind(phrases, df_temp)
}

phrases[1:3, "Sentence"]



###
### Normalisation des textes
###


# passer en minuscules
phrases$sent <- tolower(phrases$Sentence)

# retirer la ponctuation
phrases$sent <- gsub("[[:punct:]]+", " ", phrases$sent)

# retirer les chiffres
phrases$sent <- gsub("[[:digit:]]+", " ", phrases$sent)


###
### Création du corpus tm
### 

docs <- data.frame(docs = phrases$sent, stringsAsFactors = F)

corp <- VCorpus(DataframeSource(docs), readerControl=list(language="en"))

corp[[1]]


### 
### Filtrage des stopwords
###

myStopwords <- c(stopwords("en"))

# retirer les stopwords
corp <- tm_map(corp, removeWords, myStopwords) 

# fusionner les espaces multiples
corp <- tm_map(corp, stripWhitespace) 

corp[[1]]


###
### Premier nuage de mots
### 


require(wordcloud)

cols <- colorRampPalette(c("lightgrey","blue"))(40)

wordcloud(words = corp, max.words = 40, random.order = F, 
          scale = c(5, 0.5), colors = cols)

###
### Stemming
### 

corpuscopy <- corp
corp_temp <- tm_map(corp, stemDocument)

stemCompletion_mod<-function(x, dict=corpuscopy, completionType="prevalent"){
  words <- unlist(strsplit(as.character(x)," "))
  words <- words[words != ""]
  
  PlainTextDocument(
    stripWhitespace(
      paste(
        stemCompletion(
          words, dictionary=dict, type=completionType
        ),
        sep="", collapse=" "
      )
    )
  )
}


corp_stem <- tm_map(corp_temp, stemCompletion_mod)
names(corp_stem) <- names(corp_temp)
corp[[1]]
corp_temp[[1]]
corp_stem[[1]]


###
### Nuage de mots après stemming
###

require(wordcloud)
cols <- colorRampPalette(c("lightgrey","blue"))(40)

par_init <- par()
par(mfrow = c(1,2))
wordcloud(words = corp, max.words = 30, random.order=FALSE, colors=cols)
wordcloud(words = corp_stem, max.words = 30, random.order=FALSE, colors=cols)


###
### Matrice termes x documents
###

cl <- list(stopwords = FALSE,
           weighting = function(x) weightSMART(x, spec="ltn"), # pondération des termes
           language = "en")

TDM <- TermDocumentMatrix(corp_stem, control=cl)

inspect(TDM[145:155, 7:12])


###
### Termes les plus fréquents
###

findFreqTerms(TDM, lowfreq = 15)


###
### Retrait des termes rares
###

dim(TDM)
TDM <- removeSparseTerms(TDM, 0.95) 
TDM


###
### Corrélations entre termes
###

par(mfrow = c(1,1))
plot(TDM, terms=findFreqTerms(TDM,lowfreq=18), corThreshold=0.25)


###
### LSA
###

require(lsa)
LSA <- lsa(TDM)
names(LSA)
coord_doc <- LSA$dk %*% diag(LSA$sk)
coord_term <- LSA$tk %*% diag(LSA$sk)



###
### LSA : représentation des termes
###

dim1 = 2 ; dim2 = 3
par(par_init)
plot(c(min(coord_term[, dim1])-0.5, max(coord_term[, dim1])+0.5), 
     c(min(coord_term[, dim2])-0.5, max(coord_term[, dim2])+0.5),
     xlab = paste0("Dimension ", dim1) , ylab = paste0("Dimension ", dim2), type = "n")

text(coord_term[, dim1], coord_term[, dim2], labels = dimnames(coord_term)[[1]], cex = 1)
abline(h = 0, v = 0, lty = 2)


###
### Calcul des distances entre documents
### 

require(proxy)

# Distance euclidienne
DIST = proxy::dist(coord_doc[,1:length(LSA$sk)], method = "euclidean") 

as.matrix(DIST)[1:5, 1:5]


###
### Clustering
###

# Classification ascendante hierarchique
# Agrégation par la méthode de Ward
HCLUST = hclust(DIST, method = "ward.D")

# Dendrogramme
plot(HCLUST)


###
### Choix du nombre de groupes
###

n <- nrow(LSA$dk)
plot(1:n, HCLUST$height[n:1], type = "b", 
     xlab = "Nombre de clusters", ylab = "Indice de variance intra")


###
### Coupure
###

k = 8 # nombre de groupes
plot(HCLUST)
rect.hclust(HCLUST, k=k, border="red")


###
### Obtention des clusters
###

clusters <- cutree(HCLUST, k = k)

# Ajout au tableau de données initial
phrases <- merge(phrases, as.data.frame(clusters), by = "row.names")

table(clusters)


###
### Cluster 1 : Information Retrieval
###

phrases$Sentence[phrases$clusters == 1]



###
### Cluster 3 : Matrix SVD
###

phrases$Sentence[phrases$clusters == 3]



###
### Nuage de mots discriminant
###

# Concaténation des documents d'un même cluster
aggreg<-aggregate(sent ~ clusters, 
                  data=phrases, 
                  paste, collapse = " ") 

# Pré-traitements
corp_aggreg <- VCorpus(DataframeSource(aggreg))
myStopwords <- stopwords("en")
corp_aggreg <- tm_map(corp_aggreg, removeWords, myStopwords)

# Stemming
corpuscopy <- corp_aggreg
corp_temp <- tm_map(corp_aggreg, stemDocument)

corp_aggreg_stem <- tm_map(corp_temp, stemCompletion_mod)
names(corp_aggreg_stem) <- names(corp_temp)

# Matrice termes x documents
TDM_aggreg <- TermDocumentMatrix(corp_aggreg_stem) 
tdm.matrix <- as.matrix(TDM_aggreg)
colnames(tdm.matrix) <- aggreg$clusters


comparison.cloud(tdm.matrix, max.words=40, 
                 random.order=FALSE, title.size = 2)



###
### Phrases représentatives
###

typicalSent = function(data, var, LSACoord, clusters, n_comm, file_out = "typical_sent.csv"){
  avgCoord <- aggregate(LSACoord, by = list(clusters = clusters), mean)
  
  tab <- merge(as.data.frame(LSACoord), as.data.frame(clusters), by = "row.names")
  
  temp <- merge(tab, avgCoord, by = "clusters", suffixes = c(".doc", ".bary")) # ajout des coordonnées des barycentres
  
  ind_col_doc <- names(temp)[grep(".doc", names(temp), fixed = T)] # colonnes des coord des docs
  ind_col_bary <-names(temp)[grep(".bary", names(temp), fixed = T)] # colonnes des coord des barycentres
  
  temp$distBary <- apply((temp[, ind_col_doc]-temp[, ind_col_bary])^2, 1, sum) # calcul de la distance euclidienne au barycentre
  
  temp <- temp[order(temp$clusters, temp$distBary), ] # tri par cluster et par distance croissante
  
  # pour conserver les n_comm les plus proches du barycentre = les plus représentatifs
  nb_obs = aggregate(data.frame(count = temp$clusters), by = list(clusters = temp$clusters), length)
  sequence <- do.call(c, lapply(nb_obs$count, function(x) seq(1, x)))
  
  comm <- subset(temp, sequence <= n_comm, select = c(Row.names))
  comm <- subset(data, Row.names %in% comm$Row.names)
  
  comm <- comm[order(comm$clusters),]
  
  # On exporte en CSV
  write.csv2(comm, file = file_out, row.names = F)
  
  return(comm)
}


repr <- typicalSent(data = phrases, 
                    var = "Sentence", 
                    LSACoord = coord_doc, 
                    clusters = clusters, 
                    n_comm = 1, 
                    file_out = "typical_sent.csv")


###
### Synthèse : à quoi sert la LSA
###

repr$Sentence[repr$clusters==5]
repr$Sentence[repr$clusters==4]
repr$Sentence[repr$clusters==8]


###
### Synthèse : en recherche d'information
###

repr$Sentence[repr$clusters==2]
repr$Sentence[repr$clusters==1]


###
### Synthèse : comment ça marche
### 

repr$Sentence[repr$clusters==6]
repr$Sentence[repr$clusters==3]
repr$Sentence[repr$clusters==7]







