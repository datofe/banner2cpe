#cat official-cpe-dictionary_v2.3.xml | grep '<title xml:lang="en-US">' | awk -F'>' '{print $2}' | awk -F'<' '{print $1}' | tr '\/' ' ' | tr '\,' ' ' | tr '\[' ' ' | tr '\]' ' ' | tr '\(' ' ' | tr '\)' ' ' | tr '\-' ' ' | tr '\_' ' ' | tr '\:' ' ' | tr ' ' '\n' | tr [:upper:] [:lower:] | sort -u  | grep -v '^$' > titles.txt
#comm -23 titles.txt ourStopWords.txt > titlesWithoutStopWords.txt


cleanSentence <- function(s) {
  s <- gsub('[])/,([]', ' ', s)
  s <- gsub('[-_:]', ' ', s)
  s <- tolower(s)
  s <- tm::removeWords(s, tm::stopwords(kind = 'en'))
  s <- gsub('\\s+', ' ', s)
  s <- gsub('^ ', '', s)
  s
}


#' findCPE
#'
#' @param banner 
#'
#' @return
#' @export
#'
#' @examples
findCPE <- function(banner){
  if(exists("dataFrame") == FALSE){
    load(file = 'R/dataFrame.rda')
    load(file = 'R/titlesWordList.rda')
    dataFrame <<- dataFrame
    titlesWordList <<- titlesWordList
  }
  st <- Sys.time()
  print(banner)
  bannerSet <- prepareBanner(banner, titlesWordList)
  print(bannerSet)
  ndf <- NULL
  i <- 0
  lbs <- length(bannerSet)
  rdf <- sapply(bannerSet, function(word){
    print(word)
    factor <- (lbs - i) / lbs
    pattern <- paste('(^| )', word, '( |$)', sep = '')
    dataframeWithWord <- dataFrame[grep(pattern, dataFrame$titles), ]
    tdf <- FindCPEwithWord(bannerSet, dataframeWithWord, factor)
    ndf <<- rbind(ndf, tdf)
    i <<- i + 1
  })
  cutScore <- 0.07
  tmp <- aggregate(ndf$factor, by = list(CPE = ndf$names), FUN = sum)
  tmp <- data.frame(tmp$CPE[tmp$x > cutScore], tmp$x[tmp$x > cutScore])
  colnames(tmp) <- c('cpe', 'factor')
  tmp[order(-tmp$factor), ]
}

FindCPEwithWord <- function(banner, df, factor){
  print(factor)
  percentages <- similarity(banner, df$titlesSet) * factor
  cutScore <- 0
  ndf <- data.frame(names = unlist(df$names)[percentages > cutScore], 
                    factor = percentages[percentages > cutScore], 
                    titles = unlist(df$titles)[percentages > cutScore]
                    )
  ndf <- head(ndf[order(-ndf$factor, ndf$names),], n = 10)
  ndf
}

getFromFile<- function(filePath){
  st <- Sys.time()
  words <- scan(filePath, what = 'character', sep = '\n')
  et <- Sys.time()
  print(et - st)
  sets::as.set(words)
}

loadXML <- function(xmlFile){
  st <- Sys.time()
  doc <- XML::xmlTreeParse(xmlFile, useInternalNodes = TRUE)
  rootNode <- XML::xmlRoot(doc)
  nameSpace <- XML::xmlNamespace(rootNode)
  print('[+] XML loaded')
  et <- Sys.time()
  print(et - st)
  cpeNames <- '/ns:cpe-list/ns:cpe-item'
  names <- XML::xpathApply(rootNode, cpeNames, XML::xmlGetAttr, 'name', namespaces = c(ns = nameSpace))
  print('[+] Names loaded')
  et <- Sys.time()
  print(et - st)
  cpeTitles <- '/ns:cpe-list/ns:cpe-item/ns:title[@xml:lang="en-US"]'
  titles <- XML::xpathSApply(rootNode, cpeTitles, XML::xmlValue, namespaces = c(ns = nameSpace))
  print('[+] Titles loaded')
  et <- Sys.time()
  print(et - st)
  cbind(names, titles)
}

prepareBanner <- function(b, wl) {
  bs <- sentenceToSet(cleanSentence(b))
  r <- bs & wl
  words <- strsplit(cleanSentence(b), ' ')[[1]]
  fs <- NULL
  sapply(words, function(x){
    if(length(sets::as.set(x) & r) == 1){
      fs <<- paste(fs, x)
    }
  })
  sentenceToSet(cleanSentence(fs))
}

prepareDataframe <- function(m) {
  st <- Sys.time()
  t <- sapply(m[, 2], cleanSentence)
  print('[+] Titles ready')
  et <- Sys.time()
  print(et - st)
  ts <- lapply(t, sentenceToSet)
  print('[+] TitlesSet ready')
  et <- Sys.time()
  print(et - st)
  ndf <- as.data.frame(cbind(m[, 1], t, ts))
  colnames(ndf) <- c('names', 'titles', 'titlesSet')
  print('[+] Dataframe ready')
  et <- Sys.time()
  print(et - st)
  ndf
}

#' prepareGlobalVars
#'
#' @return
#' @export
#'
#' @examples
prepareGlobalVars <- function(){
  xmlFile <- 'inst/exdata/official-cpe-dictionary_v2.3.xml'
  matrix <- loadXML(xmlFile)
  dataFrame <- prepareDataframe(matrix)
  titlesWordList <- getFromFile('inst/exdata/titlesWithoutStopWords.txt')
  save(dataFrame, file = 'R/dataFrame.rda')
  save(titlesWordList, file = 'R/titlesWordList.rda')
}

sentenceToSet <- function(sentence) {
  s <- as.character(sentence)
  w <- strsplit(s, ' ')[[1]]
  uw <- unique(w)
  fs <- NULL
  sapply(uw, function(x){
    if(nchar(x) > 1){
      fs <<- paste(fs, x)
    }
  })
  sets::as.cset(strsplit(cleanSentence(fs), ' ')[[1]])
}

similarity <- function(sa, list){
  percentages <- sapply(list, function(sb){
    exactMatch <- length(sa & sb) / length(sa)
    sets::set_similarity(sa, sb, method = 'Jaccard') #* exactMatch 
  })
  as.vector(percentages)
}


